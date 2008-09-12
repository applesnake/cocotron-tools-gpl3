/* Original in GNU gdb 6.8.50.20080428-cvs,
   modified on 2008-04-28 for xcxdb by Dr. Rolf Jansen. */

/* MI Command Set - stack commands.
   Copyright (C) 2000, 2002, 2003, 2004, 2005, 2007, 2008 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions (a Red Hat company).

   This file is part of GDB.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include "defs.h"
#include "target.h"
#include "frame.h"
#include "value.h"
#include "mix-cmds.h"
#include "ui-out.h"
#include "varobj-mix.h"
#include "symtab.h"
#include "block.h"
#include "stack.h"
#include "dictionary.h"
#include "gdb_string.h"
#include "gdb_regex.h"

/* FIXME: There is no general mi header to put this kind of utility function.*/
extern void mix_report_var_creation (struct ui_out *uiout, struct varobj *var);

/* This regexp pattern buffer is used for the file_list_statics
   and file_list_globals for the filter.  It doesn't look like the
   regexp package has an explicit pattern free, it tends to just reuse
   one buffer.  I don't want to use their global buffer because the
   psymtab->symtab code uses it to do C++ method detection.  So I am going
   to keep a separate one here.  */
struct re_pattern_buffer mix_symbol_filter;

static char *print_values_bad_input_string = 
            "Unknown value for PRINT_VALUES: must be: 0 or \"--no-values\", "
            "1 or \"--all-values\", 2 or \"--simple-values\", "
            "3 or \"--make-varobj\"";

static void list_args_or_locals (int locals, enum print_values values, 
                                 struct frame_info *fi, int all_blocks);

static void print_syms_for_block (struct block *block, 
                                  struct frame_info *fi, 
                                  struct ui_stream *stb,
                                  int locals, 
                                  int consts,
                                  enum print_values values,
                                  struct re_pattern_buffer *filter);

/* Print a list of the stack frames. Args can be none, in which case
   we want to print the whole backtrace, or a pair of numbers specifying
   the frame numbers at which to start and stop the display.
   If the two numbers are equal, a single frame will be displayed.  */
enum mix_cmd_result
mix_cmd_stack_list_frames (char *command, char **argv, int argc)
{
  int frame_low;
  int frame_high;
  int i;
  struct cleanup *cleanup_stack;
  struct frame_info *fi;

  if (argc > 2 || argc == 1)
    error (_("mix_cmd_stack_list_frames: Usage: [FRAME_LOW FRAME_HIGH]"));

  if (argc == 2)
    {
      frame_low = atoi (argv[0]);
      frame_high = atoi (argv[1]);
    }
  else
    {
      /* Called with no arguments, it means we want the whole
         backtrace. */
      frame_low = -1;
      frame_high = -1;
    }

  /* Let's position fi on the frame at which to start the
     display. Could be the innermost frame if the whole stack needs
     displaying, or if frame_low is 0. */
  for (i = 0, fi = get_current_frame ();
       fi && i < frame_low;
       i++, fi = get_prev_frame (fi));

  if (fi == NULL)
    error (_("mix_cmd_stack_list_frames: Not enough frames in stack."));

  cleanup_stack = make_cleanup_ui_out_list_begin_end (uiout, "stack");

  /* Now let;s print the frames up to frame_high, or until there are
     frames in the stack. */
  for (;
       fi && (i <= frame_high || frame_high == -1);
       i++, fi = get_prev_frame (fi))
    {
      QUIT;
      /* Print the location and the address always, even for level 0.
         args == 0: don't print the arguments. */
      print_frame_info (fi, 1, LOC_AND_ADDRESS, 0 /* args */ );
    }

  do_cleanups (cleanup_stack);
  if (i < frame_high)
    error (_("mix_cmd_stack_list_frames: Not enough frames in stack."));

  return MIX_CMD_DONE;
}

enum mix_cmd_result
mix_cmd_stack_info_depth (char *command, char **argv, int argc)
{
  int frame_high;
  int i;
  struct frame_info *fi;

  if (argc > 1)
    error (_("mix_cmd_stack_info_depth: Usage: [MAX_DEPTH]"));

  if (argc == 1)
    frame_high = atoi (argv[0]);
  else
    /* Called with no arguments, it means we want the real depth of the stack.  */
    frame_high = -1;

  for (i = 0, fi = get_current_frame ();
       fi && (i < frame_high || frame_high == -1);
       i++, fi = get_prev_frame (fi))
    QUIT;

  ui_out_field_int (uiout, "depth", i);

  return MIX_CMD_DONE;
}


/* mix_decode_print_values, ARG is the mi standard "print-values"
   argument.  We decode this into an enum print_values.  */
enum print_values
mix_decode_print_values (char *arg)
{
  enum print_values print_values = 0;

  /* APPLE LOCAL: We muck with this a bit.  We set 2 to mean
     PRINT_MAKE_VAROBJ as well as 3 and --make-varobjs. To get
     the "2" behavior you have to explicitly use --simple-values.  */

  if (strcmp (arg, "0") == 0 || strcmp (arg, mix_no_values) == 0)
    print_values = PRINT_NO_VALUES;
  else if (strcmp (arg, "1") == 0 || strcmp (arg, mix_all_values) == 0)
    print_values = PRINT_ALL_VALUES;
  else if (strcmp (arg, "2") == 0)
    print_values = PRINT_MAKE_VAROBJ;
  else if (strcmp (arg, mix_simple_values) == 0)
    print_values = PRINT_SIMPLE_VALUES;
  else if (strcmp (arg, "3") == 0 || strcmp (arg, mix_make_varobjs) == 0)
    print_values = PRINT_MAKE_VAROBJ;
  else
    print_values = PRINT_BAD_INPUT;

  return print_values;
}

/* Print a list of the locals for the current frame. With argument of
   0, print only the names, with argument of 1 print also the values.  */
enum mix_cmd_result
mix_cmd_stack_list_locals (char *command, char **argv, int argc)
{
  struct frame_info *frame;
  enum print_values print_values;
  int all_blocks;

  if (argc < 1 || argc > 2)
    error ("mix_cmd_stack_list_locals: Usage: PRINT_VALUES [ALL_BLOCKS]");

  frame = get_selected_frame (NULL);

  print_values = mix_decode_print_values (argv[0]);
  if (print_values == PRINT_BAD_INPUT)
    error ("%s", print_values_bad_input_string);

  if (argc >= 2)
    all_blocks = atoi (argv[1]);
  else
    all_blocks = 0;

  list_args_or_locals (1, print_values, frame, all_blocks);

  return MIX_CMD_DONE;
}

/* Print a list of the arguments for the current frame. With argument
   of 0, print only the names, with argument of 1 print also the
   values, with argument of 2, create varobj for the arguments. */
enum mix_cmd_result
mix_cmd_stack_list_args (char *command, char **argv, int argc)
{
  int frame_low;
  int frame_high;
  int i;
  enum print_values values;
  struct frame_info *fi;
  struct cleanup *cleanup_stack_args;

  if (argc < 1 || argc > 3 || argc == 2)
    error (_("mix_cmd_stack_list_args: Usage: PRINT_VALUES [FRAME_LOW FRAME_HIGH]"));

  if (argc == 3)
    {
      frame_low = atoi (argv[1]);
      frame_high = atoi (argv[2]);
    }
  else
    {
      /* Called with no arguments, it means we want args for the whole backtrace. */
      frame_low = -1;
      frame_high = -1;
    }

  values = mix_decode_print_values (argv[0]);
  if (values == PRINT_BAD_INPUT)
    error ("%s", print_values_bad_input_string);

  /* Let's position fi on the frame at which to start the
     display. Could be the innermost frame if the whole stack needs
     displaying, or if frame_low is 0. */
  for (i = 0, fi = get_current_frame ();
       fi && i < frame_low;
       i++, fi = get_prev_frame (fi));

  if (fi == NULL)
    error (_("mix_cmd_stack_list_args: Not enough frames in stack."));

  cleanup_stack_args = make_cleanup_ui_out_list_begin_end (uiout, "stack-args");

  /* Now let's print the frames up to frame_high, or until there are frames in the stack. */
  for (;
       fi && (i <= frame_high || frame_high == -1);
       i++, fi = get_prev_frame (fi))
    {
      struct cleanup *cleanup_frame;
      QUIT;
      cleanup_frame = make_cleanup_ui_out_tuple_begin_end (uiout, "frame");
      ui_out_field_int (uiout, "level", i);
      list_args_or_locals (0, values, fi, 0);
      do_cleanups (cleanup_frame);
    }

  do_cleanups (cleanup_stack_args);
  if (i < frame_high)
    error (_("mix_cmd_stack_list_args: Not enough frames in stack."));

  return MIX_CMD_DONE;
}

/* Print a list of the locals or the arguments for the currently
   selected frame.  If the argument passed is 0, print only the names
   of the variables, if an argument of 1 is passed, print the values
   as well. If ALL_BLOCKS == 1, then print the symbols for ALL lexical
   blocks in the function that is in frame FI.  */
static void
list_args_or_locals (int locals, enum print_values values,
                     struct frame_info *fi, int all_blocks)
{
  struct block *block = NULL;
  struct block *containing_block = NULL;
  struct cleanup *cleanup_list;
  static struct ui_stream *stb = NULL;

  stb = ui_out_stream_new (uiout);

  cleanup_list = make_cleanup_ui_out_list_begin_end (uiout, locals ? "locals" : "args");

  if (all_blocks)
    {
      int i;
      CORE_ADDR fstart;
      struct blockvector *bv;
      
      /* CHECK - I assume that the function block in the innermost
         lexical block that starts at the start function of the
         PC.  If this is not correct, then I will have to run through
         the blockvector to match it to the block I get by: */
      fstart = get_pc_function_start (get_frame_pc (fi));
      if (fstart == 0)
        {
          /* Can't find the containing function for this PC.  Sigh... */
          fstart = get_frame_pc (fi);
        }

      bv = blockvector_for_pc (fstart, &containing_block);
      if (bv == NULL)
        {
          /* APPLE LOCAL: Don't error() out here - UIs will promiscuously ask
             for locals/args even in assembly language routines and there's no
             point in displaying an error message to the user.  */
          do_cleanups (cleanup_list);
          ui_out_stream_delete (stb);
          return;
        }

      for (i = 0; i < BLOCKVECTOR_NBLOCKS (bv); i++)
        if (contained_in ((block = BLOCKVECTOR_BLOCK (bv, i)), containing_block))
          print_syms_for_block (block, fi, stb, locals, 1, values, NULL);
    }
  else
    {
      block = get_frame_block (fi, 0);

      while (block != 0)
        {
          print_syms_for_block (block, fi, stb, locals, 1, values, NULL);
          
          if (BLOCK_FUNCTION (block))
            break;
          else
            block = BLOCK_SUPERBLOCK (block);
        }
    }

  do_cleanups (cleanup_list);
  ui_out_stream_delete (stb);
}

/* Print the variable symbols for block BLOCK.  VALUES is the
   print_values enum.

   LOCALS determines what scope of variables to print:
     1 - print locals AND statics.  
     0 - print args.  
    -1 - print statics. 
   CONSTS - whether to print const symbols.  Const pointers are
   always printed anyway.
   STB is the ui-stream to which the results are printed.  
   And FI, if non-null, is the frame to bind the varobj to.  
   If FILTER is non-null, then we only print expressions matching
   that compiled regexp.  */
static void
print_syms_for_block (struct block *block, 
                      struct frame_info *fi, 
                      struct ui_stream *stb,
                      int locals, 
                      int consts,
                      enum print_values values,
                      struct re_pattern_buffer *filter)
{
  int print_me;
  struct symbol *sym;
  struct dict_iterator iter;
  struct ui_stream *error_stb;
  struct cleanup *old_chain;
  
  if (dict_empty (BLOCK_DICT (block)))
    return;

  error_stb = ui_out_stream_new (uiout);
  old_chain = make_cleanup_ui_out_stream_delete (error_stb);

  ALL_BLOCK_SYMBOLS (block, iter, sym)
    {
      print_me = 0;

      /* If this is a const, and we aren't printing consts, then skop this one. 
         However, we always print const pointers, 'cause they are interesting even
         if plain int/char/etc consts aren't.  */

      switch (SYMBOL_CLASS (sym))
        {
        default:
        case LOC_UNDEF:         /* catches errors        */
        case LOC_TYPEDEF:       /* local typedef         */
        case LOC_LABEL:         /* local label           */
        case LOC_BLOCK:         /* local function        */
        case LOC_CONST_BYTES:   /* loc. byte seq.        */
        case LOC_UNRESOLVED:    /* unresolved static     */
        case LOC_OPTIMIZED_OUT: /* optimized out         */
          print_me = 0;
          break;

        case LOC_CONST:         /* constant              */
          if (consts)
            print_me = 1;
          break;

        case LOC_ARG:           /* argument              */
        case LOC_REF_ARG:       /* reference arg         */
        case LOC_REGPARM_ADDR:  /* indirect register arg */
          if (locals == 0)
            print_me = 1;
          break;
          
        case LOC_STATIC:        /* static                */
          if (locals == -1 || locals == 1)
            print_me = 1;
          break;

        case LOC_LOCAL:         /* stack local           */
        case LOC_REGISTER:      /* register              */
        case LOC_COMPUTED:
          if (locals == 1)
            print_me = 1;
          break;
        }

      /* If we were asked not to print consts, make sure we don't.  */
      if (print_me 
          && !consts && (SYMBOL_TYPE (sym) != NULL) 
          && TYPE_CONST (check_typedef (SYMBOL_TYPE (sym)))
          && (!(TYPE_CODE (check_typedef (SYMBOL_TYPE (sym))) == TYPE_CODE_PTR)))
        print_me = 0;
      
      if (print_me)
        {
          struct symbol *sym2;
          int len = strlen (SYMBOL_NATURAL_NAME (sym));

          /* If we are about to print, compare against the regexp.  */
          if (filter && re_search (filter, SYMBOL_NATURAL_NAME (sym), 
                                   len, 0, len, 
                                   (struct re_registers *) 0) >= 0)
            continue;

          if (values == PRINT_NO_VALUES)
            {
              struct cleanup *tuple_cleanup;
              tuple_cleanup = make_cleanup_ui_out_tuple_begin_end (uiout, NULL);
              ui_out_field_string (uiout, "name", SYMBOL_NATURAL_NAME (sym));
              do_cleanups (tuple_cleanup);
              continue;
            }

          if (!locals)
            sym2 = lookup_symbol (SYMBOL_NATURAL_NAME (sym),
                                  block, VAR_DOMAIN,
                                  (int *) NULL);
          else
            sym2 = sym;
          
          if (values == PRINT_MAKE_VAROBJ)
            {
              /* APPLE LOCAL: If you pass an expression with a "::" in
                 it down to parse_expression, it will choke on it.  So
                 we need to add a ' before and after the expression.
                 Only do it if there is a "::" however, just to keep
                 the uglification to a minimum.  */
              struct varobj *new_var;
              struct cleanup *tuple_cleanup, *expr_cleanup;
              char *expr = SYMBOL_NATURAL_NAME (sym2);
              if (strstr (expr, "::") != NULL) 
                {
                  char *tmp;
                  int len = strlen (expr);
                  tmp = xmalloc (len + 3);
                  tmp[0] = '\'';
                  memcpy (tmp + 1, expr, len);
                  tmp[len + 1] = '\'';
                  tmp[len + 2] = '\0';
                  expr = tmp;
                  expr_cleanup = make_cleanup (xfree, expr);
                }
              else
                {
                  expr_cleanup = make_cleanup (null_cleanup, NULL);
                }
              /* END APPLE LOCAL */

              if (fi)
                new_var = mix_varobj_create (mix_varobj_gen_name (), 
                                             expr,
                                             get_frame_base (fi),
                                             block,
                                             USE_BLOCK_IN_FRAME);
              else
                new_var = mix_varobj_create (mix_varobj_gen_name (), 
                                             expr,
                                             0,
                                             block,
                                             NO_FRAME_NEEDED);

              do_cleanups (expr_cleanup);

              /* FIXME: There should be a better way to report an error in 
                 creating a variable here, but I am not sure how to do it,
                 so I will just bag out for now. */

              if (new_var == NULL)
                continue;

              tuple_cleanup = make_cleanup_ui_out_tuple_begin_end (uiout, "varobj");
              ui_out_field_string (uiout, "exp", SYMBOL_NATURAL_NAME (sym));
              if (new_var != NULL)
                {
                  char *value_str;
                  struct ui_file *save_stderr;
                  
                  /* If we are using the varobj's, then print
                     the value as the varobj would. */
                  
                  save_stderr = gdb_stderr;
                  gdb_stderr = error_stb->stream;
                  
                  if (gdb_varobj_get_value (new_var, &value_str))
                    {
                      ui_out_field_string (uiout, "value", value_str);
                    }
                  else
                    {
                      /* FIXME: can I get the error string & put it here? */
                      ui_out_field_stream (uiout, "value", error_stb);
                    }
                  gdb_stderr = save_stderr;
                }
              else
                ui_out_field_skip (uiout, "value");
            
              mix_report_var_creation (uiout, new_var);
              do_cleanups (tuple_cleanup);
            }
          else
            {
              struct cleanup *cleanup_tuple = NULL;
              struct type *type;

              cleanup_tuple =
                make_cleanup_ui_out_tuple_begin_end (uiout, NULL);
              
              ui_out_field_string (uiout, "name", SYMBOL_PRINT_NAME (sym));
              
              switch (values)
                {
                case PRINT_SIMPLE_VALUES:
                  type = check_typedef (sym2->type);
                  type_print (sym2->type, "", stb->stream, -1);
                  ui_out_field_stream (uiout, "type", stb);
                  if (TYPE_CODE (type) != TYPE_CODE_ARRAY
                      && TYPE_CODE (type) != TYPE_CODE_STRUCT
                      && TYPE_CODE (type) != TYPE_CODE_UNION)
                    {
                      print_variable_value (sym2, fi, stb->stream);
                      ui_out_field_stream (uiout, "value", stb);
                    }
                  do_cleanups (cleanup_tuple);
                  break;
                case PRINT_ALL_VALUES:
                  print_variable_value (sym2, fi, stb->stream);
                  ui_out_field_stream (uiout, "value", stb);
                  do_cleanups (cleanup_tuple);
                  break;
                default:
                  internal_error (__FILE__, __LINE__, "Wrong print_values value for this branch.\n");
                }
            }
        }
    }

  do_cleanups (old_chain);
}

enum mix_cmd_result
mix_cmd_stack_select_frame (char *command, char **argv, int argc)
{
  if (argc == 0 || argc > 1)
    error (_("mix_cmd_stack_select_frame: Usage: FRAME_SPEC"));

  select_frame_command (argv[0], 1 /* not used */ );
  return MIX_CMD_DONE;
}

enum mix_cmd_result
mix_cmd_stack_info_frame (char *command, char **argv, int argc)
{
  if (argc > 0)
    error (_("mix_cmd_stack_info_frame: No arguments required"));
  
  print_frame_info (get_selected_frame (NULL), 1, LOC_AND_ADDRESS, 0);
  return MIX_CMD_DONE;
}
