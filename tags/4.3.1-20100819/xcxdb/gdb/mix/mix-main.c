/* Original in GNU gdb 6.8.50.20080428-cvs,
   modified on 2008-04-28 for xcxdb by Dr. Rolf Jansen. */

/* MI Command Set.
   Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2007, 2008 Free Software Foundation, Inc.
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

/* Work in progress.  */

#include <ctype.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/resource.h>
#include <signal.h>            /* for kill() */

#include "defs.h"
#include "target.h"
#include "inferior.h"
#include "gdb_string.h"
#include "exceptions.h"
#include "top.h"
#include "gdbthread.h"
#include "gdbcmd.h"
#include "mix-cmds.h"
#include "mix-parse.h"
#include "mix-getopt.h"
#include "mix-console.h"
#include "ui-out.h"
#include "mix-out.h"
#include "interps.h"
#include "event-loop.h"
#include "event-top.h"
#include "gdbcore.h"            /* For write_memory().  */
#include "value.h"
#include "regcache.h"
#include "gdb.h"
#include "frame.h"
#include "mix-main.h"
#include "language.h"

enum
  {
    FROM_TTY = 0
  };

/* Enumerations of the actions that may result from calling
   captured_mix_execute_command.  */

enum captured_mix_execute_command_actions
  {
    EXECUTE_COMMAND_DISPLAY_PROMPT,
    EXECUTE_COMMAND_SUPPRESS_PROMPT
  };

/* This structure is used to pass information from captured_mix_execute_command
   to mix_execute_command.  */
struct captured_mix_execute_command_args
{
  /* This return result of the MI command (output).  */
  enum mix_cmd_result rc;

  /* What action to perform when the call is finished (output).  */
  enum captured_mix_execute_command_actions action;

  /* The command context to be executed (input).  */
  struct mix_parse *command;
};

struct mix_continuation_arg
{
  char *token;
  struct mix_timestamp *timestamp;
  struct cleanup *cleanups;
  struct cleanup *exec_error_cleanups;
};

/* Points to the current interpreter, used by the mi context callbacks.  */
// extern struct interp *mi_interp;

struct mix_continuation_arg * mix_setup_continuation_arg (struct cleanup *cleanups);
static void free_mix_continuation_arg (struct mix_continuation_arg *arg);

int mix_debug_p;
struct ui_file *raw_stdout;

/* A pointer to the current mi_parse's command token.  This is needed
 because we can't pass the token down to the mi command levels.  This
 will get cleaned up once captured_mi_execute_command finishes, so 
 if you need it for a continuation, dup it.  */
char *current_command_token;

/* This is used to pass the current command timestamp
   down to continuation routines.  */
static struct mix_timestamp *current_command_ts;

static int do_timings = 1;

static enum mix_cmd_result mix_cmd_execute (struct mix_parse *parse);

static void mix_execute_cli_command (const char *cmd, int args_p, const char *args);
static enum mix_cmd_result mix_execute_async_cli_command (char *cli_command, char *args, int from_tty);

void mix_exec_error_cleanup (void *in_arg);
static void mix_exec_async_cli_cmd_continuation (void *continuation_arg);

static int register_changed_p (int regnum, struct regcache *, struct regcache *);
static void get_register (int regnum, int format);

static int mix_command_completes_while_target_executing (char *command);
static void timestamp (struct mix_timestamp *tv);
static void print_diff_now (struct mix_timestamp *start);
static void copy_timestamp (struct mix_timestamp *dst, struct mix_timestamp *src);

static void print_diff (struct mix_timestamp *start, struct mix_timestamp *end);
static long wallclock_diff (struct mix_timestamp *start, struct mix_timestamp *end);
static long user_diff (struct mix_timestamp *start, struct mix_timestamp *end);
static long system_diff (struct mix_timestamp *start, struct mix_timestamp *end);

/* Longjmp-safe wrapper for "execute_command".  */
static struct gdb_exception safe_execute_command (struct ui_out *uiout, char *command, int from_tty);

 /* When running a synchronous target, we would like to have interpreter-exec
    give the same output as for an asynchronous one.  Use this to tell us that
    it did run so we can fake up the output.  */
int mix_interp_exec_cmd_did_run;

/* Command implementations.  FIXME: Is this libgdb?  No.  This is the MI
   layer that calls libgdb.  Any operation used in the below should be
   formalized.  */

enum mix_cmd_result
mix_cmd_gdb_exit (char *command, char **argv, int argc)
{
  /* We have to print everything right here because we never return.  */
  if (current_command_token)
    fputs_unfiltered (current_command_token, raw_stdout);
  fputs_unfiltered ("^exit\n", raw_stdout);
  mix_out_put (uiout, raw_stdout);
  /* FIXME: The function called is not yet a formal libgdb function.  */
  quit_force (NULL, FROM_TTY);
  return MIX_CMD_DONE;
}


#define partial_path_to_executable_strify(x) "/Contents/"#x"/"
#define partial_path_to_executable_string(x) partial_path_to_executable_strify(x)
#define exec_suffix_strify(x) #x
#define exec_suffix_string(x) exec_suffix_strify(x)

enum mix_cmd_result
mix_cmd_file_exec_symbols (char *command, char **argv, int argc)
{
  int i, l = strlen(argv[0]);
  if (argv[0][l-4] == '.' && argv[0][l-3] == 'a' && argv[0][l-2] == 'p' && argv[0][l-1] == 'p')
    {
      char *bundle_part_path = partial_path_to_executable_string(BUNDLE_TARGET);
      char *bundle_name = strrchr(argv[0], '/');
      char *exec_suffix = exec_suffix_string(EXEC_SUFFIX);
      int  lpart = strlen(bundle_part_path);
      int  lbndl = strlen(bundle_name) - 4;
      int  lexec = strlen(exec_suffix);

      char *p, *q;
      bundle_name = (bundle_name) ? bundle_name + 1 : argv[0];
      argv[0] = realloc(argv[0], l + lpart + lbndl + lexec + 1);
      p = strcat(argv[0], bundle_part_path) + l + lpart;
      q = bundle_name;
      while (*q && *q != '.')
        *p++ = *q++;
      q = exec_suffix;
      while (*q)
        *p++ = *q++;
      *p = '\0';

      l += lpart + lbndl + lexec;
    }

  for (i = 1; i < argc; i++)
    {
      argv[0] = realloc(argv[0], (l += strlen(argv[i])) + 1);
      argv[0] =  strcat(argv[0], argv[1]);
    }
  return mix_execute_async_cli_command ("file", argv[0], 0);
}

enum mix_cmd_result
mix_cmd_mi_no_op (char *command, char **argv, int argc)
{
  /* how does one know when a bunch of MI commands have finished being processed?
     just send a no-op as the last command and look for that... */
  return MIX_CMD_DONE;
}

enum mix_cmd_result
mix_cmd_pid_info (char *command, char **argv, int argc)
{
  if (argc != 0)
    error ("mix_cmd_pid_info: Usage: -pid-info");

  else if (!target_has_execution)
    error ("mix_cmd_pid_info: the program being debugged is not running.");

  ui_out_text (uiout, "Inferior has process ID ");
  ui_out_field_int (uiout, "process-id", PIDGET (inferior_ptid));
  ui_out_text (uiout, ".\n");
  ui_out_flush (uiout);

  return MIX_CMD_DONE;
}


enum mix_cmd_result
mix_cmd_exec_run (char *args, int from_tty)
{
  /* FIXME: Should call a libgdb function, not a cli wrapper.  */
  if (target_has_execution)
    return mix_execute_async_cli_command ("continue", args, from_tty);
  else
    return mix_execute_async_cli_command ("run", args, from_tty);
}

enum mix_cmd_result
mix_cmd_exec_next (char *args, int from_tty)
{
  /* FIXME: Should call a libgdb function, not a cli wrapper.  */
  return mix_execute_async_cli_command ("next", args, from_tty);
}

enum mix_cmd_result
mix_cmd_exec_next_instruction (char *args, int from_tty)
{
  /* FIXME: Should call a libgdb function, not a cli wrapper.  */
  return mix_execute_async_cli_command ("nexti", args, from_tty);
}

enum mix_cmd_result
mix_cmd_exec_step (char *args, int from_tty)
{
  /* FIXME: Should call a libgdb function, not a cli wrapper.  */
  return mix_execute_async_cli_command ("step", args, from_tty);
}

enum mix_cmd_result
mix_cmd_exec_step_instruction (char *args, int from_tty)
{
  /* FIXME: Should call a libgdb function, not a cli wrapper.  */
  return mix_execute_async_cli_command ("stepi", args, from_tty);
}

enum mix_cmd_result
mix_cmd_exec_finish (char *args, int from_tty)
{
  /* FIXME: Should call a libgdb function, not a cli wrapper.  */
  return mix_execute_async_cli_command ("finish", args, from_tty);
}

enum mix_cmd_result
mix_cmd_exec_until (char *args, int from_tty)
{
  /* FIXME: Should call a libgdb function, not a cli wrapper.  */
  return mix_execute_async_cli_command ("until", args, from_tty);
}

enum mix_cmd_result
mix_cmd_exec_return (char *args, int from_tty)
{
  /* This command doesn't really execute the target, it just pops the
     specified number of frames. */
  if (*args)
    /* Call return_command with from_tty argument equal to 0 so as to
       avoid being queried.  */
    return_command (args, 0);
  else
    /* Call return_command with from_tty argument equal to 0 so as to
       avoid being queried.  */
    return_command (NULL, 0);

  /* Because we have called return_command with from_tty = 0, we need
     to print the frame here.  */
  print_stack_frame (get_selected_frame (NULL), 1, LOC_AND_ADDRESS);

  return MIX_CMD_DONE;
}

enum mix_cmd_result
mix_cmd_exec_continue (char *args, int from_tty)
{
  /* FIXME: Should call a libgdb function, not a cli wrapper.  */
  return mix_execute_async_cli_command ("continue", args, from_tty);
}

/* Interrupt the execution of the target.  Note how we must play around
   with the token variables, in order to display the current token in
   the result of the interrupt command, and the previous execution
   token when the target finally stops.  See comments in
   mix_cmd_execute.  */
enum mix_cmd_result
mix_cmd_exec_interrupt (char *args, int from_tty)
{
  if (!is_running (inferior_ptid))
    error ("mix_cmd_exec_interrupt: Inferior not executing.");

  interrupt_target_command (args, from_tty);
  if (current_command_token)
    fputs_unfiltered (current_command_token, raw_stdout);
  fputs_unfiltered ("^done", raw_stdout);
  mix_out_put (uiout, raw_stdout);
  mix_out_rewind (uiout);
  fputs_unfiltered ("\n", raw_stdout);
  return MIX_CMD_QUIET;
}

enum mix_cmd_result
mix_cmd_exec_status (char *command, char **argv, int argc)
{
  char *status;

  if (argc != 0)
    error ("mix_cmd_exec_status takes no arguments.");

  if (!target_has_execution)
    status = "not executing";
  else
    if (is_running (inferior_ptid))
      /* FIXME: The result reporting needs to be better 
         centralized.  The "^" for done and the result code
         should all come from one place, rather than being
         scattered throughout the code.  But untill this happens,
         both this command and exec-interrupt will have to play
         games to get their returns out properly... */
      {
        status = "running";
        fputs_unfiltered ("^done", raw_stdout);
        ui_out_field_string (uiout, "status", status);
        mix_out_put (uiout, raw_stdout);
        mix_out_rewind (uiout);
        fputs_unfiltered ("\n", raw_stdout);
        return MIX_CMD_DONE;
      }
    else
      status = "stopped";

  ui_out_field_string (uiout, "status", status);
  
  return MIX_CMD_DONE;
}


enum mix_cmd_result
mix_cmd_thread_select (char *command, char **argv, int argc)
{
  enum gdb_rc rc;
  char *mix_error_message;

  if (argc != 1)
    error ("mix_cmd_thread_select: USAGE: threadnum.");

  rc = gdb_thread_select (uiout, argv[0], &mix_error_message);

  if (rc == GDB_RC_FAIL)
    {
      make_cleanup (xfree, mix_error_message);
      error ("%s", mix_error_message);
    }

  return MIX_CMD_DONE;
}

enum mix_cmd_result
mix_cmd_thread_list_ids (char *command, char **argv, int argc)
{
  enum gdb_rc rc;
  char *mix_error_message;

  if (argc != 0)
    error ("mix_cmd_thread_list_ids: No arguments required.");

  rc = gdb_list_thread_ids (uiout, &mix_error_message);

  if (rc == GDB_RC_FAIL)
    {
      make_cleanup (xfree, mix_error_message);
      error ("%s", mix_error_message);
    }

  return MIX_CMD_DONE;
}

enum mix_cmd_result
mix_cmd_thread_info (char *command, char **argv, int argc)
{
  int thread = -1;
  
  if (argc != 0 && argc != 1)
    error ("Invalid MI command");

  if (argc == 1)
    thread = atoi (argv[0]);

  print_thread_info (uiout, thread);
  return MIX_CMD_DONE;
}

enum mix_cmd_result
mix_cmd_data_list_register_names (char *command, char **argv, int argc)
{
  int regnum, numregs;
  int i;
  struct cleanup *cleanup;

  /* Note that the test for a valid register must include checking the
     gdbarch_register_name because gdbarch_num_regs may be allocated for
     the union of the register sets within a family of related processors.
     In this case, some entries of gdbarch_register_name will change depending
     upon the particular processor being debugged.  */

  numregs = gdbarch_num_regs (current_gdbarch)
            + gdbarch_num_pseudo_regs (current_gdbarch);

  cleanup = make_cleanup_ui_out_list_begin_end (uiout, "register-names");

  if (argc == 0)                /* No args, just do all the regs.  */
    {
      for (regnum = 0;
           regnum < numregs;
           regnum++)
        {
          if (gdbarch_register_name (current_gdbarch, regnum) == NULL
              || *(gdbarch_register_name (current_gdbarch, regnum)) == '\0')
            ui_out_field_string (uiout, NULL, "");
          else
            ui_out_field_string (uiout, NULL,
                                 gdbarch_register_name
                                   (current_gdbarch, regnum));
        }
    }

  /* Else, list of register #s, just do listed regs.  */
  for (i = 0; i < argc; i++)
    {
      regnum = atoi (argv[i]);
      if (regnum < 0 || regnum >= numregs)
        error ("bad register number");

      if (gdbarch_register_name (current_gdbarch, regnum) == NULL
          || *(gdbarch_register_name (current_gdbarch, regnum)) == '\0')
        ui_out_field_string (uiout, NULL, "");
      else
        ui_out_field_string (uiout, NULL,
                             gdbarch_register_name (current_gdbarch, regnum));
    }
  do_cleanups (cleanup);
  return MIX_CMD_DONE;
}

enum mix_cmd_result
mix_cmd_data_list_changed_registers (char *command, char **argv, int argc)
{
  static struct regcache *this_regs = NULL;
  struct regcache *prev_regs;
  int regnum, numregs, changed;
  int i;
  struct cleanup *cleanup;

  /* The last time we visited this function, the current frame's register
     contents were saved in THIS_REGS.  Move THIS_REGS over to PREV_REGS,
     and refresh THIS_REGS with the now-current register contents.  */

  prev_regs = this_regs;
  this_regs = frame_save_as_regcache (get_selected_frame (NULL));
  cleanup = make_cleanup_regcache_xfree (prev_regs);

  /* Note that the test for a valid register must include checking the
     gdbarch_register_name because gdbarch_num_regs may be allocated for
     the union of the register sets within a family of related processors.
     In this  case, some entries of gdbarch_register_name will change depending
     upon the particular processor being debugged.  */

  numregs = gdbarch_num_regs (current_gdbarch)
            + gdbarch_num_pseudo_regs (current_gdbarch);

  make_cleanup_ui_out_list_begin_end (uiout, "changed-registers");

  if (argc == 0)                /* No args, just do all the regs.  */
    {
      for (regnum = 0;
           regnum < numregs;
           regnum++)
        {
          if (gdbarch_register_name (current_gdbarch, regnum) == NULL
              || *(gdbarch_register_name (current_gdbarch, regnum)) == '\0')
            continue;
          changed = register_changed_p (regnum, prev_regs, this_regs);
          if (changed < 0)
            error ("mix_cmd_data_list_changed_registers: Unable to read register contents.");
          else if (changed)
            ui_out_field_int (uiout, NULL, regnum);
        }
    }

  /* Else, list of register #s, just do listed regs.  */
  for (i = 0; i < argc; i++)
    {
      regnum = atoi (argv[i]);

      if (regnum >= 0
          && regnum < numregs
          && gdbarch_register_name (current_gdbarch, regnum) != NULL
          && *gdbarch_register_name (current_gdbarch, regnum) != '\000')
        {
          changed = register_changed_p (regnum, prev_regs, this_regs);
          if (changed < 0)
            error ("mix_cmd_data_list_register_change: Unable to read register contents.");
          else if (changed)
            ui_out_field_int (uiout, NULL, regnum);
        }
      else
        error ("bad register number");
    }
  do_cleanups (cleanup);
  return MIX_CMD_DONE;
}

static int
register_changed_p (int regnum, struct regcache *prev_regs,
                    struct regcache *this_regs)
{
  struct gdbarch *gdbarch = get_regcache_arch (this_regs);
  gdb_byte prev_buffer[MAX_REGISTER_SIZE];
  gdb_byte this_buffer[MAX_REGISTER_SIZE];

  /* Registers not valid in this frame return count as unchanged.  */
  if (!regcache_valid_p (this_regs, regnum))
    return 0;

  /* First time through or after gdbarch change consider all registers as
     changed.  Same for registers not valid in the previous frame.  */
  if (!prev_regs || get_regcache_arch (prev_regs) != gdbarch
      || !regcache_valid_p (prev_regs, regnum))
    return 1;

  /* Get register contents and compare.  */
  regcache_cooked_read (prev_regs, regnum, prev_buffer);
  regcache_cooked_read (this_regs, regnum, this_buffer);

  return memcmp (prev_buffer, this_buffer,
                 register_size (gdbarch, regnum)) != 0;
}

/* Return a list of register number and value pairs.  The valid
   arguments expected are: a letter indicating the format in which to
   display the registers contents.  This can be one of: x (hexadecimal), d
   (decimal), N (natural), t (binary), o (octal), r (raw).  After the
   format argumetn there can be a sequence of numbers, indicating which
   registers to fetch the content of.  If the format is the only argument,
   a list of all the registers with their values is returned.  */
enum mix_cmd_result
mix_cmd_data_list_register_values (char *command, char **argv, int argc)
{
  int regnum, numregs, format;
  int i;
  struct cleanup *list_cleanup, *tuple_cleanup;

  /* Note that the test for a valid register must include checking the
     gdbarch_register_name because gdbarch_num_regs may be allocated for
     the union of the register sets within a family of related processors.
     In this case, some entries of gdbarch_register_name will change depending
     upon the particular processor being debugged.  */

  numregs = gdbarch_num_regs (current_gdbarch)
            + gdbarch_num_pseudo_regs (current_gdbarch);

  if (argc == 0)
    error ("mix_cmd_data_list_register_values: Usage: -data-list-register-values <format> [<regnum1>...<regnumN>]");

  format = (int) argv[0][0];

  list_cleanup = make_cleanup_ui_out_list_begin_end (uiout, "register-values");

  if (argc == 1)            /* No args, beside the format: do all the regs.  */
    {
      for (regnum = 0;
           regnum < numregs;
           regnum++)
        {
          if (gdbarch_register_name (current_gdbarch, regnum) == NULL
              || *(gdbarch_register_name (current_gdbarch, regnum)) == '\0')
            continue;
          tuple_cleanup = make_cleanup_ui_out_tuple_begin_end (uiout, NULL);
          ui_out_field_int (uiout, "number", regnum);
          get_register (regnum, format);
          do_cleanups (tuple_cleanup);
        }
    }

  /* Else, list of register #s, just do listed regs.  */
  for (i = 1; i < argc; i++)
    {
      regnum = atoi (argv[i]);

      if (regnum >= 0
          && regnum < numregs
          && gdbarch_register_name (current_gdbarch, regnum) != NULL
          && *gdbarch_register_name (current_gdbarch, regnum) != '\000')
        {
          tuple_cleanup = make_cleanup_ui_out_tuple_begin_end (uiout, NULL);
          ui_out_field_int (uiout, "number", regnum);
          get_register (regnum, format);
          do_cleanups (tuple_cleanup);
        }
      else
        error ("bad register number");
    }
  do_cleanups (list_cleanup);
  return MIX_CMD_DONE;
}

/* Output one register's contents in the desired format.  */
static void
get_register (int regnum, int format)
{
  gdb_byte buffer[MAX_REGISTER_SIZE];
  int optim;
  int realnum;
  CORE_ADDR addr;
  enum lval_type lval;
  static struct ui_stream *stb = NULL;

  stb = ui_out_stream_new (uiout);

  if (format == 'N')
    format = 0;

  frame_register (get_selected_frame (NULL), regnum, &optim, &lval, &addr,
                  &realnum, buffer);

  if (optim)
    error ("Optimized out");

  if (format == 'r')
    {
      int j;
      char *ptr, buf[1024];

      strcpy (buf, "0x");
      ptr = buf + 2;
      for (j = 0; j < register_size (current_gdbarch, regnum); j++)
        {
          int idx = gdbarch_byte_order (current_gdbarch) == BFD_ENDIAN_BIG ? j
          : register_size (current_gdbarch, regnum) - 1 - j;
          sprintf (ptr, "%02x", (unsigned char) buffer[idx]);
          ptr += 2;
        }
      ui_out_field_string (uiout, "value", buf);
      /*fputs_filtered (buf, gdb_stdout); */
    }
  else
    {
      val_print (register_type (current_gdbarch, regnum), buffer, 0, 0,
                 stb->stream, format, 1, 0, Val_pretty_default, current_language);
      ui_out_field_stream (uiout, "value", stb);
      ui_out_stream_delete (stb);
    }
}

/* Write given values into registers. The registers and values are
   given as pairs.  The corresponding MI command is 
   -data-write-register-values <format> [<regnum1> <value1>...<regnumN> <valueN>]*/
enum mix_cmd_result
mix_cmd_data_write_register_values (char *command, char **argv, int argc)
{
  int numregs, i;
  char format;

  /* Note that the test for a valid register must include checking the
     gdbarch_register_name because gdbarch_num_regs may be allocated for
     the union of the register sets within a family of related processors.
     In this case, some entries of gdbarch_register_name will change depending
     upon the particular processor being debugged.  */

  numregs = gdbarch_num_regs (current_gdbarch)
            + gdbarch_num_pseudo_regs (current_gdbarch);

  if (argc == 0)
    error ("mix_cmd_data_write_register_values: Usage: -data-write-register-values <format> [<regnum1> <value1>...<regnumN> <valueN>]");

  format = (int) argv[0][0];

  if (!target_has_registers)
    error ("mix_cmd_data_write_register_values: No registers.");

  if (!(argc - 1))
    error ("mix_cmd_data_write_register_values: No regs and values specified.");

  if ((argc - 1) % 2)
    error ("mix_cmd_data_write_register_values: Regs and vals are not in pairs.");

  for (i = 1; i < argc; i = i + 2)
    {
      int regnum = atoi (argv[i]);

      if (regnum >= 0 && regnum < numregs
          && gdbarch_register_name (current_gdbarch, regnum)
          && *gdbarch_register_name (current_gdbarch, regnum))
        {
          LONGEST value;

          /* Get the value as a number.  */
          value = parse_and_eval_address (argv[i + 1]);

          /* Write it down.  */
          regcache_cooked_write_signed (get_current_regcache (), regnum, value);
        }
      else
        error ("bad register number");
    }
  return MIX_CMD_DONE;
}

extern int unwind_on_signal_p;

int
set_unwind_on_signal (int new_val)
{
  int old_val = unwind_on_signal_p;
  unwind_on_signal_p = new_val;
  return old_val;
}

void
do_set_unwind_on_signal (void *var)
{
  set_unwind_on_signal (*(int *)var);
}

/* Evaluate the value of the argument.  The argument is an
   expression. If the expression contains spaces it needs to be
   included in double quotes.  */
enum mix_cmd_result
mix_cmd_data_evaluate_expression (char *command, char **argv, int argc)
{
  struct expression *expr;
  struct cleanup *old_chain = NULL;
  struct value *val;
  struct ui_stream *stb = NULL;
  int unwinding_was_requested = 0;
  char *expr_string;

  stb = ui_out_stream_new (uiout);

  if (argc == 1)
      expr_string = argv[0];

  else if (argc == 2 && strcmp (argv[0], "-u") == 0)
    {
      unwinding_was_requested = 1;
      expr_string = argv[1];
    }
  else
    {
      ui_out_stream_delete (stb);
      error ("mix_cmd_data_evaluate_expression: Usage: -data-evaluate-expression [-u] expression");
    }

  old_chain = make_cleanup_set_restore_scheduler_locking_mode (scheduler_locking_on);
  if (unwinding_was_requested)
  {
    int unwind_rc = set_unwind_on_signal (1);
    make_cleanup (do_set_unwind_on_signal, &unwind_rc);
  }

  expr = parse_expression (expr_string);

  make_cleanup (free_current_contents, &expr);

  val = evaluate_expression (expr);

  /* Print the result of the expression evaluation.  */
  val_print (value_type (val), value_contents (val),
             value_embedded_offset (val), VALUE_ADDRESS (val),
             stb->stream, 0, 0, 0, 0, current_language);

  ui_out_field_stream (uiout, "value", stb);
  ui_out_stream_delete (stb);

  do_cleanups (old_chain);

  return MIX_CMD_DONE;
}

enum mix_cmd_result
mix_cmd_target_download (char *args, int from_tty)
{
  char *run;
  struct cleanup *old_cleanups = NULL;

  if (*args)
    error ("mix_cmd_target_download: no target specified");

  /* There may be at most one parameter -- the name of the
     file to download.  */
  run = xstrprintf ("load %s", args);
  old_cleanups = make_cleanup (xfree, run);
  execute_command (run, from_tty);

  do_cleanups (old_cleanups);
  return MIX_CMD_DONE;
}

/* Connect to the remote target.  */
enum mix_cmd_result
mix_cmd_target_select (char *args, int from_tty)
{
  char *run;
  struct cleanup *old_cleanups = NULL;

  if (*args)
    error ("mix_cmd_target_select: no target specified");
    
  run = xstrprintf ("target %s", args);
  old_cleanups = make_cleanup (xfree, run);

  /* target-select is always synchronous.  Once the call has returned
     we know that we are connected.  */
  /* NOTE: At present all targets that are connected are also
     (implicitly) talking to a halted target.  In the future this may change.  */
  execute_command (run, from_tty);

  do_cleanups (old_cleanups);

  /* Issue the completion message here.  */
  if (current_command_token)
    fputs_unfiltered (current_command_token, raw_stdout);
  fputs_unfiltered ("^connected", raw_stdout);
  mix_out_put (uiout, raw_stdout);
  mix_out_rewind (uiout);
  fputs_unfiltered ("\n", raw_stdout);
  do_exec_cleanups (ALL_CLEANUPS);
  return MIX_CMD_QUIET;
}

/* DATA-MEMORY-READ:

   ADDR: start address of data to be dumped.
   WORD-FORMAT: a char indicating format for the ``word''.  See 
   the ``x'' command.
   WORD-SIZE: size of each ``word''; 1,2,4, or 8 bytes.
   NR_ROW: Number of rows.
   NR_COL: The number of colums (words per row).
   ASCHAR: (OPTIONAL) Append an ascii character dump to each row.  Use
   ASCHAR for unprintable characters.

   Reads SIZE*NR_ROW*NR_COL bytes starting at ADDR from memory and
   displayes them.  Returns:

   {addr="...",rowN={wordN="..." ,... [,ascii="..."]}, ...}

   Returns: 
   The number of bytes read is SIZE*ROW*COL. */

enum mix_cmd_result
mix_cmd_data_read_memory (char *command, char **argv, int argc)
{
  struct cleanup *cleanups = make_cleanup (null_cleanup, NULL);
  CORE_ADDR addr;
  long total_bytes;
  long nr_cols;
  long nr_rows;
  char word_format;
  struct type *word_type;
  long word_size;
  char word_asize;
  char aschar;
  gdb_byte *mbuf;
  int nr_bytes;
  long offset = 0;
  int optind = 0;
  char *optarg;
  enum opt
    {
      OFFSET_OPT
    };
  static struct mix_opt opts[] =
  {
    {"o", OFFSET_OPT, 1},
    { 0, 0, 0 }
  };

  while (1)
    {
      int opt = mix_getopt ("mix_cmd_data_read_memory", argc, argv, opts,
                           &optind, &optarg);
      if (opt < 0)
        break;
      switch ((enum opt) opt)
        {
        case OFFSET_OPT:
          offset = atol (optarg);
          break;
        }
    }
  argv += optind;
  argc -= optind;

  if (argc < 5 || argc > 6)
    error ("mix_cmd_data_read_memory: Usage: ADDR WORD-FORMAT WORD-SIZE NR-ROWS NR-COLS [ASCHAR].");

  /* Extract all the arguments. */

  /* Start address of the memory dump.  */
  addr = parse_and_eval_address (argv[0]) + offset;
  /* The format character to use when displaying a memory word.  See
     the ``x'' command. */
  word_format = argv[1][0];
  /* The size of the memory word.  */
  word_size = atol (argv[2]);
  switch (word_size)
    {
    case 1:
      word_type = builtin_type_int8;
      word_asize = 'b';
      break;
    case 2:
      word_type = builtin_type_int16;
      word_asize = 'h';
      break;
    case 4:
      word_type = builtin_type_int32;
      word_asize = 'w';
      break;
    case 8:
      word_type = builtin_type_int64;
      word_asize = 'g';
      break;
    default:
      word_type = builtin_type_int8;
      word_asize = 'b';
    }
  /* The number of rows.  */
  nr_rows = atol (argv[3]);
  if (nr_rows <= 0)
    error ("mix_cmd_data_read_memory: invalid number of rows.");

  /* Number of bytes per row.  */
  nr_cols = atol (argv[4]);
  if (nr_cols <= 0)
    error ("mix_cmd_data_read_memory: invalid number of columns.");

  /* The un-printable character when printing ascii.  */
  if (argc == 6)
    aschar = *argv[5];
  else
    aschar = 0;

  /* Create a buffer and read it in.  */
  total_bytes = word_size * nr_rows * nr_cols;
  mbuf = xcalloc (total_bytes, 1);
  make_cleanup (xfree, mbuf);

  nr_bytes = target_read (&current_target, TARGET_OBJECT_MEMORY, NULL,
                          mbuf, addr, total_bytes);
  if (nr_bytes <= 0)
    error ("Unable to read memory.");

  /* Output the header information.  */
  ui_out_field_core_addr (uiout, "addr", addr);
  ui_out_field_int (uiout, "nr-bytes", nr_bytes);
  ui_out_field_int (uiout, "total-bytes", total_bytes);
  ui_out_field_core_addr (uiout, "next-row", addr + word_size * nr_cols);
  ui_out_field_core_addr (uiout, "prev-row", addr - word_size * nr_cols);
  ui_out_field_core_addr (uiout, "next-page", addr + total_bytes);
  ui_out_field_core_addr (uiout, "prev-page", addr - total_bytes);

  /* Build the result as a two dimentional table.  */
  {
    struct ui_stream *stream = ui_out_stream_new (uiout);
    struct cleanup *cleanup_list_memory;
    int row;
    int row_byte;
    cleanup_list_memory = make_cleanup_ui_out_list_begin_end (uiout, "memory");
    for (row = 0, row_byte = 0;
         row < nr_rows;
         row++, row_byte += nr_cols * word_size)
      {
        int col;
        int col_byte;
        struct cleanup *cleanup_tuple;
        struct cleanup *cleanup_list_data;
        cleanup_tuple = make_cleanup_ui_out_tuple_begin_end (uiout, NULL);
        ui_out_field_core_addr (uiout, "addr", addr + row_byte);
        /* ui_out_field_core_addr_symbolic (uiout, "saddr", addr + row_byte); */
        cleanup_list_data = make_cleanup_ui_out_list_begin_end (uiout, "data");
        for (col = 0, col_byte = row_byte;
             col < nr_cols;
             col++, col_byte += word_size)
          {
            if (col_byte + word_size > nr_bytes)
              {
                ui_out_field_string (uiout, NULL, "N/A");
              }
            else
              {
                ui_file_rewind (stream->stream);
                print_scalar_formatted (mbuf + col_byte, word_type, word_format,
                                        word_asize, stream->stream);
                ui_out_field_stream (uiout, NULL, stream);
              }
          }
        do_cleanups (cleanup_list_data);
        if (aschar)
          {
            int byte;
            ui_file_rewind (stream->stream);
            for (byte = row_byte; byte < row_byte + word_size * nr_cols; byte++)
              {
                if (byte >= nr_bytes)
                  {
                    fputc_unfiltered ('X', stream->stream);
                  }
                else if (mbuf[byte] < 32 || mbuf[byte] > 126)
                  {
                    fputc_unfiltered (aschar, stream->stream);
                  }
                else
                  fputc_unfiltered (mbuf[byte], stream->stream);
              }
            ui_out_field_stream (uiout, "ascii", stream);
          }
        do_cleanups (cleanup_tuple);
      }
    ui_out_stream_delete (stream);
    do_cleanups (cleanup_list_memory);
  }
  do_cleanups (cleanups);
  return MIX_CMD_DONE;
}

/* DATA-MEMORY-WRITE:

   COLUMN_OFFSET: optional argument. Must be preceeded by '-o'. The
   offset from the beginning of the memory grid row where the cell to
   be written is.
   ADDR: start address of the row in the memory grid where the memory
   cell is, if OFFSET_COLUMN is specified.  Otherwise, the address of
   the location to write to.
   FORMAT: a char indicating format for the ``word''.  See 
   the ``x'' command.
   WORD_SIZE: size of each ``word''; 1,2,4, or 8 bytes
   VALUE: value to be written into the memory address.

   Writes VALUE into ADDR + (COLUMN_OFFSET * WORD_SIZE).

   Prints nothing.  */
enum mix_cmd_result
mix_cmd_data_write_memory (char *command, char **argv, int argc)
{
  CORE_ADDR addr;
  char word_format;
  long word_size;
  /* FIXME: ezannoni 2000-02-17 LONGEST could possibly not be big
     enough when using a compiler other than GCC.  */
  LONGEST value;
  void *buffer;
  struct cleanup *old_chain;
  long offset = 0;
  int optind = 0;
  char *optarg;
  enum opt
    {
      OFFSET_OPT
    };
  static struct mix_opt opts[] =
  {
    {"o", OFFSET_OPT, 1},
    { 0, 0, 0 }
  };

  while (1)
    {
      int opt = mix_getopt ("mix_cmd_data_write_memory", argc, argv, opts,
                           &optind, &optarg);
      if (opt < 0)
        break;
      switch ((enum opt) opt)
        {
        case OFFSET_OPT:
          offset = atol (optarg);
          break;
        }
    }
  argv += optind;
  argc -= optind;

  if (argc != 4)
    error ("mix_cmd_data_write_memory: Usage: [-o COLUMN_OFFSET] ADDR FORMAT WORD-SIZE VALUE.");

  /* Extract all the arguments.  */
  /* Start address of the memory dump.  */
  addr = parse_and_eval_address (argv[0]);
  /* The format character to use when displaying a memory word.  See
     the ``x'' command.  */
  word_format = argv[1][0];
  /* The size of the memory word. */
  word_size = atol (argv[2]);

  /* Calculate the real address of the write destination.  */
  addr += (offset * word_size);

  /* Get the value as a number.  */
  value = parse_and_eval_address (argv[3]);
  /* Get the value into an array.  */
  buffer = xmalloc (word_size);
  old_chain = make_cleanup (xfree, buffer);
  store_signed_integer (buffer, word_size, value);
  /* Write it down to memory.  */
  write_memory (addr, buffer, word_size);
  /* Free the buffer.  */
  do_cleanups (old_chain);

  return MIX_CMD_DONE;
}

enum mix_cmd_result
mix_cmd_enable_timings (char *command, char **argv, int argc)
{
  if (argc == 0)
    do_timings = 1;
  else if (argc == 1)
    {
      if (strcmp (argv[0], "yes") == 0)
        do_timings = 1;
      else if (strcmp (argv[0], "no") == 0)
        do_timings = 0;
      else
        goto usage_error;
    }
  else
    goto usage_error;
    
  return MIX_CMD_DONE;

 usage_error:
  error ("mix_cmd_enable_timings: Usage: %s {yes|no}", command);
  return MIX_CMD_DONE;
}

enum mix_cmd_result
mix_cmd_mi_verify_command (char *command, char **argv, int argc)
{
  char 	        *command_name = argv[0];
  struct mix_cmd *cmd;
  
  if (argc != 1)
    {
      error ("mix_cmd_mi_verify_command: Usage: MI_COMMAND_NAME.");
    }

  cmd = mix_lookup (command_name);

  ui_out_field_string (uiout, "name", command_name);
  if (cmd != NULL) 
    {
       ui_out_field_string (uiout, "defined", "true");
       ui_out_field_string (uiout, "implemented",
            ((cmd->cli.cmd != NULL) ||
             (cmd->argv_func != NULL) ||
             (cmd->args_func != NULL)) ? "true" : "false");
    }
  else 
    {
       ui_out_field_string (uiout, "defined", "false");
    }
  
  return MIX_CMD_DONE;
}

enum mix_cmd_result
mix_cmd_list_features (char *command, char **argv, int argc)
{
  if (argc == 0)
    {
      struct cleanup *cleanup = NULL;
      cleanup = make_cleanup_ui_out_list_begin_end (uiout, "features");      

      ui_out_field_string (uiout, NULL, "frozen-varobjs");
      ui_out_field_string (uiout, NULL, "pending-breakpoints");
      ui_out_field_string (uiout, NULL, "thread-info");
      
      do_cleanups (cleanup);

      return MIX_CMD_DONE;
    }

  error ("-list-features should be passed no arguments");
  return MIX_CMD_DONE;
}
 
/* Execute a command within a safe environment.
   Return <0 for error; >=0 for ok.

   args->action will tell mix_execute_command what action
   to perfrom after the given command has executed (display/suppress
   prompt, display error). */

static void
captured_mix_execute_command (struct ui_out *uiout, void *data)
{
  struct captured_mix_execute_command_args *args = (struct captured_mix_execute_command_args *) data;
  struct mix_parse *context = args->command;

  struct mix_timestamp cmd_finished;

  switch (context->op)
    {

    case MI_COMMAND:
      /* A MI command was read from the input stream.  */
      if (mix_debug_p)
        /* FIXME: gdb_???? */
        fprintf_unfiltered (raw_stdout, " token=`%s' command=`%s' args=`%s'\n",
                            context->token, context->command, context->args);
      /* FIXME: cagney/1999-09-25: Rather than this convoluted
         condition expression, each function should return an
         indication of what action is required and then switch on
         that.  */
      args->action = EXECUTE_COMMAND_DISPLAY_PROMPT;

      /* This is a bit of a hack.  We need to pass the cmd_start down to
         the mi command so that it can be copied into continuations if
         needs be.  But we don't pass the parse but just a few bits
         instead.  So we need to route it through this instead... */

      current_command_token = context->token;

      if (do_timings)
        current_command_ts = context->cmd_start;

      /* Set this to 0 so we don't mistakenly think this command
        caused the target to run under interpreter-exec.  */
      mix_interp_exec_cmd_did_run = 0;
      args->rc = mix_cmd_execute (context);

      if (do_timings)
        timestamp (&cmd_finished);

      if (!target_can_async_p () || !is_running (inferior_ptid))
        {
          /* Print the result if there were no errors.

             Remember that on the way out of executing a command, you have
             to directly use the mix_interp's uiout, since the command could 
             have reset the interpreter, in which case the current uiout 
             will most likely crash in the mix_out_* routines.  */
          if (args->rc == MIX_CMD_DONE)
            {
              fputs_unfiltered (context->token, raw_stdout);
              fputs_unfiltered ("^done", raw_stdout);
              mix_out_put (uiout, raw_stdout);
              mix_out_rewind (uiout);
              /* Have to check cmd_start, since the command could be
                 -enable-timings.  */
              if (do_timings && context->cmd_start)
                  print_diff (context->cmd_start, &cmd_finished);
              fputs_unfiltered ("\n", raw_stdout);
            }
          else
            mix_out_rewind (uiout);
        }
      else if (sync_execution)
        {
          /* Don't print the prompt. We are executing the target in
             synchronous mode.  */
          args->action = EXECUTE_COMMAND_SUPPRESS_PROMPT;
          return;
        }
      break;

    case CLI_COMMAND:
      {
        char *argv[2];
        /* A CLI command was read from the input stream.  */
        /* This "feature" will be removed as soon as we have a
           complete set of mi commands.  */
        /* Echo the command on the console.  */
        fprintf_unfiltered (gdb_stdlog, "%s\n", context->command);
        /* Call the "console" interpreter.  */
        argv[0] = "console";
        argv[1] = context->command;
        args->rc = mix_cmd_interpreter_exec ("-interpreter-exec", argv, 2);

        /* If we changed interpreters, DON'T print out anything.  */
        if (current_interp_named_p (INTERP_MI)
            || current_interp_named_p (INTERP_MI1)
            || current_interp_named_p (INTERP_MI2)
            || current_interp_named_p (INTERP_MI3))
          {
            if (args->rc == MIX_CMD_DONE)
              {
                fputs_unfiltered (context->token, raw_stdout);
                fputs_unfiltered ("^done", raw_stdout);
                mix_out_put (uiout, raw_stdout);
                mix_out_rewind (uiout);
                fputs_unfiltered ("\n", raw_stdout);
                args->action = EXECUTE_COMMAND_DISPLAY_PROMPT;
              }
            else
              mix_out_rewind (uiout);
          }
        break;
      }

    }

  return;
}


void
mix_execute_command (char *cmd, int from_tty)
{
  struct mix_parse *command;
  struct captured_mix_execute_command_args args;
  struct ui_out *saved_uiout = uiout;

  /* This is to handle EOF (^D). We just quit gdb.  */
  /* FIXME: we should call some API function here.  */
  if (cmd == 0)
    quit_force (NULL, from_tty);

  command = mix_parse (cmd);

  if (command != NULL)
    {
      struct gdb_exception result;

      if (do_timings)
        {
          command->cmd_start = (struct mix_timestamp *)
            xmalloc (sizeof (struct mix_timestamp));
          timestamp (command->cmd_start);
        }

      args.command = command;
      result = catch_exception (uiout, captured_mix_execute_command, &args, RETURN_MASK_ALL);
      if (result.reason < 0)
        {
          /* The command execution failed and error() was called somewhere.  */
          fputs_unfiltered (command->token, raw_stdout);
          fputs_unfiltered ("^error,msg=\"", raw_stdout);
          if (result.message == NULL)
            fputs_unfiltered ("unknown error", raw_stdout);
          else
            fputstr_unfiltered (result.message, '"', raw_stdout);
          fputs_unfiltered ("\"\n", raw_stdout);
          mix_out_rewind (uiout);
        }

      mix_parse_free (command);

      if (args.action == EXECUTE_COMMAND_SUPPRESS_PROMPT)
        /* The command is executing synchronously.  Bail out early
           suppressing the finished prompt.  */
        return;
    }

  fputs_unfiltered ("(gdb) \n", raw_stdout);
  gdb_flush (raw_stdout);
  /* Print any buffered hook code.  */
  /* ..... */
}

static int 
mix_command_completes_while_target_executing (char *command)
{
  if (strcmp (command, "exec-interrupt")
      && strcmp (command, "exec-status")
      && strcmp (command, "pid-info"))
    return 0;
  else
    return 1;
}

static enum mix_cmd_result
mix_cmd_execute (struct mix_parse *parse)
{
  struct cleanup *cleanup;
  enum mix_cmd_result r;
  free_all_values ();

  if (parse->cmd->args_func != NULL || parse->cmd->argv_func != NULL)
    {
      if (is_running (inferior_ptid))
        {
          if (!mix_command_completes_while_target_executing(parse->command))
            {
              fputs_unfiltered (parse->token, raw_stdout);
              fputs_unfiltered ("^error,msg=\"", raw_stdout);
              fputs_unfiltered ("Cannot execute command ", raw_stdout);
              fputstr_unfiltered (parse->command, '"', raw_stdout);
              fputs_unfiltered (" while target running", raw_stdout);
              fputs_unfiltered ("\"\n", raw_stdout);
              return MIX_CMD_ERROR;
            }
        }

      /* FIXME: DELETE THIS! */
      if (parse->cmd->args_func != NULL)
        return parse->cmd->args_func (parse->args, 0 /*from_tty */ );
      return parse->cmd->argv_func (parse->command, parse->argv, parse->argc);
    }
  else if (parse->cmd->cli.cmd != 0)
    {
      /* FIXME: DELETE THIS. */
      /* The operation is still implemented by a cli command.  */
      /* Must be a synchronous one.  */
      mix_execute_cli_command (parse->cmd->cli.cmd, parse->cmd->cli.args_p, parse->args);
      return MIX_CMD_DONE;
    }
  else
    {
      /* FIXME: DELETE THIS. */
      fputs_unfiltered (parse->token, raw_stdout);
      fputs_unfiltered ("^error,msg=\"", raw_stdout);
      fputs_unfiltered ("Undefined mi command: ", raw_stdout);
      fputstr_unfiltered (parse->command, '"', raw_stdout);
      fputs_unfiltered (" (missing implementation)", raw_stdout);
      fputs_unfiltered ("\"\n", raw_stdout);
      return MIX_CMD_ERROR;
    }
}

/* FIXME: This is just a hack so we can get some extra commands going.
   We don't want to channel things through the CLI, but call libgdb directly.
   Use only for synchronous commands.  */

void
mix_execute_cli_command (const char *cmd, int args_p, const char *args)
{
  if (cmd != 0)
    {
      struct cleanup *old_cleanups;
      char *run;
      if (args_p)
        run = xstrprintf ("%s %s", cmd, args);
      else
        run = xstrdup (cmd);
      if (mix_debug_p)
        /* FIXME: gdb_???? */
        fprintf_unfiltered (gdb_stdout, "cli=%s run=%s\n",
                            cmd, run);
      old_cleanups = make_cleanup (xfree, run);
      execute_command ( /*ui */ run, 0 /*from_tty */ );
      do_cleanups (old_cleanups);
      return;
    }
}

enum mix_cmd_result
mix_execute_async_cli_command (char *cli_command, char *args, int from_tty)
{
  char *run;
  char *async_args;

  if (!target_can_async_p ())
    {
      struct cleanup *old_cleanups;
      xasprintf (&run, "%s %s", cli_command, args);
      old_cleanups = make_cleanup (xfree, run);

      /* NOTE: For synchronous targets asynchronous behavour is faked by
         printing out the GDB prompt before we even try to execute the command. */
      if (current_command_token)
        fputs_unfiltered (current_command_token, raw_stdout);
      fputs_unfiltered ("^running\n", raw_stdout);
      fputs_unfiltered ("(gdb) \n", raw_stdout);
      gdb_flush (raw_stdout);
      
      execute_command ( /*ui */ run, 0 /*from_tty */ );

      /* Do this before doing any printing.  It would appear that some
         print code leaves garbage around in the buffer. */
      do_cleanups (old_cleanups);

      /* We should do whatever breakpoint actions are pending.
         This works even if the breakpoint command causes the target
         to continue, but we won't exit bpstat_do_actions here till
         the target is all the way stopped.

         Note, I am doing this before putting out the *stopped.  If we
         told the UI that we had stopped & continued again, we would
         have to figure out how to tell it that as well.  This is
         easier.  If the command continues the target, the UI will
         just think it hasn't stopped yet, which is probably also okay.  */
      bpstat_do_actions ();

      /* If the target was doing the operation synchronously we fake
         the stopped message. */
      if (current_command_token)
        fputs_unfiltered (current_command_token, raw_stdout);
      fputs_unfiltered ("*stopped", raw_stdout);
      mix_out_put (uiout, raw_stdout);
      mix_out_rewind (uiout);
      if (do_timings && current_command_ts)
        {
          print_diff_now (current_command_ts);
          xfree (current_command_ts);
          current_command_ts = NULL;  /* Indicate that the ts was printed */
        }
      fputs_unfiltered ("\n", raw_stdout);
      return MIX_CMD_QUIET;
    }
  else
    {
      struct mix_continuation_arg *arg = NULL; 
      struct cleanup *old_cleanups = NULL;
      volatile struct gdb_exception except;

      async_args = (char *) xmalloc (strlen (args) + 2);
      old_cleanups = make_cleanup (free, async_args);
      strcpy (async_args, args);
      strcat (async_args, "&");
      xasprintf (&run, "%s %s", cli_command, async_args);
      make_cleanup (free, run);

      /* Transfer the command token to the continuation.  That
         will now print the results associated with this command. 
         Tricky point: have to add the continuation BEFORE running
         execute_command, or it will get run before any continuations
         that might get added by execute_command, in which case the
         cleanups will be out of order. */
      
      arg = mix_setup_continuation_arg (NULL);
      add_continuation (inferior_thread (), mix_exec_async_cli_cmd_continuation, (struct continuation *) arg, NULL);

      arg->exec_error_cleanups = make_exec_error_cleanup (mix_exec_error_cleanup, (void *) arg);

      except = safe_execute_command (uiout, /*ui */ run, 0 /*from_tty */ );
      do_cleanups (old_cleanups);

      if (is_running (inferior_ptid))
        {
          if (current_command_token)
            fputs_unfiltered (current_command_token, raw_stdout);
          fputs_unfiltered ("^running\n", raw_stdout);
          
        }
//      /* APPLE LOCAL begin inlined subroutine  */
//      /* If we are stepping from an inlined subroutine call site into the
//         inlined subroutine, the target will not be executing, but it is
//         not an error.  */
//      else if (strcmp (cli_command, "step") == 0 && stepping_into_inlined_subroutine)
//        {
//          stop_step = 1;
//          if (current_command_token)
//            fputs_unfiltered (current_command_token, raw_stdout);
//          fputs_unfiltered ("^running\n", raw_stdout);
//              
//          ui_out_field_string (uiout, "reason", async_reason_lookup (EXEC_ASYNC_END_STEPPING_RANGE));
//          mix_exec_async_cli_cmd_continuation (arg);
//        }
//      /* APPLE LOCAL end inlined subroutine  */
      else
        {
          /* If we didn't manage to set the inferior going, that's most likely an error... */
          discard_all_continuations ();
          if ((void *)arg->exec_error_cleanups != (void *)-1)
            discard_exec_error_cleanups (arg->exec_error_cleanups);
          free_mix_continuation_arg (arg);
          if (except.message != NULL)
            error ("mix_execute_async_cli_command: %s", except.message);
          else
            return MIX_CMD_ERROR;
        }

    }

  return MIX_CMD_DONE;
}

void
mix_exec_error_cleanup (void *in_arg)
{
  struct mix_continuation_arg *arg = (struct mix_continuation_arg *) in_arg;
  struct ui_out *saved_ui_out = uiout;

//  uiout = interp_ui_out (mi_interp);

  if (arg && arg->token)
    {
      fputs_unfiltered (arg->token, raw_stdout);
    }
  fputs_unfiltered ("*stopped", raw_stdout);
  ui_out_field_string (uiout, "reason", "error");
  if (do_timings && arg && arg->timestamp)
    print_diff_now (arg->timestamp);
  mix_out_put (uiout, raw_stdout);
  fputs_unfiltered ("\n", raw_stdout);
  fputs_unfiltered ("(gdb) \n", raw_stdout);
  gdb_flush (raw_stdout);
  uiout = saved_ui_out;
}

void
mix_exec_async_cli_cmd_continuation (void *continuation_arg)
{
  /* Assume 'error' means that target is stopped, too.  */
  if (current_command_token)
    fputs_unfiltered (current_command_token, raw_stdout);
  fputs_unfiltered ("*stopped", raw_stdout);
  mix_out_put (uiout, raw_stdout);
  fputs_unfiltered ("\n", raw_stdout);
  fputs_unfiltered ("(gdb) \n", raw_stdout);
  gdb_flush (raw_stdout);
}

void
mix_load_progress (const char *section_name,
                  unsigned long sent_so_far,
                  unsigned long total_section,
                  unsigned long total_sent,
                  unsigned long grand_total)
{
  struct timeval time_now, delta, update_threshold;
  static struct timeval last_update;
  static char *previous_sect_name = NULL;
  int new_section;
  struct ui_out *saved_uiout;

  /* This function is called through deprecated_show_load_progress
     which means uiout may not be correct.  Fix it for the duration
     of this function.  */
  saved_uiout = uiout;

  if (current_interp_named_p (INTERP_MI)
      || current_interp_named_p (INTERP_MI2))
    uiout = mix_out_new (2);
  else if (current_interp_named_p (INTERP_MI1))
    uiout = mix_out_new (1);
  else if (current_interp_named_p (INTERP_MI3))
    uiout = mix_out_new (3);
  else
    return;

  update_threshold.tv_sec = 0;
  update_threshold.tv_usec = 500000;
  gettimeofday (&time_now, NULL);

  delta.tv_usec = time_now.tv_usec - last_update.tv_usec;
  delta.tv_sec = time_now.tv_sec - last_update.tv_sec;

  if (delta.tv_usec < 0)
    {
      delta.tv_sec -= 1;
      delta.tv_usec += 1000000L;
    }

  new_section = (previous_sect_name ?
                 strcmp (previous_sect_name, section_name) : 1);
  if (new_section)
    {
      struct cleanup *cleanup_tuple;
      xfree (previous_sect_name);
      previous_sect_name = xstrdup (section_name);

      if (current_command_token)
        fputs_unfiltered (current_command_token, raw_stdout);
      fputs_unfiltered ("+download", raw_stdout);
      cleanup_tuple = make_cleanup_ui_out_tuple_begin_end (uiout, NULL);
      ui_out_field_string (uiout, "section", section_name);
      ui_out_field_int (uiout, "section-size", total_section);
      ui_out_field_int (uiout, "total-size", grand_total);
      do_cleanups (cleanup_tuple);
      mix_out_put (uiout, raw_stdout);
      fputs_unfiltered ("\n", raw_stdout);
      gdb_flush (raw_stdout);
    }

  if (delta.tv_sec >= update_threshold.tv_sec &&
      delta.tv_usec >= update_threshold.tv_usec)
    {
      struct cleanup *cleanup_tuple;
      last_update.tv_sec = time_now.tv_sec;
      last_update.tv_usec = time_now.tv_usec;
      if (current_command_token)
        fputs_unfiltered (current_command_token, raw_stdout);
      fputs_unfiltered ("+download", raw_stdout);
      cleanup_tuple = make_cleanup_ui_out_tuple_begin_end (uiout, NULL);
      ui_out_field_string (uiout, "section", section_name);
      ui_out_field_int (uiout, "section-sent", sent_so_far);
      ui_out_field_int (uiout, "section-size", total_section);
      ui_out_field_int (uiout, "total-sent", total_sent);
      ui_out_field_int (uiout, "total-size", grand_total);
      do_cleanups (cleanup_tuple);
      mix_out_put (uiout, raw_stdout);
      fputs_unfiltered ("\n", raw_stdout);
      gdb_flush (raw_stdout);
    }

  xfree (uiout);
  uiout = saved_uiout;
}

/* mix_setup_continuation_arg - sets up a continuation structure
   with the timer info and the command token, for use with
   an asyncronous mi command.  Will only cleanup the exec_cleanup
   chain back to CLEANUPS, or not at all if CLEANUPS is NULL. */

struct mix_continuation_arg *
mix_setup_continuation_arg (struct cleanup *cleanups)
{
  struct mix_continuation_arg *arg = (struct mix_continuation_arg *)xmalloc (sizeof (struct mix_continuation_arg));

  if (current_command_token)
    {
      arg->token = xstrdup (current_command_token);
    }
  else
    arg->token = NULL;

  if (do_timings && current_command_ts)
    {
      arg->timestamp = (struct mix_timestamp *) 
      xmalloc (sizeof (struct mix_timestamp));
      copy_timestamp (arg->timestamp, current_command_ts);
      current_command_ts = NULL;
    }
  else
    arg->timestamp = NULL;

  arg->cleanups = cleanups;
  arg->exec_error_cleanups = (struct cleanup *) -1;

  return arg;
}

static void
free_mix_continuation_arg (struct mix_continuation_arg *arg)
{
  if (arg)
    {
      if (arg->token)
        xfree (arg->token);
      if (arg->timestamp)
        {
          xfree (arg->timestamp);
          arg->timestamp = NULL;
        }
      xfree (arg);
    }
}

/* The only three called from other parts of mix-main.c will probably be
   timestamp(), print_diff_now() and copy_timestamp() */

static void 
timestamp (struct mix_timestamp *tv)
{
  gettimeofday (&tv->wallclock, NULL);
  getrusage (RUSAGE_SELF, &tv->rusage);
}


static void 
print_diff_now (struct mix_timestamp *start)
{
  struct mix_timestamp now;
  timestamp (&now);
  print_diff (start, &now);
}

static void
copy_timestamp (struct mix_timestamp *dst, struct mix_timestamp *src)
{
  memcpy (dst, src, sizeof (struct mix_timestamp));
}

static void 
print_diff (struct mix_timestamp *start, struct mix_timestamp *end)
{
  fprintf_unfiltered (raw_stdout,
     ",time={wallclock=\"%0.5f\",user=\"%0.5f\",system=\"%0.5f\",start=\"%d.%06d\",end=\"%d.%06d\"}", 
     wallclock_diff (start, end) / 1000000.0, 
     user_diff (start, end) / 1000000.0, 
     system_diff (start, end) / 1000000.0,
     (int) start->wallclock.tv_sec, (int) start->wallclock.tv_usec,
     (int) end->wallclock.tv_sec, (int) end->wallclock.tv_usec);
}

static long 
wallclock_diff (struct mix_timestamp *start, struct mix_timestamp *end)
{
  struct timeval result;
  timersub (&end->wallclock, &start->wallclock, &result);
  return result.tv_sec * 1000000 + result.tv_usec;
}

static long 
user_diff (struct mix_timestamp *start, struct mix_timestamp *end)
{
  struct timeval result;
  timersub (&(end->rusage.ru_utime), &(start->rusage.ru_utime), &result);
  return result.tv_sec * 1000000 + result.tv_usec;
}

static long 
system_diff (struct mix_timestamp *start, struct mix_timestamp *end)
{
  struct timeval result;
  timersub (&(end->rusage.ru_stime), &(start->rusage.ru_stime), &result);
  return result.tv_sec * 1000000 + result.tv_usec;
}

struct captured_execute_command_args
{
  char *command;
  int from_tty;
};

static void
do_captured_execute_command (struct ui_out *uiout, void *data)
{
  struct captured_execute_command_args *args =
    (struct captured_execute_command_args *) data;
  execute_command (args->command, args->from_tty);
}

static struct gdb_exception
safe_execute_command (struct ui_out *uiout, char *command, int from_tty)
{
  struct gdb_exception e;
  struct captured_execute_command_args args;
  args.command = command;
  args.from_tty = from_tty;
  e = catch_exception (uiout, do_captured_execute_command, &args, RETURN_MASK_ALL);
  exception_print (gdb_stderr, e);
  return e;
}

int should_auto_raise_load_state = 0;
int inferior_auto_start_cfm_flag = 0;
char *dyld_load_rules = NULL;
struct cmd_list_element *shliblist = NULL;
struct cmd_list_element *setshliblist = NULL;
struct cmd_list_element *showshliblist = NULL;

void 
_initialize_mix_main (void)
{
  /* APPLE LOCAL begin */
  /* Lets create a gdb "set" variable to control mi timings.  This
     seems gross, but it will allow control from the .gdbinit. */
  add_setshow_boolean_cmd ("mi-timings-enabled", class_obscure,
                           &do_timings, _("\
Set whether timing information is displayed for mi commands."), _("\
Show whether timing information is displayed for mi commands."), NULL,
			   NULL, NULL, &setlist, &showlist);

  /* We don't want to raise load levels for MetroWerks.  */
  add_setshow_boolean_cmd ("auto-raise-load-levels", class_obscure,
                           &should_auto_raise_load_state, _("\
Set if GDB should raise the symbol loading level on all frames found in backtraces."), _("\
Show if GDB should raise the symbol loading level on all frames found in backtraces."), NULL,
                           NULL, NULL, &setlist, &showlist);
  /* APPLE LOCAL end */


  /* xcxdb local: The extra set subcommands of Apples gdb will all be added here
     NOTE: these are only dummy definitions, in order to prevent errors 
     when Xcode tries to set the values. These settings won't have any effect */

  add_setshow_boolean_cmd ("inferior-auto-start-cfm", class_obscure,
                           &inferior_auto_start_cfm_flag, _("\
Set if GDB should enable debugging of CFM shared libraries."), _("\
Show if GDB should enable debugging of CFM shared libraries."), NULL,
                           NULL, NULL,
                           &setlist, &showlist);

  add_prefix_cmd ("sharedlibrary", no_class, not_just_help_class_command,
                  "Generic command for setting shlib settings.",
                  &setshliblist, "set sharedlibrary ", 0, &setlist);

  add_setshow_string_cmd ("load-rules", class_support,
                          &dyld_load_rules, _("\
Set the rules governing the level of symbol loading for shared libraries.\n\
 * Each load rule is a triple.\n\
 * The command takes a flattened list of triples.\n\
 * The first two elements of the triple specify the library, by giving \n\
      - \"who loaded it\" (i.e. dyld, cfm or all) in the first element, \n\
      - and a regexp to match the library name in the second. \n\
 * The last element specifies the level of loading for that library\n\
      - The options are:  all, extern, container or none.\n\
\n\
Example: To load only external symbols from all dyld-based system libraries, use: \n\
    set sharedlibrary load-rules dyld ^/System/Library.* extern\n"),
                          "XYZ",
                          NULL,
                          NULL, NULL,
                          &setshliblist, &showshliblist);
}
