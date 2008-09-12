/* Original in GNU gdb 6.8.50.20080718-cvs,
   modified on 2008-07-18 for xcxdb by Dr. Rolf Jansen. */

/* Objective-C language support routines for GDB, the GNU debugger.

   Copyright (C) 2002, 2003, 2004, 2005, 2007, 2008
   Free Software Foundation, Inc.

   Contributed by Apple Computer, Inc.
   Written by Michael Snyder.

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
#include "symtab.h"
#include "gdbtypes.h"
#include "expression.h"
#include "parser-defs.h"
#include "language.h"
#include "c-lang.h"
#include "objc-lang.h"
#include "exceptions.h"
#include "complaints.h"
#include "value.h"
#include "symfile.h"
#include "objfiles.h"
#include "gdb_string.h"         /* for strchr */
#include "target.h"             /* for target_has_execution */
#include "gdbcore.h"
#include "gdbcmd.h"
#include "frame.h"
#include "gdb_regex.h"
#include "regcache.h"
#include "block.h"
#include "infcall.h"
#include "valprint.h"
#include "gdb_assert.h"
#include "osabi.h"

#include <ctype.h>

struct objc_object {
  CORE_ADDR isa;
};

struct objc_class {
  CORE_ADDR isa; 
  CORE_ADDR super_class; 
  CORE_ADDR name;               
  long version;
  long info;
  long instance_size;
  CORE_ADDR ivars;
  CORE_ADDR methods;
  CORE_ADDR cache;
  CORE_ADDR protocols;
};

struct objc_super {
  CORE_ADDR receiver;
  CORE_ADDR class;
};

struct objc_method {
  CORE_ADDR name;
  CORE_ADDR types;
  CORE_ADDR imp;
};


/* APPLE LOCAL: This tree keeps the map of {class, selector} -> implementation.  */
enum rb_tree_colors { RED, BLACK, UNINIT };

/* Basic tree node definition for red-black trees.  This particular
   node allows for three keys (primary, secondary, third) to be used
   for sorting the nodes.  The inlined call site red-black trees need
   all three keys.  */

struct rb_tree_node {
  CORE_ADDR  key;              /* Primary sorting key                      */
  int secondary_key;           /* Secondary sorting key                    */
  CORE_ADDR third_key;         /* Third sorting key                        */
  void *data;                  /* Main data; varies between different apps */
  enum rb_tree_colors color;   /* Color of the tree node (for balancing)   */
  struct rb_tree_node *parent; /* Parent in the red-black tree             */
  struct rb_tree_node *left;   /* Left child in the red-black tree         */
  struct rb_tree_node *right;  /* Right child in the red-black tree        */
};

struct rb_tree_node *rb_tree_find_node (struct rb_tree_node *, long long, int);
struct rb_tree_node *rb_tree_find_node_all_keys (struct rb_tree_node *, long long, int, long long);
void rb_tree_insert (struct rb_tree_node **, struct rb_tree_node *, struct rb_tree_node *);

static struct rb_tree_node *implementation_tree = NULL;
static CORE_ADDR  lookup_implementation_in_cache (CORE_ADDR class, CORE_ADDR sel);
static void add_implementation_to_cache (CORE_ADDR class, CORE_ADDR sel, CORE_ADDR implementation);

/* APPLE LOCAL: This tree keeps the map of class -> class type.  The tree is probably overkill
   in this case, but it's easy to use and we won't have all that many of them.  */
static struct rb_tree_node *classname_tree = NULL;
static char *lookup_classname_in_cache (CORE_ADDR class);
static void add_classname_to_cache (CORE_ADDR class, char *classname);

/* APPLE LOCAL: At several places we grope in the objc runtime to
   find addresses of methods and such; we need to know how large
   an address is on the executable program.  */

static int
get_addrsize (void)
{
  // if (exec_bfd && gdbarch_lookup_osabi (exec_bfd) == GDB_OSABI_DARWIN64)
  //   return 8;
  // else
    return 4;
}

/* Lookup a structure type named "struct NAME", visible in lexical
   block BLOCK.  If NOERR is nonzero, return zero if NAME is not
   suitably defined.  */

struct symbol *
lookup_struct_typedef (char *name, struct block *block, int noerr)
{
  struct symbol *sym;

  sym = lookup_symbol (name, block, STRUCT_DOMAIN, 0);

  if (sym == NULL)
    {
      if (noerr)
        return 0;
      else 
        error (_("No struct type named %s."), name);
    }
  if (TYPE_CODE (SYMBOL_TYPE (sym)) != TYPE_CODE_STRUCT)
    {
      if (noerr)
        return 0;
      else
        error (_("This context has class, union or enum %s, not a struct."), name);
    }
  return sym;
}

CORE_ADDR 
lookup_objc_class (char *classname)
{
  struct value * function, *classval;

  if (! target_has_execution)
    {
      /* Can't call into inferior to lookup class.  */
      return 0;
    }

  if (lookup_minimal_symbol("objc_lookUpClass", 0, 0))
    function = find_function_in_inferior("objc_lookUpClass", NULL);
  else if (lookup_minimal_symbol ("objc_lookup_class", 0, 0))
    function = find_function_in_inferior("objc_lookup_class", NULL);
  else
    {
      complaint (&symfile_complaints, _("no way to lookup Objective-C classes"));
      return 0;
    }

  classval = value_string (classname, strlen (classname) + 1);
  classval = value_coerce_array (classval);
  return (CORE_ADDR) value_as_long (call_function_by_hand (function, 
                                                           1, &classval));
}

CORE_ADDR
lookup_child_selector (char *selname)
{
  struct value * function, *selstring;

  if (! target_has_execution)
    {
      /* Can't call into inferior to lookup selector.  */
      return 0;
    }

  if (lookup_minimal_symbol("sel_getUid", 0, 0))
    function = find_function_in_inferior("sel_getUid", NULL);
  else if (lookup_minimal_symbol ("sel_get_any_uid", 0, 0))
    function = find_function_in_inferior("sel_get_any_uid", NULL);
  else
    {
      complaint (&symfile_complaints, _("no way to lookup Objective-C selectors"));
      return 0;
    }

  selstring = value_coerce_array (value_string (selname, 
                                                strlen (selname) + 1));
  return value_as_long (call_function_by_hand (function, 1, &selstring));
}

struct value * 
value_nsstring (char *ptr, int len)
{
  struct value *stringValue[3];
  struct value *function, *nsstringValue;
  struct symbol *sym;
  struct type *type;
  struct objfile *objf;
  struct gdbarch *gdbarch;

  if (!target_has_execution)
    return 0;           /* Can't call into inferior to create NSString.  */

  stringValue[2] = value_string(ptr, len);
  stringValue[2] = value_coerce_array(stringValue[2]);
  /* _NSNewStringFromCString replaces "istr" after Lantern2A.  */
  if (lookup_minimal_symbol("_NSNewStringFromCString", 0, 0))
    {
      function = find_function_in_inferior("_NSNewStringFromCString", &objf);
      nsstringValue = call_function_by_hand(function, 1, &stringValue[2]);
    }
  else if (lookup_minimal_symbol("istr", 0, 0))
    {
      function = find_function_in_inferior("istr", &objf);
      nsstringValue = call_function_by_hand(function, 1, &stringValue[2]);
    }
  else if (lookup_minimal_symbol("+[NSString stringWithCString:]", 0, 0))
    {
      function
	= find_function_in_inferior("+[NSString stringWithCString:]", &objf);
      type = builtin_type (get_objfile_arch (objf))->builtin_long;

      stringValue[0] = value_from_longest 
	(type, lookup_objc_class ("NSString"));
      stringValue[1] = value_from_longest 
	(type, lookup_child_selector ("stringWithCString:"));
      nsstringValue = call_function_by_hand(function, 3, &stringValue[0]);
    }
  else
    error (_("NSString: internal error -- no way to create new NSString"));

  gdbarch = get_objfile_arch (objf);

  sym = lookup_struct_typedef("NSString", 0, 1);
  if (sym == NULL)
    sym = lookup_struct_typedef("NXString", 0, 1);
  if (sym == NULL)
    type = builtin_type (gdbarch)->builtin_data_ptr;
  else
    type = lookup_pointer_type(SYMBOL_TYPE (sym));

  deprecated_set_value_type (nsstringValue, type);
  return nsstringValue;
}

/* Objective-C name demangling.  */

char *
objc_demangle (const char *mangled, int options)
{
  char *demangled, *cp;

  if (mangled[0] == '_' &&
     (mangled[1] == 'i' || mangled[1] == 'c') &&
      mangled[2] == '_')
    {
      cp = demangled = xmalloc(strlen(mangled) + 2);

      if (mangled[1] == 'i')
        *cp++ = '-';            /* for instance method */
      else
        *cp++ = '+';            /* for class    method */

      *cp++ = '[';              /* opening left brace  */
      strcpy(cp, mangled+3);    /* tack on the rest of the mangled name */

      while (*cp && *cp == '_')
        cp++;                   /* skip any initial underbars in class name */

      cp = strchr(cp, '_');
      if (!cp)                  /* find first non-initial underbar */
        {
          xfree(demangled);     /* not mangled name */
          return NULL;
        }
      if (cp[1] == '_') {       /* easy case: no category name     */
        *cp++ = ' ';            /* replace two '_' with one ' '    */
        strcpy(cp, mangled + (cp - demangled) + 2);
      }
      else {
        *cp++ = '(';            /* less easy case: category name */
        cp = strchr(cp, '_');
        if (!cp)
          {
            xfree(demangled);   /* not mangled name */
            return NULL;
          }
        *cp++ = ')';
        *cp++ = ' ';            /* overwriting 1st char of method name...  */
        strcpy(cp, mangled + (cp - demangled)); /* get it back */
      }

      while (*cp && *cp == '_')
        cp++;                   /* skip any initial underbars in method name */

      for (; *cp; cp++)
        if (*cp == '_')
          *cp = ':';            /* replace remaining '_' with ':' */

      *cp++ = ']';              /* closing right brace */
      *cp++ = 0;                /* string terminator */
      return demangled;
    }
  else
    return NULL;        /* Not an objc mangled name.  */
}

/* Print the character C on STREAM as part of the contents of a
   literal string whose delimiter is QUOTER.  Note that that format
   for printing characters and strings is language specific.  */

static void
objc_emit_char (int c, struct ui_file *stream, int quoter)
{

  c &= 0xFF;                    /* Avoid sign bit follies.  */

  if (PRINT_LITERAL_FORM (c))
    {
      if (c == '\\' || c == quoter)
        {
          fputs_filtered ("\\", stream);
        }
      fprintf_filtered (stream, "%c", c);
    }
  else
    {
      switch (c)
        {
        case '\n':
          fputs_filtered ("\\n", stream);
          break;
        case '\b':
          fputs_filtered ("\\b", stream);
          break;
        case '\t':
          fputs_filtered ("\\t", stream);
          break;
        case '\f':
          fputs_filtered ("\\f", stream);
          break;
        case '\r':
          fputs_filtered ("\\r", stream);
          break;
        case '\033':
          fputs_filtered ("\\e", stream);
          break;
        case '\007':
          fputs_filtered ("\\a", stream);
          break;
        default:
          fprintf_filtered (stream, "\\%.3o", (unsigned int) c);
          break;
        }
    }
}

static void
objc_printchar (int c, struct ui_file *stream)
{
  fputs_filtered ("'", stream);
  objc_emit_char (c, stream, '\'');
  fputs_filtered ("'", stream);
}

/* Print the character string STRING, printing at most LENGTH
   characters.  Printing stops early if the number hits print_max;
   repeat counts are printed as appropriate.  Print ellipses at the
   end if we had to stop before printing LENGTH characters, or if
   FORCE_ELLIPSES.  */

static void
objc_printstr (struct ui_file *stream, const gdb_byte *string, 
               unsigned int length, int width, int force_ellipses)
{
  unsigned int i;
  unsigned int things_printed = 0;
  int in_quotes = 0;
  int need_comma = 0;

  /* If the string was not truncated due to `set print elements', and
     the last byte of it is a null, we don't print that, in
     traditional C style.  */
  if ((!force_ellipses) && length > 0 && string[length-1] == '\0')
    length--;

  if (length == 0)
    {
      fputs_filtered ("\"\"", stream);
      return;
    }

  for (i = 0; i < length && things_printed < print_max; ++i)
    {
      /* Position of the character we are examining to see whether it
         is repeated.  */
      unsigned int rep1;
      /* Number of repetitions we have detected so far.  */
      unsigned int reps;

      QUIT;

      if (need_comma)
        {
          fputs_filtered (", ", stream);
          need_comma = 0;
        }

      rep1 = i + 1;
      reps = 1;
      while (rep1 < length && string[rep1] == string[i])
        {
          ++rep1;
          ++reps;
        }

      if (reps > repeat_count_threshold)
        {
          if (in_quotes)
            {
              if (inspect_it)
                fputs_filtered ("\\\", ", stream);
              else
                fputs_filtered ("\", ", stream);
              in_quotes = 0;
            }
          objc_printchar (string[i], stream);
          fprintf_filtered (stream, " <repeats %u times>", reps);
          i = rep1 - 1;
          things_printed += repeat_count_threshold;
          need_comma = 1;
        }
      else
        {
          if (!in_quotes)
            {
              if (inspect_it)
                fputs_filtered ("\\\"", stream);
              else
                fputs_filtered ("\"", stream);
              in_quotes = 1;
            }
          objc_emit_char (string[i], stream, '"');
          ++things_printed;
        }
    }

  /* Terminate the quotes if necessary.  */
  if (in_quotes)
    {
      if (inspect_it)
        fputs_filtered ("\\\"", stream);
      else
        fputs_filtered ("\"", stream);
    }

  if (force_ellipses || i < length)
    fputs_filtered ("...", stream);
}

/* Determine if we are currently in the Objective-C dispatch function.
   If so, get the address of the method function that the dispatcher
   would call and use that as the function to step into instead. Also
   skip over the trampoline for the function (if any).  This is better
   for the user since they are only interested in stepping into the
   method function anyway.  */
static CORE_ADDR 
objc_skip_trampoline (struct frame_info *frame, CORE_ADDR stop_pc)
{
  CORE_ADDR real_stop_pc;
  CORE_ADDR method_stop_pc;
  
  real_stop_pc = gdbarch_skip_trampoline_code
                   (current_gdbarch, frame, stop_pc);

  if (real_stop_pc != 0)
    find_objc_msgcall (real_stop_pc, &method_stop_pc);
  else
    find_objc_msgcall (stop_pc, &method_stop_pc);

  if (method_stop_pc)
    {
      real_stop_pc = gdbarch_skip_trampoline_code
                       (current_gdbarch, frame, method_stop_pc);
      if (real_stop_pc == 0)
        real_stop_pc = method_stop_pc;
    }

  return real_stop_pc;
}


/* Table mapping opcodes into strings for printing operators
   and precedences of the operators.  */

static const struct op_print objc_op_print_tab[] =
  {
    {",",  BINOP_COMMA, PREC_COMMA, 0},
    {"=",  BINOP_ASSIGN, PREC_ASSIGN, 1},
    {"||", BINOP_LOGICAL_OR, PREC_LOGICAL_OR, 0},
    {"&&", BINOP_LOGICAL_AND, PREC_LOGICAL_AND, 0},
    {"|",  BINOP_BITWISE_IOR, PREC_BITWISE_IOR, 0},
    {"^",  BINOP_BITWISE_XOR, PREC_BITWISE_XOR, 0},
    {"&",  BINOP_BITWISE_AND, PREC_BITWISE_AND, 0},
    {"==", BINOP_EQUAL, PREC_EQUAL, 0},
    {"!=", BINOP_NOTEQUAL, PREC_EQUAL, 0},
    {"<=", BINOP_LEQ, PREC_ORDER, 0},
    {">=", BINOP_GEQ, PREC_ORDER, 0},
    {">",  BINOP_GTR, PREC_ORDER, 0},
    {"<",  BINOP_LESS, PREC_ORDER, 0},
    {">>", BINOP_RSH, PREC_SHIFT, 0},
    {"<<", BINOP_LSH, PREC_SHIFT, 0},
    {"+",  BINOP_ADD, PREC_ADD, 0},
    {"-",  BINOP_SUB, PREC_ADD, 0},
    {"*",  BINOP_MUL, PREC_MUL, 0},
    {"/",  BINOP_DIV, PREC_MUL, 0},
    {"%",  BINOP_REM, PREC_MUL, 0},
    {"@",  BINOP_REPEAT, PREC_REPEAT, 0},
    {"-",  UNOP_NEG, PREC_PREFIX, 0},
    {"!",  UNOP_LOGICAL_NOT, PREC_PREFIX, 0},
    {"~",  UNOP_COMPLEMENT, PREC_PREFIX, 0},
    {"*",  UNOP_IND, PREC_PREFIX, 0},
    {"&",  UNOP_ADDR, PREC_PREFIX, 0},
    {"sizeof ", UNOP_SIZEOF, PREC_PREFIX, 0},
    {"++", UNOP_PREINCREMENT, PREC_PREFIX, 0},
    {"--", UNOP_PREDECREMENT, PREC_PREFIX, 0},
    {NULL, OP_NULL, PREC_NULL, 0}
};

const struct language_defn objc_language_defn = {
  "objective-c",                /* Language name */
  language_objc,
  range_check_off,
  type_check_off,
  case_sensitive_on,
  array_row_major,
  &exp_descriptor_standard,
  objc_parse,
  objc_error,
  null_post_parser,
  objc_printchar,               /* Print a character constant */
  objc_printstr,                /* Function to print string constant */
  objc_emit_char,
  c_print_type,                 /* Print a type using appropriate syntax */
  c_val_print,                  /* Print a value using appropriate syntax */
  c_value_print,                /* Print a top-level value */
  objc_skip_trampoline,         /* Language specific skip_trampoline */
  "self",                       /* name_of_this */
  basic_lookup_symbol_nonlocal, /* lookup_symbol_nonlocal */
  basic_lookup_transparent_type,/* lookup_transparent_type */
  objc_demangle,                /* Language specific symbol demangler */
  NULL,                         /* Language specific class_name_from_physname */
  objc_op_print_tab,            /* Expression operators for printing */
  1,                            /* C-style arrays */
  0,                            /* String lower bound */
  default_word_break_characters,
  default_make_symbol_completion_list,
  c_language_arch_info,
  default_print_array_index,
  default_pass_by_reference,
  LANG_MAGIC
};

/*
 * ObjC:
 * Following functions help construct Objective-C message calls 
 */

struct selname          /* For parsing Objective-C.  */
  {
    struct selname *next;
    char *msglist_sel;
    int msglist_len;
  };

static int msglist_len;
static struct selname *selname_chain;
static char *msglist_sel;

void
start_msglist(void)
{
  struct selname *new = 
    (struct selname *) xmalloc (sizeof (struct selname));

  new->next = selname_chain;
  new->msglist_len = msglist_len;
  new->msglist_sel = msglist_sel;
  msglist_len = 0;
  msglist_sel = (char *)xmalloc(1);
  *msglist_sel = 0;
  selname_chain = new;
}

void
add_msglist(struct stoken *str, int addcolon)
{
  char *s, *p;
  int len, plen;

  if (str == 0) {               /* Unnamed arg, or...  */
    if (addcolon == 0) {        /* variable number of args.  */
      msglist_len++;
      return;
    }
    p = "";
    plen = 0;
  } else {
    p = str->ptr;
    plen = str->length;
  }
  len = plen + strlen(msglist_sel) + 2;
  s = (char *)xmalloc(len);
  strcpy(s, msglist_sel);
  strncat(s, p, plen);
  xfree(msglist_sel);
  msglist_sel = s;
  if (addcolon) {
    s[len-2] = ':';
    s[len-1] = 0;
    msglist_len++;
  } else
    s[len-2] = '\0';
}

int
end_msglist(void)
{
  int val = msglist_len;
  struct selname *sel = selname_chain;
  char *p = msglist_sel;
  CORE_ADDR selid;

  selname_chain = sel->next;
  msglist_len = sel->msglist_len;
  msglist_sel = sel->msglist_sel;
  selid = lookup_child_selector(p);
  if (!selid)
    error (_("Can't find selector \"%s\""), p);
  write_exp_elt_longcst (selid);
  xfree(p);
  write_exp_elt_longcst (val);  /* Number of args */
  xfree(sel);

  return val;
}

/*
 * Function: specialcmp (char *a, char *b)
 *
 * Special strcmp: treats ']' and ' ' as end-of-string.
 * Used for qsorting lists of objc methods (either by class or selector).
 */

static int
specialcmp (char *a, char *b)
{
  while (*a && *a != ' ' && *a != ']' && *b && *b != ' ' && *b != ']')
    {
      if (*a != *b)
        return *a - *b;
      a++, b++;
    }
  if (*a && *a != ' ' && *a != ']')
    return  1;          /* a is longer therefore greater */
  if (*b && *b != ' ' && *b != ']')
    return -1;          /* a is shorter therefore lesser */
  return    0;          /* a and b are identical */
}

/*
 * Function: compare_selectors (const void *, const void *)
 *
 * Comparison function for use with qsort.  Arguments are symbols or
 * msymbols Compares selector part of objc method name alphabetically.
 */

static int
compare_selectors (const void *a, const void *b)
{
  char *aname, *bname;

  aname = SYMBOL_PRINT_NAME (*(struct symbol **) a);
  bname = SYMBOL_PRINT_NAME (*(struct symbol **) b);
  if (aname == NULL || bname == NULL)
    error (_("internal: compare_selectors(1)"));

  aname = strchr(aname, ' ');
  bname = strchr(bname, ' ');
  if (aname == NULL || bname == NULL)
    error (_("internal: compare_selectors(2)"));

  return specialcmp (aname+1, bname+1);
}

/*
 * Function: selectors_info (regexp, from_tty)
 *
 * Implements the "Info selectors" command.  Takes an optional regexp
 * arg.  Lists all objective c selectors that match the regexp.  Works
 * by grepping thru all symbols for objective c methods.  Output list
 * is sorted and uniqued. 
 */

static void
selectors_info (char *regexp, int from_tty)
{
  struct objfile        *objfile;
  struct minimal_symbol *msymbol;
  char                  *name;
  char                  *val;
  int                    matches = 0;
  int                    maxlen  = 0;
  int                    ix;
  char                   myregexp[2048];
  char                   asel[256];
  struct symbol        **sym_arr;
  int                    plusminus = 0;

  if (regexp == NULL)
    strcpy(myregexp, ".*]");    /* Null input, match all objc methods.  */
  else
    {
      if (*regexp == '+' || *regexp == '-')
        { /* User wants only class methods or only instance methods.  */
          plusminus = *regexp++;
          while (*regexp == ' ' || *regexp == '\t')
            regexp++;
        }
      if (*regexp == '\0')
        strcpy(myregexp, ".*]");
      else
        {
          strcpy(myregexp, regexp);
          if (myregexp[strlen(myregexp) - 1] == '$') /* end of selector */
            myregexp[strlen(myregexp) - 1] = ']';    /* end of method name */
          else
            strcat(myregexp, ".*]");
        }
    }

  if (regexp != NULL)
    {
      val = re_comp (myregexp);
      if (val != 0)
        error (_("Invalid regexp (%s): %s"), val, regexp);
    }

  /* First time thru is JUST to get max length and count.  */
  ALL_MSYMBOLS (objfile, msymbol)
    {
      QUIT;
      name = SYMBOL_NATURAL_NAME (msymbol);
      if (name &&
         (name[0] == '-' || name[0] == '+') &&
          name[1] == '[')               /* Got a method name.  */
        {
          /* Filter for class/instance methods.  */
          if (plusminus && name[0] != plusminus)
            continue;
          /* Find selector part.  */
          name = (char *) strchr(name+2, ' ');
          if (regexp == NULL || re_exec(++name) != 0)
            { 
              char *mystart = name;
              char *myend   = (char *) strchr(mystart, ']');
              
              if (myend && (myend - mystart > maxlen))
                maxlen = myend - mystart;       /* Get longest selector.  */
              matches++;
            }
        }
    }
  if (matches)
    {
      printf_filtered (_("Selectors matching \"%s\":\n\n"), 
                       regexp ? regexp : "*");

      sym_arr = alloca (matches * sizeof (struct symbol *));
      matches = 0;
      ALL_MSYMBOLS (objfile, msymbol)
        {
          QUIT;
          name = SYMBOL_NATURAL_NAME (msymbol);
          if (name &&
             (name[0] == '-' || name[0] == '+') &&
              name[1] == '[')           /* Got a method name.  */
            {
              /* Filter for class/instance methods.  */
              if (plusminus && name[0] != plusminus)
                continue;
              /* Find selector part.  */
              name = (char *) strchr(name+2, ' ');
              if (regexp == NULL || re_exec(++name) != 0)
                sym_arr[matches++] = (struct symbol *) msymbol;
            }
        }

      qsort (sym_arr, matches, sizeof (struct minimal_symbol *), 
             compare_selectors);
      /* Prevent compare on first iteration.  */
      asel[0] = 0;
      for (ix = 0; ix < matches; ix++)  /* Now do the output.  */
        {
          char *p = asel;

          QUIT;
          name = SYMBOL_NATURAL_NAME (sym_arr[ix]);
          name = strchr (name, ' ') + 1;
          if (p[0] && specialcmp(name, p) == 0)
            continue;           /* Seen this one already (not unique).  */

          /* Copy selector part.  */
          while (*name && *name != ']')
            *p++ = *name++;
          *p++ = '\0';
          /* Print in columns.  */
          puts_filtered_tabular(asel, maxlen + 1, 0);
        }
      begin_line();
    }
  else
    printf_filtered (_("No selectors matching \"%s\"\n"), regexp ? regexp : "*");
}

/*
 * Function: compare_classes (const void *, const void *)
 *
 * Comparison function for use with qsort.  Arguments are symbols or
 * msymbols Compares class part of objc method name alphabetically. 
 */

static int
compare_classes (const void *a, const void *b)
{
  char *aname, *bname;

  aname = SYMBOL_PRINT_NAME (*(struct symbol **) a);
  bname = SYMBOL_PRINT_NAME (*(struct symbol **) b);
  if (aname == NULL || bname == NULL)
    error (_("internal: compare_classes(1)"));

  return specialcmp (aname+1, bname+1);
}

/*
 * Function: classes_info(regexp, from_tty)
 *
 * Implements the "info classes" command for objective c classes.
 * Lists all objective c classes that match the optional regexp.
 * Works by grepping thru the list of objective c methods.  List will
 * be sorted and uniqued (since one class may have many methods).
 * BUGS: will not list a class that has no methods. 
 */

static void
classes_info (char *regexp, int from_tty)
{
  struct objfile        *objfile;
  struct minimal_symbol *msymbol;
  char                  *name;
  char                  *val;
  int                    matches = 0;
  int                    maxlen  = 0;
  int                    ix;
  char                   myregexp[2048];
  char                   aclass[256];
  struct symbol        **sym_arr;

  if (regexp == NULL)
    strcpy(myregexp, ".* ");    /* Null input: match all objc classes.  */
  else
    {
      strcpy(myregexp, regexp);
      if (myregexp[strlen(myregexp) - 1] == '$')
        /* In the method name, the end of the class name is marked by ' '.  */
        myregexp[strlen(myregexp) - 1] = ' ';
      else
        strcat(myregexp, ".* ");
    }

  if (regexp != NULL)
    {
      val = re_comp (myregexp);
      if (val != 0)
        error (_("Invalid regexp (%s): %s"), val, regexp);
    }

  /* First time thru is JUST to get max length and count.  */
  ALL_MSYMBOLS (objfile, msymbol)
    {
      QUIT;
      name = SYMBOL_NATURAL_NAME (msymbol);
      if (name &&
         (name[0] == '-' || name[0] == '+') &&
          name[1] == '[')                       /* Got a method name.  */
        if (regexp == NULL || re_exec(name+2) != 0)
          { 
            /* Compute length of classname part.  */
            char *mystart = name + 2;
            char *myend   = (char *) strchr(mystart, ' ');
            
            if (myend && (myend - mystart > maxlen))
              maxlen = myend - mystart;
            matches++;
          }
    }
  if (matches)
    {
      printf_filtered (_("Classes matching \"%s\":\n\n"), 
                       regexp ? regexp : "*");
      sym_arr = alloca (matches * sizeof (struct symbol *));
      matches = 0;
      ALL_MSYMBOLS (objfile, msymbol)
        {
          QUIT;
          name = SYMBOL_NATURAL_NAME (msymbol);
          if (name &&
             (name[0] == '-' || name[0] == '+') &&
              name[1] == '[')                   /* Got a method name.  */
            if (regexp == NULL || re_exec(name+2) != 0)
                sym_arr[matches++] = (struct symbol *) msymbol;
        }

      qsort (sym_arr, matches, sizeof (struct minimal_symbol *), 
             compare_classes);
      /* Prevent compare on first iteration.  */
      aclass[0] = 0;
      for (ix = 0; ix < matches; ix++)  /* Now do the output.  */
        {
          char *p = aclass;

          QUIT;
          name = SYMBOL_NATURAL_NAME (sym_arr[ix]);
          name += 2;
          if (p[0] && specialcmp(name, p) == 0)
            continue;   /* Seen this one already (not unique).  */

          /* Copy class part of method name.  */
          while (*name && *name != ' ')
            *p++ = *name++;
          *p++ = '\0';
          /* Print in columns.  */
          puts_filtered_tabular(aclass, maxlen + 1, 0);
        }
      begin_line();
    }
  else
    printf_filtered (_("No classes matching \"%s\"\n"), regexp ? regexp : "*");
}

/* 
 * Function: find_imps (char *selector, struct symbol **sym_arr)
 *
 * Input:  a string representing a selector
 *         a pointer to an array of symbol pointers
 *         possibly a pointer to a symbol found by the caller.
 *
 * Output: number of methods that implement that selector.  Side
 * effects: The array of symbol pointers is filled with matching syms.
 *
 * By analogy with function "find_methods" (symtab.c), builds a list
 * of symbols matching the ambiguous input, so that "decode_line_2"
 * (symtab.c) can list them and ask the user to choose one or more.
 * In this case the matches are objective c methods
 * ("implementations") matching an objective c selector.
 *
 * Note that it is possible for a normal (c-style) function to have
 * the same name as an objective c selector.  To prevent the selector
 * from eclipsing the function, we allow the caller (decode_line_1) to
 * search for such a function first, and if it finds one, pass it in
 * to us.  We will then integrate it into the list.  We also search
 * for one here, among the minsyms.
 *
 * NOTE: if NUM_DEBUGGABLE is non-zero, the sym_arr will be divided
 *       into two parts: debuggable (struct symbol) syms, and
 *       non_debuggable (struct minimal_symbol) syms.  The debuggable
 *       ones will come first, before NUM_DEBUGGABLE (which will thus
 *       be the index of the first non-debuggable one). 
 */

/*
 * Function: total_number_of_imps (char *selector);
 *
 * Input:  a string representing a selector 
 * Output: number of methods that implement that selector.
 *
 * By analogy with function "total_number_of_methods", this allows
 * decode_line_1 (symtab.c) to detect if there are objective c methods
 * matching the input, and to allocate an array of pointers to them
 * which can be manipulated by "decode_line_2" (also in symtab.c).
 */

char * 
parse_selector (char *method, char **selector)
{
  char *s1 = NULL;
  char *s2 = NULL;
  int found_quote = 0;

  char *nselector = NULL;

  gdb_assert (selector != NULL);

  s1 = method;

  while (isspace (*s1))
    s1++;
  if (*s1 == '\'') 
    {
      found_quote = 1;
      s1++;
    }
  while (isspace (*s1))
    s1++;
   
  nselector = s1;
  s2 = s1;

  for (;;) {
    if (isalnum (*s2) || (*s2 == '_') || (*s2 == ':'))
      *s1++ = *s2;
    else if (isspace (*s2))
      ;
    else if ((*s2 == '\0') || (*s2 == '\''))
      break;
    else
      return NULL;
    s2++;
  }
  *s1++ = '\0';

  while (isspace (*s2))
    s2++;
  if (found_quote)
    {
      if (*s2 == '\'') 
        s2++;
      while (isspace (*s2))
        s2++;
    }

  if (selector != NULL)
    *selector = nselector;

  return s2;
}

char * 
parse_method (char *method, char *type, char **class, 
              char **category, char **selector)
{
  char *s1 = NULL;
  char *s2 = NULL;
  int found_quote = 0;

  char ntype = '\0';
  char *nclass = NULL;
  char *ncategory = NULL;
  char *nselector = NULL;

  gdb_assert (type != NULL);
  gdb_assert (class != NULL);
  gdb_assert (category != NULL);
  gdb_assert (selector != NULL);
  
  s1 = method;

  while (isspace (*s1))
    s1++;
  if (*s1 == '\'') 
    {
      found_quote = 1;
      s1++;
    }
  while (isspace (*s1))
    s1++;
  
  if ((s1[0] == '+') || (s1[0] == '-'))
    ntype = *s1++;

  while (isspace (*s1))
    s1++;

  if (*s1 != '[')
    return NULL;
  s1++;

  nclass = s1;
  while (isalnum (*s1) || (*s1 == '_'))
    s1++;
  
  s2 = s1;
  while (isspace (*s2))
    s2++;
  
  if (*s2 == '(')
    {
      s2++;
      while (isspace (*s2))
        s2++;
      ncategory = s2;
      while (isalnum (*s2) || (*s2 == '_'))
        s2++;
      *s2++ = '\0';
    }

  /* Truncate the class name now that we're not using the open paren.  */
  *s1++ = '\0';

  nselector = s2;
  s1 = s2;

  for (;;) {
    if (isalnum (*s2) || (*s2 == '_') || (*s2 == ':'))
      *s1++ = *s2;
    else if (isspace (*s2))
      ;
    else if (*s2 == ']')
      break;
    else
      return NULL;
    s2++;
  }
  *s1++ = '\0';
  s2++;

  while (isspace (*s2))
    s2++;
  if (found_quote)
    {
      if (*s2 != '\'') 
        return NULL;
      s2++;
      while (isspace (*s2))
        s2++;
    }

  if (type != NULL)
    *type = ntype;
  if (class != NULL)
    *class = nclass;
  if (category != NULL)
    *category = ncategory;
  if (selector != NULL)
    *selector = nselector;

  return s2;
}

static void
find_methods (struct symtab *symtab, char type, 
              const char *class, const char *category, 
              const char *selector, struct symbol **syms, 
              unsigned int *nsym, unsigned int *ndebug)
{
  struct objfile *objfile = NULL;
  struct minimal_symbol *msymbol = NULL;
  struct block *block = NULL;
  struct symbol *sym = NULL;

  char *symname = NULL;

  char ntype = '\0';
  char *nclass = NULL;
  char *ncategory = NULL;
  char *nselector = NULL;

  unsigned int csym = 0;
  unsigned int cdebug = 0;

  static char *tmp = NULL;
  static unsigned int tmplen = 0;

  gdb_assert (nsym != NULL);
  gdb_assert (ndebug != NULL);

  if (symtab)
    block = BLOCKVECTOR_BLOCK (BLOCKVECTOR (symtab), STATIC_BLOCK);

  ALL_MSYMBOLS (objfile, msymbol)
    {
      QUIT;

      if ((msymbol->type != mst_text) && (msymbol->type != mst_file_text))
        /* Not a function or method.  */
        continue;

      if (symtab)
        if ((SYMBOL_VALUE_ADDRESS (msymbol) <  BLOCK_START (block)) ||
            (SYMBOL_VALUE_ADDRESS (msymbol) >= BLOCK_END (block)))
          /* Not in the specified symtab.  */
          continue;

      symname = SYMBOL_NATURAL_NAME (msymbol);
      if (symname == NULL)
        continue;

      if ((symname[0] != '-' && symname[0] != '+') || (symname[1] != '['))
        /* Not a method name.  */
        continue;
      
      while ((strlen (symname) + 1) >= tmplen)
        {
          tmplen = (tmplen == 0) ? 1024 : tmplen * 2;
          tmp = xrealloc (tmp, tmplen);
        }
      strcpy (tmp, symname);

      if (parse_method (tmp, &ntype, &nclass, &ncategory, &nselector) == NULL)
        continue;
      
      if ((type != '\0') && (ntype != type))
        continue;

      if ((class != NULL) 
          && ((nclass == NULL) || (strcmp (class, nclass) != 0)))
        continue;

      if ((category != NULL) && 
          ((ncategory == NULL) || (strcmp (category, ncategory) != 0)))
        continue;

      if ((selector != NULL) && 
          ((nselector == NULL) || (strcmp (selector, nselector) != 0)))
        continue;

      sym = find_pc_function (SYMBOL_VALUE_ADDRESS (msymbol));
      if (sym != NULL)
        {
          const char *newsymname = SYMBOL_NATURAL_NAME (sym);
          
          if (strcmp (symname, newsymname) == 0)
            {
              /* Found a high-level method sym: swap it into the
                 lower part of sym_arr (below num_debuggable).  */
              if (syms != NULL)
                {
                  syms[csym] = syms[cdebug];
                  syms[cdebug] = sym;
                }
              csym++;
              cdebug++;
            }
          else
            {
              warning (
"debugging symbol \"%s\" does not match minimal symbol (\"%s\"); ignoring",
                       newsymname, symname);
              if (syms != NULL)
                syms[csym] = (struct symbol *) msymbol;
              csym++;
            }
        }
      else 
        {
          /* Found a non-debuggable method symbol.  */
          if (syms != NULL)
            syms[csym] = (struct symbol *) msymbol;
          csym++;
        }
    }

  if (nsym != NULL)
    *nsym = csym;
  if (ndebug != NULL)
    *ndebug = cdebug;
}

char *find_imps (struct symtab *symtab, struct block *block,
                 char *method, struct symbol **syms, 
                 unsigned int *nsym, unsigned int *ndebug)
{
  char type = '\0';
  char *class = NULL;
  char *category = NULL;
  char *selector = NULL;

  unsigned int csym = 0;
  unsigned int cdebug = 0;

  unsigned int ncsym = 0;
  unsigned int ncdebug = 0;

  char *buf = NULL;
  char *tmp = NULL;

  gdb_assert (nsym != NULL);
  gdb_assert (ndebug != NULL);

  if (nsym != NULL)
    *nsym = 0;
  if (ndebug != NULL)
    *ndebug = 0;

  buf = (char *) alloca (strlen (method) + 1);
  strcpy (buf, method);
  tmp = parse_method (buf, &type, &class, &category, &selector);

  if (tmp == NULL) {
    
    struct symbol *sym = NULL;
    struct minimal_symbol *msym = NULL;
    
    strcpy (buf, method);
    tmp = parse_selector (buf, &selector);
    
    if (tmp == NULL)
      return NULL;
    
    sym = lookup_symbol (selector, block, VAR_DOMAIN, 0);
    if (sym != NULL) 
      {
        if (syms)
          syms[csym] = sym;
        csym++;
        cdebug++;
      }

    if (sym == NULL)
      msym = lookup_minimal_symbol (selector, 0, 0);

    if (msym != NULL) 
      {
        if (syms)
          syms[csym] = (struct symbol *)msym;
        csym++;
      }
  }

  if (syms != NULL)
    find_methods (symtab, type, class, category, selector, 
                  syms + csym, &ncsym, &ncdebug);
  else
    find_methods (symtab, type, class, category, selector, 
                  NULL, &ncsym, &ncdebug);

  /* If we didn't find any methods, just return.  */
  if (ncsym == 0 && ncdebug == 0)
    return method;

  /* Take debug symbols from the second batch of symbols and swap them
   * with debug symbols from the first batch.  Repeat until either the
   * second section is out of debug symbols or the first section is
   * full of debug symbols.  Either way we have all debug symbols
   * packed to the beginning of the buffer.  
   */

  if (syms != NULL) 
    {
      while ((cdebug < csym) && (ncdebug > 0))
        {
          struct symbol *s = NULL;
          /* First non-debugging symbol.  */
          unsigned int i = cdebug;
          /* Last of second batch of debug symbols.  */
          unsigned int j = csym + ncdebug - 1;

          s = syms[j];
          syms[j] = syms[i];
          syms[i] = s;

          /* We've moved a symbol from the second debug section to the
             first one.  */
          cdebug++;
          ncdebug--;
        }
    }

  csym += ncsym;
  cdebug += ncdebug;

  if (nsym != NULL)
    *nsym = csym;
  if (ndebug != NULL)
    *ndebug = cdebug;

  if (syms == NULL)
    return method + (tmp - buf);

  if (csym > 1)
    {
      /* Sort debuggable symbols.  */
      if (cdebug > 1)
        qsort (syms, cdebug, sizeof (struct minimal_symbol *), 
               compare_classes);
      
      /* Sort minimal_symbols.  */
      if ((csym - cdebug) > 1)
        qsort (&syms[cdebug], csym - cdebug, 
               sizeof (struct minimal_symbol *), compare_classes);
    }
  /* Terminate the sym_arr list.  */
  syms[csym] = 0;

  return method + (tmp - buf);
}

static void 
print_object_command (char *args, int from_tty)
{
  struct value *object, *function, *description;
  CORE_ADDR string_addr, object_addr;
  int i = 0;
  gdb_byte c = 0;

  if (!args || !*args)
    error (
"The 'print-object' command requires an argument (an Objective-C object)");

  {
    struct expression *expr = parse_expression (args);
    struct cleanup *old_chain = 
      make_cleanup (free_current_contents, &expr);
    int pc = 0;

    object = expr->language_defn->la_exp_desc->evaluate_exp 
      (builtin_type (expr->gdbarch)->builtin_data_ptr, expr, &pc, EVAL_NORMAL);
    do_cleanups (old_chain);
  }

  /* Validate the address for sanity.  */
  object_addr = value_as_long (object);
  read_memory (object_addr, &c, 1);

  function = find_function_in_inferior ("_NSPrintForDebugger", NULL);
  if (function == NULL)
    error (_("Unable to locate _NSPrintForDebugger in child process"));

  description = call_function_by_hand (function, 1, &object);

  string_addr = value_as_long (description);
  if (string_addr == 0)
    error (_("object returns null description"));

  read_memory (string_addr + i++, &c, 1);
  if (c != 0)
    do
      { /* Read and print characters up to EOS.  */
        QUIT;
        printf_filtered ("%c", c);
        read_memory (string_addr + i++, &c, 1);
      } while (c != 0);
  else
    printf_filtered(_("<object returns empty description>"));
  printf_filtered ("\n");
}

/* The data structure 'methcalls' is used to detect method calls (thru
 * ObjC runtime lib functions objc_msgSend, objc_msgSendSuper, etc.),
 * and ultimately find the method being called. 
 */

struct objc_methcall {
  char *name;
 /* Return instance method to be called.  */
  int (*stop_at) (CORE_ADDR, CORE_ADDR *);
  /* Start of pc range corresponding to method invocation.  */
  CORE_ADDR begin;
  /* End of pc range corresponding to method invocation.  */
  CORE_ADDR end;
};

static int resolve_msgsend (CORE_ADDR pc, CORE_ADDR *new_pc);
static int resolve_msgsend_stret (CORE_ADDR pc, CORE_ADDR *new_pc);
static int resolve_msgsend_super (CORE_ADDR pc, CORE_ADDR *new_pc);
static int resolve_msgsend_super_stret (CORE_ADDR pc, CORE_ADDR *new_pc);

static struct objc_methcall methcalls[] = {
  { "_objc_msgSend", resolve_msgsend, 0, 0},
  { "_objc_msgSend_stret", resolve_msgsend_stret, 0, 0},
  { "_objc_msgSendSuper", resolve_msgsend_super, 0, 0},
  { "_objc_msgSendSuper_stret", resolve_msgsend_super_stret, 0, 0},
  { "_objc_getClass", NULL, 0, 0},
  { "_objc_getMetaClass", NULL, 0, 0}
};

#define nmethcalls (sizeof (methcalls) / sizeof (methcalls[0]))

/* The following function, "find_objc_msgsend", fills in the data
 * structure "objc_msgs" by finding the addresses of each of the
 * (currently four) functions that it holds (of which objc_msgSend is
 * the first).  This must be called each time symbols are loaded, in
 * case the functions have moved for some reason.  
 */

static void 
find_objc_msgsend (void)
{
  unsigned int i;
  for (i = 0; i < nmethcalls; i++) {

    struct minimal_symbol *func;

    /* Try both with and without underscore.  */
    func = lookup_minimal_symbol (methcalls[i].name, NULL, NULL);
    if ((func == NULL) && (methcalls[i].name[0] == '_')) {
      func = lookup_minimal_symbol (methcalls[i].name + 1, NULL, NULL);
    }
    if (func == NULL) { 
      methcalls[i].begin = 0;
      methcalls[i].end = 0;
      continue; 
    }
    
    methcalls[i].begin = SYMBOL_VALUE_ADDRESS (func);
    do {
      methcalls[i].end = SYMBOL_VALUE_ADDRESS (++func);
    } while (methcalls[i].begin == methcalls[i].end);
  }
}

/* find_objc_msgcall (replaces pc_off_limits)
 *
 * ALL that this function now does is to determine whether the input
 * address ("pc") is the address of one of the Objective-C message
 * dispatch functions (mainly objc_msgSend or objc_msgSendSuper), and
 * if so, it returns the address of the method that will be called.
 *
 * The old function "pc_off_limits" used to do a lot of other things
 * in addition, such as detecting shared library jump stubs and
 * returning the address of the shlib function that would be called.
 * That functionality has been moved into the gdbarch_skip_trampoline_code and
 * IN_SOLIB_TRAMPOLINE macros, which are resolved in the target-
 * dependent modules.  
 */

struct objc_submethod_helper_data {
  int (*f) (CORE_ADDR, CORE_ADDR *);
  CORE_ADDR pc;
  CORE_ADDR *new_pc;
};

static int 
find_objc_msgcall_submethod_helper (void * arg)
{
  struct objc_submethod_helper_data *s = 
    (struct objc_submethod_helper_data *) arg;

  if (s->f (s->pc, s->new_pc) == 0) 
    return 1;
  else 
    return 0;
}

static int 
find_objc_msgcall_submethod (int (*f) (CORE_ADDR, CORE_ADDR *),
                             CORE_ADDR pc, 
                             CORE_ADDR *new_pc)
{
  struct objc_submethod_helper_data s;

  s.f = f;
  s.pc = pc;
  s.new_pc = new_pc;

  if (catch_errors (find_objc_msgcall_submethod_helper,
                    (void *) &s,
                    "Unable to determine target of Objective-C method call (ignoring):\n",
                    RETURN_MASK_ALL) == 0) 
    return 1;
  else 
    return 0;
}

int 
find_objc_msgcall (CORE_ADDR pc, CORE_ADDR *new_pc)
{
  unsigned int i;

  find_objc_msgsend ();
  if (new_pc != NULL)
    {
      *new_pc = 0;
    }

  for (i = 0; i < nmethcalls; i++) 
    if ((pc >= methcalls[i].begin) && (pc < methcalls[i].end)) 
      {
        if (methcalls[i].stop_at != NULL) 
          return find_objc_msgcall_submethod (methcalls[i].stop_at, 
                                              pc, new_pc);
        else 
          return 0;
      }

  return 0;
}

extern initialize_file_ftype _initialize_objc_language; /* -Wmissing-prototypes */

void
_initialize_objc_language (void)
{
  add_language (&objc_language_defn);
  add_info ("selectors", selectors_info,    /* INFO SELECTORS command.  */
            _("All Objective-C selectors, or those matching REGEXP."));
  add_info ("classes", classes_info,        /* INFO CLASSES   command.  */
            _("All Objective-C classes, or those matching REGEXP."));
  add_com ("print-object", class_vars, print_object_command, 
           _("Ask an Objective-C object to print itself."));
  add_com_alias ("po", "print-object", class_vars, 1);
}

static void 
read_objc_method (CORE_ADDR addr, struct objc_method *method)
{
  int addrsize = get_addrsize();
  method->name  = read_memory_unsigned_integer (addr, addrsize);
  method->types = read_memory_unsigned_integer (addr += addrsize, addrsize);
  method->imp   = read_memory_unsigned_integer (addr + addrsize, addrsize);
}

static 
unsigned long read_objc_methlist_nmethods (CORE_ADDR addr)
{
  int intsize  = get_addrsize();
  int addrsize = get_addrsize();
  return read_memory_unsigned_integer (addr + addrsize, intsize);
}

static void 
read_objc_methlist_method (CORE_ADDR addr, unsigned long num, 
                           struct objc_method *method)
{
  int intsize  = get_addrsize();
  int addrsize = get_addrsize();
  // Apples 64bit ObjC runtime contains an extra int field
  int offset = (addrsize == 8) ? addrsize + 2*intsize : addrsize + intsize;
  gdb_assert (num < read_objc_methlist_nmethods (addr));
  read_objc_method (addr + offset + (3 * addrsize * num), method);
}

static void 
read_objc_object (CORE_ADDR addr, struct objc_object *object)
{
  object->isa = read_memory_unsigned_integer (addr, get_addrsize());
}

static void 
read_objc_super (CORE_ADDR addr, struct objc_super *super)
{
  int addrsize = get_addrsize();
  super->receiver = read_memory_unsigned_integer (addr, addrsize);
  super->class = read_memory_unsigned_integer (addr + addrsize, addrsize);
};

static void 
read_objc_class (CORE_ADDR addr, struct objc_class *class)
{
  int longsize = get_addrsize();
  int addrsize = get_addrsize();
  class->isa = read_memory_unsigned_integer (addr, addrsize);
  class->super_class = read_memory_unsigned_integer (addr += addrsize, addrsize);
  class->name = read_memory_unsigned_integer (addr += addrsize, addrsize);
  class->version = read_memory_unsigned_integer (addr += addrsize, longsize);
  class->info = read_memory_unsigned_integer (addr += longsize, longsize);
  class->instance_size = read_memory_unsigned_integer (addr += longsize, longsize);
  class->ivars = read_memory_unsigned_integer (addr += longsize, addrsize);
  class->methods = read_memory_unsigned_integer (addr += addrsize, addrsize);
  class->cache = read_memory_unsigned_integer (addr += addrsize, addrsize);
  class->protocols = read_memory_unsigned_integer (addr + addrsize, addrsize);
}

static CORE_ADDR
find_implementation_from_class (CORE_ADDR class, CORE_ADDR sel)
{
  int addrsize = get_addrsize();
  CORE_ADDR subclass = class;

  while (subclass != 0) 
    {

      struct objc_class class_str;
      unsigned mlistnum = 0;

      read_objc_class (subclass, &class_str);

      for (;;) 
        {
          CORE_ADDR mlist;
          unsigned long nmethods;
          unsigned long i;
      
          mlist = read_memory_unsigned_integer (class_str.methods + 
                                                (addrsize * mlistnum), addrsize);
          if (mlist == 0) 
            break;

          nmethods = read_objc_methlist_nmethods (mlist);

          for (i = 0; i < nmethods; i++) 
            {
              struct objc_method meth_str;
              read_objc_methlist_method (mlist, i, &meth_str);

#if 0
              fprintf (stderr, 
                       "checking method 0x%lx against selector 0x%lx\n", 
                       meth_str.name, sel);
#endif

              if (meth_str.name == sel) 
                /* FIXME: hppa arch was doing a pointer dereference
                   here. There needs to be a better way to do that.  */
                return meth_str.imp;
            }
          mlistnum++;
        }
      subclass = class_str.super_class;
    }

  return 0;
}

static CORE_ADDR
find_implementation (CORE_ADDR object, CORE_ADDR sel)
{
  struct objc_object ostr;

  if (object == 0)
    return 0;
  read_objc_object (object, &ostr);
  if (ostr.isa == 0)
    return 0;

  return find_implementation_from_class (ostr.isa, sel);
}

static int
resolve_msgsend (CORE_ADDR pc, CORE_ADDR *new_pc)
{
  struct frame_info *frame = get_current_frame ();
  struct gdbarch *gdbarch = get_frame_arch (frame);
  struct type *ptr_type = builtin_type (gdbarch)->builtin_func_ptr;

  CORE_ADDR object;
  CORE_ADDR sel;
  CORE_ADDR res;

  object = gdbarch_fetch_pointer_argument (gdbarch, frame, 0, ptr_type);
  sel = gdbarch_fetch_pointer_argument (gdbarch, frame, 1, ptr_type);

  res = find_implementation (object, sel);
  if (new_pc != 0)
    *new_pc = res;
  if (res == 0)
    return 1;
  return 0;
}

static int
resolve_msgsend_stret (CORE_ADDR pc, CORE_ADDR *new_pc)
{
  struct frame_info *frame = get_current_frame ();
  struct gdbarch *gdbarch = get_frame_arch (frame);
  struct type *ptr_type = builtin_type (gdbarch)->builtin_func_ptr;

  CORE_ADDR object;
  CORE_ADDR sel;
  CORE_ADDR res;

  object = gdbarch_fetch_pointer_argument (gdbarch, frame, 1, ptr_type);
  sel = gdbarch_fetch_pointer_argument (gdbarch, frame, 2, ptr_type);

  res = find_implementation (object, sel);
  if (new_pc != 0)
    *new_pc = res;
  if (res == 0)
    return 1;
  return 0;
}

static int
resolve_msgsend_super (CORE_ADDR pc, CORE_ADDR *new_pc)
{
  struct frame_info *frame = get_current_frame ();
  struct gdbarch *gdbarch = get_frame_arch (frame);
  struct type *ptr_type = builtin_type (gdbarch)->builtin_func_ptr;

  struct objc_super sstr;

  CORE_ADDR super;
  CORE_ADDR sel;
  CORE_ADDR res;

  super = gdbarch_fetch_pointer_argument (gdbarch, frame, 0, ptr_type);
  sel = gdbarch_fetch_pointer_argument (gdbarch, frame, 1, ptr_type);

  read_objc_super (super, &sstr);
  if (sstr.class == 0)
    return 0;
  
  res = find_implementation_from_class (sstr.class, sel);
  if (new_pc != 0)
    *new_pc = res;
  if (res == 0)
    return 1;
  return 0;
}

static int
resolve_msgsend_super_stret (CORE_ADDR pc, CORE_ADDR *new_pc)
{
  struct frame_info *frame = get_current_frame ();
  struct gdbarch *gdbarch = get_frame_arch (frame);
  struct type *ptr_type = builtin_type (gdbarch)->builtin_func_ptr;

  struct objc_super sstr;

  CORE_ADDR super;
  CORE_ADDR sel;
  CORE_ADDR res;

  super = gdbarch_fetch_pointer_argument (gdbarch, frame, 1, ptr_type);
  sel = gdbarch_fetch_pointer_argument (gdbarch, frame, 2, ptr_type);

  read_objc_super (super, &sstr);
  if (sstr.class == 0)
    return 0;
  
  res = find_implementation_from_class (sstr.class, sel);
  if (new_pc != 0)
    *new_pc = res;
  if (res == 0)
    return 1;
  return 0;
}

static char *  
lookup_classname_in_cache (CORE_ADDR class)
{
  struct rb_tree_node *found;

  found = rb_tree_find_node_all_keys (classname_tree, class, -1, -1);
  if (found == NULL)
    return NULL;
  else
    return (char *) found->data;

}

static void 
add_classname_to_cache (CORE_ADDR class, char *classname)
{
  struct rb_tree_node *new_node = (struct rb_tree_node *) xmalloc (sizeof (struct rb_tree_node));

  new_node->key = class;
  new_node->secondary_key = -1;
  new_node->third_key = -1;
  new_node->data = xstrdup (classname);
  new_node->left = NULL;
  new_node->right = NULL;
  new_node->parent = NULL;
  new_node->color = UNINIT;

  rb_tree_insert (&classname_tree, classname_tree, new_node);
}

/* If the value VAL points to an objc object, look up its
   isa pointer, and see if you can find the type for its
   dynamic class type.  Will resolve typedefs etc...  */

/* APPLE LOCAL: This is from objc-class.h:  */
#define CLS_META 0x2L

/* APPLE LOCAL: Extract the code that finds the target type given the address OBJECT_ADDR of
   an objc object.  If BLOCK is provided, the symbol will be looked up in the context of
   that block.  ADDRSIZE is the size of an address on this architecture.  
   If CLASS_NAME is not NULL, then a copy of the class name will be returned in CLASS_NAME.  */

struct type *
objc_target_type_from_object (CORE_ADDR object_addr, 
                              struct block *block, 
                              int addrsize, 
                              char **class_name_ptr)
{
  struct symbol *class_symbol;
  struct type *dynamic_type = NULL;
  char class_name[256];
  char *name_ptr;
  CORE_ADDR name_addr;
  CORE_ADDR isa_addr;
  long info_field;
  int retval = 1;

  isa_addr = read_memory_unsigned_integer (object_addr, addrsize);

  /* isa_addr now points to a struct objc_class in the inferior.  */
  
  name_ptr = lookup_classname_in_cache (isa_addr);
  if (name_ptr == NULL)
    {
      name_ptr = class_name;
      // if (new_objc_runtime_internals ())
      //  retval = new_objc_runtime_get_classname (isa_addr, class_name, 256);
      //else
        {
          /* APPLE LOCAL: Don't look up the dynamic type if the isa is the
             MetaClass class, since then we are looking at the Class object
             which doesn't have the fields of an object of the class.  */
          info_field = read_memory_unsigned_integer (isa_addr + addrsize * 4, addrsize);
          if (info_field & CLS_META)
            return NULL;
          
          name_addr =  read_memory_unsigned_integer (isa_addr + addrsize * 2, addrsize);
          read_memory_string (name_addr, class_name, 255);
        }
      if (retval == 0)
        return NULL;
      add_classname_to_cache (isa_addr, class_name);
    }

  if (class_name_ptr != NULL)
    *class_name_ptr = xstrdup (name_ptr);

  class_symbol = lookup_symbol (name_ptr, block, STRUCT_DOMAIN, 0);
  if (! class_symbol)
    {
      warning ("can't find class named `%s' given by ObjC class object", class_name);
      return NULL;
    }
  
  /* Make sure the type symbol is sane.  (An earlier version of this
     code would find constructor functions, who have the same name as
     the class.)  */
  if (SYMBOL_CLASS (class_symbol) != LOC_TYPEDEF || TYPE_CODE (SYMBOL_TYPE (class_symbol)) != TYPE_CODE_CLASS)
    {
      warning ("The \"isa\" pointer gives a class name of `%s', but that isn't a type name", name_ptr);
      return NULL;
    }
  
  /* This is the object's run-time type!  */
  dynamic_type = SYMBOL_TYPE (class_symbol);

  return dynamic_type;

}

/* Given a value VAL, look up the dynamic type for the object 
   pointed to by VAL in BLOCK, and return it.  If we can't find
   the full type info for the dynamic type of VAL, but we can find
   the class name, then we will return a malloc'ed copy of the name
   in DYNAMIC_TYPE_HANDLE (if it is not NULL).  Note, if we can find
   the full type info, we will set DYNAMIC_TYPE_HANDLE to NULL.  That
   way, if the return value is non-null, you won't have to free the 
   name string.  */

struct type *
value_objc_target_type (struct value *val, struct block *block,
                        char ** dynamic_type_handle)
{
  struct type *base_type, *dynamic_type = NULL;
  int addrsize = get_addrsize();
  if (dynamic_type_handle != NULL)
    *dynamic_type_handle = NULL;

  base_type = check_typedef (value_type (val));

  for (;;)
    {
      CHECK_TYPEDEF (base_type);
      if (TYPE_CODE (base_type) != TYPE_CODE_PTR
          && TYPE_CODE (base_type) != TYPE_CODE_REF)
        break;
      base_type = TYPE_TARGET_TYPE (base_type);
    }

  /* Don't try to get the dynamic type of an objc_class object.  This is the
     class object, not an instance object, so it won't have the fields the
     instance object has.  Also be careful to check for NULL, since val may be
     a typedef or pointer to an incomplete type.  */

  if ((base_type == NULL) || (TYPE_TAG_NAME (base_type) != NULL 
      && (strcmp (TYPE_TAG_NAME (base_type), "objc_class") == 0)))
    return NULL;

  if (TYPE_CODE (base_type) == TYPE_CODE_CLASS)
    {
      char *t_field_name;
      short nfields;
      
      t_field_name = NULL;
      nfields = TYPE_NFIELDS (base_type); 
      
      /* The situation is a little complicated here.
         1) With stabs, the ObjC type hierarchy is not represented in the
         debug info, so the first field is the "isa" field.
         2) With the early DWARF implementations the base class was
         the first field of the child class, but it had a NULL name.
         2) This was corrected so we actually had an inheritance tag,
         and so TYPE_N_BASECLASSES is now correct.
         In all cases, we need to check here that there is an "isa"
         field (so we only try to look up the dynamic type in that
         case.
         isa points to the dynamic type class object.  The "name" field of
         that object gives us the dynamic class name.  

         Finally, again we might get an incomplete type that we have
         baseclass info for, so make sure we aren't indexing past the
         end of the fields array.  */

      while (base_type && nfields != 0)
        {
          int n_base_class;

          n_base_class = TYPE_N_BASECLASSES (base_type);
          if (n_base_class == 1)
            {
              /* If we actually have inheritance, we come in here. */
              base_type = TYPE_FIELD_TYPE (base_type, 0);
              if (base_type)
                nfields = TYPE_NFIELDS (base_type);
              else
                nfields = 0;
            }
          else
            {         
              t_field_name = TYPE_FIELD_NAME (base_type, n_base_class);
              
              if (t_field_name && t_field_name[0] == '\0')
                {
                  /* If we have the weird DWARF first field with no 
                     name we come in here.  */
                  base_type = TYPE_FIELD_TYPE (base_type, n_base_class);
                  if (base_type)
                    nfields = TYPE_NFIELDS (base_type); 
                  else
                    nfields = 0;
                }
              else
                /* We get here for stabs, or if we've reached the end
                   of the inheritance hierarchy.  */
                break;
            }
       }

      if (t_field_name && (strcmp_iw (t_field_name, "isa") == 0))
        {
          /* APPLE LOCAL: Extract the code that finds this into a separate
             routine so I can reuse it.  */
          dynamic_type = objc_target_type_from_object (value_as_address (val), 
                                                       block, addrsize, 
                                                       dynamic_type_handle);
          /* Only pass out dynamic name if the dynamic type is NULL.  */
          if (dynamic_type != NULL && dynamic_type_handle != NULL)
            {
              xfree (*dynamic_type_handle);
              *dynamic_type_handle = NULL;
            }
        }
    }
  return dynamic_type;
}


/* APPLE LOCAL begin red-black trees, part 2.  */

/* The following data structures and function declarations are for the
   implementaion of red-black tree data structures.  Red-black trees are
   an efficient form of pseudo-balanced binary trees.  For more information
   see the comments labelled "red-black trees, part 2.  */

/* The following data structure is used by the Dwarf types repository
   code.  When the types repository uses a red-black tree, the important
   types repository data is stored in this structure, which the red-black
   tree  nodes point to via their "void * data" field.  */

struct rb_repository_data {
  struct type *type_data;
  struct die_info *die_data;
};

/* The definitions for rb_tree_colors and rb_tree_struct can be found in
   inlining.h.  */

/* Function declarations for red-black tree manipulation functions.  See
   function definitions for more informatino about the functions.  */

static struct rb_tree_node *rb_tree_find_and_remove_node (struct rb_tree_node **, struct rb_tree_node *, long long, int);
static struct rb_tree_node *rb_tree_remove_node (struct rb_tree_node **, struct rb_tree_node *);
static struct rb_tree_node *rb_tree_minimum (struct rb_tree_node *);
static struct rb_tree_node *rb_tree_successor (struct rb_tree_node *);
static void left_rotate (struct rb_tree_node **, struct rb_tree_node *);
static void right_rotate (struct rb_tree_node **, struct rb_tree_node *);
static void rb_delete_fixup (struct rb_tree_node **, struct rb_tree_node *);
static void plain_tree_insert (struct rb_tree_node **, struct rb_tree_node *);
static int verify_rb_tree (struct rb_tree_node *);
static int verify_tree_heights (struct rb_tree_node *);
static int verify_tree_colors (struct rb_tree_node *);
static int tree_height (struct rb_tree_node *);
static int num_nodes_in_tree (struct rb_tree_node *);
static void rb_print_tree (struct rb_tree_node *, int);

/* Begin repository sub-section 1: Red-black trees.  This section  implements
   the red-black tree algorithms from "Introduction to Algorithms" by Cormen,
   Leiserson, and Rivest.  A red-black tree is a 'semi-balanced' binary tree,
   where by semi-balanced it means that for any node in the tree, the height of
   one sub-tree is guaranteed to never be greater than twice the height of the
   other sub-tree.  Each node is colored either red or black, and a parent must
   never be the same color as its children.

   The following types, used by the functions in this section, are defined
   near the beginning of this file  (look for the label "red-black trees, 
   part 1"):

        enum rb_tree_colors;  (type)
        struct rb_tree_node;  (type)

   This section defines the following functions:

        rb_tree_find_node (function)
        rb_tree_find_node_all_keys (function)
        rb_tree_find_and_remove_node (function)
        left_rotate       (function)
        right_rotate      (function)
        plain_tree_insert (function)
        rb_tree_insert    (function)
        rb_tree_remove_node (function)
        rb_tree_minimun (function)
        rb_tree_successor (function)
        rb_delete_fixup (function)
        rb_tree_remove_node (function)
*/

/* This function searches the tree ROOT recursively until it
   finds a node with the key KEY, which it returns.  If there
   is no such node in the tree it returns NULL.  */

struct rb_tree_node *
rb_tree_find_node (struct rb_tree_node *root, long long key, int secondary_key)
{
  if (!root)
    return NULL;

  if (key == root->key)
    {
      if (secondary_key < 0)
        return root;
      else if (secondary_key < root->secondary_key)
        return rb_tree_find_node (root->left, key, secondary_key);
      else
        return rb_tree_find_node (root->right, key, secondary_key);
    }
  else if (key < root->key)
    return rb_tree_find_node (root->left, key, secondary_key);
  else
    return rb_tree_find_node (root->right, key, secondary_key);
}


/* This function searches the tree ROOT recursively until it
   finds a node with the key KEY, secondary key SECONDARY_KEY and third key
   THIRD_KEY, which it returns.  If there is no such node in the tree it 
   returns NULL.  */

struct rb_tree_node *
rb_tree_find_node_all_keys (struct rb_tree_node *root, long long key, 
                            int secondary_key, long long third_key)
{
  if (!root)
    return NULL;

  if (key == root->key)
    {
      if (secondary_key < root->secondary_key)
        return rb_tree_find_node_all_keys (root->left, key, secondary_key,
                                           third_key);
      else if (secondary_key > root->secondary_key)
        return rb_tree_find_node_all_keys (root->right, key, secondary_key, 
                                           third_key);
      else /* (secondary_key == root->secondary_key)  */
        {
          if (third_key == root->third_key)
            return root;
          else if (third_key < root->third_key)
            return rb_tree_find_node_all_keys (root->left, key, secondary_key,
                                               third_key);
          else
            return rb_tree_find_node_all_keys (root->right, key, secondary_key,
                                               third_key);
        }
    }
  else if (key < root->key)
    return rb_tree_find_node_all_keys (root->left, key, secondary_key, 
                                       third_key);
  else
    return rb_tree_find_node_all_keys (root->right, key, secondary_key,
                                       third_key);
}


/* This function, given a red-black tree (ROOT), a current position in the
   tree (CUR_NODE), a primary key (KEY), and a SECONDARY_KEY,  searches for
   a node in the tree that matches the keys given, removes the node from
   the tree, and returns a copy of the node.  */

static struct rb_tree_node *
rb_tree_find_and_remove_node (struct rb_tree_node **root, 
                              struct rb_tree_node *cur_node, long long key, 
                              int secondary_key)
{
  struct rb_tree_node *result;

  if (!cur_node)
    return NULL;

  if (key == cur_node->key)
    {
      if (cur_node->left
          && cur_node->left->key == key)
        return rb_tree_find_and_remove_node (root, cur_node->left, key, 
                                             secondary_key);
     
      result = rb_tree_remove_node (root, cur_node);
      return result;
    }
  else if (key < cur_node->key)
    return rb_tree_find_and_remove_node (root, cur_node->left, key, 
                                         secondary_key);
  else
    return rb_tree_find_and_remove_node (root, cur_node->right, key, 
                                         secondary_key);
}

/* Given a red-black tree NODE, return the node in the tree that has the
   smallest "value".  */

static struct rb_tree_node *
rb_tree_minimum (struct rb_tree_node *node)
{
  while (node->left)
    node = node->left;
  return  node;
}

/* Given a NODE in a red-black tree, this function returns the
   descendant of that node in the tree that has the smallest "value"
   that is greater than the "value" of NODE.  */

static struct rb_tree_node *
rb_tree_successor (struct rb_tree_node *node)
{
  struct rb_tree_node *y;
  if (node->right)
    return rb_tree_minimum (node->right);
  else
    {
      y = node->parent;
      while (y && node == y->right)
        {
          node = y;
          y = node->parent;
        }
    }
  return y;
}

/* This function takes a red-black tree (ROOT) that has had a node
   removed at X, and restores the red-black properties to the tree. 
   It uses the algorithm from pate 274 of the Corman et. al. textbook.  */

static void
rb_delete_fixup (struct rb_tree_node **root, struct rb_tree_node *x)
{
  struct rb_tree_node *w;

  /* On entering this function, the tree is not correct.  'x' is carrying
     the "blackness" of the node that was deleted as well as its own color.
     If x is red we can just color it black and be done.  But if 'x' is black
     we need to do some re-coloring and rotating to push the extra blackness
     up the tree (once it reaches the root of the tree everything is properly
     balanced again).

     'w' is the sibling in the tree of 'x'.  'w' must be non-NULL, otherwise
     the tree was messed up to begin with. 

     For details about the particular cases mentioned below, see the
     algorithm explanation in the book.  */

  while (x != *root
         && x->color == BLACK)
    {
      if (x == x->parent->left)  /* x LEFT child of its parent.  */
        {
          w = x->parent->right;

          /* Case 1:  w is RED.  Color it black and do a rotation,
             converting this to case 2, 3 or 4.  */

          if (w->color == RED)   /* Case 1 */
            {
              w->color = BLACK;
              x->parent->color = RED;
              left_rotate (root, x->parent);
              w = x->parent->right;
            }

          /* Case 2: Both of w's children are BLACK (where NULL counts
             as BLACK).  In this case, color w red, and push the blackness
             up the tree one node, making what used to be x's parent be 
             the new x (and return to top of loop).  */

          if ((!w->left || w->left->color == BLACK)   /* Case 2  */
              && (!w->right || w->right->color == BLACK))
            {
              w->color = RED;
              x = x->parent;
            }
          else  /* Cases 3 & 4 (w is black, one of its children is red)  */
            {

              /* Case 3: w's right child is black.  */

              if (!w->right || w->right->color == BLACK)  /* Case 3  */
                {
                  if (w->left)
                    w->left->color = BLACK;
                  w->color = RED;
                  right_rotate (root, w);
                  w = x->parent->right;
                }

              /* Case 4  */
              
              w->color = x->parent->color;
              x->parent->color = BLACK;
              if (w->right)
                w->right->color = BLACK;
              left_rotate (root, x->parent);
              x = *root;
            }
        }
      else  /* x is the RIGHT child of its parent.  */
        {
          w = x->parent->left;

          /* Case 1:  w is RED.  Color it black and do a rotation,
             converting this to case 2, 3 or 4.  */

          if (w->color == RED)
            {
              w->color = BLACK;
              x->parent->color = RED;
              right_rotate (root, x->parent);
              w = x->parent->left;
            }

          /* Case 2: Both of w's children are BLACK (where NULL counts
             as BLACK).  In this case, color w red, and push the blackness
             up the tree one node, making what used to be x's parent be 
             the new x (and return to top of loop).  */

          if ((!w->right || w->right->color == BLACK)
              && (!w->left || w->left->color == BLACK))
            {
              w->color = RED;
              x = x->parent;
            }
          else /* Cases 3 & 4 (w is black, one of its children is red)  */
            {

              /* Case 3: w's left  child is black.  */

              if (!w->left || w->left->color == BLACK)
                {
                  if (w->right)
                    w->right->color = BLACK;
                  w->color = RED;
                  left_rotate (root, w);
                  w = x->parent->left;
                }

              /* Case 4  */

              w->color = x->parent->color;
              x->parent->color = BLACK;
              if (w->left)
                w->left->color = BLACK;
              right_rotate (root, x->parent);
              x = *root;
            }
        }
    }
  x->color = BLACK;
}

/* Red-Black tree delete node:  Given a tree (ROOT) and a node in the tree
   (NODE), remove the NODE from the TREE, keeping the tree properly balanced
   and colored, and return a copy of the removed node.  This function uses
   the algorithm on page 273 of the Corman, Leiserson and Rivest textbook
   mentioned previously. 

   First we make a copy of the node to be deleted, so we can return the
   data from that node.  We need to make a copy rather than returning the
   node because of the way some tree deletions are handled (see the next
   paragraph).

   The basic idea is: If NODE has no children, just remove it.  If
   NODE has one child, splice out NODE (make its parent point to its
   child).  The tricky part is when NODE has two children.  In that
   case we find the successor to NODE in NODE's right subtree (the
   "smallest" node whose "value" is larger than the "value" of node,
   where "smallest" and "value" are determined by the nodes' keys).
   The successor is guaranteed to have ony one child.  Therefore we
   first splice out the successor (make its parent point to its
   child).  Next we *overwrite the keys and data* of NODE with the
   keys and data of its successor node.  The net effect of this is
   that NODE has been replaced by its successor, and NODE is no longer
   in the tree.

   Finally, we may need to re-color or re-balance a portion of the tree.
 */


static struct rb_tree_node *
rb_tree_remove_node (struct rb_tree_node **root, struct rb_tree_node *node)
{
  struct rb_tree_node *deleted_node;
  struct rb_tree_node *z = node;
  struct rb_tree_node *x;
  struct rb_tree_node *y;
  struct rb_tree_node *y_parent;
  int x_child_pos;  /* 0 == left child; 1 == right child  */

  /* Make a copy of the node to be "deleted" from the tree.  The copy is what
     will be returned by this function.  */

  deleted_node = (struct rb_tree_node *) xmalloc (sizeof (struct rb_tree_node));
  deleted_node->key = node->key;
  deleted_node->secondary_key = node->secondary_key;
  deleted_node->third_key = node->third_key;
  deleted_node->data = node->data;
  deleted_node->color = node->color;
  deleted_node->left = NULL;
  deleted_node->right = NULL;
  deleted_node->parent = NULL;

  /* Now proceed to 'delete' the node ("z") from the tree.  */
  

  /* Removing a node with one child from a red-black tree is not too
     difficult, but removing a node with two children IS difficult.
     Therefore if the node to be removed has at most one child, it
     will be removed directly.

     If "z" has TWO children, we will not actually remove node "z"
     from the tree; instead we will find z's successor in the tree
     (which is guaranteed to have at most one child), remove THAT node
     from the tree, and overwrite the keys and data value in z with
     the keys and data value in z's successor.  */

  /* 'y' will point to the node that actually gets removed from the
     tree.  If 'z' has at most one child, 'y' will point to the same
     node as 'z'.  If 'z' has two children, 'y' will point to 'z's
     successor in the tree.  */
  
  if (!z->left || !z->right)
    y = z;
  else
    y = rb_tree_successor (z);

  /* 'y' is now guaranteed to have at most one child.  Make 'x' point
     to that child.  If y has no children, x will be NULL.  */

  if (y->left)
    x = y->left;
  else
    x = y->right;

  /* Make y's parent be x's parent (it used to be x's grandparent).  */

  if (x)
    x->parent = y->parent;

  y_parent = y->parent;

  /* Make 'x' be the child of y's parent that y used to be.  */

  if (!y->parent)
    *root = x;
  else if (y == y->parent->left)
    {
      y->parent->left = x;
      x_child_pos = 0;
    }
  else
    {
      y->parent->right = x;
      x_child_pos = 1;
    }

  /* If y is not the same as 'node', then y is the successor to
     'node'; since node has two children and cannot actually be
     removed from the tree, and since y has now been spliced out of
     the tree, overwrite node's keys and data with y's keys and data.
     (This is why we made a copy of node above, to be the return
     value.)  */

  if (y != node)
    {
      node->key = y->key;
      node->secondary_key = y->secondary_key;
      node->third_key = y->third_key;
      node->data = y->data;
    }

  /* If the color of 'y' was RED, then the properties of the red-black
     tree have not been violated by removing it so nothing else needs
     to be done.  But if the color of y was BLACK, then we need to fix
     up the tree, starting at 'x' (which now occupies the position
     where y was removed).  */

  if (y->color == BLACK && x == NULL && y_parent != NULL)
    {
      struct rb_tree_node *w;

      /* Since x is NULL, we can't call rb_delete_fixup directly (it
         assumes a non-NULL x.  Therefore we do the first iteration of
         the while loop from that function here.  At the end of this
         first iteration, x is no longer NULL, so we can call the
         function on the new non-NULL x.  */

      if (x_child_pos == 0)
        w = y_parent->right;
      else
        w = y_parent->left;

      if (!w)
        x = *root;
      else
        {
          if (w->color == RED)
            {
              w->color = BLACK;
              y_parent->color = RED;
              if (x_child_pos == 0)
                {
                  left_rotate (root, y_parent);
                  w = y_parent->right;
                }
              else
                {
                  right_rotate (root, y_parent);
                  w = y_parent->left;
                }
            }
          
          if ((!w->left || w->left->color == BLACK)
              && (!w->right || w->right->color == BLACK))
            {
              w->color = RED;
              x = y_parent;
            }
          else if (x_child_pos == 0)
            {
              if (!w->right || w->right->color == BLACK)
                {
                  if (w->left)
                    w->left->color = BLACK;
                  w->color = RED;
                  right_rotate (root, w);
                  w = y_parent->right;
                }
              
              w->color = y_parent->color;
              y_parent->color = BLACK;
              if (w->right)
                w->right->color = BLACK;
              left_rotate (root, y_parent);
              x = *root;
            }
          else
            {
              if (!w->left || w->left->color == BLACK)
                {
                  if (w->right)
                    w->right->color = BLACK;
                  w->color = RED;
                  left_rotate (root, w);
                  w = y_parent->left;
                }
              
              w->color = y_parent->color;
              y_parent->color = BLACK;
              if (w->left)
                w->left->color = BLACK;
              right_rotate (root, y_parent);
              x = *root;
            }
        }
    }

  if (y->color == BLACK && x)
    rb_delete_fixup (root, x);

  return deleted_node;
}

/* Given a (red-black) tree structure like the one on the left, 
   perform a "left-rotation" so that the result is like the one
   on the right (parent, x, and y are individual tree nodes; a, b,
   and c represent sub-trees, possibly null):

     parent                            parent
        |                                |
        x                                y
       / \               ==>>           / \
     a    y                            x   c
         / \                          / \
        b   c                        a   b

*/

static void
left_rotate (struct rb_tree_node **root, struct rb_tree_node *x)
{
  struct rb_tree_node *y;
  
  if (!x->right)
    return;

  y = x->right;

  x->right = y->left;
  if (y->left != NULL)
    y->left->parent = x;

  y->parent = x->parent;

  if (x->parent == NULL)
    *root = y;
  else if (x == x->parent->left)
    x->parent->left = y;
  else
    x->parent->right = y;

  y->left = x;
  x->parent = y;
}

/* Given a (red-black) tree structure like the one on the left, 
   perform a "right-rotation" so that the result is like the one
   on the right (parent, x, and y are individual tree nodes; a, b,
   and c represent sub-trees, possibly null):

     parent                            parent
        |                                |
        x                                y
       / \               ==>>           / \
     y    c                            a   x
    / \                                   / \
   a   b                                 b   c

*/

static void
right_rotate (struct rb_tree_node **root, struct rb_tree_node *x)
{
  struct rb_tree_node *y;

  if (!x->left)
    return;

  y = x->left;

  x->left = y->right;
  if (y->right != NULL)
    y->right->parent = x;

  y->parent = x->parent;
  
  if (x->parent == NULL)
    *root = y;
  else if (x == x->parent->left)
    x->parent->left = y;
  else 
    x->parent->right = y;

  y->right = x;
  x->parent = y;
}

/* Basic binary tree insertion, with parent node, and assuming we know the
   NEW_NODE is not already in the tree.  */

static void
plain_tree_insert (struct rb_tree_node **root, struct rb_tree_node *new_node)
{
  struct rb_tree_node *tree = *root;

  if (tree == NULL)
    *root = new_node;
  else if (new_node->key < tree->key)
    {
      if (tree->left)
        plain_tree_insert (&tree->left, new_node);
      else
        {
          tree->left = new_node;
          new_node->parent = tree;
        }
    }
  else if (new_node->key > tree->key)
    {
      if (tree->right)
        plain_tree_insert (&tree->right, new_node);
      else
        {
          tree->right = new_node;
          new_node->parent = tree;
        }
    }
  else if (new_node->key == tree->key)
    {
      if (new_node->secondary_key < tree->secondary_key)
        {
          if (tree->left)
            plain_tree_insert (&tree->left, new_node);
          else
            {
              tree->left = new_node;
              new_node->parent = tree;
            }
        }
      else if (new_node->secondary_key > tree->secondary_key)
        {
          if (tree->right)
            plain_tree_insert (&tree->right, new_node);
          else
            {
              tree->right = new_node;
              new_node->parent = tree;
            }
        }
      else if (new_node->secondary_key == tree->secondary_key)
        {
          if (new_node->third_key < tree->third_key)
            {
              if (tree->left)
                plain_tree_insert (&tree->left, new_node);
              else
                {
                  tree->left = new_node;
                  new_node->parent = tree;
                }
            }
          else /* if (new_node->third_key > tree->third_key) */
            {
              if (tree->right)
                plain_tree_insert (&tree->right, new_node);
              else
                {
                  tree->right = new_node;
                  new_node->parent = tree;
                }
            }
        }
    }
}

/* Red-Black tree node insert.  Based on algorithm in "Introduction to
   Algorithms", by Corman, Leiserson, and Rivest, Chapter 14.  The
   resulting binary tree is "roughly balanced", i.e. for any node, the height
   of one subtree will never be more than twice the height of the other.
   Every node has a color, either red or black.  The root is always black;
   the color of a node's children are supposed to be different from the
   color of the node.
*/

void
rb_tree_insert (struct rb_tree_node **root, struct rb_tree_node *tree,
                struct rb_tree_node *new_node)
{
  struct rb_tree_node *y;

  plain_tree_insert (root, new_node);
  new_node->color = RED;
  while (new_node != *root
         && new_node->parent->color == RED)
    {
      if (new_node->parent == new_node->parent->parent->left)
        {
          y = new_node->parent->parent->right;
          if (y && y->color == RED)
            {
              new_node->parent->color = BLACK;
              y->color = BLACK;
              new_node->parent->parent->color = RED;
              new_node = new_node->parent->parent;
            }
          else if (new_node == new_node->parent->right)
            {
              new_node = new_node->parent;
              left_rotate (root, new_node);
            }
          else
            {
              new_node->parent->color = BLACK;
              new_node->parent->parent->color = RED;
              right_rotate (root, new_node->parent->parent);
            }
        }
      else
        {
          y = new_node->parent->parent->left;
          if (y && y->color == RED)
            {
              new_node->parent->color = BLACK;
              y->color = BLACK;
              new_node->parent->parent->color = RED;
              new_node = new_node->parent->parent;
            }
          else if (new_node == new_node->parent->left)
            {
              new_node = new_node->parent;
              right_rotate (root, new_node);
            }
          else
            {
              new_node->parent->color = BLACK;
              new_node->parent->parent->color = RED;
              left_rotate (root, new_node->parent->parent);
            }
        }
    }
  (*root)->color = BLACK;
}

/* End repository sub-section 1:  Red-black trees.  */
/* APPLE LOCAL end red-black trees, part 2.  */
