/* Original in GNU gdb 6.8.50.20080428-cvs,
   modified on 2008-04-28 for xcxdb by Dr. Rolf Jansen. */

/* MI Command Set - breakpoint and watchpoint commands.
   Copyright (C) 2000, 2001, 2002, 2007, 2008 Free Software Foundation, Inc.
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
#include "mix-cmds.h"
#include "ui-out.h"
#include "mix-out.h"
#include "breakpoint.h"
#include "gdb_string.h"
#include "mix-getopt.h"
#include "gdb.h"
#include "exceptions.h"
#include "observer.h"
#include "filenames.h"

enum
  {
    FROM_TTY = 0
  };

/* True if MI breakpoint observers have been registered.  */

static int mi_breakpoint_observers_installed;

/* Control whether breakpoint_notify may act.  */

static int mi_can_breakpoint_notify;

/* Output a single breakpoint, when allowed. */

static void
breakpoint_notify (int b)
{
  if (mi_can_breakpoint_notify)
  gdb_breakpoint_query (uiout, b, NULL);
}

enum bp_type
  {
    REG_BP,
    HW_BP,
    REGEXP_BP
  };

/* Insert a breakpoint. The type of breakpoint is specified by the
   first argument: 
   -break-insert <location> --> insert a regular breakpoint.  
   -break-insert -t <location> --> insert a temporary breakpoint.  
   -break-insert -h <location> --> insert an hardware breakpoint.  
   -break-insert -t -h <location> --> insert a temporary hw bp.
   -break-insert -f <location> --> insert a future breakpoint.  
   -break-insert -r <regexp> --> insert a bp at functions matching
   <regexp> 

   You can also specify the shared-library in which to set the breakpoint
   by passing the -s argument, as:

   -break-insert -s <shlib>

   If this is a path, the full path must match, otherwise the base
   filename must match.  This can be given in combination with
   the other options above.

   You can also specify a list of indices which will be applied to the
   list of matches that gdb builds up for the breakpoint if there are
   multiple matches.  If the first element of the list is "-1" ALL
   matches will be accepted.

   -break-insert -l "<NUM> <NUM> ... " <expression>
*/

enum mix_cmd_result
mix_cmd_break_insert (char *command, char **argv, int argc)
{
  char *address = NULL;
  enum bp_type type = REG_BP;
  int temp_p = 0;
  int thread = -1;
  int ignore_count = 0;
  char *condition = NULL;
  char *requested_shlib = NULL;
  char realpath_buf[PATH_MAX];
  enum gdb_rc rc;
  int *indices = NULL;
  int pending = 0;
  struct gdb_exception e;
  struct gdb_events *old_hooks;
  enum opt
    {
      HARDWARE_OPT, TEMP_OPT /*, REGEXP_OPT */ , CONDITION_OPT,
      IGNORE_COUNT_OPT, THREAD_OPT, PENDING_OPT, SHLIB_OPT, LIST_OPT
    };
  static struct mix_opt opts[] =
  {
    {"h", HARDWARE_OPT, 0},
    {"t", TEMP_OPT, 0},
    {"c", CONDITION_OPT, 1},
    {"i", IGNORE_COUNT_OPT, 1},
    {"p", THREAD_OPT, 1},
    {"f", PENDING_OPT, 0},
    {"s", SHLIB_OPT, 1},
    {"l", LIST_OPT, 1},
    { 0, 0, 0 }
  };

  /* Parse arguments. It could be -r or -h or -t, <location> or ``--''
     to denote the end of the option list. */
  int optind = 0;
  char *optarg;
  struct cleanup *indices_cleanup = NULL;
  while (1)
    {
      int opt = mix_getopt ("mix_cmd_break_insert", argc, argv, opts, &optind, &optarg);
      if (opt < 0)
        break;
      switch ((enum opt) opt)
        {
        case TEMP_OPT:
          temp_p = 1;
          break;
        case HARDWARE_OPT:
          type = HW_BP;
          break;
#if 0
        case REGEXP_OPT:
          type = REGEXP_BP;
          break;
#endif
        case CONDITION_OPT:
          condition = optarg;
          break;
        case IGNORE_COUNT_OPT:
          ignore_count = atol (optarg);
          /* APPLE LOCAL: Same behavior as set_ignore_count().  */
          if (ignore_count < 0)
            ignore_count = 0;
          break;
        case THREAD_OPT:
          thread = atol (optarg);
          break;
        case PENDING_OPT:
          pending = 1;
          break;
        case SHLIB_OPT:
          requested_shlib = optarg;
          break;
        case LIST_OPT:
          {
            char *numptr;
            int nelem = 0, i;
            /* First count the number of elements, which is the
               number of spaces plus one.  */
            numptr = optarg;
            while (*numptr)
              {
                if (*numptr != ' ')
                  {
                    nelem++;
                    while (*numptr != ' ' && *numptr != '\0')
                      numptr++;
                  }
                else
                  numptr++;
              }

            if (nelem == 0)
              error ("mix_cmd_break_insert: Got index with no elements");

            indices = (int *) xmalloc ((nelem + 1) * sizeof (int *));
            indices_cleanup = make_cleanup (xfree, indices);

            /* Now extract the elements.  */

            numptr = optarg;
            i = 0;
            errno = 0;
            while (*numptr != '\0')
              {
                indices[i++] = strtol (numptr, &numptr, 10);
                if (errno == EINVAL)
                    error ("mix_cmd_break_insert: bad index at \"%s\"", numptr);
              }

            /* Since we aren't passing a number of elements, we terminate the
               indices by putting in a -1 element.  */
            
            indices[i] = -1;

            break;
          }
        }
    }

  if (optind >= argc)
    error (_("mix_cmd_break_insert: Missing <location>"));
  if (optind < argc - 1)
    error (_("mix_cmd_break_insert: Garbage following <location>"));
  address = argv[optind];

  /* APPLE LOCAL: realpath() the incoming shlib name, as we do with all
     objfile/dylib/executable names.  NB this condition is incorrect if
     we're passed something like "./foo.dylib", "../foo.dylib", or
     "~/bin/foo.dylib", but that shouldn't happen....  */
  if (requested_shlib && IS_ABSOLUTE_PATH (requested_shlib))
    {
      realpath (requested_shlib, realpath_buf);
      /* It'll be xstrdup()'ed down in the breakpoint command, so just point
         to the stack array until then. */
      requested_shlib = realpath_buf; 
    }

  /* Now we have what we need, let's insert the breakpoint! */
  if (! mi_breakpoint_observers_installed)
    {
      observer_attach_breakpoint_created (breakpoint_notify);
      observer_attach_breakpoint_modified (breakpoint_notify);
      observer_attach_breakpoint_deleted (breakpoint_notify);
      mi_breakpoint_observers_installed = 1;
    }

  mi_can_breakpoint_notify = 1;
  /* Make sure we restore hooks even if exception is thrown.  */
  TRY_CATCH (e, RETURN_MASK_ALL)
    {
      switch (type)
        {
        case REG_BP:
          set_breakpoint (address, condition,
                          0 /*hardwareflag */, temp_p,
                          thread, ignore_count,
                          pending);
          break;
        case HW_BP:
          set_breakpoint (address, condition,
                          1 /*hardwareflag */, temp_p,
                          thread, ignore_count,
                          pending);
          break;
#if 0
        case REGEXP_BP:
          if (temp_p)
            error (_("mix_cmd_break_insert: Unsupported tempoary regexp breakpoint"));
          else
            rbreak_command_wrapper (address, FROM_TTY);
          return MIX_CMD_DONE;
          break;
#endif
        default:
          internal_error (__FILE__, __LINE__,
                          _("mix_cmd_break_insert: Bad switch."));
        }
    }
  mi_can_breakpoint_notify = 0;
  if (e.reason < 0)
    throw_exception (e);

  return MIX_CMD_DONE;
}

enum wp_type
{
  REG_WP,
  READ_WP,
  ACCESS_WP
};

/* Insert a watchpoint. The type of watchpoint is specified by the
   first argument: 
   -break-watch <expr> --> insert a regular wp.  
   -break-watch -r <expr> --> insert a read watchpoint.
   -break-watch -a <expr> --> insert an access wp. */

enum mix_cmd_result
mix_cmd_break_watch (char *command, char **argv, int argc)
{
  char *expr = NULL;
  enum wp_type type = REG_WP;
  enum opt
    {
      READ_OPT, ACCESS_OPT
    };
  static struct mix_opt opts[] =
  {
    {"r", READ_OPT, 0},
    {"a", ACCESS_OPT, 0},
    { 0, 0, 0 }
  };

  /* Parse arguments. */
  int optind = 0;
  char *optarg;
  while (1)
    {
      int opt = mix_getopt ("mix_cmd_break_watch", argc, argv, opts, &optind, &optarg);
      if (opt < 0)
        break;
      switch ((enum opt) opt)
        {
        case READ_OPT:
          type = READ_WP;
          break;
        case ACCESS_OPT:
          type = ACCESS_WP;
          break;
        }
    }
  if (optind >= argc)
    error (_("mix_cmd_break_watch: Missing <expression>"));
  if (optind < argc - 1)
    error (_("mix_cmd_break_watch: Garbage following <expression>"));
  expr = argv[optind];

  /* Now we have what we need, let's insert the watchpoint! */
  switch (type)
    {
    case REG_WP:
      watch_command_wrapper (expr, FROM_TTY);
      break;
    case READ_WP:
      rwatch_command_wrapper (expr, FROM_TTY);
      break;
    case ACCESS_WP:
      awatch_command_wrapper (expr, FROM_TTY);
      break;
    default:
      error (_("mix_cmd_break_watch: Unknown watchpoint type."));
    }
  return MIX_CMD_DONE;
}
