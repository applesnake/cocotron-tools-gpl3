/* Original in GNU gdb 6.8.50.20080428-cvs,
   modified on 2008-05-05 for xcxdb by Dr. Rolf Jansen. */

/* MI Command Set - MI Command Parser.
   Copyright (C) 2000, 2007, 2008 Free Software Foundation, Inc.
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

#ifndef MIX_PARSE_H
#define MIX_PARSE_H

/* MI parser */

/* Timestamps for current command and last asynchronous command.  */
struct mix_timestamp
  {
    struct timeval wallclock;
    struct rusage rusage;
  };

enum mix_command_type
  {
    MI_COMMAND, CLI_COMMAND
  };

struct mix_parse
  {
    enum mix_command_type op;
    char *command;
    char *token;
    const struct mix_cmd *cmd;
    struct mix_timestamp *cmd_start;
    char *args;
    char **argv;
    int argc;
  };

/* Attempts to parse CMD returning a ``struct mix_command''.  If CMD is
   invalid, an error mesage is reported (MI format) and NULL is
   returned. For a CLI_COMMAND, COMMAND, TOKEN and OP are initialized.
   For an MI_COMMAND COMMAND, TOKEN, ARGS and OP are
   initialized. Un-initialized fields are zero. */

extern struct mix_parse *mix_parse (char *cmd);

/* Free a command returned by mix_parse_command. */

extern void mix_parse_free (struct mix_parse *cmd);

#endif
