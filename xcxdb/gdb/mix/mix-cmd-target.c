/* Original in GNU gdb 6.8.50.20080428-cvs,
   modified on 2008-05-05 for xcxdb by Dr. Rolf Jansen. */

/* MI Command Set - target commands.
   Copyright (C) 2007, 2008 Free Software Foundation, Inc.

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
#include "mix-getopt.h"
#include "remote.h"

/* Get a file from the target.  */

enum mix_cmd_result
mix_cmd_target_file_get (char *command, char **argv, int argc)
{
  int optind = 0;
  char *optarg;
  const char *remote_file, *local_file;
  static struct mix_opt opts[] =
  {
    { 0, 0, 0 }
  };
  static const char *prefix = "mix_cmd_target_file_get";

  if (mix_getopt (prefix, argc, argv, opts, &optind, &optarg) != -1
      || optind != argc - 2)
    error (_("mix_cmd_target_file_get: Usage: REMOTE_FILE LOCAL_FILE"));

  remote_file = argv[optind];
  local_file = argv[optind + 1];

  remote_file_get (remote_file, local_file, 0);

  return MIX_CMD_DONE;
}

/* Send a file to the target.  */

enum mix_cmd_result
mix_cmd_target_file_put (char *command, char **argv, int argc)
{
  int optind = 0;
  char *optarg;
  const char *remote_file, *local_file;
  static struct mix_opt opts[] =
  {
    { 0, 0, 0 }
  };
  static const char *prefix = "mix_cmd_target_file_put";

  if (mix_getopt (prefix, argc, argv, opts, &optind, &optarg) != -1
      || optind != argc - 2)
    error (_("mix_cmd_target_file_put: Usage: LOCAL_FILE REMOTE_FILE"));

  local_file = argv[optind];
  remote_file = argv[optind + 1];

  remote_file_put (local_file, remote_file, 0);

  return MIX_CMD_DONE;
}

/* Delete a file on the target.  */

enum mix_cmd_result
mix_cmd_target_file_delete (char *command, char **argv, int argc)
{
  int optind = 0;
  char *optarg;
  const char *remote_file, *local_file;
  static struct mix_opt opts[] =
  {
    { 0, 0, 0 }
  };
  static const char *prefix = "mix_cmd_target_file_delete";

  if (mix_getopt (prefix, argc, argv, opts, &optind, &optarg) != -1
      || optind != argc - 1)
    error (_("mix_cmd_target_file_delete: Usage: REMOTE_FILE"));

  remote_file = argv[optind];

  remote_file_delete (remote_file, 0);

  return MIX_CMD_DONE;
}

