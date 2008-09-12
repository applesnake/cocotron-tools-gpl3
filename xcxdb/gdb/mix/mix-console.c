/* Original in GNU gdb 6.8.50.20080428-cvs,
   modified on 2008-04-28 for xcxdb by Dr. Rolf Jansen. */

/* MI Console code.
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
#include "mix-console.h"
#include "gdb_string.h"

/* MI-console: send output to std-out but correcty encapsulated */

static ui_file_fputs_ftype mix_console_file_fputs;
static ui_file_flush_ftype mix_console_file_flush;
static ui_file_delete_ftype mix_console_file_delete;

struct mix_console_file
  {
    int *magic;
    struct ui_file *raw;
    struct ui_file *buffer;
    const char *prefix;
    char quote;
  };

int mix_console_file_magic;

struct ui_file *
mix_console_file_new (struct ui_file *raw,
                     const char *prefix, char quote)
{
  struct ui_file *ui_file = ui_file_new ();
  struct mix_console_file *mix_console = XMALLOC (struct mix_console_file);
  mix_console->magic = &mix_console_file_magic;
  mix_console->raw = raw;
  mix_console->buffer = mem_fileopen ();
  mix_console->prefix = prefix;
  mix_console->quote = quote;
  set_ui_file_fputs (ui_file, mix_console_file_fputs);
  set_ui_file_flush (ui_file, mix_console_file_flush);
  set_ui_file_data (ui_file, mix_console, mix_console_file_delete);
  return ui_file;
}

static void
mix_console_file_delete (struct ui_file *file)
{
  struct mix_console_file *mix_console = ui_file_data (file);
  if (mix_console->magic != &mix_console_file_magic)
    internal_error (__FILE__, __LINE__,
                    _("mix_console_file_delete: bad magic number"));
  xfree (mix_console);
}

static void
mix_console_file_fputs (const char *buf,
                       struct ui_file *file)
{
  struct mix_console_file *mix_console = ui_file_data (file);
  if (mix_console->magic != &mix_console_file_magic)
    internal_error (__FILE__, __LINE__,
                    "mix_console_file_fputs: bad magic number");
  /* Append the text to our internal buffer */
  fputs_unfiltered (buf, mix_console->buffer);
  /* Flush when an embedded \n */
  if (strchr (buf, '\n') != NULL)
    gdb_flush (file);
}

/* Transform a byte sequence into a console output packet. */
static void
mix_console_raw_packet (void *data,
                       const char *buf,
                       long length_buf)
{
  struct mix_console_file *mix_console = data;
  if (mix_console->magic != &mix_console_file_magic)
    internal_error (__FILE__, __LINE__,
                    _("mix_console_file_transform: bad magic number"));

  if (length_buf > 0)
    {
      fputs_unfiltered (mix_console->prefix, mix_console->raw);
      if (mix_console->quote)
        {
          fputs_unfiltered ("\"", mix_console->raw);
          fputstrn_unfiltered (buf, length_buf, mix_console->quote, mix_console->raw);
          fputs_unfiltered ("\"\n", mix_console->raw);
        }
      else
        {
          fputstrn_unfiltered (buf, length_buf, 0, mix_console->raw);
          fputs_unfiltered ("\n", mix_console->raw);
        }
      gdb_flush (mix_console->raw);
    }
}

static void
mix_console_file_flush (struct ui_file *file)
{
  struct mix_console_file *mix_console = ui_file_data (file);
  if (mix_console->magic != &mix_console_file_magic)
    internal_error (__FILE__, __LINE__,
                    _("mix_console_file_flush: bad magic number"));
  ui_file_put (mix_console->buffer, mix_console_raw_packet, mix_console);
  ui_file_rewind (mix_console->buffer);
}
