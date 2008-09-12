/* Original in GNU gdb 6.8.50.20080428-cvs,
   modified on 2008-04-28 for xcxdb by Dr. Rolf Jansen. */

/* MI Command Set - output generating routines.
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
#include "ui-out.h"
#include "mix-out.h"

struct ui_out_data
  {
    int suppress_field_separator;
    int suppress_output;
    int mix_version;
    struct ui_file *buffer;
  };
typedef struct ui_out_data mix_out_data;

/* These are the MI output functions */

static void mix_table_begin (struct ui_out *uiout, int nbrofcols,
                            int nr_rows, const char *tblid);
static void mix_table_body (struct ui_out *uiout);
static void mix_table_end (struct ui_out *uiout);
static void mix_table_header (struct ui_out *uiout, int width,
                             enum ui_align alig, const char *col_name,
                             const char *colhdr);
static void mix_begin (struct ui_out *uiout, enum ui_out_type type,
                      int level, const char *id);
static void mix_end (struct ui_out *uiout, enum ui_out_type type, int level);
static void mix_field_int (struct ui_out *uiout, int fldno, int width,
                          enum ui_align alig, const char *fldname, int value);
static void mix_field_skip (struct ui_out *uiout, int fldno, int width,
                           enum ui_align alig, const char *fldname);
static void mix_field_string (struct ui_out *uiout, int fldno, int width,
                             enum ui_align alig, const char *fldname,
                             const char *string);
static void mix_field_fmt (struct ui_out *uiout, int fldno,
                          int width, enum ui_align align,
                          const char *fldname, const char *format,
                          va_list args) ATTR_FORMAT (printf, 6, 0);
static void mix_spaces (struct ui_out *uiout, int numspaces);
static void mix_text (struct ui_out *uiout, const char *string);
static void mix_message (struct ui_out *uiout, int verbosity,
                        const char *format, va_list args)
     ATTR_FORMAT (printf, 3, 0);
static void mix_wrap_hint (struct ui_out *uiout, char *identstring);
static void mix_flush (struct ui_out *uiout);

/* This is the MI ui-out implementation functions vector */

/* FIXME: This can be initialized dynamically after default is set to
   handle initial output in main.c */

struct ui_out_impl mix_ui_out_impl =
{
  mix_table_begin,
  mix_table_body,
  mix_table_end,
  mix_table_header,
  mix_begin,
  mix_end,
  mix_field_int,
  mix_field_skip,
  mix_field_string,
  mix_field_fmt,
  mix_spaces,
  mix_text,
  mix_message,
  mix_wrap_hint,
  mix_flush,
  NULL,
  1, /* Needs MI hacks.  */
};

/* Prototypes for local functions */

extern void _initialize_mix_out (void);
static void field_separator (struct ui_out *uiout);
static void mix_open (struct ui_out *uiout, const char *name,
                     enum ui_out_type type);
static void mix_close (struct ui_out *uiout, enum ui_out_type type);

/* Mark beginning of a table */

void
mix_table_begin (struct ui_out *uiout,
                int nr_cols,
                int nr_rows,
                const char *tblid)
{
  mix_out_data *data = ui_out_data (uiout);
  mix_open (uiout, tblid, ui_out_type_tuple);
  mix_field_int (uiout, -1/*fldno*/, -1/*width*/, -1/*alin*/,
                "nr_rows", nr_rows);
  mix_field_int (uiout, -1/*fldno*/, -1/*width*/, -1/*alin*/,
                "nr_cols", nr_cols);
  mix_open (uiout, "hdr", ui_out_type_list);
}

/* Mark beginning of a table body */

void
mix_table_body (struct ui_out *uiout)
{
  mix_out_data *data = ui_out_data (uiout);
  if (data->suppress_output)
    return;
  /* close the table header line if there were any headers */
  mix_close (uiout, ui_out_type_list);
  mix_open (uiout, "body", ui_out_type_list);
}

/* Mark end of a table */

void
mix_table_end (struct ui_out *uiout)
{
  mix_out_data *data = ui_out_data (uiout);
  data->suppress_output = 0;
  mix_close (uiout, ui_out_type_list); /* body */
  mix_close (uiout, ui_out_type_tuple);
}

/* Specify table header */

void
mix_table_header (struct ui_out *uiout, int width, enum ui_align alignment,
                 const char *col_name,
                 const char *colhdr)
{
  mix_out_data *data = ui_out_data (uiout);
  if (data->suppress_output)
    return;
  mix_open (uiout, NULL, ui_out_type_tuple);
  mix_field_int (uiout, 0, 0, 0, "width", width);
  mix_field_int (uiout, 0, 0, 0, "alignment", alignment);
  mix_field_string (uiout, 0, 0, 0, "col_name", col_name);
  mix_field_string (uiout, 0, width, alignment, "colhdr", colhdr);
  mix_close (uiout, ui_out_type_tuple);
}

/* Mark beginning of a list */

void
mix_begin (struct ui_out *uiout,
          enum ui_out_type type,
          int level,
          const char *id)
{
  mix_out_data *data = ui_out_data (uiout);
  if (data->suppress_output)
    return;
  mix_open (uiout, id, type);
}

/* Mark end of a list */

void
mix_end (struct ui_out *uiout,
        enum ui_out_type type,
        int level)
{
  mix_out_data *data = ui_out_data (uiout);
  if (data->suppress_output)
    return;
  mix_close (uiout, type);
}

/* output an int field */

void
mix_field_int (struct ui_out *uiout, int fldno, int width,
              enum ui_align alignment, const char *fldname, int value)
{
  char buffer[20];              /* FIXME: how many chars long a %d can become? */
  mix_out_data *data = ui_out_data (uiout);
  if (data->suppress_output)
    return;

  sprintf (buffer, "%d", value);
  mix_field_string (uiout, fldno, width, alignment, fldname, buffer);
}

/* used to ommit a field */

void
mix_field_skip (struct ui_out *uiout, int fldno, int width,
               enum ui_align alignment, const char *fldname)
{
  mix_out_data *data = ui_out_data (uiout);
  if (data->suppress_output)
    return;
  mix_field_string (uiout, fldno, width, alignment, fldname, "");
}

/* other specific mix_field_* end up here so alignment and field
   separators are both handled by mix_field_string */

void
mix_field_string (struct ui_out *uiout,
                 int fldno,
                 int width,
                 enum ui_align align,
                 const char *fldname,
                 const char *string)
{
  mix_out_data *data = ui_out_data (uiout);
  if (data->suppress_output)
    return;
  field_separator (uiout);
  if (fldname)
    fprintf_unfiltered (data->buffer, "%s=", fldname);
  fprintf_unfiltered (data->buffer, "\"");
  if (string)
    fputstr_unfiltered (string, '"', data->buffer);
  fprintf_unfiltered (data->buffer, "\"");
}

/* This is the only field function that does not align */

void
mix_field_fmt (struct ui_out *uiout, int fldno,
              int width, enum ui_align align,
              const char *fldname,
              const char *format,
              va_list args)
{
  mix_out_data *data = ui_out_data (uiout);
  if (data->suppress_output)
    return;
  field_separator (uiout);
  if (fldname)
    fprintf_unfiltered (data->buffer, "%s=\"", fldname);
  else
    fputs_unfiltered ("\"", data->buffer);
  vfprintf_unfiltered (data->buffer, format, args);
  fputs_unfiltered ("\"", data->buffer);
}

void
mix_spaces (struct ui_out *uiout, int numspaces)
{
}

void
mix_text (struct ui_out *uiout, const char *string)
{
}

void
mix_message (struct ui_out *uiout, int verbosity,
            const char *format,
            va_list args)
{
}

void
mix_wrap_hint (struct ui_out *uiout, char *identstring)
{
  wrap_here (identstring);
}

void
mix_flush (struct ui_out *uiout)
{
  mix_out_data *data = ui_out_data (uiout);
  gdb_flush (data->buffer);
}

/* local functions */

/* access to ui_out format private members */

static void
field_separator (struct ui_out *uiout)
{
  mix_out_data *data = ui_out_data (uiout);
  if (data->suppress_field_separator)
    data->suppress_field_separator = 0;
  else
    fputc_unfiltered (',', data->buffer);
}

static void
mix_open (struct ui_out *uiout,
         const char *name,
         enum ui_out_type type)
{
  mix_out_data *data = ui_out_data (uiout);
  field_separator (uiout);
  data->suppress_field_separator = 1;
  if (name)
    fprintf_unfiltered (data->buffer, "%s=", name);
  switch (type)
    {
    case ui_out_type_tuple:
      fputc_unfiltered ('{', data->buffer);
      break;
    case ui_out_type_list:
      fputc_unfiltered ('[', data->buffer);
      break;
    default:
      internal_error (__FILE__, __LINE__, _("bad switch"));
    }
}

static void
mix_close (struct ui_out *uiout,
          enum ui_out_type type)
{
  mix_out_data *data = ui_out_data (uiout);
  switch (type)
    {
    case ui_out_type_tuple:
      fputc_unfiltered ('}', data->buffer);
      break;
    case ui_out_type_list:
      fputc_unfiltered (']', data->buffer);
      break;
    default:
      internal_error (__FILE__, __LINE__, _("bad switch"));
    }
  data->suppress_field_separator = 0;
}

/* add a string to the buffer */

void
mix_out_buffered (struct ui_out *uiout, char *string)
{
  mix_out_data *data = ui_out_data (uiout);
  fprintf_unfiltered (data->buffer, "%s", string);
}

/* clear the buffer */

void
mix_out_rewind (struct ui_out *uiout)
{
  mix_out_data *data = ui_out_data (uiout);
  ui_file_rewind (data->buffer);
}

/* dump the buffer onto the specified stream */

static void
do_write (void *data, const char *buffer, long length_buffer)
{
  ui_file_write (data, buffer, length_buffer);
}

void
mix_out_put (struct ui_out *uiout,
            struct ui_file *stream)
{
  mix_out_data *data = ui_out_data (uiout);
  ui_file_put (data->buffer, do_write, stream);
  ui_file_rewind (data->buffer);
}

/* Current MI version.  */

int
mix_version (struct ui_out *uiout)
{
  mix_out_data *data = ui_out_data (uiout);
  return data->mix_version;
}

/* initalize private members at startup */

struct ui_out *
mix_out_new (int mix_version)
{
  int flags = 0;
  mix_out_data *data = XMALLOC (mix_out_data);
  data->suppress_field_separator = 0;
  data->suppress_output = 0;
  data->mix_version = mix_version;
  /* FIXME: This code should be using a ``string_file'' and not the
     TUI buffer hack. */
  data->buffer = mem_fileopen ();
  return ui_out_new (&mix_ui_out_impl, data, flags);
}

/* standard gdb initialization hook */
void
_initialize_mix_out (void)
{
  /* nothing happens here */
}
