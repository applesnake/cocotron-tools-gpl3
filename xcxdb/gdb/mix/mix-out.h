/* Original in GNU gdb 6.8.50.20080428-cvs,
   modified on 2008-05-05 for xcxdb by Dr. Rolf Jansen. */

/* MI Command Set - MI output generating routines for GDB.
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

#ifndef MIX_OUT_H
#define MIX_OUT_H

struct ui_out;
struct ui_file;

extern struct ui_out *mix_out_new (int mix_version);
extern void mix_out_put (struct ui_out *uiout, struct ui_file *stream);
extern void mix_out_rewind (struct ui_out *uiout);
extern void mix_out_buffered (struct ui_out *uiout, char *string);

/* Return the version number of the current MI.  */
extern int mix_version (struct ui_out *uiout);

#endif /* MI_OUT_H */
