/* Original in GNU gdb 6.3.50-20050815 (Apple version gdb-956),
   modified on 2008-05-30 for xcxdb by Dr. Rolf Jansen. */

/* GDB variable objects API.

   Copyright (C) 1999, 2000, 2005 Free Software Foundation, Inc.

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

#ifndef VAROBJ_MIX_H
#define VAROBJ_MIX_H 1

#include "symtab.h"
#include "gdbtypes.h"

/* Enumeration for the format types.  If you add elements here, be sure
   to update format_code in varobj.c to match.  */
enum varobj_display_formats
  {
    FORMAT_NATURAL,             /* What gdb actually calls 'natural'     */
    FORMAT_BINARY,              /* Binary display                        */
    FORMAT_DECIMAL,             /* Decimal display                       */
    FORMAT_HEXADECIMAL,         /* Hex display                           */
    FORMAT_OCTAL,               /* Octal display                         */
    FORMAT_UNSIGNED,            /* APPLE LOCAL: Unsigned decimal display */
    FORMAT_OSTYPE               /* APPLE LOCAL: OSType display           */
  };

enum varobj_type
  {
    USE_SPECIFIED_FRAME,        /* Use the frame passed to varobj_create */
    USE_CURRENT_FRAME,          /* Use the current frame */
    USE_SELECTED_FRAME,         /* Always reevaluate in selected frame */
    USE_BLOCK_IN_FRAME,         /* Use the address as a block in the current frame. */
    NO_FRAME_NEEDED             /* No frame is needed, this is a file static or a global variable. */
  };
    
/* String representations of gdb's format codes (defined in varobj-mix.c) */
extern char *mix_varobj_format_string[];

/* Languages supported by this variable objects system. */
enum varobj_languages
  {
    vlang_unknown = 0, vlang_c, vlang_cplus, vlang_java, vlang_end
  };

/* String representations of gdb's known languages (defined in varobj.c) */
extern char *varobj_language_string[];

/* This represents the possible type-changed states: */

enum varobj_type_change {
  VAROBJ_TYPE_UNCHANGED,
  VAROBJ_TYPE_CHANGED,
  VAROBJ_SCOPE_CHANGED,
  VAROBJ_DYNAMIC_TYPE_CHANGED
};

/* Struct that describes a variable object instance  */
struct varobj;

/* A linked list of varobjs.  Used to package the result of varobj_update.  */

struct varobj_changelist;

/* API functions */

extern struct varobj *mix_varobj_create (char *objname,
                                         char *expression, CORE_ADDR frame,
                                         struct block *block,
                                         enum varobj_type type);

extern char *mix_varobj_gen_name (void);

extern struct varobj *mix_varobj_get_handle (char *name);

extern char *mix_varobj_get_objname (struct varobj *var);

extern char *mix_varobj_get_expression (struct varobj *var);

extern int mix_varobj_delete (struct varobj *var, char ***dellist,
                              int only_children);

extern enum varobj_display_formats mix_varobj_set_display_format (struct varobj *var,
                                                                  enum varobj_display_formats format);

extern enum varobj_display_formats mix_varobj_get_display_format (struct varobj *var);

extern int mix_varobj_get_num_children (struct varobj *var);

extern int mix_varobj_list_children (struct varobj *var,
                                     struct varobj ***childlist);

extern int mix_varobj_is_fake_child (struct varobj *var);

extern char *mix_varobj_get_type (struct varobj *var);

extern char *mix_varobj_get_dynamic_type (struct varobj *var);

extern char *mix_varobj_get_path_expr (struct varobj *var);

extern struct type *mix_varobj_get_type_struct (struct varobj *var);

extern struct type *mix_varobj_get_gdb_type (struct varobj *var);

extern enum varobj_languages mix_varobj_get_language (struct varobj *var);

extern int mix_varobj_get_attributes (struct varobj *var);

extern char *mix_varobj_get_value (struct varobj *var);

extern int mix_varobj_set_value (struct varobj *var, char *expression);

extern int mix_varobj_list (struct varobj ***rootlist);

extern int mix_varobj_in_scope_p (struct varobj *var);

extern int mix_varobj_pc_in_valid_block_p (struct varobj *var);

extern int mix_varobj_update (struct varobj **varp, 
                              struct varobj_changelist **changelist);

extern struct varobj *mix_varobj_changelist_pop (struct varobj_changelist *changelist, 
                                                 enum varobj_type_change *type_changed);

extern void mix_varobj_get_valid_block (struct varobj *var, CORE_ADDR *start, CORE_ADDR *end);

int gdb_varobj_get_value (struct varobj *val1, char **result);

#endif /* VAROBJ_MIX_H */
