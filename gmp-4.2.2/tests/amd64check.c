/* AMD64 calling conventions checking.

Copyright 2000, 2001, 2004, 2007 Free Software Foundation, Inc.

This file is part of the GNU MP Library.

The GNU MP Library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

The GNU MP Library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
License for more details.

You should have received a copy of the GNU Lesser General Public License
along with the GNU MP Library.  If not, see http://www.gnu.org/licenses/.  */

#include <stdio.h>
#include "gmp.h"
#include "gmp-impl.h"
#include "tests.h"


/* Vector if constants and register values.  We use one vector to allow access
   via a base pointer, very beneficial for the PIC-enabled amd64call.asm.  */
long calling_conventions_values[23] =
{
  0x1234567887654321L,		/* want_rbx */
  0x89ABCDEFFEDCBA98L,		/* want_rbp */
  0xDEADBEEFBADECAFEL,		/* want_r12 */
  0xFFEEDDCCBBAA9988L,		/* want_r13 */
  0x0011223344556677L,		/* want_r14 */
  0x1234432156788765L,		/* want_r15 */

  0xFEEDABBACAAFBEED,		/* JUNK_RAX */
  0xAB78DE89FF5125BB,		/* JUNK_R10 */
  0x1238901890189031		/* JUNK_R11 */

  /* rest of array used for dynamic values.  */
};

/* Index starts for various regions in above vector.  */
#define WANT	0
#define JUNK	6
#define SAVE	9
#define RETADDR	15
#define VAL	16
#define RFLAGS	22

/* values to check */
struct {
  int  control;
  int  status;
  int  tag;
  int  other[4];
} calling_conventions_fenv;


char *regname[6] = {"rbx", "rbp", "r12", "r13", "r14", "r15"};

#define DIR_BIT(rflags)   (((rflags) & (1<<10)) != 0)


/* Return 1 if ok, 0 if not */

int
calling_conventions_check (void)
{
  const char  *header = "Violated calling conventions:\n";
  int  ret = 1;
  int i;

#define CHECK(callreg, regstr, value)			\
  if (callreg != value)					\
    {							\
      printf ("%s   %s	got 0x%016lX want 0x%016lX\n",	\
	      header, regstr, callreg, value);		\
      header = "";					\
      ret = 0;						\
    }

  for (i = 0; i < 6; i++)
    {
      CHECK (calling_conventions_values[VAL+i], regname[i], calling_conventions_values[WANT+i]);
    }

  if (DIR_BIT (calling_conventions_values[RFLAGS]) != 0)
    {
      printf ("%s   rflags dir bit  got %d want 0\n",
	      header, DIR_BIT (calling_conventions_values[RFLAGS]));
      header = "";
      ret = 0;
    }

  if ((calling_conventions_fenv.tag & 0xFFFF) != 0xFFFF)
    {
      printf ("%s   fpu tags  got 0x%lX want 0xFFFF\n",
	      header, calling_conventions_fenv.tag & 0xFFFF);
      header = "";
      ret = 0;
    }

  return ret;
}
