c ----------------------------------------------------------------------
c   $Id$
c   
c   Copyright (2008) Sandia Corporation. Under the terms of
c   Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
c   Government retains certain rights in this software.
c   
c   Hermes is free software: you can redistribute it and/or modify
c   it under the terms of the GNU Lesser General Public License as
c   published by the Free Software Foundation, either version 3 of
c   the License, or (at your option) any later version.
c   
c   Hermes is distributed in the hope that it will be useful, but
c   WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU Lesser General Public License for more details.
c   
c   You should have received a copy of the GNU Lesser General
c   Public License along with Hermes.  If not, see
c   <http://www.gnu.org/licenses/>.
c   
C_Groups @(#)
#include "mdptr.h"
c ----------------------------------------------------------------------
c> \file stak.inc
c> \brief Parameters and global variables for STAK memory management software.
c
c ----------------------------------------------------------------------
c
c> \name Machine-Independent Parameters
c> \{
c
c>  integer value to which cookie is set to indicate that software has been
c>  properly initialized
      integer MAGIC
c>  negative value indicating that a memory bin or pointer has been reserved
c>  but has not yet been used
      integer RESRVED
c>  negative value indicating that a memory bin or pointer is not currently
c>  reserved or in use
      integer NOT_SET
c>  maximum # of memory bins that can be allocated
c>  \note Must be consistent with \b MAX_BINS in \ref namell.c
      integer NBINS
c>  maximum # of characters for a user-supplied array name
      integer CMAX
c>  last (minimum) pointer on pointer type enumeration list (see stkenum.inc)
      integer LASTPTR
c>  indicates the last allocated pointer of a memory bin
c>  \note Must be consistent with \b LAST in \ref namell.c
      integer L_LAST
c>  indicates the first allocated pointer of a memory bin
c>  \note Must be consistent with \b FIRST in \ref namell.c
      integer L_FIRST
c>  indicates the next allocated pointer of a memory bin
c>  \note Must be consistent with \b NEXT in \ref namell.c
      integer L_NXT

      parameter ( MAGIC = 999, RESRVED = -1, NOT_SET = -2 )
      parameter ( NBINS = 100, CMAX = 80 )
      parameter ( L_LAST = 0 , L_FIRST = -1 , L_NXT = -2, 
     &            LASTPTR = -9  )
c> \}
c
c> \name Machine-Dependent Parameters
c> \{
c>  \note  If a specific byte alignment for a give type \e x is not required, 
c>         then set \e BA_x to zero.  Otherwise, set \e BA_x to the # of bytes
c>         in the word to which the datatype must be aligned.  Note that
c>         this value MUST BE a multiple of NBPI !!!
c
c>  maximum # of integers for a user-supplied array name (in Hollerth)
      integer HMAX
c>  # of bytes in an integer word
      integer NBPI
c>  # of bytes in an integer*2 word
      integer NBPI2
c>  # of bytes in an integer*4 word
      integer NBPI4
c>  # of bytes in a logical word
      integer NBPL
c>  # of bytes in a logical*1 word
      integer NBPL1
c>  # of bytes in a real word
      integer NBPF
c>  # of bytes in a double precision word
      integer NBPD
c>  # of bytes in a real*4 word
      integer NBPR4
c>  # of bytes in a real*8 word
      integer NBPR8
c>  # of bytes in a pointer
      integer NBPPTR
c>  # of bytes/character in a character variable
      integer NBPC
c>  number of bytes per address unit
      integer NBPAU
c>  byte alignment for an integer word
      integer BA_I
c>  byte alignment for an integer*2 word
      integer BA_I2
c>  byte alignment for an integer*4 word
      integer BA_I4
c>  byte alignment for a logical word
      integer BA_L
c>  byte alignment for a logical*1 word
      integer BA_L1
c>  byte alignment for a real word
      integer BA_F
c>  byte alignment for a double precision word
      integer BA_D
c>  byte alignment for a real*4 word
      integer BA_R4
c>  byte alignment for a real*8 word
      integer BA_R8
c>  byte alignment for a pointer
      integer BA_PTR
c>  byte alignment for a character word
      integer BA_C
c
#if defined(CRAsys)
      parameter ( NBPI = 8, NBPI2 = 8, NBPI4 = 8, NBPL = 8, NBPL1 = 8 ,
     &            NBPF = 8, NBPD = 16, NBPR4 = 8, NBPR8 = 8, NBPC = 0 ,
     &            NBPPTR = HU_PTR_BYTES )
      parameter ( BA_I = 0, BA_I2 = 0, BA_I4 = 0, BA_L = 0, BA_L1 = 0 ,
     &            BA_F = 0, BA_D =  0, BA_R4 = 0, BA_R8 = 0, BA_C = 0 ,
     &            BA_PTR = 0 )
      parameter ( NBPAU = 8 )
#else
# if HU_PTR_BYTES == 4
#  define PTR_B_ALIGN  0
# else
#  define PTR_B_ALIGN  HU_PTR_BYTES
# endif
      parameter ( NBPI = 4, NBPI2 = 2, NBPI4 = 4, NBPL = 4, NBPL1 = 1 ,
     &            NBPF = 4, NBPD  = 8, NBPR4 = 4, NBPR8 = 8, NBPC = 1 ,
     &            NBPPTR = HU_PTR_BYTES )
      parameter ( BA_I = 0, BA_I2 = 0, BA_I4 = 0, BA_L = 0, BA_L1 = 0 ,
     &            BA_F = 0, BA_D =  8, BA_R4 = 0, BA_R8 = 8, BA_C = 0 ,
     &            BA_PTR = PTR_B_ALIGN )
      parameter ( NBPAU = 1 )
#endif
c
#if defined(CRAsys)
      parameter ( HMAX = CMAX/NBPI )
#else
      parameter ( HMAX = CMAX*NBPC/NBPI )
#endif
c> \}
c> \name Common Block /stakmem/
c> \{
c
c>  variable that when set to MAGIC value indicates that software has been
c>  properly initialized
      integer cookie
c>  logical unit to which error messages are written if lustk > MINLU
      integer lustk
c>  array containing the # of guard words to be used for each memory bin
      integer ngard(1:NBINS)
c>  array containing count of allocations for each memory bin
      integer count(1:NBINS)
c>  array containing unit number for file where diagnostic information
c>  regarding allocation and deallocation for this bin is to be written.
c>  If <= MINLU, diagnostics are not output.
      integer luall(1:NBINS)
c>  array containing logical flag indicating that bin is in "reset" mode.
c>  This mode is designed to allow pointers to be properly re-instantiated
c>  for a memory bin that has been restored from an external source, e.g.,
c>  a restart file.
      logical reset(1:NBINS)
c>  array containing logical flag indicating that a linked list of names and
c>  other info is to be maintained for this bin.
      logical snames(1:NBINS)
c>  integer (Hollerith) workspace for C to Fortran translation
      integer hname(1:HMAX)
c>  list of bytes/integer word for each of the enumerated word types
      integer typebyts(LASTPTR:0)
c>  list of byte alignment values for each of the enumerated word types
      integer alignbyts(LASTPTR:0)
c
c     Common block definition
      common /stakmem/  cookie, lustk, count, ngard, reset, snames,
     &                  luall, hname, typebyts, alignbyts
c> \}
c
c> \name Common Block /stakmemt/
c> \{
c
c>  array containing the next unused word for each memory bin
      HU_SIZE_T next(1:NBINS)
c>  array containing the allocated size for each memory bin. If NOT_SET,
c>  indicates bin has not been initialized or reserved. If RESRVED, indicates
c>  bin has been reserved but not initialized.  If zero, indicates that bin
c>  has been initialized but not allocated, allowing needed size to be computed
c>  via a two-pass pointer allocation sequence
      HU_SIZE_T biggest(1:NBINS)
c
c     Common block definition
      common /stakmemt/  next, biggest
c> \}
c
c> \name Common Block /stakmemc/
c> \{
c
c>  character workspace for C to Fortran translation
      character cname*(CMAX)
c
c     Common block definition
      common /stakmemc/ cname
c> \}
c
c> \name Common Block /stakmemp/
c> \{
c
c>   array of pointers to dynamically allocated memory bins
      HU_PTR_TYPE binptr(1:NBINS)
c
c     Common block definition
      common /stakmemp/ binptr
c> \}
