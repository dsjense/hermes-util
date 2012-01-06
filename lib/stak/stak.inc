c
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
c ----------------------------------------------------------------------
c
c     Parameters and global variables for STAK memory management 
c     software.
c
c ----------------------------------------------------------------------
c
c     Parameters
c       MAGIC    -  integer value to which cookie is set to indicate
c                   that software has been properly initialized
c       RESRVED  -  negative value indicating that a memory bin or 
c                   pointer has been reserved but has not yet been used
c       NOT_SET  -  negative value indicating that a memory bin or
c                   pointer is not currently reserved or in use
c       NBPAU    -  number of bytes per address unit
c       BA_I     -  byte alignment for an integer word
c       BA_I2    -  byte alignment for an integer*2 word
c       BA_I4    -  byte alignment for an integer*4 word
c       BA_L     -  byte alignment for a logical word
c       BA_L1    -  byte alignment for a logical*1 word
c       BA_F     -  byte alignment for a real word
c       BA_D     -  byte alignment for a double precision word
c       BA_R4    -  byte alignment for a real*4 word
c       BA_R8    -  byte alignment for a real*8 word
c       BA_PTR   -  byte alignment for a pointer
c       BA_C     -  byte alignment for a character word
c       NBPI     -  # of bytes in an integer word
c       NBPI2    -  # of bytes in an integer*2 word
c       NBPI4    -  # of bytes in an integer*4 word
c       NBPL     -  # of bytes in a logical word
c       NBPL1    -  # of bytes in a logical*1 word
c       NBPF     -  # of bytes in a real word
c       NBPD     -  # of bytes in a double precision word
c       NBPR4    -  # of bytes in a real*4 word
c       NBPR8    -  # of bytes in a real*8 word
c       NBPPTR   -  # of bytes in a real*8 word
c       NBPC     -  # of bytes/character in an character word
c       NBINS    -  maximum # of memory bins that can be allocated
c       LASTPTR  -  last pointer on enumeration list
c
c       Note of byte alignment:
c          If a specific byte alignment for a give type is not required, 
c          then set BA_x to zero.  Otherwise, set BA_x to the # of bytes
c          in the word to which the datatype must be aligned.  Note that
c          this value MUST BE a multiple of NBPI !!!
c
c ----------------------------------------------------------------------
c
c     Global Variables (common block stakmem)
c       binptr   -  array of pointers to dynamically allocated memory bins
c       cookie   -  variable that when set to MAGIC value indicates 
c                   that software has been properly initialized
c       count    -  (*) array containing count of allocations for each 
c                   memory bin
c       lustk    -  logical unit to which error messages are written if 
c                   lustk > MINLU
c       next     -  (*) array containing the next unused word for each 
c                   memory bin
c       ngard    -  (*) array containing the # of guard words to be 
c                   used for each memory bin
c       biggest  -  (*) array containing the allocated size for each 
c                   memory bin.  If NOT_SET, indicates bin has not been
c                   initialized or reserved.  If RESRVED, indicates bin 
c                   has been reserved but not initialized.  If zero, 
c                   indicates that bin has been initialized but not 
c                   allocated, allowing needed size to be computed via a 
c                   two-pass pointer allocation sequence
c       reset    -  (*) array containing logical flag indicating that 
c                   bin is in "reset" mode.  This mode is designed to 
c                   allow pointers to be properly re-instantiated for
c                   a memory bin that has been restored from an external
c                   source, e.g., a restart file.
c       snames   -  (*) array containing logical flag indicating that 
c                   a linked list of names and other info is to be 
c                   maintained for this bin.
c       luall    -  (*) array containing unit number for file where 
c                   diagnostic information regarding allocation and
c                   deallocation for this bin is to be written.  If <=
c                   MINLU, diagnostics are not output.
c       cname    -  character workspace for C to Fortran translation
c       hname    -  (*) integer (Hollerith) workspace for C to Fortran 
c                   translation
c       typebyts -  (*) list of bytes/integer word for each of the 
c                   enumerated word types
c       alignbyts-  (*) list of byte alignment values for each of the 
c                   enumerated word types
c
c ----------------------------------------------------------------------
c       
c     Structure of Memory Bins
c
c       Each memory bin is a dynamically allocated array.  The pointer
c       allocation function stkptr returns pointers to locations in this 
c       array.  Each block of space allocated from the bin is bracketed 
c       by a leading and trailing control word.  These control words 
c       contain the length (in integer words) of the block that they 
c       bracket, thus allowing the bin to be traversed in either 
c       direction.  Also, between the control words for each block of 
c       space allocated from the bin are optional guard words, preloaded 
c       with the negative of the word index of the bin.
c
c ----------------------------------------------------------------------
c
#include "mdptr.h"
c
c ... Parameters
      integer MAGIC, RESRVED, NOT_SET, NBINS, CMAX, HMAX
      integer NBPI, NBPI2, NBPI4, NBPL, NBPL1, NBPF, NBPD, NBPR4, NBPR8,
     &        NBPPTR, NBPC
      integer BA_I, BA_I2, BA_I4, BA_L, BA_L1, BA_F, BA_D, BA_R4, BA_R8,
     &        BA_PTR, BA_C
      integer NBPAU, L_LAST, L_FIRST, L_NXT, LASTPTR
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
      parameter ( MAGIC = 999, RESRVED = -1, NOT_SET = -2 )
      parameter ( CMAX = 80 )
#if defined(CRAsys)
      parameter ( HMAX = CMAX/NBPI )
#else
      parameter ( HMAX = CMAX*NBPC/NBPI )
#endif
      parameter ( L_LAST = 0 , L_FIRST = -1 , L_NXT = -2, 
     &            LASTPTR = -9  )
c
      parameter ( NBINS = 100 )
c
c ----------------------------------------------------------------------
c
c ... Global variables
      integer cookie, lustk
      integer ngard(1:NBINS), count(1:NBINS), luall(1:NBINS)
      logical reset(1:NBINS), snames(1:NBINS)
      integer hname(1:HMAX)
      integer typebyts(LASTPTR:0), alignbyts(LASTPTR:0)
      HU_SIZE_T next(1:NBINS), biggest(1:NBINS)
      character cname*(CMAX)
c
c ... array containing bin pointers
c
      HU_PTR_TYPE binptr(1:NBINS)
c
c ----------------------------------------------------------------------
c
c     Common block definition
c
      common /stakmem/  next, biggest, cookie, lustk, count, ngard,
     &                  reset, snames, luall, hname, typebyts, alignbyts
      common /stakmemp/ binptr
c
      common /stakmemc/ cname
c
