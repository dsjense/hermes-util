      subroutine pfui2l ( ival, lval, ierr )
c
c***********************************************************************
c     $Id$
c     
c     Copyright (2008) Sandia Corporation. Under the terms of
c     Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
c     Government retains certain rights in this software.
c     
c     Hermes is free software: you can redistribute it and/or modify
c     it under the terms of the GNU Lesser General Public License as
c     published by the Free Software Foundation, either version 3 of
c     the License, or (at your option) any later version.
c     
c     Hermes is distributed in the hope that it will be useful, but
c     WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU Lesser General Public License for more details.
c     
c     You should have received a copy of the GNU Lesser General
c     Public License along with Hermes.  If not, see
c     <http://www.gnu.org/licenses/>.
c     
C_Groups @(#)
c***********************************************************************
c
c
c ----------------------------------------------------------------------
c
c     Summary:
c
c       - This routine is a UTILITY routine to decode three 15-bit
c         (unsigned) integers into a long signed integer.  The most
c         significant part is stored in the first word, along with the
c         sign bit.  The range of the long integer is +/- (2**44 - 1).
c         The decoding algorithm is:
c
c           Long  =  S*( ( mod(i1,M1)*M2 + i2 )*M3 + i3 ) ,
c           M1    =  16384   (2**14)
c           M2    =  32768   (2**15)
c           M3    =  32768   (2**15)
c                    /  1,  if i1 < M1
c           S     =  |
c                    \ -1,  otherwise
c
c
c     Input:
c       ival    -  (*)  array of 3 encoded integers to be decoded
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       lval    -  integer that has been decoded
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   Encoded Integers Larger Than 15-bit
c                      2,   Long word (lval) overflow
c
c ----------------------------------------------------------------------
c
c     Internals:
c       lval64  -  64-bit long word
c       tmin    -  minimum of encoded integers
c       tmax    -  maximum of encoded integers
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pferrs.inc"
c
#include "mdfpos.h"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   ival(3), lval, ierr
c
c     Internal
      integer   imsk, tmin, tmax
      HU_FPOS_TYPE lval64
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFUI2L' )
c
c ... External Block Data
      external  pffbd
c
c ----------------------------------------------------------------------
c
c     Internal Data Statements:
c
      data imsk  / 32768 /
c
c ----------------------------------------------------------------------
c
c
c ... write status message if DEBUG is on
c
      if (ludbg.ne.OFFLUN)then
        write (ludbg,1000) PFMODU, 'Entry', ierr
      endif
c
c ... return if error flag is not zero on entry
c
      if(ierr.ne.0)then
        call pfserr ( PFMODU, 0, 0, 'Error Flag Non-zero on Entry' )
        goto 999
      endif
c
c ----------------------------------------------------------------------
c
c ... make sure encoded integers are in 15-bit unsigned range
c
      tmin = min ( ival(1), ival(2), ival(3) )
      tmax = max ( ival(1), ival(2), ival(3) )
c
      if ( tmin.lt.0 .OR. tmax.ge.imsk ) then
c
        ierr = 1
        call pfserr ( PFMODU, ierr, 0,
     &             'Encoded Integers Signed or Larger Than 15-bits' )
        goto 999
c
      endif 
c
c ... call 64-bit version -- then check for 32-bit overflow
c
      call pfui2l64 ( ival, lval64, ierr )
      lval = lval64
      if ( lval.ne.lval64 ) then
        ierr = 2
        call pfserr ( PFMODU, ierr, 0, 'Long word (lval) overflow' )
      endif
c
  999 continue
c
c     write status message if DEBUG is on and return
c
      if (ludbg.ne.OFFLUN)then
        write (ludbg,1000) PFMODU, 'Exit', ierr
      endif
c
      return
c
 1000 format(' %PFF-',a,'-DBG:  ',a,' Error Status = ',i5)
c
      end
