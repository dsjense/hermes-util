      subroutine pful2i ( lval, ival, ierr )
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
c       - This routine is a UTILITY routine to encode a long signed
c         integer into three 15-bit (unsigned) integers.  The most
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
c       lval    -  integer to be encoded
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       ival    -  (*)  array of 3 encoded integers
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   Long Integer Out of Range
c
c ----------------------------------------------------------------------
c
c     Internals:
c       i       -  do loop index encoded words
c       imsk    -  (*)  array of masks
c       tval    -  temporary for shifted long integer
c       tvaln   -  temporary for shifted long integer
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
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   lval, ival(3), ierr
c
c     Internal
      integer   i, imsk(3), tval, tvaln
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFUL2I' )
c
c ... External Block Data
      external  pffbd
c
c ----------------------------------------------------------------------
c
c     Internal Data Statements:
c
      data imsk  / 16384, 32768, 32768 /
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
c ... store absolute value of integer to be encoded in temporary
c
      tval = abs(lval)
c
c ... loop over words in reverse so that most-significant word is first
c
      do 1 i=3,1,-1
c
c ..... if tval > mask, divide to extract next word; then get value for
c ..... this word
c
        if ( tval.ge.imsk(i) ) then
          tvaln   = tval/imsk(i)
          ival(i) = tval - imsk(i)*tvaln
          tval    = tvaln
c
c ..... otherwise, this word = value, and value for next word is zero
c
        else
          ival(i) = tval
          tval    = 0
        endif
c
    1 continue
c
c ... if value remains, we have overflow error
c
      if (tval.ne.0) then
c
        ierr = 1
        call pfserr ( PFMODU, ierr, 0, 'Long Integer Out of Range' )
        goto 999
c
c ... if no overflow, check to see if we need to store the sign bit
c
      else if (lval.lt.0) then
c
        ival(1) = ival(1) + imsk(1)
c
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
