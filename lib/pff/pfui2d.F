      subroutine pfui2d ( ival, dval, ierr )
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
c       - This routine is a UTILITY routine to decode a double precision
c         number from five 15-bit unsigned integers.
c            ival(1) = 16 most significant bits of mantissa (with most
c                      significant bit hidden/zeroed)
c            ival(2) = 15 next most significant bits of mantissa
c            ival(3) = 15 next most significant bits of mantissa
c            ival(4) = Remaining bits of mantissa
c            ival(5) = low bit    --  sign bit
c                      bits 2-15  --  excess-8192 base-2 exponent
c       - If the excess-8192 exponent is zero, the real value is 0.0.
c
c ----------------------------------------------------------------------
c
c     Input:
c       ival    -  (*)  array of 5 encoded integers to be decoded
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       dval    -  decoded double precision value to be returned
c       ierr    -  error status flag;
c                    0,   No error
c                    1,   Encoded Integers Signed or Larger Than 15-bits
c
c ----------------------------------------------------------------------
c
c     Internals:
c       ie      -  excess-8192 exponent
c       is      -  sign bit
c       i2      -  power-of-two exponent (shifted since mantissa is shifted)
c       tmax    -  maximum of encoded integers
c       tmin    -  minimum of encoded integers
c
c ----------------------------------------------------------------------
c
c     Parameters:
c       PFMODU  -  module name
c       RTWO15  -  1./(2.**15)
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
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFUI2F' )
c
      double precision RTWO15
      parameter ( RTWO15 = 1.0D0/32768.0D0)
c
c     Declare variables:
c
c     Passed:
      integer   ival(5), ierr
      double precision   dval
c
c     Internal
      integer   ie, is, i2, tmin, tmax
c
c ... External Block Data
      external  pffbd
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
c ... make sure all encoded integers are in 15-bit unsigned range
c
      tmin = min ( ival(1), ival(2), ival(3), ival(4), ival(5) )
      tmax = max ( ival(1), ival(2), ival(3), ival(4), ival(5) )
c
      if ( tmin.lt.0 .OR. tmax.ge.32768 ) then
c
        ierr = 1
        call pfserr ( PFMODU, ierr, 0,
     &             'Encoded Integers Signed or Larger Than 15-bits' )
        goto 999
c
      endif
c
c ... extract excess-8192 exponent
c
      ie = ival(5)/2
c
      if ( ie.eq.0 ) then
c
c ..... if its zero, the real # is real
c
        dval = 0.0
c
      else
c
c ..... Get sign multiplier and power-of-two exponent. remove one
c       power-of-2 since mantissa will be constructed between
c       1.0 and 2.0 instead of between 0.5 and 1.0
c
        is = ival(5) - 2*ie
        i2 = ie - 8193
c
c ..... reconstruct double precision value
c
        dval = (1 - 2*is) * 2.0D0**i2 *
     &           ((((ival(4)*RTWO15  + ival(3))*RTWO15 +
     &               ival(2))*RTWO15 + ival(1))*RTWO15 + 1.0D0)
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
