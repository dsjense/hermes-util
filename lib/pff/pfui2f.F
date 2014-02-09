      subroutine pfui2f ( keep, ival, xval, off10, ierr )
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
c       - This routine is a UTILITY routine to decode a floating point
c         number from three 15-bit unsigned integers.  The decoded real
c         value is returned with XVAL and OFF10 and given by:
c                       real value = XVAL * 10.**OFF10
c       - The integer OFF10 is provided to allow machines with with
c         floating point ranges comparable to VAX (0.29e-38 - 1.7e38) to
c         decode numbers with higher (Cray-type) ranges.  Encoded
c         integers contain:
c            ival(1) = 16 most significant bits of mantissa (with most
c                      significant bit hidden/zeroed)
c            ival(2) = 15 next most significant bits of mantissa
c            ival(3) = low bit    --  sign bit
c                      bits 2-15  --  excess-8192 base-2 exponent
c       - If the excess-8192 exponent is zero, the real value is 0.0.
c       - If IVAL(3)/2 < PF2MIN, then
c                If KEEP = .TRUE.,   OFF10 = IVAL(3)/2 and non-zero XVAL
c                                    is returned
c                If KEEP = .FALSE.,  XVAL = 0.0 and OFF10 = 0 are
c                                    returned
c       - Two machine-dependent parameters, PF2MIN and PF2MAX, the
c         minimum and maximum base-2 exponents that are within the range
c         of the machine's default REAL data type, must be defined.
c       - If you are certain that the floating value lies within the range
c         of the machine's REAL data type, then the following statement
c         function can be used in place of this routine.  Note that the
c         minimum value that is returned non-zero is 4 times the machine's
c         minimum non-zero value.
c
c              REAL FVALUE
c              FVALUE(i1,i2,i3) = ( 1 - 2*mod(i3,2) ) *
c             &                   ( (i2*RTWO15 + i1)*RTWO15 + 1.D0 ) *
c             &                   2.**(i3/2 - 8193)
c
c              XVAL = FVALUE( ival(1), ival(2), ival(3) )
c
c ----------------------------------------------------------------------
c
c     Input:
c       keep    -  logical flag indicating whether or not to keep a
c                  non-zero value in the case of underflow
c       ival    -  (*)  array of 3 encoded integers to be decoded
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       xval    -  decoded real value to be returned (along with OFF10)
c       off10   -  power-of-ten multiplier of XVAL to specify the real
c                  number that was decoded
c       ierr    -  error status flag;
c                    0,   No error
c                    1,   Encoded Integers Signed or Larger Than 15-bits
c
c ----------------------------------------------------------------------
c
c     Internals:
c       ie      -  excess-8192 exponent
c       is      -  sign bit
c       i2      -  power-of-two exponent
c       i2m     -  i2 - 1 (shifted since mantissa is shifted)
c       i2rest  -  i2m - i2x
c       i2x     -  i2m, shifted up if necessary to allow full resolution
c                  of machine's smallest non-zero float value
c       pf2max  -  Maximum power-of-2 exponent that is in range for this
c                  machine's default REAL data type
c       pf2min  -  Minimum power-of-2 exponent that is in range for this
c                  machine's default REAL data type
c       pfrmin  -  Minimum floating point number that can be inverted 
c                  without floating point overflow
c       pfrmax  -  Minimum floating point number that can be represented 
c                  without floating point overflow
c       tmax    -  maximum of encoded integers
c       tmin    -  minimum of encoded integers
c       twooff  -  power-of-2 multiplier of mantissa
c       y       -  power-of-2 exponent shifted to account for non-zero
c                  value of OFF10 for out-of-range values
c       xsign   -  sign multiplier
c
c ----------------------------------------------------------------------
c
c     Parameters:
c       PFMODU  -  module name
c       LOG2    -  base-10 logarithm of 2
c       RLOG2   -  inverse of base-10 logarithm of 2
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
      double precision LOG2, RLOG2, RTWO15
      parameter ( LOG2   = 0.30102999566398119521 )
      parameter ( RLOG2  = 1./LOG2 )
      parameter ( RTWO15 = 1.d0/32768.0d0 )
c
c     Declare variables:
c
c     Passed:
      integer   ival(3), off10, ierr
      real      xval
      logical   keep
c
c     Internal
      integer   ie, is, i2, i2m, i2rest, i2x, pf2max, pf2min, tmin, tmax
      real      pfrmax, pfrmin, twooff, y, xsign
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
      tmin = min ( ival(1), ival(2), ival(3) )
      tmax = max ( ival(1), ival(2), ival(3) )
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
c ... get FLOAT LIMITS
c
      call pf_flt_limits(pf2max, pf2min, pfrmin, pfrmax)
c
c ... extract excess-8192 exponent and sign bit from 3rd integer
c
      ie    = ival(3)/2
      is    = ival(3) - 2*ie
c
c ... get power-of-two exponent and sign multiplier
c
      i2    = ie - 8192
      xsign = float(1 - 2*is)
c
c ... remove one power-of-2 since mantissa will be constructed between
c ... 1.0 and 2.0 instead of between 0.5 and 1.0
c
      i2m   = i2 - 1
c
      if ( ie.eq.0 ) then
c
c ..... if its zero, the real # is real
c
        xval  = 0.0
        off10 = 0
c
        goto 999
c
      else if ( i2.gt.pf2max ) then
c
c ..... encoded # is too large for this machine's default REAL data type;
c ..... shift by some power of 10 to leave a # near 1.0
c
        off10  = i2m*LOG2
        y      = dble(i2m) - off10*RLOG2
        twooff = 2.**y
c
      else if (  i2.lt.pf2min ) then
c
c ..... encoded # is too large for this machine's default REAL data type
c
        if (keep) then
c
c ....... user wants to keep precision; shift by some power of 10 to
c ....... leave a # near 1.0
c
          off10  = -i2m*LOG2
          y      = dble(i2m) + off10*RLOG2
          twooff = 2.**y
          off10  = -off10
c
        else
c
c ....... user wants to truncate to zero
c
          xval  = 0.0
          off10 = 0
c
          goto 999
c
        endif
c
      else
c
c ..... encoded # is within this machine's default REAL data type
c
        off10  = 0
c
c ..... shift exponent if it will go below this machine's low value;
c ..... put any excess into sign multiplier
c
        i2x    = max( i2m, pf2min + 2 )
        i2rest = i2m - i2x
        xsign  = xsign*(2.**i2rest)
c
c ..... compute power-of-two multiplier
c
        twooff = 2.**i2x
c
      endif
c
c ... reconstruct floating value; use double precision for mantissa to
c ... preserve low-order bits on 32-bit machines
c
      xval = xsign *
     &       ( ( (ival(2)*RTWO15 + ival(1))*RTWO15 + 1.d0 ) * twooff )
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
