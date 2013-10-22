      subroutine pfuf2i ( xval, off10, ival, ierr )
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
c       - This routine is a UTILITY routine to encode a floating point
c         number into three 15-bit unsigned integers.  The real value to
c         be encoded is given by:
c                       real value = xval * 10.**off10
c       - The integer OFF10 is provided to allow machines with with
c         floating point ranges comparable to VAX (0.29e-38 - 1.7e38) to
c         encode numbers with higher (Cray-type) ranges.  Encoded
c         integers contain:
c            ival(1) = 16 most significant bits of mantissa (with most
c                      significant bit hidden/zeroed)
c            ival(2) = 15 next most significant bits of mantissa
c            ival(3) = low bit    --  sign bit
c                      bits 2-15  --  excess-8192 base-2 exponent
c       - If the excess-8192 exponent is zero, the real value is 0.0.
c
c     Input:
c       xval    -  real value to be encoded (along with OFF10)
c       off10   -  power-of-ten multiplier of XVAL to specify the real
c                  number to be encoded
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       ival    -  (*)  array of 3 encoded integers
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   Float Out of Range
c
c ----------------------------------------------------------------------
c
c     Internals:
c       is      -  sign bit
c       n       -  Base 2 exponent of real number being encoded
c       y       -  log-base-2 of XVAL*(10**OFF10)
c       r       -  fractional part of y  (y-n)
c       frac    -  normalized mantissa w/ hidden bit removed
c       f15     -  2**15 * frac  (truncate to give 15 most significant
c                                 bits after hidden bit)
c       xd      -  absolute value of XVAL
c
c ----------------------------------------------------------------------
c
c     Parameters:
c       PFMODU  -  module name
c       LN2     -  natural logarithm of 2
c       LOG2    -  base-10 logarithm of 2
c       RLN2    -  inverse of natural logarithm of 2
c       RLOG2   -  inverse of base-10 logarithm of 2
c       TWO15   -  2.**15
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
      parameter ( PFMODU = 'PFUF2I' )
c
      integer   EXCESS
      parameter ( EXCESS = 8192 )
      double precision LN2, LOG2, RLN2, RLOG2, TWO15
      parameter ( LN2   = 0.6931471805599453094d0 )
      parameter ( LOG2  = 0.30102999566398119521d0 )
      parameter ( RLN2  = 1.d0/LN2 )
      parameter ( RLOG2 = 1.d0/LOG2 )
      parameter ( TWO15 = 32768.0D0 )
c
c     Declare variables:
c
c     Passed:
      integer   off10, ival(3), ierr
      real      xval
c
c     Internal
      integer   is, n
      double precision y, r, frac, f15, xd
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
c
      if ( xval.eq.0. ) then
c
c ..... value is zero, send a zero
c
        ival(1) = 0
        ival(2) = 0
        ival(3) = 0
c
      else
c
c       sign bit -- 0 for positive and 1 for negative
c
        is = ( 1 - ifix(sign(1.,xval)) )/2
c
c ..... find base-2 exponent  (including supplied power-of-10 offset)
c
        xd = dble(abs(xval))
        y  = RLN2*log(xd) + off10*RLOG2
        n  = y
c
c ..... check to see if power-of-2 exponent is out of range
c
        if ( n.le.-EXCESS ) then
c
c ....... out-of-range low; set to zero
c
          ival(1) = 0
          ival(2) = 0
          ival(3) = 0
c
        elseif ( n.ge.EXCESS ) then
c
c ....... out-of-range high; set error flag
c
          ierr = 1
          call pfserr ( PFMODU, ierr, 0, 'Float Out of Range' )
c
        else
c
c ....... remainder
c
          r = y - n
c
c ....... want r between 0 and -1 to get proper value for n so that
c ....... radix is in right place [to left of most significant (hidden)
c ....... bit]; however, we'll want to add 1 to r later so we can easily
c ....... truncate most significant bit of mantissa for hidden bit
c ....... notation.  Consequently, if we increment n, don't decrement r,
c ....... or, if we don't increment n, increment r by 1
c
          if(r.ge.0.)then
            n = n + 1
          else
            r = r + 1.
          endif
c
c ....... convert log back to mantissa and truncate most significant bit
c
          frac = max( exp(LN2*r) - 1.d0, 0.d0 )
c
c ....... load ival(1) with most significant 15 bits of truncated mantissa
c
          f15     = TWO15*frac
          ival(1) = f15
c
c ....... load ival(2) with next most significant 15 bits of mantissa
c
          ival(2) = TWO15*(f15 - ival(1))
c
c ....... pack sign in bit 1 (low), excess-8192 exponent next 14 bits
c
          ival(3) = 2*(n + EXCESS) + is
c
        endif
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
