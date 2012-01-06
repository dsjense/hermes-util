      real function chrtor (c,ierr)
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
c     CHRTOR converts a character string to a REAL number.
c     It does nothing and returns the value 0.0 if the error flag
c     is non-zero.  If a syntax error is detected in the input string,
c     it returns the value 0.0, and error status IERR = -1.
c
c     The following syntax features in the string are supported:
c       1. Leading blanks
c       2. Multiple leading signs
c
c-----------------------------------------------------------------------
c
c ... Function arguments
c
c     INPUT:
c       c       -  character string to be decoded
c
c     INPUT/OUTPUT:
c       ierr    -  INPUT:  Do nothing if ierr.NE.0
c                  OUTPUT: Error flag (0=normal return, -1=error)
c
      character c*(*)
      integer   ierr
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS
c
      integer INT, FRAC, EXP
      parameter (INT=1, FRAC=2, EXP=3)
c
      real      ZERO, ONE, TEN
      parameter (ZERO=0.0, ONE=1.0, TEN=10.0)
c
c ... Local variables
c
      integer  curint, digit, expi, expsgn, i, ic, lc
      real     sign, tenfr
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      chrtor = ZERO
c
      if (ierr.NE.0 .OR. c.EQ.' ') return
c
c ... Find location of first non-blank token in character string
c
      do 1 i=1,len(c)
        if (c(i:i) .NE. ' ') then
          ic = i
          go to 2
        endif
    1 continue
c
    2 continue
      lc = ic + index(c(ic:),' ') - 2
      if (lc .LT. ic) lc = len(c)
c
c-----------------------------------------------------------------------
c     Process string
c-----------------------------------------------------------------------
c
      curint = INT
      expi   = 0
      expsgn = 1
c
c ... Handle leading sign(s)
c
      sign = ONE
   10 continue
      if (c(ic:ic).eq.'-') then
        sign = -sign
        ic   = ic + 1
        go to 10
      elseif (c(ic:ic).eq.'+') then
        ic   = ic + 1
        go to 10
      endif
c
c ... Process next character
c
  100 continue
      digit = ichar(c(ic:ic)) - 48
c
c ... Digit
c
      if (digit.ge.0 .and. digit.le.9) then
c
        if (curint.eq.INT) then
          chrtor = chrtor*TEN + digit
        elseif (curint.eq.FRAC) then
          chrtor = chrtor + digit*tenfr
          tenfr  = tenfr/TEN
        elseif (curint.eq.EXP) then
          expi   = expi*10 + digit
        endif
c
c ... Decimal point
c
      elseif (c(ic:ic).eq.'.') then
c
        if(curint.eq.INT) then
          curint = FRAC
          tenfr  = ONE/TEN
        else
          chrtor = ZERO
          ierr   = -1
          return
        endif
c
c ... Exponent character
c
      elseif ( c(ic:ic).eq.'e' .or. c(ic:ic).eq.'E' .or.
     1         c(ic:ic).eq.'d' .or. c(ic:ic).eq.'D' ) then
c
        curint = EXP
c
c ..... Test for exponent sign
c
        if (c(ic+1:ic+1).eq.'-') then
          expsgn = -1
          ic     = ic + 1
        elseif (c(ic+1:ic+1).eq.'+') then
          expsgn = 1
          ic     = ic + 1
        else
          expsgn = 1
        endif
c
        if (ic .GE. lc) then
          chrtor = ZERO
          ierr   = -1
          return
        endif
c
c ... Anything else is an error!!
c
      else
c
        chrtor = ZERO
        ierr   = -1
        return
c
      endif
c
      ic = ic + 1
      if (ic .LE. lc) go to 100
c
c-----------------------------------------------------------------------
c     Build number from its pieces
c-----------------------------------------------------------------------
c
      expi   = expsgn * expi
      chrtor = sign * chrtor * TEN**expi
c
      return
      end
