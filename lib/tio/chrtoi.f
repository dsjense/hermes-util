      integer function chrtoi (c,ierr)
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
c     CHRTOR converts a character string to an INTEGER number.
c     It does nothing and returns the value 0 if the error flag
c     is non-zero.  If a syntax error is detected in the input string,
c     it returns the value 0, and error status IERR = -1.
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
c ... Local variables
c
      integer  digit, i, ic, lc, sign
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      chrtoi = 0
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
c ... Handle leading sign(s)
c
      sign = 1
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
      if (digit.lt.0 .or. digit.gt.9) then
        chrtoi = 0
        ierr   = -1
        return
      endif
c
      chrtoi = chrtoi*10 + digit
      ic   = ic + 1
      if (ic .LE. lc) go to 100
c
      chrtoi = sign*chrtoi
      return
      end
