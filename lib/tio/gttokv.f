      integer function gttokv (
c ... INPUT
     1 string, ntok,
c ... OUTPUT
     2 token )
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
c ... GTTOKV returns a whitespace-delimited token from a string.
c
c     Note: Whitespace is defined to be a substring of characters 
c           comprised only of blanks and/or tabs.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       ntok    -  the number of the requested token
c       string  -  the string from which to extract the token
c     OUTPUT:
c       token   -  the token to be returned
c
c     Return value:
c                    >0 - # of characters in the returned token
c                     0 - token not found
c
      integer ntok
      character string*(*), token*(*)
c
c-----------------------------------------------------------------------
c
c ... Include parameter and common block decks
c
c     NONE
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer   lennb
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      integer   last, itok, ic, first
      logical   ws
      character tab*1
c
c
c=======================================================================
c ... BEGIN:
c=======================================================================
c
      first = 0
c
c ... if ntok <= 0, just return a zero-length string
      if ( ntok.le.0 ) goto 20
c
      tab = char(9)
c
      last = lennb(string)
c
      itok = 0
c
      ws = .TRUE.
      ic = 0
c
   10 continue
c
        ic = ic + 1
c
c ..... need to stop if we're past the end of the input string
        if ( ic.gt.last ) goto 20
c
        if ( string(ic:ic) .EQ. ' ' .OR. string(ic:ic) .EQ. tab ) then
c ....... This character is whitespace
c
          if ( .NOT. ws ) then
c ......... last character wasn't whitespace -- we're at end of token
            ws = .TRUE.
c ......... if it's the one we were looking for, goto end to finish up
            if ( itok .EQ. ntok ) goto 20
          endif
c
        else
c ....... This character is not whitespace
c
          if ( ws ) then
c ......... last character was whitespace -- we've found a new token
            ws = .FALSE.
            itok = itok + 1
c ......... if this is token we're looking for, save its start location
            if ( itok .EQ. ntok ) first = ic
          endif
c
        endif
c
      goto 10
c
   20 continue
c
      if ( first .GT. 0 ) then
c ..... found token -- extract from string and return it's length
        token = string(first:ic-1)
        gttokv = ic - first
      else
c ..... didn't find token -- return blank string and zero length
        token = ' '
        gttokv = 0
      endif
c
      return
      end
