      logical function strmch (
c ... INPUT
     1 strng1, strng2)
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
c     STRMCH sees if STRNG1 matches STRNG2, and returns a TRUE value
c     if it does.  The comparison is a case insensitive match of
c     the non-blank length of STRNG1 versus the same number of
c     characters in STRNG2.
c
c     Note that the value returned by STRMCH depends on the order of
c     the arguments.  If the non-blank length of STRNG1 is greater than
c     STRNG2, the result is unconditionally FALSE, while in the reverse
c     case STRMCH compares STRNG1 against the same # of characters in
c     STRNG2.
c
c-----------------------------------------------------------------------
c
c ... Passed arguments:
c
      character*(*) strng1, strng2
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer      lennb
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character*80 lstr1, lstr2
      integer      len, len1, len2
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      len1 = lennb(strng1)
      len2 = lennb(strng2)
c
      if (len1 .GT. len2) then
        strmch = .FALSE.
      else
        len = max (1, min (len1, len2))
c
        lstr1 = strng1(1:len)
        call fxcase (lstr1(1:len), 'lower')
        lstr2 = strng2(1:len)
        call fxcase (lstr2(1:len), 'lower')
c
        if (lstr1(1:len) .EQ. lstr2(1:len) ) then
          strmch = .TRUE.
        else
          strmch = .FALSE.
        endif
      endif
c
      return
      end
