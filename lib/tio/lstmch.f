      integer function lstmch (
c ... INPUT
     1 string, strlst, lstlen)
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
c     LSTMCH compares CHARACTER variable STRING against the elements
c     of the array STRLST, and returns the following values:-
c
c        >0  -  Index of unique array element matching string
c        -1  -  No match
c        -2  -  Matches more than one element (not unique)
c
c     The comparison of STRING with each element of STRLST is made using
c     the case-insensitive minimum sub-string match routine STRMCH.
c
c-----------------------------------------------------------------------
c
c ... Function arguments:
c
      character*(*) string, strlst(1)
      integer       lstlen
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer   lennb
      logical   strmch
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      integer  i, nmatch
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      nmatch = 0
      do 100 i=1,lstlen
        if (strmch(string,strlst(i))) then
          nmatch = nmatch + 1
          lstmch = i
        endif
  100 continue
c
      if (nmatch .EQ. 0) then
        lstmch = -1
      elseif (nmatch .GT. 1) then
        nmatch = 0
        do 110 i=1,lstlen
          if (strmch(string,strlst(i))) then
            if(lennb(string) .EQ. lennb(strlst(i))) then
              nmatch = nmatch + 1
              lstmch = i
            endif
          endif
  110   continue
        if (nmatch .EQ. 0 .OR. nmatch .GT. 1) lstmch = -2
      endif
c
      return
      end
