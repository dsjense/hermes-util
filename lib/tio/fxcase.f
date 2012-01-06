      subroutine fxcase (string, case)
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
c     Fixes the case of all alphabetic characters in the string in
c     variable STRING, based on the first character of variable CASE,
c     as follows:-
c
c       CASE(1:1) = 'u' or 'U' - UPPER CASE
c       CASE(1:1) = 'l' or 'L' - LOWER CASE
c       Otherwise, leave string untouched
c
c-----------------------------------------------------------------------
c
c ... Subroutine Arguments:
c
c     INPUT:
c      case   - Requested case
c
c     INPUT/OUTPUT:
c      string - Character string whose case is to be modified
c
      character*(*) string, case
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS (ASCII codes for 'a', 'z', 'A', and 'Z')
c
      integer  ASCILA, ASCILZ, ASCIUA, ASCIUZ
      parameter (ASCILA=97, ASCILZ=122, ASCIUA=65, ASCIUZ=90)
c
c ... Local variables:
c
      logical  uprcas
      integer  i, ichari
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
c ... Identify requested case
c
      if (case(1:1).EQ.'U' .OR. case(1:1).EQ.'u') then
        uprcas = .TRUE.
      elseif (case(1:1).EQ.'L' .OR. case(1:1).EQ.'l') then
        uprcas = .false.
      else
        return
      endif
c
c ... Convert to requested case
c
      if (uprcas) then
        do 100 i=1,len(string)
          ichari = ichar(string(i:i))
          if (ichari.GE.ASCILA .AND. ichari.LE.ASCILZ) string(i:i) =
     1                                                 char(ichari-32)
  100   continue
      else
        do 200 i=1,len(string)
          ichari = ichar(string(i:i))
          if (ichari.GE.ASCIUA .AND. ichari.LE.ASCIUZ) string(i:i) =
     1                                                 char(ichari+32)
  200   continue
      endif
c
      return
      end
