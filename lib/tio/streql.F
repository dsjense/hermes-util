      logical function streql (
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
c     STREQL sees if STRNG1 is equal to STRNG2, and returns a TRUE 
c     value if it is.  The comparison may be case-sensitive or case-
c     insensitive depending on the value of the case-sensitive mode
c     switch CSMODE, which is 'off' or case-insensitive by default.
c     The comparison is done for the entire length of both strings.
c
c-----------------------------------------------------------------------
c
c ... Passed arguments:
c
      character*(*) strng1, strng2
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "ioctrl.inc"
c
c-----------------------------------------------------------------------
c
c ... Local variables:
c
      character*80 lstr1, lstr2
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      lstr1 = strng1
      lstr2 = strng2
c
      if(.NOT.csmode) then
        call fxcase(lstr1, 'lower')
        call fxcase(lstr2, 'lower')
      endif
c
      if (lstr1 .EQ. lstr2) then
        streql = .TRUE.
      else
        streql = .FALSE.
      endif
c
      return
      end
