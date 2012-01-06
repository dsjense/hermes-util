      subroutine ljustf (
c ... INPUT
     & ftype, dval,
c ... OUTPUT
     & string, nch)
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
c     LJUSTF writes either a DOUBLE-PRECISION variable into a 
c     CHARACTER variable STRING, left justified, using the TIO formats:
c       REA:  1pe12.5    (6  decimal place precision)
c       DUBL: 1pe19.12   (13 decimal place precision)
c     It also returns the non-blank length of the string.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       ftype - Numeric type: either REA, or DUBL
c       dval  - Double precision value
c
c     OUTPUT:
c       string - Character string returning value
c       nch    - Non-blank length of string
c
      integer          ftype, nch
      character*(*)    string
      double precision dval
c
c-----------------------------------------------------------------------
c
c ... Include files
c
#include "iopar.inc"
c
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character*20 tstring
      integer      fstnbc
c
c
c=======================================================================
c *** BEGIN:
c=======================================================================
c
c ... REAL
c
      if (ftype.EQ.INT .OR. ftype.EQ.REA) then
        write(tstring,'(1pe12.5)') dval
c
        if (tstring(1:1) .NE. ' ') then
          fstnbc = 1
        elseif (tstring(2:2) .NE. ' ') then
          fstnbc = 2
        else
          fstnbc = 3
        endif
c
        string = tstring(fstnbc:12)
        nch    = 13 - fstnbc
c
c ... DOUBLE 
c
      elseif (ftype .EQ. DUBL) then
        write(tstring,'(1pd19.12)') dval
c
        if (tstring(1:1) .NE. ' ') then
          fstnbc = 1
        elseif (tstring(2:2) .NE. ' ') then
          fstnbc = 2
        else
          fstnbc = 3
        endif
c
        string = tstring(fstnbc:19)
        nch    = 20 - fstnbc
      endif
c
      return
      end
