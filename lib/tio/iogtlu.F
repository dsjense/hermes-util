      integer function iogtlu (
c ... INPUT
     1 filtyp )
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
c ... Return TIOLIB logical unit number corresponding to FILTYP:-
c
c       INPUT    -  Input unit
c       ERROR    -  ERROR report unit
c       ECHO     -  Command input echoing
c       ARCHIVE  -  Archive file unit
c
c    It file type is unkown, it returns the value -99
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       filtyp  -  Unit type to change
c
      character*(*) filtyp
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and common block decks:
c
#include "iolun.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      logical  strmch
      integer  tioerh
c
c-----------------------------------------------------------------------
c
      character errmsg*132
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      if (strmch(filtyp,'input')) then
        iogtlu = luin
      elseif (strmch(filtyp,'error')) then
        iogtlu = luerr
      elseif (strmch(filtyp,'echo')) then
        iogtlu = luecho
      elseif (strmch(filtyp,'archive')) then
        iogtlu = luarcv
      else
        write(errmsg,9800) filtyp
        iogtlu =  tioerh ('TIO', 'IOGTLU', 2, errmsg, -99)
      endif
c
      return
c
c
c=======================================================================
c     ERROR messages
c=======================================================================
c
 9800 format('Unknown FILE type: ',a)
c
      end
