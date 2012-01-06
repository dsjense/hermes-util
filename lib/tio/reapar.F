      real function reapar (
c ... INPUT
     1 rqsta, pdscrp, dfault, uspchr,
c ... INPUT/OUTPUT
     2 ierr)
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
c ... REAPAR returns a real parameter to the calling routine.
c
c     It uses CHRPAR to get the next character parameter, using the
c     same handling of RQSTA, PDSCRP, and USPCHR.
c
c     If the returned parameter is blank, the default value is returned,
c     otherwise, it tries to convert it to a real.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       rqsta   -  Required/Optional switch for parameter
c       pdscrp  -  Brief description of parameter (for interactive
c                  parameter request)
c       dfault  -  Default value
c       uspchr  -  (*) array of user special characters
c
c     INPUT/OUTPUT:
c       ierr    -  INPUT:   Do nothing if ierr.NE.0
c                  OUTPUT:  ERROR flag:-
c                     0 - normal return
c                    -1 - No parameter returned from CHRPAR
c                    -2 - Parameter is not a real
c
        character*(*) pdscrp, uspchr
        integer       rqsta, ierr
        real          dfault
c
c-----------------------------------------------------------------------
c
c ... Include parameter and common block decks
c
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      character*32 chrpar
      integer      nxttyp, tioerh
      real         chrtor
c
c
c-----------------------------------------------------------------------
c
      character*132 errmsg
      character*32  lparam
      integer       lnchp, type
c
c
c=======================================================================
c ... BEGIN:
c=======================================================================
c
      if (ierr .NE. 0) go to 500
c
c ... If parameter is optional return default if next token is not real
c
      if (rqsta .EQ. OPT)then
        type = nxttyp(' ', ierr)
        if (type .NE. REA .AND. type .NE. INT) go to 500
      endif
c
c ... Get next character parameter
c
      lparam = chrpar (rqsta, pdscrp, ' ', uspchr, ierr, lnchp)
c
      if (ierr .NE. 0) then
        ierr = -1
        go to 500
      endif
c
c ... And convert it to a real if non-blank
c
      if (lparam .EQ. ' ') then
        reapar = dfault
      else
        reapar = chrtor (lparam, ierr)
      endif
c
      if (ierr .EQ. 0) return
c
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
      write(errmsg,9800) lparam(1:lnchp)
      ierr = tioerh ('TIO', 'REAPAR', 2, errmsg, -2)
c 
  500 continue
      reapar = dfault
      return
c
 9800 format('Parameter is not REAL: ',a)
c
      end
