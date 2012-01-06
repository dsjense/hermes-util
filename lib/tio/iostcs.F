      subroutine iostcs (
c ... INPUT
     1 csmodsw,
c ... OUTPUT
     2 status )
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
c ... Set switch controlling case-sensitivity in checking for equality 
c     of strings.  CSMODSW can take the values 'on' or 'off'.  Any 
c     other value results in an error.  By default, case-sensitivity
c     is 'off'.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       csmodsw  -  switch controlling case-sensitivity in 
c                   checking for equality of strings
c
c     OUTPUT:
c       status  -  Status flag:-
c                     0  -  Normal return
c                    -1  -  Illegal value for CSMODSW
c
      character*(*) csmodsw
      integer       status
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and common block decks:
c
#include "ioctrl.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      logical  strmch
      integer  tioerh
c-----------------------------------------------------------------------
c
      character errmsg*132
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
c
      status = 0
c
      if (strmch(csmodsw,'on')) then
        csmode = .TRUE.
      elseif (strmch(csmodsw,'off')) then
        csmode = .FALSE.
      else
        write(errmsg,9800) csmodsw
        status = tioerh ('TIO', 'IOSTCS', 2, errmsg, -1)
      endif
c
      return
c
c
c=======================================================================
c     ERROR messages
c=======================================================================
c
 9800 format('Illegal <CR> MODE value: ',a)
c
      end
