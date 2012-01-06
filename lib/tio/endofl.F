      logical function endofl (
c ... INPUT
     1 status)
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
c     END-OF-LINE logical function.  .TRUE. if at end of line, .FALSE.
c     otherwise
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments
c
c     INPUT:
c       status  -  If status.NE.0 ENDOFL unconditionally
c
      integer status
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "ioinln.inc"
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      if (status.NE.0 .OR. cuichr.GE.lenxln) then
        endofl = .TRUE.
      else
        endofl = .FALSE.
      endif
c
      return
      end
