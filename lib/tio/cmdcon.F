      subroutine cmdcon (
c ... INPUT
     1  uprmpt,
c ... OUTPUT
     2  status)
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
c ... CMDCON continues a command started by a call to CMDINI.  It
c     appends new lines to the user input line buffer, and then
c     expands these lines into a new expanded line buffer.  NOTE that
c     the previous contents of the expanded line buffer are overwritten,
c     so the calling program is responsible for processing it all first.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       uprmpt -  Input prompt (overrides TIOLIB's default prompt if
c                 non-blank)
c
c     OUTPUT:
c       status  -  Status flag:-
c                     0  -  Normal return
c                    -1  -  EOF encountered on zero level input
c                    -2  -  Unable to return continuation line(s)
c
      character*(*)  uprmpt
      integer        status
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and common block decks:
c
#include "iocmdf.inc"
#include "ioinln.inc"
#include "ioloop.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer tioerh
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      integer ierr, nuiln0
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
c ... TEST FOR ERRORS: cannot call this routine from a command file
c     a FOR loop, or if we do not already have at least one line in
c     the input line buffer
c
      if (cmflvl .GT. 0) then
        status = tioerh ('TIO', 'CMDCON', 2,
     1                   'Currently running from command file', -2)
        return
      endif
c
      if (looplv(cmflvl) .GT. 0) then
        status = tioerh ('TIO', 'CMDCON', 2,
     1                   'Currently running FOR loop', -2)
        return
      endif
c
      if (nuiln .EQ. 0) then
        status = tioerh ('TIO', 'CMDCON', 2,
     1                   'No input to append to', -2)
        return
      endif
c
      status = 0
c
c ... Save current # lines, and append new sub-lines
c
      nuiln0 = nuiln
      call getlin (1, uprmpt, status)
c
      if (status .LT. 0) then
        if (status .LT. -1) status = -2
        return
      endif
c
c ... Expand out new input sub-lines
c
      call xpndln (nuiln0+1, status)
c
      if (status .LT. 0) then
        call cmdxit (.FALSE., status, ierr)
        status = -2
      endif
c
      return
      end
