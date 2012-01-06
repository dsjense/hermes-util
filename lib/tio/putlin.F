      subroutine putlin (
c ... INPUT
     1 mode, uline,
c ... INPUT/OUTPUT
     2 ierr )
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
c ... Based on the MODE switch, PUTLIN either creates a new input line
c     or appends a new sub-line to the current input line. The new line
c     or sub-line is contained in "uline".
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       mode    -  Mode switch
c                       0  -  Create new line
c                   .NE.0  -  Append new sub-line to current input line
c       uline   -  line or sub-line to add to current input line
c
c     INPUT/OUTPUT:
c       ierr    -  INPUT:  Do nothing if ierr.NE.0
c                  OUTPUT: Error flag:-
c                            0  = normal return
c                            -1 = # continuation lines exceeded
c                            -2 = # characters in buffer exceeded
c
      character*(*) uline
      integer       mode, ierr
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "ioctrl.inc"
#include "ioinln.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer      lennb, tioerh
c
c-----------------------------------------------------------------------
c
c ... Local variables:
c
      character*132 errmsg
      integer   lenrem, lenuln, locul
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      if (ierr .NE. 0) return
c
c ... New line mode
c
      if (mode .EQ. 0) then
        locul = 1
        nuiln = 0
c
      else
c
c ... Append line mode
c
        if(nuiln+1 .GT. MXUILN) then
          write(errmsg,9810) MXUILN-1
          ierr = tioerh ('TIO', 'PUTLIN', 2, errmsg, -1)
          return
        endif
c
c     Add a continuation char. to end of active part of last line
c     (if last line has a trailing comment, this char. is INSERTED)
c
        locul = locuil(nuiln) + lnauil(nuiln)
        if (lnauil(nuiln) .EQ. lenuil(nuiln)) then
          write(uinptl(locul:locul+1),'(2a)') ' ', conchr
          lnauil(nuiln) = lnauil(nuiln) + 2
          lenuil(nuiln) = lenuil(nuiln) + 2
        else
          uinptl(locul+3:) = uinptl(locul:)
          write(uinptl(locul:locul+2),'(3a)') ' ', conchr, ' '
          lnauil(nuiln) = lnauil(nuiln) + 3
          lenuil(nuiln) = lenuil(nuiln) + 3
        endif
        locul = locuil(nuiln) + lenuil(nuiln)
c
      endif
c
c     Add line to current input line
c
      nuiln = nuiln + 1
      lenuln = max (1, lennb(uline))
      lenrem = MXICHL-locul+1
      if(lenuln .GT. lenrem) then
        write(errmsg,9820) lenrem
        ierr = tioerh ('TIO', 'PUTLIN', 2, errmsg, -2)
        lenuln = lenrem
      endif
      locuil(nuiln) = locul
      lnauil(nuiln) = lenuln
      lenuil(nuiln) = lenuln
      write(uinptl(locul:),'(a)') uline(1:lenuln)
c
      return
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9810 format('Exceeded maximum # continuation lines ',i3)
 9820 format('Line overflows buffer (',i3,' chars.)')
c
      end
