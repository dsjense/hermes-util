      subroutine iostec (
c ... INPUT
     1 echosw, ueclvl, uecio, ueccmt, elwid,
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
c ... Set control for echoing of input lines.  ECHOSW can take the
c     values 'on', 'off'.  The user can also control the level of
c     command files above which echoing is enabled, and whether I/O
c     command lines or stand-alone comment lines are echoed.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       echosw  -  Echo switch
c       ueclvl  -  Minimum level at which commands are echoed
c       uecio   -  Logical flag controlling echoing of I/O command lines
c       ueccmt  -  Logical flag controlling echoing of comment lines
c       elwid   -  Echo line width in columns
c
c     OUTPUT:
c       status  -  Status flag:-
c                     0  -  Normal return
c                    -1  -  Illegal value for ECHOSW
c
      character*(*) echosw
      integer       elwid, ueclvl, status
      logical       uecio, ueccmt
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and common block decks:
c
#include "ioctrl.inc"
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
c
      if (strmch(echosw,'on')) then
        ecolvl = ueclvl
        echoio = uecio
        ecocmt = ueccmt
      elseif (strmch(echosw,'off')) then
        ecolvl = 1000
        echoio = .FALSE.
        ecocmt = .FALSE.
      else
        write(errmsg,9800) echosw
        status = tioerh ('TIO', 'IOSTEC', 2, errmsg, -1)
        return
      endif
c
      echowd = elwid
      status = 0
      return
c
c
c=======================================================================
c     ERROR messages
c=======================================================================
c
 9800 format('Illegal value for ECHO SWITCH: ',a)
c
      end
