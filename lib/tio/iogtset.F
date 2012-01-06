      subroutine iogtset (
c ... INPUT
     1 getitm,
c ... OUTPUT
     2 intval, logval, realval, charval, status )
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
c     IOGTSET returns information about the current settings of TIOLIB,
c     based on requested GET ITEM.  The value is returned in the 
c     return variable of the matching type.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments
c
c     INPUT
c       getitm  -  Requested GET item:-
c                    ECHO_LEVEL      (integer)
c                    ECHO_COMMENT    (logical)
c                    ECHO_IO         (logical)
c                    ECHO_WIDTH      (integer)
c                    COMMENT_CHAR    (character)
c     OUTPUT
c       intval  -  Setting value for integer types
c       logval  -  Setting value for logical types
c       realval -  Setting value for real types
c       charval -  Setting value for character types
c       status  -  Status flag:-
c                     0  -  Normal return
c                    -1  -  Illegal value for GETITM
c
      character*(*)  getitm, charval
      integer intval, status
      logical logval
      real realval
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "ioctrl.inc"
#include "iolun.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      logical strmch
      integer tioerh
c
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      intval = 0
      logval = .FALSE.
      realval = 0.0
      charval = ' '
c
      if (strmch(getitm,'echo_level')) then
c
        intval = ecolvl
c
      elseif (strmch(getitm,'echo_comment')) then
c
        logval = ecocmt
c
      elseif (strmch (getitm,'echo_io')) then
c
        logval = echoio
c
      elseif (strmch (getitm,'echo_width')) then
c
        intval = echowd
c
      elseif (strmch (getitm,'comment_char')) then
c
        charval = cmtchr
c
      else
c
        status = tioerh ('TIO', 'IOGTSET', 2, 'Unknown option', -1)
c
      endif
c
      return
      end
