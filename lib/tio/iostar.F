      subroutine iostar (
c ... INPUT
     1 arcvsw, uarclv,
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
c ... Controls for archiving interactive input lines.  The types
c     of call possible are:
c
c       ARCVSW  UARCLV    Action
c       ------  ------    ------
c        off      -       Turn off archiving
c        on      lvl      Archive commands up to command file level LVL
c        all      -       Archive all command file levels
c        uon     lvl      Archive commands unconditionally
c        uoff     -       Turn archiving off unconditionally
c        usr      -       Return archiving control to user
c
c ... If turning on archiving, and the archive file has not been
c     opened, and the user supplies an archive file name, open the
c     archive file.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       arcvsw  -  Archive control switch
c       uarclv  -  Maximum command file level to archive
c
c     OUTPUT:
c       status  -  Status flag:-
c                     0  -  Normal return
c                    -1  -  Illegal value for ARCVSW
c                    -2  -  Unable to get LU for archive file
c                    -3  -  Error opening archive file
c
      character*(*) arcvsw
      integer       uarclv, status
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
      integer  gtfrlu, lennb, mdotxf, tioerh
      logical  strmch
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character     errmsg*132
      integer       lnarcf, lutmp
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      if ( arcfnm.eq.' ') then
        arcbas = -1
        arclvl = -1
        write(errmsg,9820) arcbas
        status = tioerh ('TIO', 'IOSTAR', 1, errmsg, 0 )
        return
      endif
c
      if (strmch(arcvsw,'on')) then
        if(arcunc) then
          write(errmsg,9820) arcbas
          status = tioerh ('TIO', 'IOSTAR', 1, errmsg, 0 )
        else
          arcbas = uarclv
        endif
      elseif (strmch(arcvsw,'all')) then
        if(arcunc) then
          write(errmsg,9820) arcbas
          status = tioerh ('TIO', 'IOSTAR', 1, errmsg, 0 )
        else
          arcbas = 1000
        endif
      elseif (strmch(arcvsw,'off')) then
        if(arcunc) then
          write(errmsg,9820) arcbas
          status = tioerh ('TIO', 'IOSTAR', 1, errmsg, 0 )
        else
          arcbas = -1
        endif
      elseif (strmch(arcvsw,'uon')) then
        arcbas = uarclv
        arcunc = .TRUE.
      elseif (strmch(arcvsw,'uoff')) then
        arcbas = -1
        arcunc = .TRUE.
      elseif (strmch(arcvsw,'usr')) then
        arcunc = .FALSE.
      else
        write(errmsg,9800) arcvsw
        status = tioerh ('TIO', 'IOSTAR', 2, errmsg, -1 )
        return
      endif
c
      status = 0
c
c ... If we have just turned on archiving, open archive file if it
c     has not yet been opened
c
      if (arcbas.GE.0 .AND. luarcv.EQ.-1) then
c
        lutmp = gtfrlu (20, 90, 0)
c
        if (lutmp .GT. 0) then
          status = mdotxf (.FALSE., lutmp, arcfnm,
     1                     'new', 'list', ' ', luerr)
          if (status .LT. 0) status = -3
        else
          lnarcf = lennb(arcfnm)
          write(errmsg,9810) arcfnm(1:lnarcf)
          status = tioerh ('TIO', 'IOSTAR', 2, errmsg, -1 )
          status = -2
        endif
c
        if (status .EQ. 0) then
          luarcv = lutmp
          arcvln = .TRUE.
        else
          arcbas = -1
        endif
      endif
c
      arclvl = arcbas
c
      return
c
c
c=======================================================================
c     ERROR messages
c=======================================================================
c
 9800 format('Illegal value for ARCHIVE SWITCH: ',a)
 9810 format('Cannot get LU for archive file: ',a)
 9820 format('Archiving level set unconditionally to: ',i5)
c
      end
