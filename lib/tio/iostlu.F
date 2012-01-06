      subroutine iostlu (
c ... INPUT
     1 filtyp, lu,
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
c ... Set TIOLIB logical unit numbers.  FILTYP is a switch selecting
c     which logical unit # to change
c
c       INPUT    -  Input unit
c       ERROR    -  ERROR report unit
c       ECHO     -  Command input echoing
c       ARCHIVE  -  Archive file unit
c
c ... NOTE: you can only change the input unit at command file level
c           zero (i.e. no command file running!!)
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       filtyp  -  Unit type to change
c       lu      -  New value
C
c     OUTPUT:
c       status  -  Status flag:-
c                     0  -  Normal return
c                    -1  -  Unknown file type
c                    -2  -  Cannot change input unit while running
c                           command file!!
c
c
      character*(*) filtyp
      integer       lu, status
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and common block decks:
c
#include "iocmdf.inc"
#include "iolun.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      logical  strmch
      integer  tioerh
      integer  NS_freename
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character errmsg*132
      integer   ierr
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      status = 0
c
      if (strmch(filtyp,'input')) then
        if (cmflvl .EQ. 0) then
          luin      = lu
          luin0     = lu
          lucmd(0)  = lu
ccc       if (hcmfnam(0) .GT. 0) ierr = NS_freename (hcmfnam(0))
          lnumcf(0) = 0
        else
          status = tioerh ('TIO', 'IOSTLU', 2, 
     1       'Cannot change input unit while running command file', -2)
        endif
      elseif (strmch(filtyp,'error')) then
        luerr  = lu
      elseif (strmch(filtyp,'echo')) then
        luecho = lu
      elseif (strmch(filtyp,'archive')) then
        luarcv = lu
      else
        write(errmsg,9800) filtyp
        status = tioerh ('TIO', 'IOSTLU', 2, errmsg, -1)
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
