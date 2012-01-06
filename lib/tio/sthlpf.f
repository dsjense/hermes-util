      subroutine sthlpf (
c ... INPUT
     1  inname, nnames,
c ... OUTPUT
     2  fulnam )
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
c ... "Sets" a help file name to be used by calling program.  The full
c     name is expanded out, and the file is checked to see if it exists.
c     If any errors occur, the full help file name is returned as a
c     BLANK string.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments
c
c     INPUT:
c       inname  -  "Raw" help file name
c
c     OUTPUT:
c       fulnam  -  Full expanded help file name
c
c     NOTE: INNAME and FULNAM must be distinct locations.
c
      integer        nnames
      character*(*)  inname(1:nnames), fulnam
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer  lennb, mdpath, tioerh
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS and variables
c
      character*132 errmsg
      integer       ierr, lenfnm, iname
      logical       hfexst
c
c
c=======================================================================
c=======================================================================
c

      do iname=1,nnames
        if (inname(iname) .NE. ' ') then
c
c ....... Get full file name
c
          ierr = mdpath (inname(iname), fulnam)
c
          if (ierr .NE. 0) then
            fulnam = ' '
          else
c
c ......... Check that file exists
c
            inquire (file=fulnam, exist=hfexst)
            if (.NOT. hfexst) fulnam = ' '
          endif
c
        else
          fulnam = ' '
        endif
        if (fulnam .NE. ' ') return
      end do
c
      return
c
c-----------------------------------------------------------------------
c
 9800 format('Error processing file name: ',a)
 9810 format('File does not exist: ',a)
c
      end
