      subroutine savarc (
c ... INPUT
     1  lusv, mode, nusv, usavc, nasv, asavc,
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
c     Rewind and scan the archive file to save to the output file only
c     those commands in the command lists passed from the application.
c
c     The following values for STATUS are returned:
c
c        0  -  Normal return: data saved in output file
c       -1  -  Error opening output file
c       -2  -  Error reading from archive file
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments
c
c     INPUT:
c       lusv    -  Unit # for output data file
c       mode    -  Mode switch for saving archived lines
c                    mode = 0:  save all unexpanded input
c                    mode = 1:  save selected expanded input
c       nusv    -  # user commands to save (-1 to save all)
c       usavc   -  (*) List of user commands to save in output file
c       nasv    -  # auxiliary commands to save (-1 to save all)
c       asavc   -  (*) List of axiliary commands to save in output file
c
      character*(*) usavc(*), asavc(*)
      integer       lusv, mode, nusv, nasv, status
c
c-----------------------------------------------------------------------
c
#include "ioctrl.inc"
#include "iocmdf.inc"
#include "ioinln.inc"
#include "iolun.inc"
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer   lstmch, tioerh
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS and variables
c
      integer    cmdcti, ierr, nchcmd
      integer    sav_arclvl, svlvl
      logical    lopen
      character  chrpar*80, comand*32, errmsg*132
      logical    svcmt, svoio
c
c=======================================================================
c    BEGIN:
c=======================================================================
c
      status = 0
      ierr = 0
c
c ... Check that archive file is open and rewind it
c
      inquire(luarcv, OPENED=lopen)
      if(.NOT.lopen) then
        status = tioerh('TIO', 'SAVARC', 2, 
     &  'Archive file is not open', -2)
        go to 9100
      endif
c
      rewind(luarcv)
c
c ... Set up to read commands from archive file
c
      if (cmflvl .EQ. MXCMFL) then
        write(errmsg,9850) cmflvl
        status = tioerh('TIO', 'SAVARC', 2, errmsg, -2)
        go to 9100
      endif
c
c ... Turn off archiving
c
      sav_arclvl = arcbas
      arcbas = -1
      arclvl = arcbas
c
c ... Save current values and turn off echoing
c
      svlvl = ecolvl
      svoio = echoio
      svcmt = ecocmt
      ecolvl = 1000
      echoio = .FALSE.
      ecocmt = .FALSE.
c
c ... Set command file level, logical units, and command file
c     parameter control
c
      cmflvl          = cmflvl + 1
      hcmfnam(cmflvl) = -1
      lucmd(cmflvl)   = luarcv
      luin            = lucmd(cmflvl)
c
      ncfpar(cmflvl) = 0
c
c-----------------------------------------------------------------------
c    Mode = 0; Read and save all the unexpanded lines in archive file
c-----------------------------------------------------------------------
c
      if(mode .EQ. 0) then
  100   continue
          call gnflin(ierr)
          if(ierr .EQ. -1) then
            go to 400
          elseif(ierr .NE. 0) then
            go to 9000
          endif
          call svlin(lusv, .FALSE., ierr)
c
        go to 100
c
c-----------------------------------------------------------------------
c    Mode = 1; Read and save specified lines from the expanded lines
c              in archive file
c-----------------------------------------------------------------------
c
c
      else if(mode .EQ. 1) then
        call preproc(1, status)
        if(status .ne. 0) go to 9000
c
  200   continue
          call gnflin(ierr)
          if(ierr .EQ. -1) then
            go to 400
          elseif(ierr .NE. 0) then
            go to 9000
          endif
c
c ... Write all comment lines to output file
c
          if(uiltyp(1) .EQ. CMTLIN) then
            call svlin(lusv, .FALSE., ierr)
          else
c
c ... Check for command type
c
            call xpndln (1, ierr)
            if(ierr .NE. 0) go to 9000
            comand = chrpar (RQD, ' ', ' ', cmdchr, ierr, nchcmd)
            cmdcti = index(cmdchr(1:ncmdch),comand(1:1))
c
c ... Command is an auxiliary command
c
            if(cmdcti .GT. 1) then
              comand = chrpar (RQD, ' ', ' ', cmdchr, ierr, nchcmd)
              if(nasv .EQ. -1) then
                call svlin(lusv, .FALSE., ierr)
              else if(lstmch(comand,asavc,nasv) .GT. 0) then
                call svlin(lusv, .FALSE., ierr)
              else
                call svlin(lusv, .TRUE., ierr)
              endif
c
c ... Command is a user command
c
            else
              if(nusv .EQ. -1) then
                call svlin(lusv, .FALSE., ierr)
              else if(lstmch(comand,usavc,nusv) .GT. 0) then
                call svlin(lusv, .FALSE., ierr)
              else
                call svlin(lusv, .TRUE., ierr)
              endif
            endif
            if(ierr .NE. 0) go to 9000
c
          endif
c
        go to 200
      endif
c
  400 continue
c
      ierr = 0
c
      call cmfxit(1)
c
c ... Reset archiving
c
      arcbas = sav_arclvl
      arclvl = arcbas

c ... Restore echoing
c
      ecolvl = svlvl
      echoio = svoio
      ecocmt = svcmt
c
      return
c
c=======================================================================
c     ERROR conditions and messages
c=======================================================================
c
 9000 continue
      call cmfxit(1)
      arcbas = sav_arclvl
      arclvl = arcbas
      ecolvl = svlvl
      echoio = svoio
      ecocmt = svcmt
 9100 continue
      status = tioerh (' ', ' ', 2, 
     1                 '          Error reading archive file', -2)
      return
c
 9850 format('Maximum # of levels of command file (',i2,') open')
c
      end
