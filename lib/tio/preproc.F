      subroutine preproc (
c ... INPUT
     1 mode, 
c ... OUTPUT
     2 status)
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
c ... Preprocess a command file by processing all TIO commands and just
c     echoing other lines to a temporary file.  This allows the 
c     embedding of TIO commands in application commands.
c
c        1.  Open temporary file
c        2.  Read line
c        3.  Process TIO command / echo application command
c        4.  Close original command file (if mode = 0)
c        5.  Rewind temporary file
c        6.  Set command file unit number to that of temporary file
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT
c       mode    -  mode for this call
c                  mode = 0  - close command file after reading
c                  mode = 1  - don't close command file after reading
c 
c     OUTPUT
c       status  -  Status Flag:
c                    0  -  Normal return (temporary file ready to read)
c                   -4  -  Unable to get free LU for temporary file
c                   -5  -  Error opening file
c                   -6  -  TIOLIB input errors
c
      integer mode, status
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "iocmdf.inc"
#include "ioctrl.inc"
#include "ioif.inc"
#include "ioloop.inc"
#include "iolun.inc"
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer    gtfrlu, tioerh
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character  comand*32, errmsg*132, sav_case*16
      integer    arcerr, i, ierr, lutmp, ntioer, sav_ecolvl
      integer    sav_luecho
      logical    sav_ecocmt, sav_echoio
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      status = 0
c
c-----------------------------------------------------------------------
c ... Open file
c-----------------------------------------------------------------------
c
c ... Get logical unit # for temporary file
c
      lutmp = gtfrlu (20, 90, 0)
c
      if (lutmp .LT. 0) then
        write(errmsg,9830) 'temporary file'
        status = tioerh ('TIO', 'PREPROC', 2, errmsg, -4)
        return
      endif
c
c ... And open it
c
      open(unit=lutmp, status='scratch', err=9000)
c
c-----------------------------------------------------------------------
c ... File successfully opened
c-----------------------------------------------------------------------
c
      status = 0
c
c ... Save current values and set up TIOLIB to send echo output to 
c ... temporary file
c
cc      call iostfe ('CONTINUE', ierr)
      sav_ecolvl = ecolvl
      sav_echoio = echoio
      sav_ecocmt = ecocmt
      sav_luecho = luecho
      ecolvl = 0
      echoio = .FALSE.
      ecocmt = .TRUE.
      luecho = lutmp
      sav_case = case
ccc      call iostca ('NONE', status)
c
c ... Set mode switch to indicate that we are currently preprocessing
c
      ppmode = .TRUE.
c
c-----------------------------------------------------------------------
c ... Process lines
c-----------------------------------------------------------------------
c
      ntioer = 0
      ierr = 0
c
  100 continue
        call cmdini (0, ' ', comand, ierr)
c
cc      print *, 'comand, ierr: ',comand, ierr
        if (ierr .EQ. IOINTERR) then
          ntioer = ntioer + 1
        elseif (ierr.EQ.IOEOF .OR. ierr.EQ.IOEXIT)  then
          go to 200
        elseif (ierr .NE. 0) then
          go to 200
        endif
c
        call cmdxit(.TRUE., ierr, arcerr)
        go to 100
c
c-----------------------------------------------------------------------
c ... Finish Up
c-----------------------------------------------------------------------
c
  200 continue
c
      if (ntioer .GT. 0) then
        write(errmsg,9840) ntioer
        status = tioerh ('TIO', 'PREPROC', 2, errmsg, -6)
      endif
c
c ... Delete symbols defined at this level
c
      call iosymdel ('*', LOCALSYM, 0, ierr)
c
c ... Gracefully shut down any loops running at this level
c
      if (looplv(cmflvl) .GT. 0) then
        nlpln  = beglpl(1,cmflvl) - 1
        lstlch = lplche(nlpln)
        looplv(cmflvl) = 0
      endif
c
c ... And shut down any conditional statement processing
c
      iflvl(cmflvl) = 0
c
c ... Reset echoing and case conversion parameters and mode switch
c
      ecolvl = sav_ecolvl
      echoio = sav_echoio
      ecocmt = sav_ecocmt
      luecho = sav_luecho
ccc      call iostca (sav_case, status)
      ppmode = .FALSE.
c
c ... Close command file if requested
c
      if(mode .eq. 0) close (unit=luin)
c
c ... Reset command file and logical units to read from temporary file
c
      rewind (unit=lutmp)
      lucmd(cmflvl)  = lutmp
      luin           = lucmd(cmflvl)
c
c ... Turn archiving off for expanded input
c ... Reset level to base level when end of command file is reached
c
      arclvl = -1
c
      return
c
 9000 continue
      write(errmsg,9820)
      status = tioerh('TIO', 'PREPROC', 2, errmsg, -5)
      return
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9820 format('Error opening temporary file')
 9830 format('Unable to get free LU for ',a)
 9840 format(i5,' errors detected in input file')
c
      end
