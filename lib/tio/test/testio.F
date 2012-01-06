      program testio
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
C_Groups main
c***********************************************************************
c
c ... Sample TIOLIB-based application.  This program illustrates the
c     basic setup for an interactive program using TIOLIB for input.
c     It has several dummy commands allowing the user to print the
c     parsed input returned to the application.
c
c-----------------------------------------------------------------------
c
c ... Include parameter and common block decks
c
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      character*80 chrpar
      integer      lennb, tioini
      logical      strmch
c
c-----------------------------------------------------------------------
c
c ... User command definition:
c
      integer        NCMD  , NXCMD
      parameter     (NCMD=3)
      parameter     (NXCMD=3)
      character*32   cmdnam(1:NCMD), xitcmd(1:NXCMD)
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character*32  comand, ecoval
      integer       ierr, lencmd, nch, status, stdin, stdout, stderr
      logical       echo
c
c-----------------------------------------------------------------------
c
      data  cmdnam / 'cmd1', 'cmd2', 'echo' /
      data  xitcmd / 'quit', 'exit', 'end' /
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      echo = .TRUE.
c
#include "stdio.inc"
c
c ... For this application, explicitly redirect STDERR to STDOUT, so 
c     that error messages and standard output are correctly interlaced
c     if outputting to a file
c
      stderr = stdout
c
c ... Initialize TIOLIB
c
      status = tioini (',=', '#', 'graphics', NXCMD, xitcmd,
     1                 stdin, stdout, stdin, ' ', stdout, stderr,
     2                 'testio.arc', ' ', ' ', 'TESTIO>')
      if (status .LT. 0) goto 200
c
c ... Process next command (NOTE: this loop is standard for typical
c     applications)
c
  100 continue
c
c ..... Get next TESTIO command
c
        status = 0
        call cmdini (NCMD, cmdnam, comand, status)
c
        if (echo) then
          lencmd = lennb (comand)
          write(stdout,110) comand(1:lencmd), status
  110     format(/' Command: ',a/' Status = ',i3/)
        endif
c
c ..... Handle non-zero CMDINI status flag:
c         1. Stop on TIOLIB exit command, EOF or FATAL error
c         2. Go back for next command if:
c             a. Internal TIO error
c             b. Unknown/ambiguous application command
c
        if (status.EQ.IOEOF .OR. status.EQ.IOEXIT .OR.
     &      status.EQ.IOFTLERR) then
          goto 200
        elseif (status.EQ.IOINTERR .OR. status.EQ.IOBADCMD) then
          go to 100
        endif
c
c ..... Handle each supported command (NOTE: at this point, STATUS = 0.
c       We pass this flag through each TIOLIB input command.  At the
c       end of the command processing stage, this flag can be tested
c       to see if an error occurred in TIOLIB
c
        if (strmch(comand,'cmd1')) then
c
          if (echo) call printp (stdout, status)
c
        elseif (strmch(comand, 'cmd2')) then
c
          if (echo) call printp (stdout, status)
c
        elseif (strmch(comand, 'echo')) then
c
          ecoval = chrpar (OPT, ' ', ' ', ' ', status, nch)
c
          if (ecoval .EQ. ' ') then
            echo = .NOT. echo
          elseif (strmch(ecoval,'on')) then
            echo = .TRUE.
          elseif (strmch(ecoval,'off')) then
            echo = .FALSE.
          endif
c
        endif
c
c ..... Finish up command processing loop.  CMDXIT performs the
c       following functions:-
c
c          1.  Write command to archive file if enabled
c
c          2.  Handle TIOLIB processing errors (STATUS .ne. 0).  If
c              TESTIO is running a command file, CMDXIT can stop
c              execution of this file if there are errors
c
        call cmdxit (.TRUE., status, ierr)
      go to 100
c
  200 continue
c
      end
