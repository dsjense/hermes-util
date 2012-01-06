      subroutine cmdini (
c ... INPUT
     1  ncmd, cmdnam,
c ... OUTPUT
     2  comand, status)
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
c ... CMDINI is the master input routine for a calling program using
c     TIOLIB, initializing a calling program's next command.  It
c     performs the following functions:-
c
c       1.  Get the next input line (which may be several lines
c           connected by '&') from the current source (FOR loop,
c           command file, or terminal)
c
c       2.  Expand the input line(s) into a single input character
c           buffer, performing any variable substitution.
c
c       3.  Get the first token on the line, and handle it as follows:-
c
c            3a.  If it is the I/O command character, pass control
c                 to IOCMND to process command line
c
c            3b.  If it is a user auxiliary command character, pass
c                 control to AUXCMD to process command line
c
c            3c.  Check that it is a user command: If it is an
c                 application or exit command, return.  If it is
c                 a 'HELP' command, open file if the user supplied
c                 a help file.  Otherwise, simply list known user
c                 commands (unless NCMD=0)
c
c ... Note that when operating interactively (base input unit LUIN
c     is terminal unit LUTTYI), CMDINI only returns to the calling
c     program with a valid command to be processed (or an EXIT
c     condition).  However, for non-interactive processing LUIN .ne.
c     LUTTYI, CMDINI returns if ANY errors are detected, to inform
c     the application (error conditions IOINTERR and IOBADCMD).
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       ncmd   -  # commands recognized by calling program
c       cmdnam -  (*) Names of commands recognized by calling program
c
c     OUTPUT:
c       comand  -  Command name
c       status  -  Status flag:-
c                    0        - Normal return
c                    IOEOF    - EOF encountered on zero level input
c                    IOEXIT   - Terminate TIO processing request (exit
c                               program, or all input read in)
c                    IOINTERR - Error in internal TIO processing of input,
c                               or in auxiliary command sets (only returned
c                               non-interactive mode)
c                    IOBADCMD - Unknown/ambiguous command (only returned in
c                               non-interactive mode)
c                    IOFTLERR - ERROR reading from unit LUIN, and no
c                               terminal device (must be FATAL ERROR!!)
c
      integer        ncmd, status
      character*(*)  cmdnam(1:*)
      character*(*)  comand
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and common block decks:
c
#include "ioctrl.inc"
#include "iocmdf.inc"
#include "iolun.inc"
#include "iopar.inc"
#include "ioinln.inc"
#ifdef USE_MPI
# include "mpi_tio.inc"
# include "mpif.h"
#endif
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      character*132  chrpar
      integer        fsystm, lennb, lstmch, tioerh
      logical        endofl, strmch
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character*132 errmsg, hlplin
      integer       cmdcti, cmdndx, i, ierr, nchcmd, xitndx
      logical       hlpcmd
#ifdef USE_MPI
      integer  ibuf(2)
#endif
c
c
c=======================================================================
c     BEGIN: Get next processed input line
c=======================================================================
c
#ifdef USE_MPI
      if ( node.eq.0 ) then
c
#endif
    1   continue
        status = 0
c
c ..... Get next input line
c
        call getlin (0, ' ', status)
c
c ..... EOF on zero level input: Either return as EOF or EXIT
c
        if (status .EQ. IOEOF) then
          comand = ' '
          lenxln = 0
          if (eofxit) status = IOEXIT
          go to 900
c
c ..... Error on read, try to switch to terminal input to salvage
c       application.
c
        elseif (status .EQ. -2) then
          if (luttyi.GE.0) then
            luin = luttyi
            status = tioerh ('TIO', 'CMDINI', 2,
     1               'Input error: switching to terminal input', 0)
            go to 1
          else
            status = tioerh ('TIO', 'CMDINI', 2,
     1               'Input error: aborting ... ', IOFTLERR)
            go to 900
          endif
        endif
c
c ..... Expand out the input line
c
        call xpndln (1, status)
c
        if (status .LT. 0) then
          status = IOINTERR
          go to 900
        endif
c
c-----------------------------------------------------------------------
c ..... Process Commands
c-----------------------------------------------------------------------
c
c ..... Process command, based upon command type:-
c
c        I/O command     -  Transfer control to IOCMND
c        Aux. Command    -  Transfer control to AUXCMD
c        User command    -  Handle HELP here, otherwise pass legal
c                           command back to calling program
c
c       Note that the first two options unconditionally return for the
c       next command without returning to the calling program, unless
c       they encounter errors and this is a non-interactive application.
c
c ..... Get command from first parameter in input line
c
        comand = chrpar (RQD, ' ', ' ', cmdchr, status, nchcmd)
c
c ..... See if this is an I/O or AUXILIARY command
c
        cmdcti = index (cmdchr(1:ncmdch),comand(1:1))
c
c ..... We are done with the current line if it is a comment, or a
c       TIO-comment line. Go back for next input line immediately,
c       BEFORE incrementing ncmdf(*)
c
        if (comand(1:1) .EQ. CMTCHR) then
          if (arcvln) call ioarcl (2, uinptc(1:lenxln), ierr)
          go to 1
        elseif (cmdcti.EQ.1 .AND. endofl(status) ) then
          go to 1
        endif
c
c ..... Increment counter for # commands processed in current file
c
        ncmdf(cmflvl) = ncmdf(cmflvl) + 1
c
c ..... Process I/O command
c
        if (cmdcti .EQ. 1) then
c
          call iocmnd (status)
          if (status .EQ. 0) go to 1
c
c ..... Process user auxiliary command
c
        elseif (cmdcti .GT. 1) then
c
          call auxcmd (cmdcti-1, status)
          if (status .EQ. 0) go to 1
c
c ..... USER command:
c         1. Application commands (CMDNAM list)
c         2. Exit commands        (XITCMD list)
c         3. HELP                 (terminal input only)
c
        else
          cmdndx = lstmch (comand, cmdnam, ncmd)
          xitndx = lstmch (comand, xitcmd, nexitc)
c
          if (luin .EQ. luttyi) then
            hlpcmd = strmch (comand,'help')
          else
            hlpcmd = .FALSE.
          endif
c
c ....... Abort command if it is ambiguous
c
          if (cmdndx.EQ.-2 .OR. xitndx.EQ.-2 .OR.
     1       (cmdndx.GT.0 .AND. xitndx.GT.0) .OR.
     2       (hlpcmd .AND. (xitndx.GT.0 .OR. cmdndx.GT.0)) ) then
            write(errmsg,9800) 'Ambiguous', comand(1:nchcmd)
            status = tioerh ('TIO', 'CMDINI', 2, errmsg, IOBADCMD)
            go to 900
          endif
c
c ....... HELP command: Process only if user supplies file name, or
c                       list of commands
c
          if (hlpcmd .AND. (ncmd.GT.0 .OR. uhlpfl.NE.' ') ) then
c
c ......... If user supplied a HELP file to TIOLIB, open it
c
            if (uhlpfl .NE. ' ') then
c
              hlplin = reader(1:lennb(reader))//' '//
     1                 uhlpfl(1:lennb(uhlpfl))//' &'
              write(luttyo,'(2a)')' Starting ',hlplin(1:lennb(hlplin)-2)
              status = fsystm(hlplin)
              if (status .NE. 0) status = IOINTERR
c
c ......... If not, give a list of known commands (if user passes
c           down NCMD=0, this is disabled)
c
            else
              write(luttyo,'(/''     Recognized commands:''/)')
              write(luttyo,'((5x,4a16))') (cmdnam(i),i=1,ncmd)
              write(luttyo,'((5x,4a16))') (xitcmd(i),i=1,nexitc)
              write(luttyo,'()')
            endif
c
c ....... APPLICATION or EXIT command
c
          elseif (ncmd.EQ.0 .OR. xitndx.GT.0 .OR. cmdndx.GT.0) then
c
            if (xitndx .GT. 0) status = IOEXIT
c
c ....... Unknown command
c
          else
            write(errmsg,9800) 'Unknown', comand(1:nchcmd)
            status = tioerh ('TIO', 'CMDINI', 2, errmsg, IOBADCMD)
          endif
c
        endif
c
c=======================================================================
c ..... Complete handling of command
c=======================================================================
c
c ..... All USER commands, and I/O or AUXILIARY commands with errors
c       route to this point. If status = IOINTERR or IOBADCMD, complete
c       the command by calling CMDXIT right here (closing command files
c       as enabled or necessary). If input for the next command comes
c       from the terminal, return. Otherwise return the error condition
c       back to the calling program.
c
  900   continue
c
        if (status.eq.IOINTERR .OR. status.eq.IOBADCMD) then
          call cmdxit (.FALSE., -1, ierr)
          if (luin .EQ. luttyi) go to 1
        endif
#ifdef USE_MPI
c
      endif
c
      if ( nprocs.gt.1 ) then
        if ( node.eq.0 ) then
          ibuf(1) = status
          ibuf(2) = lenxln
        endif
        call mpi_bcast(ibuf, 2, mpi_integer, 0, 
     1                 mpi_comm_world, ierr)
        if ( node.ne.0 ) then
          status = ibuf(1)
          lenxln = ibuf(2)
          comand = ' '
        endif
        if ( lenxln.gt.0 ) then
          call mpi_bcast(uinptc, lenxln, mpi_character, 0, 
     1                   mpi_comm_world, ierr)
          if ( node.ne.0 ) then
            cuichr = 0
            comand = chrpar (RQD, ' ', ' ', cmdchr, ierr, nchcmd)
          endif
        endif
      endif
#endif
c
      if ( status.NE.0 ) comand = ' '
c
c     At this point, if status = 0, the expanded line is located in 
c     then buffer uinptc(1:lenxln), located in include file ioinln.inc.
c     The passed variable 'comand' contains the 1st token from the 
c     buffer.  The pointer to the current character in the expanded 
c     input line (cuichr) points to the next token in the buffer.
c
      return
c
 9800 format(a,' command: ',a)
c
      end
