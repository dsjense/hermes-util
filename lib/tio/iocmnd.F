      subroutine iocmnd (status)
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
c     Process I/O control commands
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     OUTPUT
c       status  -  Status flag
c                    0        - Normal return
c                    IOINTERR - Error in internal TIO processing of input,
c                    IOBADCMD - Unknown/ambiguous IO command
c
      integer  status
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and common block decks:
c
#include "ioctrl.inc"
#include "iocmdf.inc"
#include "ioif.inc"
#include "ioinln.inc"
#include "ioloop.inc"
#include "iolun.inc"
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      character*132 chrpar, nxtpar
      integer      fsystm, intpar, iogifv, ioifdf, lennb, lstmch, tioerh
      logical      endofl, strmch
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS and variables:
c
      integer    MXICMD
      parameter (MXICMD = 23)
c
      character*132 errmsg, hlplin
      character*32  acont, arcsw, chtyp, chval, cmferr, iocmd, onofsw,
     1              nextp, pausop, shoitm, spprmt, symnam, ucase
      character*32  iocnam(1:MXICMD)
      integer       arclv, elwid, i, icmd, iecoio, ieccmt, ierr, ifval,
     1              lnsppr, nch, ueclvl, wflag
      logical       ioarcv, uecoio, ueccmt
c
c-----------------------------------------------------------------------
c
      data  iocnam / 'help', 'archive', 'case', 'char', 'cmferr',
     &               'cmfparam', 'define', 'echo', '^endfor', 'else',
     &               'endif', 'for', 'gdefine', 'if ', 'ifdef',
     &               'ifndef', 'pause', 'prompt', 'run', 'show',
     &               'spawn', 'undefine', 'wide' /
c
c
c=======================================================================
c     BEGIN: Get command
c=======================================================================
c
      ioerr  = 0
      status = 0
      ioarcv = .TRUE.
      iocmd  = chrpar (OPT, ' ', ' ', ' ', ioerr, nch)
      if (ioerr .NE. 0) go to 8000
c
c ... Find index of command.  Note that to support the ambiguity of
c     an 'IF' command matching 'IF', 'IFDEF', and 'IFNDEF', we must
c     explicitly override the return value from LSTMCH (ICMD is not
c     used anywhere else)
c
      icmd  = lstmch (iocmd, iocnam, MXICMD)
      if (strmch(iocmd(1:2),'if')) icmd = 0
c
      if (icmd .LT. 0) then
        if (icmd .EQ. -1) write(errmsg,9800) 'Unknown', iocmd
        if (icmd .EQ. -2) write(errmsg,9800) 'Ambiguous', iocmd
        status = tioerh ('TIO', 'IOCMND', 2, errmsg, IOBADCMD)
        go to 8010
      endif
c
c=======================================================================
c ... Process I/O command
c=======================================================================
c
c-----------------------------------------------------------------------
c ... HELP
c-----------------------------------------------------------------------
c
      if (strmch(iocmd,'help')) then
c
        if ( luttyi.ge.0 ) then
          if (iohlpf .NE. ' ') then
            hlplin = reader(1:lennb(reader))//' '//
     1               iohlpf(1:lennb(iohlpf))//' &'
            write(luttyo,'(2a)')' Starting ',hlplin(1:lennb(hlplin)-2)
            status = fsystm(hlplin)
          else
            write(luttyo,'(/''     Recognized commands:''/)')
            write(luttyo,'((5x,4a16))') (iocnam(i),i=1,MXICMD)
            write(luttyo,'()')
          endif
c
          ioarcv = .FALSE.
        endif
c
c-----------------------------------------------------------------------
c ... ARCHIVE
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'archive')) then
c
        arcsw = chrpar (RQD, 'archive switch', ' ', ' ', ioerr, nch)
        arclv = intpar (OPT, ' ', 0, ' ', ioerr)
c
        if (ioerr .EQ. 0) call iostar (arcsw, arclv, status)
c
c-----------------------------------------------------------------------
c ... CASE
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'case')) then
c
        ucase = chrpar (RQD, 'case', ' ', ' ', ioerr, nch)
c
        if (ioerr .EQ. 0) call iostca (ucase, status)
c
c-----------------------------------------------------------------------
c ... CHAR
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'char')) then
c
        chtyp = chrpar (RQD, 'char. type', ' ', ' ', ioerr, nch)
        chval = chrpar (OPT, 'character' , ' ', ' ', ioerr, nch)
c
        if (ioerr .EQ. 0 .AND. chval .NE. ' ') 
     &      call iostch (chtyp, chval, status)
c
c-----------------------------------------------------------------------
c ... CMFERR
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'cmferr')) then
c
        cmferr = chrpar (RQD, 'error mode', ' ', ' ', ioerr, nch)
        if (ioerr .EQ. 0) call iostfe (cmferr, status)
c
c-----------------------------------------------------------------------
c ... CMFPARAM
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'cmfparam')) then
c
        call iocmfpdef (status)
c
c-----------------------------------------------------------------------
c ... DEFINE
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'define')) then
c
c ..... Get symbol name
c
        symnam = chrpar (RQD, 'symbol name', ' ', ' ', status, nch)
c
c ..... If next parameter in the input string is a left bracket,
c       symbol is an array; if not, it is a scalar symbol
c
        nextp = nxtpar('[]', status, nch)
c
        if (status .EQ. 0) then
          if (nextp .NE. LBRAKT) then
            call iodefsca (LOCALSYM, symnam, ' ', status)
          else
            call iodefarr (LOCALSYM, symnam, 0, ' ', status)
          endif
        endif
c
c-----------------------------------------------------------------------
c ... ECHO
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'echo')) then
c
        onofsw = chrpar (RQD, '"ON" or "OFF"', ' ', ' ', ioerr, nch)
        ueclvl = intpar (OPT, ' ', 1, ' ', ioerr)
        iecoio = intpar (OPT, ' ', 1, ' ', ioerr)
        ieccmt = intpar (OPT, ' ', 0, ' ', ioerr)
        elwid  = intpar (OPT, ' ', ntcols, ' ', ioerr)
c
        uecoio = .FALSE.
        if (iecoio .GT. 0) uecoio = .TRUE.
c
        ueccmt = .FALSE.
        if (ieccmt .GT. 0) ueccmt = .TRUE.
c
        if (ioerr .EQ. 0) call iostec (onofsw,
     1                    ueclvl, uecoio, ueccmt, elwid, status)
c
c-----------------------------------------------------------------------
c ... ENDFOR (This is an ERROR condition here)
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'endfor')) then
c
        status = tioerh ('TIO', 'IOCMND', 2,
     &    'ENDFOR statement outside FOR loop', IOINTERR)
c
c-----------------------------------------------------------------------
c ... ELSE
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'else')) then
c
        if (iflvl(cmflvl) .EQ. 0) then
          write(errmsg,9830) 'ELSE'
          status = tioerh ('TIO', 'IOCMND', 2, errmsg, IOINTERR)
        elseif (ifstat(iflvl(cmflvl),cmflvl) .EQ. 2) then
c
          if (endofl(ioerr)) then
            ifstat(iflvl(cmflvl),cmflvl) = 1
          else
            nextp = chrpar(RQD, ' ', ' ', ' ', ioerr, nch)
            if (strmch(nextp,'if ')) then
              ifval = iogifv (status)
            elseif (strmch(nextp,'ifdef')) then
              ifval = ioifdf (1, status)
            elseif (strmch(nextp,'ifndef')) then
              ifval = ioifdf (0, status)
            else
              status = tioerh ('TIO', 'IOCMND', 2,
     1                   'Syntax error in ELSE statement', IOINTERR)
            endif
c
            if (ioerr.EQ.0 .AND. status.EQ.0 .AND. ifval.EQ.1)
     1        ifstat(iflvl(cmflvl),cmflvl) = 1
          endif
c
        else
          ifstat(iflvl(cmflvl),cmflvl) = 3
        endif
c
c-----------------------------------------------------------------------
c ... ENDIF
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'endif')) then
c
        if (iflvl(cmflvl) .EQ. 0) then
          write(errmsg,9830) 'ENDIF'
          status = tioerh ('TIO', 'IOCMND', 2, errmsg, IOINTERR)
        else
          iflvl(cmflvl) = iflvl(cmflvl) - 1
        endif
c
c-----------------------------------------------------------------------
c ... FOR loop
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'for')) then
c
        call flpini (status)
        ioarcv = .FALSE.
c
c-----------------------------------------------------------------------
c ... GDEFINE
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'gdefine')) then
c
c ..... Get symbol name
c
        symnam = chrpar (RQD, 'symbol name', ' ', ' ', status, nch)
c
c ..... If next parameter in the input string is a left bracket,
c       symbol is an array; if not, it is a scalar symbol
c
        nextp = nxtpar('[]', status, nch)
c
        if (status .EQ. 0) then
          if (nextp .NE. LBRAKT) then
            call iodefsca (GLOBALSYM, symnam, ' ', status)
          else
            call iodefarr (GLOBALSYM, symnam, 0, ' ', status)
          endif
        endif
c
c-----------------------------------------------------------------------
c ... IF / IFDEF / IFNDEF
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd(1:2),'if')) then
c
        if (iflvl(cmflvl) .GE. MXIFLV) then
          write(errmsg,9820) MXIFLV
          status = tioerh ('TIO', 'IOCMND', 2, errmsg, IOINTERR)
          iflvl(cmflvl) = 0
          go to 8010
        endif
c
        if (strmch(iocmd,'if ')) then
          ifval = iogifv (status)
        elseif (strmch(iocmd,'ifdef')) then
          ifval = ioifdf (1, status)
        elseif (strmch(iocmd,'ifndef')) then
          ifval = ioifdf (0, status)
        endif
c
        if (ioerr.EQ.0 .AND. status.EQ.0) then
          iflvl(cmflvl) = iflvl(cmflvl) + 1
c
c ....... If this is a nested IF, and the calling IF-level is 
c         currently rejecting lines, set up to reject ENTIRE block
c         IF statement
c
          if (iflvl(cmflvl).GT.1 .AND.
     1        ifstat(iflvl(cmflvl)-1,cmflvl).GE.2) then
            ifstat(iflvl(cmflvl),cmflvl) = 3
c
c ....... Otherwise either accept or reject lines based on the
c         IF-test for this level
c
          else
            if (ifval .EQ. 1) then
              ifstat(iflvl(cmflvl),cmflvl) = 1
            else
              ifstat(iflvl(cmflvl),cmflvl) = 2
            endif
          endif
        endif
c
c-----------------------------------------------------------------------
c ... PAUSE
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'pause')) then
c
        if (luin0 .EQ. luttyi) then
          pausop = chrpar (OPT, ' ', ' ', ' ', ioerr, nch)
c
          if (pausop .EQ. ' ') then
            if (pause .AND. (cmflvl.GT.0 .OR. looplv(cmflvl).GE.1)) then
              call mdprmt (luttyo, 'PAUSE>')
              ierr = 0
              call getttyline(acont,ierr)
              if (index(acont,'q').GT.0 .OR. index(acont,'Q').GT.0) then
                call cmfxit(2)
              endif
            endif
          elseif (strmch(pausop,'on')) then
            if (cmflvl.EQ.0 .AND. looplv(cmflvl).EQ.0) pause = .TRUE.
          elseif (strmch(pausop,'off')) then
            pause = .FALSE.
          endif
        endif
c
c-----------------------------------------------------------------------
c ... PROMPT
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'prompt')) then
c
        prompt = chrpar (OPT, ' ', '*', ' ', ioerr, nch)
c
c-----------------------------------------------------------------------
c ... RUN
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'run')) then
c
        call cmfini (status)
c
c ..... Comment out the the command line if we are going to archive
c       the commands in the file
c
        if (status.EQ.0 .AND. cmflvl.LE.arclvl) uinptl(1:1) = cmtchr
c
c-----------------------------------------------------------------------
c ... SHOW
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'show')) then
c
        if ( luttyi.ge.0 ) then
          shoitm = chrpar (RQD, 'show item', ' ', ' ', ioerr, nch)
c
          if (ioerr .EQ. 0) call ioshow (shoitm)
          ioarcv = .FALSE.
        endif
c
c-----------------------------------------------------------------------
c ... SPAWN
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'spawn')) then
c
        spprmt = chrpar (OPT, ' ', dfsppr, ' ', ioerr, nch)
        lnsppr = lennb(spprmt)
c
        if (ioerr .EQ. 0) call mdspwn (spprmt(1:lnsppr+1))
        ioarcv = .FALSE.
c
c-----------------------------------------------------------------------
c ... UNDEFINE
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'undefine')) then
c
        call iosymdel (' ', 0, 0, status)
c
c-----------------------------------------------------------------------
c ... WIDE
c-----------------------------------------------------------------------
c
      elseif (strmch(iocmd,'wide')) then
c
 
        if ( luttyi.ge.0 ) then
          if (endofl(ioerr)) then
            wflag = 1 - wstate
          else
            onofsw = chrpar(OPT, ' ', 'off', ' ', ioerr, nch)
            if (strmch(onofsw,'off')) then
              wflag = 0
            elseif (strmch(onofsw,'on')) then
              wflag = 1
            else
              wflag = wstate
            endif
          endif
c
          if (wflag .NE. wstate) then
            wstate = wflag
            call mdswid (luttyo, wstate, ntcols)
          endif
        endif
c
      endif
c
c-----------------------------------------------------------------------
c
 8000 continue
      if (ioerr.LT.0 .OR. status.NE.0) status = IOINTERR
c
c ... Complete command if there are no errors. If there are errors,
c     the call to cmdxit will take place in subroutine CMDINI.
c
 8010 continue
      if(status .EQ. 0) call cmdxit (ioarcv, status, ierr)
c
      return
c
c
c=======================================================================
c     ERROR messages
c=======================================================================
c
 9800 format(a,' command: ',a)
 9820 format('Exceeded maximum IF level (',i1,')')
 9830 format('Illegal IF-ELSE-ENDIF: ',a,' with no preceding IF')
c
      end
