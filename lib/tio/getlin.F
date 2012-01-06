      subroutine getlin (
c ... INPUT
     1 mode, uprmpt,
c ... INPUT/OUTPUT
     2 ierr )
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
c ... GETLIN either gets a new full input line or appends new sub-lines
c     to the current input line, from the current input source, based
c     on the MODE switch.  The 'input line' consists of one or more
c     'sub-lines' connected with continuation characters at the end of
c     the active line (i.e. part of a line BEFORE a comment character).
c
c     In interactive mode, the user is prompted with the default prompt
c     PROMPT in the IOCTRL common block, unless the user overrides this
c     with a non-blank UPRMPT.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       mode    -  Mode switch
c                       0  -  Get new line
c                   .NE.0  -  Append new sub-lines to current input line
c       uprmpt  -  Input prompt
c
c     INPUT/OUTPUT:
c       ierr    -  INPUT:  Do nothing if ierr.NE.0
c                  OUTPUT: Error flag:-
c                            0  = normal return
c                            -1 = End-of-File on zero level input
c                            -2 = Error on read
c                            -3 = # continuation lines exceeded
c
      character*(*) uprmpt
      integer       mode, ierr
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "ioctrl.inc"
#include "iocmdf.inc"
#include "ioif.inc"
#include "ioinln.inc"
#include "ioloop.inc"
#include "iolun.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      character*80 iogioc
      integer      lstmch, tioerh
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS and variables:
c
      integer      NIFCMD
      parameter   (NIFCMD = 5)
c
      character*132 errmsg, lprmpt
      character*80  iocmd
      character*16  ifcmds(NIFCMD)
      integer       frstln, locul, nl
c
      data  ifcmds / 'if', 'ifdef', 'ifndef', 'else', 'endif' /
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
    1 continue
c
      if (ierr .LT. 0) then
        nuiln = 0
        return
      endif
c
c ... Initialize new line mode
c
      if (mode .EQ. 0) then
        locul = 1
        nuiln = 0
c
c ... Initialize append line mode:-
c        1.  Add a continuation char. to end of active part of last line
c            (if last line has a trailing comment, this char. is INSERTED)
c        2.  Set local pointer into line buffer
c
      else
        locul = locuil(nuiln) + lnauil(nuiln)
        if (lnauil(nuiln) .EQ. lenuil(nuiln)) then
          write(uinptl(locul:locul+1),'(2a)') ' ', conchr
          lnauil(nuiln) = lnauil(nuiln) + 2
          lenuil(nuiln) = lenuil(nuiln) + 2
        else
          uinptl(locul+3:) = uinptl(locul:)
          write(uinptl(locul:locul+2),'(3a)') ' ', conchr, ' '
          lnauil(nuiln) = lnauil(nuiln) + 3
          lenuil(nuiln) = lenuil(nuiln) + 3
        endif
        locul = locuil(nuiln) + lenuil(nuiln)
      endif
c
c-----------------------------------------------------------------------
c ... Loop over input sub-lines
c-----------------------------------------------------------------------
c
      frstln = nuiln + 1
c
      do 100 nl=frstln,MXUILN
c
        if (nl .EQ. frstln) then
          lprmpt = uprmpt
        else
          write(lprmpt,'(a,''>'')') conchr
        endif
c
        call getsln (lprmpt, MXICHL-locul+1, uinptl(locul:),
     1               uiltyp(nl), lenuil(nl), lnauil(nl), ierr)
c
c ..... END-OF-FILE on input
c
        if (ierr .EQ. -1) then
c
c ....... If running a command file, simply bump down one level (except
c         if we were already building a command line)
c
          if (cmflvl .GT. 0) then
            if (nl .GT. 1) then
              ierr = tioerh ('TIO', 'GETLIN', 2,
     1               'EOF on continuation line', -1)
              call cmfxit (3)
            else
c
c ........... If preprocessing a file, just return
c
              if (ppmode .AND. luin0.EQ.luttyi .AND. cmflvl.EQ.1) return
              call cmfxit (0)
            endif
            ierr = 0
            go to 1
          else
            if (luin .NE. luttyi .AND. .NOT. crmode .AND. nl.GT.1) 
     1        ierr = tioerh ('TIO', 'GETLIN', 2,
     2               'EOF on continuation line', -1)
            return
          endif
c
c ..... For other errors, just unconditionally return
c
        elseif (ierr .LT. -1) then
          return
        endif
c
        locuil(nl) = locul
        locul      = locul + lenuil(nl)
c
c-----------------------------------------------------------------------
c ***** Process line
c-----------------------------------------------------------------------
c
c ..... Set switch for archiving
c
        if (nl .EQ. 1) then
          if (cmflvl.LE.arclvl .AND. looplv(cmflvl).EQ.0) then
            arcvln = .TRUE.
          else
            arcvln = .FALSE.
          endif
        endif
c
c ..... If conditional processing is on, archive (if needed) and ignore
c       this line, unless it is a TIO conditional structure control 
c       line.
c
        if (mode.EQ.0 .AND. iflvl(cmflvl).GT.0) then
          if (ifstat(iflvl(cmflvl),cmflvl) .GE. 2) then
            iocmd = iogioc( uinptl(1:lenuil(1)) )
            if ( lstmch(iocmd, ifcmds, NIFCMD) .EQ. -1 ) then
              if (arcvln) call ioarcl (2, uinptl(1:lenuil(nl)), ierr)
              go to 1
            endif
          endif
        endif
c
c ..... Handle first sub-line 
c
        if (nl .EQ. 1) then
c
c ....... Ignore stand-alone comment lines, except for archiving
c         (Embedded comment lines will be put into input line buffer),
c         and echoing
c
          if (uiltyp(nl) .EQ. CMTLIN) then
c
            if (ecocmt .AND. cmflvl.GE.ecolvl) then
              write(luecho,'(a)',err=9000) uinptl(1:lenuil(nl))
            endif
            if (arcvln) call ioarcl (2, uinptl(1:lenuil(nl)), ierr)
            go to 1
c
c ....... Record file line number of first line of command
c
          else
            lnumc0(cmflvl) = lnumcf(cmflvl)
          endif
        endif
c
c ..... Return if we have a user line
c
        if (uiltyp(nl) .EQ. USRLIN) then
          nuiln  = nl
          return
        endif
c
  100 continue
c
      write(errmsg,9810) MXUILN-1
      ierr = tioerh ('TIO', 'GETLIN', 2, errmsg, -3)
      return
c
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9000 continue
      write(errmsg,9820) luecho
      ierr = tioerh ('TIO', 'GETLIN', 1, errmsg, 0)
      ierr = tioerh (' ', ' ', 1, 
     1               '            *** DISABLING ECHOING ***', 0)
      ecolvl = 1000
      go to 1
c 
 9810 format('Exceeded maximum # continuation lines ',i3)
 9820 format('Error echoing input line to unit #',i3)
c
      end
