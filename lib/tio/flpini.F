      subroutine flpini (
c ... OUTPUT
     1 status )
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
c     Handle the "^FOR" statement of a FOR loop.  This routine works
c     in one of two modes:-
c
c       1. Not currently running a loop:  Store ALL input lines into
c          loop line buffer (including nested loops), and set up to
c          execute first loop level.
c
c       2. Currently running a loop -- this must be execution of a
c          nested loop: Bump up loop nest level, and set up to execute
c          nested loop.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments
c
c     OUTPUT
c       ierr  -  Error status:
c                  0      - normal return
c                IOINTERR - Errors encountered
c
      integer status
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "ioctrl.inc"
#include "iocmdf.inc"
#include "ioinln.inc"
#include "ioloop.inc"
#include "iolun.inc"
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      character chrpar*132, iogioc*32
      integer   intpar, tioerh
      logical   strmch
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERs and variables
c
      character*255 errmsg, tmplin
      character*80  iocmd, lpvn
      integer ln, lntyp, lenil, lnail, lpi, lpf, lpst, lplev,
     1        nch, niter, oldlty
c
c
c=======================================================================
c=======================================================================
c
      status = 0
c
c=======================================================================
c *** MODE 1: Load FOR loop command line buffer
c=======================================================================
c
      if (looplv(cmflvl) .EQ. 0) then
        ilpln(cmflvl)    = nlpln
        beglpl(1,cmflvl) = nlpln + 1
c
c ..... Loop over input lines: Note that we keep going until we find
c       a matching "^endfor" or hit an EOF, so that we do not return
c       until we have read the entire loop.
c
        oldlty = 0
        lplev  = 1
c
        do while (lplev .GT. 0)
c
c ....... Load next command line into temporary buffer
c
          ioerr = 0
          call getsln ('FOR>', 255, tmplin, lntyp, lenil, lnail, ioerr)
c
c ....... If we hit an EOF reading the input line:-
c           1. Set the flag to unconditionally close the input file if it
c              is a command file at level 1 or higher
c           2. Abort the line-processing loop with error status IOINTERR
c         NOTE: If we hit an EOF on command file level 0, we do NOT
c         return with status IOEOF, to ensure that we return an ERROR
c         status to non-interactive main programs.  They will hit the EOF
c         on the next call to CMDINI.
c
          if (ioerr .EQ. -1) then
            status = tioerh ('TIO', 'FLPINI', 2,
     1               'EOF on file processing FOR loop', IOINTERR)
            if (cmflvl .GT. 0) fatalcfer = .TRUE.
            go to 100
c
c ....... If we had any other trouble reading the line, set error flag,
c         but keep on reading lines (attempt to find matching ^endfor)
c
          elseif (ioerr .LT. -1) then
            status = IOINTERR
          endif
c
          nlpln = nlpln + 1
          if (nlpln .EQ. MXLPLN+1) then
            write(errmsg,9800) 'lines', MXLPLN
            status = tioerh ('TIO', 'FLPINI', 2, errmsg, IOINTERR)
          endif
c
c ....... Set file line number of loop line for error messages
c
          lplnmf(nlpln) = lnumcf(cmflvl)
c
c ....... Get I/O command from I/O command line, and process if it
c         is either '^endfor' or '^for'
c
          iocmd = iogioc(tmplin)
c
          if (strmch(iocmd,'endfor') .OR. strmch(iocmd,'for')) then
c
c ......... Syntax error if preceding line is continuation line
c
            if (oldlty .EQ. CONLIN) status = tioerh ('TIO', 'FLPINI', 2,
     1        'Last loop line is continuation line', IOINTERR)
c
c ......... ENDFOR: Decrement loop nest level
c
            if (strmch(iocmd,'endfor')) then
              lplev = lplev - 1
c
c ......... FOR: Increment loop level
c
            elseif (strmch(iocmd,'for')) then
              lplev = lplev + 1
              if (lplev .GT. MXLPLV) then
                write(errmsg,9800) 'nest levels', MXLPLV
                status = tioerh ('TIO', 'FLPINI', 2, errmsg, IOINTERR)
              endif
              lplev = min(lplev,MXLPLV)
            endif
c
          endif
c
c ....... Store command line in loop line character buffer
c
          if (status .EQ. 0) then
            if (lstlch+lenil .GT. MXLPCH) then
              write(errmsg,9800) 'characters', MXLPCH
              status = tioerh ('TIO', 'FLPINI', 2, errmsg, IOINTERR)
            endif
c
            if (status .EQ. 0) then
              lplchr(lstlch+1:lstlch+lenil) = tmplin(1:lenil)
              lplchb(nlpln) = lstlch + 1
              lplche(nlpln) = lstlch + lenil
              lstlch        = lstlch + lenil
            endif
          endif
c
          oldlty = lntyp
        enddo
c
c ..... Abort loop if errors were encountered
c
  100   continue
        if (status .NE. 0) then
          status = tioerh ('TIO', 'FLPINI', 2,
     1                    '           Aborting FOR loop', status)
          nlpln  = ilpln(cmflvl)
          lstlch = lplche(nlpln)
          return
        endif
c
c ..... Archive loop lines if selected (note that the original
c       ^FOR command line is still in the multi-line buffer, waiting
c       to be archived)
c
        if (arcvln) then
          call ioarcl (1, ' ', ioerr)
          do 200 ln=beglpl(1,cmflvl),nlpln
            call ioarcl (2, lplchr(lplchb(ln):lplche(ln)), ioerr)
  200     continue
        endif
c
c ..... Save current input file line number for when we exit the loop
c
        lnumcx(cmflvl) = lnumcf(cmflvl)
      endif
c
      if (status .NE. 0) return
c
c=======================================================================
c ... MODE 2: execute the loop
c=======================================================================
c
c ... Process loop iteration control from command line input:
c
      ioerr = 0
      lpvn  = chrpar (RQD, ' ', ' ', ' ', ioerr, nch)
      lpi   = intpar (RQD, ' ',   0, ' ', ioerr)
      lpf   = intpar (RQD, ' ',   0, ' ', ioerr)
      lpst  = intpar (OPT, ' ',   1, ' ', ioerr)
c
      if (ioerr .GE. 0) then
        if (lpst .NE. 0) then
          niter = (lpf - lpi)/lpst + 1
        else
          status = tioerh ('TIO', 'FLPINI', 2,
     &                     'Zero loop increment', IOINTERR)
        endif
      else
        status = IOINTERR
      endif
c
c ... Abort on error
c
      if (status.NE.0 .OR. ioerr.NE.0) return
c
c ... Set the beginning line # in the buffer for the executable commands
c     in this loop and find line # of corresponding '^endfor' command
c
      lplev = looplv(cmflvl) + 1
      ln    = ilpln(cmflvl)
      beglpl(lplev,cmflvl) = ilpln(cmflvl) + 1
      do while (lplev .GT. looplv(cmflvl))
        ln = ln + 1
        iocmd = iogioc(lplchr(lplchb(ln):lplche(ln)))
        if (strmch(iocmd,'endfor')) then
          lplev = lplev - 1
        elseif (strmch(iocmd,'for')) then
          lplev = lplev + 1
        endif
      enddo
c
c ... Set up loop if there is at least one iteration. Note that we set
c     nlpit(*,*) and ilpln(*) so that the first call to nxtlpl.F will
c     trip the loop variable increment
c
      if (niter .GT. 0) then
        looplv(cmflvl) = looplv(cmflvl) + 1
        lplev = looplv(cmflvl)
c
        loopvn(lplev,cmflvl) = lpvn
        lpstep(lplev,cmflvl) = lpst
        nlpit(lplev,cmflvl)  = niter + 1
        loopv(lplev,cmflvl)  = lpi - lpst
        endlpl(lplev,cmflvl) = ln
c
        ilpln(cmflvl) = ln - 1
c
c ... Otherwise ...
c       *  looplev > 0: Set pointer for next command at lower level
c       *  looplev = 0: Shut down loop processing at the command file lev.
c
      elseif (looplv(cmflvl) .GT. 0) then
        ilpln(cmflvl) = ln
      else
        nlpln  = beglpl(1,cmflvl) - 1
        lstlch = lplche(nlpln)
        lnumcf(cmflvl) = lnumcx(cmflvl)
      endif
c
      return
c
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9800 format('Maximum # loop ',a,' exceeded (',i6,')')
c
      end
