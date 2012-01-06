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
      character*80 chrpar, iogioc
      integer      intpar, tioerh
      logical      strmch
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERs and variables
c
      character*132 errmsg
      character*80  iocmd, lpvn, tmplin
      integer ln, lntyp, lenil, lnail, lpi, lpf, lpst, lplev,
     1        lstlc0, nch, nlpln0, oldlty
c
c
c=======================================================================
c=======================================================================
c
      status = 0
c
c ... Process input:
c
      lpvn = chrpar (RQD, ' ', ' ', ' ', ioerr, nch)
      lpi  = intpar (RQD, ' ',   0, ' ', ioerr)
      lpf  = intpar (RQD, ' ',   0, ' ', ioerr)
      lpst = intpar (OPT, ' ',   1, ' ', ioerr)
c
      if (ioerr .GE. 0) then
        if (lpst .EQ. 0) status = tioerh ('TIO', 'FLPINI', 2,
     1                              'Zero loop increment', IOINTERR)
      else
        status = IOINTERR
      endif
c
c=======================================================================
c *** Load FOR loop command line buffer, if not currently running loop
c=======================================================================
c
      if (looplv(cmflvl) .EQ. 0) then
c
        lstlc0 = lstlch
        nlpln0 = nlpln
c
        lplev = 1
        beglpl(lplev,cmflvl) = nlpln + 1
c
c ..... Loop over input lines: Note that we keep going until we find
c       a matching "^endfor" or hit an EOF, so that we do not return
c       until we have read the entire loop.
c
        oldlty = 0
c
        do 100 ln=1,99999
c
c ....... Load next command line into temporary buffer
c
          ioerr = 0
          call getsln ('FOR>', 80, tmplin, lntyp, lenil, lnail, ioerr)
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
            go to 101
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
c ....... Set file line number of loop line
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
              if (lplev .LE. MXLPLV) endlpl(lplev,cmflvl) = nlpln
              lplev = lplev - 1
c
c ......... FOR: Increment loop level
c
            elseif (strmch(iocmd,'for')) then
              lplev = lplev + 1
              if (lplev .LE. MXLPLV) then
                beglpl(lplev,cmflvl) = nlpln+1
              else
                write(errmsg,9800) 'nest levels', MXLPLV
                status = tioerh ('TIO', 'FLPINI', 2, errmsg, IOINTERR)
              endif
            endif
c
          endif
c
c ....... Store command line in loop line buffer
c
          if (status .EQ. 0) then
            if (lstlch+lenil .GT. MXLPCH) then
              write(errmsg,9800) 'characters', MXLPCH
              status = tioerh ('TIO', 'FLPINI', 2, errmsg, IOINTERR)
              go to 99
            endif
c
            lplbuf(lstlch+1:lstlch+lenil) = tmplin(1:lenil)
            lplchb(nlpln) = lstlch + 1
            lplche(nlpln) = lstlch + lenil
            lstlch        = lstlch + lenil
          endif
c
   99     continue
          oldlty = lntyp
c
c ....... Exit loading loop lines if we are at nest level ZERO
c
          if (lplev .EQ. 0) go to 101
  100   continue
c
c ..... Abort loop if errors were encountered
c
  101   continue
        if (status .NE. 0) then
          status = tioerh(' ', ' ', 2, 
     1                    '           Aborting FOR loop', status)
          nlpln  = nlpln0
          lstlch = lstlc0
          return
        endif
c
c ..... Archive loop lines if selected (note that the original
c       ^FOR command line is still in the multi-line buffer, waiting
c       to be archived)
c
        if (arcvln) then
          call ioarcl (1, ' ', ioerr)
          do 200 ln=beglpl(1,cmflvl),endlpl(1,cmflvl)
            call ioarcl (2, lplbuf(lplchb(ln):lplche(ln)), ioerr)
  200     continue
        endif
c
c ..... Save current input file line number for when we exit the loop
c
        lnumcx(cmflvl) = lnumcf(cmflvl)
      endif
c
c=======================================================================
c ... Set up loop for execution if there are no errors
c=======================================================================
c
      if (status .LT. 0) return
c
      looplv(cmflvl) = looplv(cmflvl) + 1
c
      loopvn(looplv(cmflvl),cmflvl) = lpvn
      lpstep(looplv(cmflvl),cmflvl) = lpst
      loopv(looplv(cmflvl),cmflvl)  = lpi - lpst
c
      nlpit(looplv(cmflvl),cmflvl)  = (lpf - lpi + lpst)/lpst + 1
      curlpl(looplv(cmflvl),cmflvl) = endlpl(looplv(cmflvl),cmflvl) - 1
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
