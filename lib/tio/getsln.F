      subroutine getsln (
c ... INPUT
     1 uprmpt, linlen,
c ... OUTPUT
     2 line, lintyp, lennbl, lenal,
c ... INPUT/OUTPUT
     3 ierr)
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
c     Returns A SINGLE input line from the current input source, either
c     currently executing FOR loop, a command procedure or the terminal.
c
c     In interactive mode, the user is prompted with the default prompt
c     PROMPT in the TIOLIB common block, unless the user overrides this
c     with a non-blank UPRMPT.
c
c     After reading the line, it processes it for the line type
c
c       1  -  "normal" user input line
c       2  -  User line w/ trailing continuation character
c       3  -  Comment line
c
c     It also returns the non-blank length of the entire line, and of
c     the "active" part of the line, i.e. the part of the line BEFORE
c     a COMMENT character.  Note that it also identifies any unbalanced
c     QUOTES while checking the line type.
c
c-----------------------------------------------------------------------
c
c ... Function arguments:
c
c     INPUT:
c       uprmpt  -  Input prompt
c       linlen  -  Length of LINE (for overflow checks)
c
c     OUTPUT:
c       line    -  Input line
c       lintyp  -  Line type:
c                    CMTLIN - Comment line
c                    CONLIN - Line ends with continuation character
c                    USRLIN - Command line w/o cont. char.
c       lennbl  -  Non-blank length of entire line
c       lenal   -  Non-blank length of "active" part of line (note
c                  cont. char. is NOT included for CONLIN lines)
c
c     INPUT/OUTPUT:
c       ierr    -  INPUT:  Do nothing if ierr.NE.0
c                  OUTPUT: Error flag:-
c                            0 = normal return
c                           -1 = End-of-File on input
c                           -2 = Error on read
c                           -3 = Input line overflows char. buffer
c                           -4 = Unbalanced QUOTES in line
c
      character*(*) uprmpt, line
      integer       lintyp, linlen, lennbl, lenal, ierr
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
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      character*134 nxtlpl
      integer       lennb, tioerh
c
c-----------------------------------------------------------------------
c
c ... Local variables:
c
      character*134 errmsg, tmplin
      character*1   quochr
      integer       i, lpstat
      logical       blnkln
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      line   = ' '
      lintyp = 0
      lennbl = 0
      lenal  = 0
c
      if (ierr .NE. 0) return
c
c-----------------------------------------------------------------------
c ... Get line
c-----------------------------------------------------------------------
c
    1 continue
c
      if (looplv(cmflvl) .GT. 0) then
c
c ..... FOR loop: If LPSTAT = 1, loop has just terminated, so go
c       back for input from current logical unit
c
        tmplin = nxtlpl(lpstat)
        if (lpstat .EQ. 1) go to 1
c
      else
c
c ..... COMMAND FILE or TERMINAL:  If current input source is terminal,
c                                  put out prompt
c
        if (luin .EQ. luttyi) then
          if (uprmpt .NE. ' ') then
            call mdprmt (luttyo, uprmpt)
          else
            if (prompt .NE. ' ') call mdprmt (luttyo, prompt)
          endif
          call getttyline(tmplin,ierr)
          if ( ierr.ne.0 ) return
        else
          read(luin,'(a)',end=800,err=9000) tmplin
        endif
c
        lnumcf(cmflvl) = lnumcf(cmflvl) + 1
c
      endif
c
      lennbl = lennb(tmplin)
      if (lennbl .LE. 0) then
        if(crmode .AND. luin .EQ. luttyi) go to 800
        go to 1
      endif
c
      if (lennbl .GT. linlen) then
        write (errmsg,9800) linlen
        ierr = tioerh ('TIO', 'GETSLN', 2, errmsg, -3)
        return
      endif
c
      line = tmplin(1:lennbl)
c
c-----------------------------------------------------------------------
c ... Determine line type, and # characters in active part of line
c-----------------------------------------------------------------------
c
      blnkln = .TRUE.
      quochr = ' '
      i      = 0
c
c ... Loop over characters in line (note this is a non-DO loop because
c     we alter the loop index (i) inside the loop
c
c************************************************** BEGIN LOOP *********
  100 continue
        i = i + 1
        if (line(i:i).NE.SPACE .AND. line(i:i).NE.TAB .AND.
     &      line(i:i).NE.cmtchr) blnkln = .FALSE.
c
c ..... Handle QUOTEs - we are in literal string if QUOCHR.NE.' '
c
        if (line(i:i).EQ.SNGLQT .OR. line(i:i).EQ.DUBLQT) then
          if (quochr .EQ. ' ') then
            quochr = line(i:i)
          elseif (line(i:i) .EQ. quochr) then
            quochr = ' '
          endif
        endif
c
c ..... If character is the prefix literal character (\),
c       unconditionally skip over the next character
c
        if (line(i:i) .EQ. LITCHR) then
          if (i .LT. lennbl) i = i + 1
          go to 100
        endif
c
c ..... Otherwise, if not in QUOTE mode, see if we have reached
c       the end of the active part of the line
c
        if (quochr .EQ. ' ') then
          if (line(i:i) .EQ. conchr) then
            lintyp = CONLIN
            lenal  = lennb(line(1:i-1))
            return
          elseif (line(i:i) .EQ. cmtchr) then
            if (blnkln) then
              lintyp = CMTLIN
            else
              lintyp = USRLIN
              lenal  = lennb(line(1:i-1))
            endif
            return
          endif
        endif
c
      if (i .LT. lennbl) go to 100
c************************************************** END LOOP ***********
c
      if (quochr .NE. ' ') then
        ierr = tioerh ('TIO', 'GETSLN', 2, 'Unbalanced quotes', -4)
      else
        lintyp = USRLIN
        lenal  = lennbl
      endif
c
      return
c
c-----------------------------------------------------------------------
c ... EOF on input
c-----------------------------------------------------------------------
c
  800 continue
      ierr = -1
      line = ' '
      return
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9000 continue
      ierr = tioerh ('TIO', 'GETSLN', 2, 'Error on read', -2)
      return
c
 9800 format('Line overflows buffer (',i3,' chars.)')
c
      end
