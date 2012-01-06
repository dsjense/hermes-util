      subroutine listxt (
c ... INPUT
     1 txtbuf, nltext, blnkop)
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
c     LISTXT, lists text to the terminal, ensuring that this
c     information does not scroll of the screen.  The variable NTLINS
c     in common block IOCTRL is the # lines on terminal screen
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments
c
c     INPUT:
c       txtbuf  -  (*) Text line buffer
c       nltext  -  # lines of text to list
c       blnkop  -  Blank line option:-
c                   0  -  No leading blank line, No trailing blank line
c                   1  -   1 leading blank line, No trailing blank line
c                   2  -  No leading blank line,  1 trailing blank line
c                   3  -   1 leading blank line,  1 trailing blank line
c
      character  txtbuf(1:*)*(*)
      integer    nltext, blnkop
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and COMMON BLOCK decks
c
#include "ioctrl.inc"
#include "iolun.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer  lennb
c
c-----------------------------------------------------------------------
c
c ... Local Parameters and variables:
c
      character acont
      integer   l, lenlin, termln, ierr
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      if (luttyi .LT. 0) return
c
      if (blnkop.EQ.1 .OR. blnkop.EQ. 3) write(luttyo,'()')
      termln = 0
c
      do 200 l=1,nltext
        termln = termln + 1
c
c ..... Ensure output does not scroll off screen
c
        if (termln .EQ. ntlins-3) then
          write(luttyo,'()')
          call mdprmt (luttyo, 'CR to continue:')
          ierr = 0
          call getttyline(acont,ierr)
          write(luttyo,'()')
          termln = 1
        endif
c
        lenlin = max (1, lennb(txtbuf(l)))
        write(luttyo,'(1x,a)') txtbuf(l)(1:lenlin)
  200 continue
c
      if (blnkop.EQ.2 .OR. blnkop.EQ.3) write(luttyo,'()')
c
      return
      end
