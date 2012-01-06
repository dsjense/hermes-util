      character*(*) function nxtlpl (
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
c     NXTLPL returns the next line from the currently executing loop,
c     or flags the end of the loop.  Nested loops are handled
c     transparently by NXTLPL, which always returns the appropriate
c     line, until the outer loop is complete.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c       status  -  Return value:-
c                    0 - Normal return
c                    1 - End-of-loop -- no line returned
c
      integer status
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "iocmdf.inc"
#include "ioloop.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer  lennb
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      integer  ierr, lnum
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
    1 continue
      curlpl(looplv(cmflvl),cmflvl) = curlpl(looplv(cmflvl),cmflvl) + 1
c
c ... Handle last line of current loop: Note that the last line of a
c     loop is the "^endfor" line which does not need to be returned to
c     the calling program
c
      if (curlpl(looplv(cmflvl),cmflvl) .EQ.
     1    endlpl(looplv(cmflvl),cmflvl) ) then
c
c ..... Decrement iteration count
c
        nlpit(looplv(cmflvl),cmflvl) = nlpit(looplv(cmflvl),cmflvl) - 1
c
c ..... Handle loop if it is complete
c
        if (nlpit(looplv(cmflvl),cmflvl) .EQ. 0) then
          looplv(cmflvl) = looplv(cmflvl) - 1
c
c ....... Zero level: Handle END-OF-LOOP
c
          if (looplv(cmflvl) .EQ. 0) then
            nlpln  = beglpl(1,cmflvl) - 1
            lstlch = lplche(nlpln)
            lnumcf(cmflvl) = lnumcx(cmflvl)
            status = 1
            return
c
c ....... Completed loop was nested loop.  Reset to process
c         lower level loop
c
          else
            curlpl(looplv(cmflvl),cmflvl) =
     1      curlpl(looplv(cmflvl)+1,cmflvl)
            go to 1
          endif
c
c ..... Or reset to go through the loop again
c
        else
          loopv(looplv(cmflvl),cmflvl)  = loopv(looplv(cmflvl),cmflvl) +
     1                                    lpstep(looplv(cmflvl),cmflvl)
          call ljusti (loopv(looplv(cmflvl),cmflvl), 0,
     &      loopvc(looplv(cmflvl),cmflvl),
     &      nchlpv(looplv(cmflvl),cmflvl), ierr )
          curlpl(looplv(cmflvl),cmflvl) = beglpl(looplv(cmflvl),cmflvl)
     1                                    - 1
          go to 1
        endif
      endif
c
c ... Return loop line
c
      lnum   = curlpl(looplv(cmflvl),cmflvl) 
      nxtlpl = lplbuf(lplchb(lnum):lplche(lnum))
      lnumcf(cmflvl) = lplnmf(lnum)
      status = 0
      return
c
      end
