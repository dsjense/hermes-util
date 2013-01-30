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
      integer  ierr, lnum, lplev
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
    1 continue
      lplev = looplv(cmflvl)
c
c ... Increment loop line pointer for this level. Note that this
c     automatically handles finishing a nested loop and getting the
c     next command at the lower level
c
      ilpln(cmflvl) = ilpln(cmflvl) + 1
c
c ... Last line of the loop is the '^endfor' line
c
      if (ilpln(cmflvl) .EQ. endlpl(lplev,cmflvl) ) then
c
c ..... Decrement iteration count
c
        nlpit(lplev,cmflvl) = nlpit(lplev,cmflvl) - 1
c
c ..... Handle loop if it is complete
c
        if (nlpit(lplev,cmflvl) .EQ. 0) then
          lplev = lplev - 1
          looplv(cmflvl) = lplev
c
c ....... Zero level: Handle END-OF-LOOP
c
          if (lplev .EQ. 0) then
            nlpln  = beglpl(1,cmflvl) - 1
            lstlch = lplche(nlpln)
            lnumcf(cmflvl) = lnumcx(cmflvl)
            status = 1
            return
c
c ....... Completed loop was nested loop. Reset to process
c         lower level loop
c
          else
            go to 1
          endif
c
c ..... Or reset to go through the loop again
c
        else
          loopv(lplev,cmflvl) = loopv(lplev,cmflvl) +
     1                          lpstep(lplev,cmflvl)
          call ljusti (loopv(lplev,cmflvl), 0,
     &      loopvc(lplev,cmflvl), nchlpv(lplev,cmflvl), ierr )
          ilpln(cmflvl) = beglpl(lplev,cmflvl) - 1
          go to 1
        endif
      endif
c
c ... Return loop line
c
      lnum   = ilpln(cmflvl) 
      nxtlpl = lplchr(lplchb(lnum):lplche(lnum))
      lnumcf(cmflvl) = lplnmf(lnum)
      status = 0
      return
c
      end
