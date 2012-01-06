      subroutine gnflin (
c ... INPUT/OUTPUT
     1 ierr )
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
c     GNFLIN gets a new full input line from the current input source.
c     The 'input line' consists of one or more 'sub-lines' connected
c     with continuation characters at the end of the active line (i.e.
c     part of a line BEFORE a comment character).
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT/OUTPUT:
c       ierr    -  INPUT:  Do nothing if ierr.NE.0
c                  OUTPUT: Error flag:-
c                            0  = normal return
c                            -1 = End-of-File on zero level input
c                            -2 = Error on read
c                            -3 = # continuation lines exceeded
c
      integer       ierr
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "iocmdf.inc"
#include "ioinln.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer      tioerh
c
c-----------------------------------------------------------------------
c
c ... Local variables:
c
      character*132 errmsg
      integer   frstln, locul, nl
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      if (ierr .LT. 0) then
        nuiln = 0
        return
      endif
c
c ... Initialize new line mode
c
      locul = 1
      nuiln = 0
c
c-----------------------------------------------------------------------
c ... Loop over input sub-lines
c-----------------------------------------------------------------------
c
      frstln = nuiln + 1
c
      do 100 nl=frstln,MXUILN
c
        call getsln (' ', MXICHL-locul+1, uinptl(locul:),
     1               uiltyp(nl), lenuil(nl), lnauil(nl), ierr)
c
c ..... END-OF-FILE on input
c
        if (ierr .EQ. -1) then
c
c ....... Write message if we were already building a command line
c
          if (nl.GT.1) ierr = tioerh ('TIO', 'GNFLIN', 2,
     1                          'EOF on continuation line', -1)
          return
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
c ..... Handle first sub-line 
c
        if (nl .EQ. 1) then
c
c ....... Record file line number of first line of command
c
          lnumc0(cmflvl) = lnumcf(cmflvl)
c
c ....... Return for stand-alone comment lines
c
          if (uiltyp(nl) .EQ. CMTLIN) then
            nuiln  = nl
            return
          endif
        endif
c
c ..... Return if we have a user line
c
        if (uiltyp(nl).EQ.USRLIN) then
          nuiln  = nl
          return
        endif
c
  100 continue
c
      write(errmsg,9810) MXUILN-1
      ierr = tioerh ('TIO', 'GNFLIN', 2, errmsg, -3)
      return
c
c
c=======================================================================
c ... ERROR messages
c=======================================================================
c
 9810 format('Exceeded maximum # continuation lines ',i3)
c
      end
