      subroutine mdgpsz(paglen)
c
c **********************************************************************
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
c **********************************************************************
c
c     Summary:
c
c     Machine-dependent routine to return # of lines on terminal display
c
c
c     **********************  UNIX Version  ****************************
c
c     The UNIX version attempts to translate the LINES environment 
c     variable.  Failing that, it returns a default value.
c
c ----------------------------------------------------------------------
c
c     Input:  
c       NONE
c
c     Output:  
c       paglen  -  # of lines on terminal  (PAGE terminal characteristic)
c
c     Internals:  
c       ierr    -  status indicator for system call
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks: NONE
c
c ... Included common decks:  NONE
c
c ----------------------------------------------------------------------
c
c     Declare variables:  
c
c     Passed:
      integer   paglen
c
c ... Internal:
      integer   ierr
c
c     UNIX specific internals:
c       i       -  loop index
c       ic      -  ASCII equivalent of character
c       lines   -  value of LINES environment variable
c
c ... UNIX specific Parameters:
      integer   DEFPAG
      parameter ( DEFPAG = 24 )
c
      integer   i, ic
      character lines*8
      integer   fgeten, lstchr
c
c ----------------------------------------------------------------------
c
c ... try to translate LINES, if error or blank, use default
c
      ierr = fgeten('LINES',lines)
      if ( ierr.ne.0 ) goto 2
      if ( lines.eq.' ' ) goto 2
c
c ... attempt to decode value of LINES as an integer, otherwise use default
c
      paglen = 0
      do 1 i=1,lstchr(lines)
        ic = ichar(lines(i:i)) - 48
        if ( ic.lt.0 .OR. ic.gt.9) goto 2
        paglen = 10*paglen + ic
    1 continue
c
        return
c
    2 continue
c
        paglen = DEFPAG
c
      return
c
      end
