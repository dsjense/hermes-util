      subroutine pfstty ( lui, luo, lines, cols, ierr )
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
c
c ----------------------------------------------------------------------
c
c     Summary:
c
c       - This routine is a SET routine to set the terminal
c         characteristics
c
c ----------------------------------------------------------------------
c
c     Input:
c       cols    -  # of columns on terminal display
c       lui     -  logical unit # of terminal input device
c       luo     -  logical unit # of terminal output device
c       lines   -  # of lines on terminal display
c       ierr    -  if not zero,  return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:  NONE
c
c ----------------------------------------------------------------------
c
c     Internals:  NONE
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pftty
c       w:    luttyi, luttyo, ttylin, ttycol
c
#include "pfbsiz.inc"
#include "pferrs.inc"
#include "pftty.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   lui, luo, lines, cols, ierr
c
c     Internal
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFSTTY' )
c
c ... External Block Data
      external  pffbd
c
c ----------------------------------------------------------------------
c
c     write status message if DEBUG is on
c
      if (ludbg.ne.OFFLUN)then
        write (ludbg,1000) PFMODU, 'Entry', ierr
      endif
c
c     return if error flag is not zero on entry
c
      if(ierr.ne.0)then
        call pfserr ( PFMODU, 0, 0, 'Error Flag Non-zero on Entry' )
        goto 999
      endif
c
c ----------------------------------------------------------------------
c
c ... set common block varibles to supplied input values
c
      luttyi = lui
      luttyo = luo
      ttylin = lines
      ttycol = cols
c
c ----------------------------------------------------------------------
c
  999 continue
c
c     write status message if DEBUG is on and return
c
      if (ludbg.ne.OFFLUN)then
        write (ludbg,1000) PFMODU, 'Exit', ierr
      endif
c
      return
c
 1000 format(' %PFF-',a,'-DBG:  ',a,' Error Status = ',i5)
c
      end
