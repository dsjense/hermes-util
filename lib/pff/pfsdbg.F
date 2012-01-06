      subroutine pfsdbg (lun,ierr)
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
c       - This routine is a SET routine to turn DEBUG mode on and off.
c
c ----------------------------------------------------------------------
c
c     Input:
c       lun     -  if lun = OFFLUN,    turn off DEBUG mode
c                  otherwise,          logical unit # for DEBUG output;
c                                        turn on DEBUG mode
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   DEBUG File NOT Open
c                      2,   Error Writing to DEBUG File
c
c ----------------------------------------------------------------------
c
c     Internals:
c       lopen   -  logical open status of DEBUG file
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
c       r:    OFFLUN, luverb
c       w:    ludbg, vrbflg, v2dflg
c
#include "pfbsiz.inc"
#include "pferrs.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   lun, ierr
c
c     Internal
      logical   lopen
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFSDBG' )
c
c ... External Block Data
      external  pffbd
c
c ----------------------------------------------------------------------
c
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
c ... if status has changed, check to see if file is open and write
c ... message to debug file
c
      if (lun.ne.OFFLUN .AND. ludbg.eq.OFFLUN) then
        inquire(unit=lun,opened=lopen)
        if (.not.lopen) goto 10
c
        write(lun,2000,err=20) lun
c
      else if (lun.eq.OFFLUN .AND. ludbg.ne.OFFLUN) then
        inquire(unit=ludbg,opened=lopen)
        if (.not.lopen) goto 10
c
        write(ludbg,3000,err=20)
c
      endif
c
c ... if no errors, set new value for ludbg; compute logical flags
c
      ludbg  = lun
      vrbflg = ludbg.ne.OFFLUN  .OR.   luverb.ne.OFFLUN
      v2dflg = ludbg.ne.OFFLUN  .AND.  ludbg.ne.luverb
c
      goto 999
c
   10 continue
c
c ..... process DEBUG file not open error
c
        ierr = 1
        call pfserr ( PFMODU, ierr, 0, 'DEBUG File NOT Open' )
        goto 999
c
   20 continue
c
c ..... process DEBUG file write error
c
        ierr = 2
        call pfserr ( PFMODU, ierr, 0, 'Error Writing to DEBUG File')
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
 2000 format(' %PFF-PFSDBG-DBG:  DEBUG Mode ON -- Unit =',i5)
 3000 format(' %PFF-PFSDBG-DBG:  DEBUG Mode OFF')
c
      end
