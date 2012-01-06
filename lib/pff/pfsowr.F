      subroutine pfsowr (ovwrok,ierr)
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
c       - This routine is a SET routine to turn VERBOSE mode on and off.
c
c ----------------------------------------------------------------------
c
c     Input:
c       ovwrok  -  if true,  PFF will overwrite an existing file when
c                            opening it in WRITE_ONLY mode
c                  if false, attempting to open an existing file in 
c                            WRITE_ONLY mode will result in an error
c       ierr    -  if not zero,  return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                      0,   No error
c
c ----------------------------------------------------------------------
c
c     Internals:
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c
c ... Included common decks:
c ... pffils
c       w:    stwrop
c ... pferrs
c       r:    OFFLUN, ludbg
c       w:    luverb, vrbflg, v2dflg
c
#include "pfbsiz.inc"
#include "pferrs.inc"
#include "pffils.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   ierr
      logical   ovwrok
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFSOWR' )
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
c ... set value of write status string depending on overwrite flag value
c
      if ( ovwrok ) then
        stwrop = 'unknown'
      else
        stwrop = 'new'
      endif
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
