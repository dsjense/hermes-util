      subroutine pffscr ( irec, linsav, center, npoint, poifid, cptr,
     &                    ierr )
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
c
c     Summary:
c
c       - This routine is a FILE routine that scrolls some portion
c         of PFF's open file list to terminal
c       - Terminal characteristics (# of lines, # of columns, unit #)
c         are taken from common block PFTTY.
c
c ----------------------------------------------------------------------
c
c     Input:
c       center  -  center of scroll region
c       cptr    -  (*) 2-character strings containing the pointer text
c                  for the user-defined pointers
c       irec    -  center of scroll indicator:
c                  = -2,  scroll up (backward) one screen in list
c                  = -1,  scroll down (forward) one screen in list
c                  =  0,  center about current "center" value
c                  >  0,  center about irec
c       linsav  -  # of lines to save at end of list
c       npoint  -  # of user-defined FID pointers
c       poifid  -  (*) FIDs associated with user-defined pointers
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                     0,          No error
c                     otherwise,  error returned by called PFF routine
c
c ----------------------------------------------------------------------
c
c     Internals:
c       actsiz  -  actual # of file entries to be listed
c       hi      -  upper scroll limit
c       low     -  lower scroll limit
c       off     -  offset between pointer and 1st file entry
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  MXFILS
c
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pffils
c       r:    filcnt
c ... pftty
c       r:    luttyo, ttylin, ttycol
c
#include "pfbsiz.inc"
#include "pferrs.inc"
#include "pffils.inc"
#include "pftty.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   center, ierr, irec, linsav, npoint
      integer   poifid(*)
      character cptr(*)*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFFSCR' )
      integer   DHEADR
      parameter ( DHEADR = 5 )
c
c ... Internal
      integer   actsiz, hi, low, off
c
c
c ----------------------------------------------------------------------
c
c ... write status message if DEBUG is on
c
      if (ludbg.ne.OFFLUN)then
        write (ludbg,1000) PFMODU, 'Entry', ierr
      endif
c
c ... return if error flag is not zero on entry
c
      if(ierr.ne.0)then
        call pfserr ( PFMODU, 0, 0, 'Error Flag Non-zero on Entry' )
        goto 999
      endif
c
c ----------------------------------------------------------------------
c
c
c ... set menu scroll length
c
      actsiz = ttylin - DHEADR - linsav
c
      if(irec.eq.-2)then
        center = max(1,center-actsiz)
      else if(irec.eq.-1)then
        center = min(MXFILS,filcnt,center+actsiz)
      else if(irec.gt.0)then
        center = max(1,min(MXFILS,filcnt,irec))
      endif
c
c
      off = (actsiz - 1)/2
      low = max( 1, center - off )
      hi  = min( MXFILS, filcnt, low + actsiz - 1 )
      low = max( 1, hi - actsiz + 1 )
c
      call pffprt ( luttyo, ttycol, low, hi, npoint, poifid, cptr,
     &              ierr )
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
