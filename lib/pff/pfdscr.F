      subroutine pfdscr ( fid, irec, pmode, linsav, point, poidir, cptr,
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
c       - This routine is a DIRECTORY routine that scrolls some portion
c         of a PFF file's directory structure to terminal
c       - Terminal characteristics (# of lines, # of columns, unit #)
c         are taken from common block PFTTY.
c
c ----------------------------------------------------------------------
c
c     Input:
c       cptr    -  (*) 2-character strings containing the pointer text
c                  for the user-defined pointers  (used ONLY if
c                  "pmode" > 0)
c       fid     -  File ID of PFF file
c       irec    -  center of scroll indicator:
c                  = -2,  scroll up (backward) one screen in directory
c                  = -1,  scroll down (forward) one screen in directory
c                  =  0,  center about current pointer value
c                  >  0,  center about irec
c       linsav  -  # of lines to save at end of list
c       pmode   -  pointer mode -- indicates how pointer symbol ('->')
c                  is to be used on output listing:
c                     0,  suppress pointer symbol
c                    -1,  tie pointer symbol to current dataset entry
c                    -2,  tie pointer symbol to scroll pointer ("point")
c                   > 0,  use "pmode" user-supplied directory pointers
c       poidir  -  (*) directory #s associated with user-defined
c                  pointers (used ONLY if "pmode" > 0)
c       point   -  input value of scroll pointer
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       point   -  output value of scroll pointer;
c                                    point = center of directory list
c       ierr    -  error status flag;
c                     0,   No error
c                     1,   Illegal file ID (FID)
c                     2,   File NOT open
c
c ----------------------------------------------------------------------
c
c     Internals:
c       actsiz  -  actual # of directory entries to be listed
c       dptr    -  dataset tied to pointer symbol
c       hi      -  upper scroll limit
c       low     -  lower scroll limit
c       npoint  -  # of directory pointers passed to PFDPRT
c       off     -  offset between pointer and 1st directory entry
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  MXFILS
c     pfstfg  -  NOPEN
c
c ... Included common decks:
c ... pfbufm
c       r:    dircnt(*), wrflag(*)
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pftty
c       r:    luttyo, ttylin, ttycol
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pfbufm.inc"
#include "pferrs.inc"
#include "pftty.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ierr, irec, linsav, pmode, point
      integer   poidir(*)
      character cptr(*)*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFDSCR' )
      integer   DHEADR
      parameter ( DHEADR = 5 )
c
c ... Internal
      integer   actsiz, dptr, dptr_arr(1), hi, low, npoint, off
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
c ... is this a legal FID ??
c
      if ( fid.lt.1 .OR. fid.gt.MXFILS ) then
        ierr = 1
        call pfserr ( PFMODU, ierr, fid, 'Illegal file ID (FID)' )
        goto 999
      endif
c
c ... is the file open ??
c
      if ( wrflag(fid).eq.NOPEN ) then
        ierr = 2
        call pfserr ( PFMODU, ierr, fid, 'File NOT open' )
        goto 999
      endif
c
c ... set menu scroll length
c
      actsiz = ttylin - DHEADR - linsav
c
      point = max(1,min(dircnt(fid)+1,point))
c
      if(irec.eq.-2)then
        point = max(1,point-actsiz)
      else if(irec.eq.-1)then
        point = min(dircnt(fid)+1,point+actsiz)
      else if(irec.gt.0)then
        point = max(1,min(dircnt(fid)+1,irec))
      endif
c
      if (pmode.eq.-1) then
        npoint = 0
      else if (pmode.eq.-2) then
        npoint = 1
        dptr = point
      else if (pmode.le.0) then
        npoint = 1
        dptr = 0
      endif
c
      off = (actsiz - 1)/2
      low = max( 1, point - off )
      hi  = min( dircnt(fid) + 1, low + actsiz - 1 )
      low = max( 1, hi - actsiz + 1 )
c
      if ( pmode.gt.0 ) then
        call pfdprt ( luttyo, fid, ttycol, low, hi, pmode, poidir,
     &                cptr, ierr )
      else
        dptr_arr = dptr
        call pfdprt ( luttyo, fid, ttycol, low, hi, npoint, dptr_arr,
     &                '->', ierr )
        dptr = dptr_arr(1)
      endif
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
