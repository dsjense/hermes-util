      subroutine pfdfbk ( fid, iblk, ierr )
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
c     Summary:
c
c       - This routine returns (frees) a pointer to a directory block.
c         In static mode, it returns it back to the free directory block
c         list.
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file
c       iblk    -  the file's directory block #
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       ierr    -  error status flag;
c                    0,   No error
#ifdef STATIC_MEM
c                    1,   Illegal directory-block pointer
c                    2,   Attempt to free an unallocated directory-block
c                         pointer
#else
c                    1,   Error freeing Directory block
#endif
c
c     Internals:
#ifdef STATIC_MEM
c       dirblk  -  pointer to directory block that is to be freed
#endif
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
#ifdef STATIC_MEM
c     pfbsiz  -  DBLKMX
c     pfstfg  -  DFAULT
c
c ... Included common decks:
c ... pfbufd
c       r/w:  tpdblk, nfdblk, dblksk(*), dblkpr(*)
#else
c     pfbsiz
#endif
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#ifdef STATIC_MEM
# include "pfstfg.inc"
# include "pfbufd.inc"
#endif
#include "pferrs.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, iblk, ierr
c
c     Internal
#ifdef STATIC_MEM
      integer   dirblk
#endif
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFDFBK' )
c
c     External functions
#ifndef STATIC_MEM
      integer   free_dirblk
#endif
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
c ----------------------------------------------------------------------
c
#ifdef STATIC_MEM
c     Is this a legal block pointer ??
c
      dirblk = dblkpr(iblk,fid)
      if ( dirblk.lt.1 .OR. dirblk.gt.DBLKMX ) then
        ierr   = 1
        call pfserr ( PFMODU, ierr, 0,
     &                'Illegal directory-block pointer' )
        goto 999
      endif
c
c     Is this block allocated now ??
c
      if ( dblksk(dirblk).ne.DFAULT ) then
        ierr   = 2
        call pfserr ( PFMODU, ierr, 0,
     &       'Attempt to free an unallocated directory-block pointer' )
        goto 999
      endif
c
c ... put freed block pointer on top of stack
c
      dblksk(dirblk) = tpdblk
      tpdblk = dirblk
      dblkpr(iblk,fid) = 0
c
c ... increment free block count
c
      nfdblk = nfdblk + 1
#else
      if ( free_dirblk(iblk,fid).ne.0 ) then
        ierr = 1
        call pfserr (PFMODU, ierr, fid, 'Error freeing Directory block')
        goto 999
      endif
#endif
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
