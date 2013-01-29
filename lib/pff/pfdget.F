      subroutine pfdget ( fid, den, traw, tapp, lendir, locdir, st, sc,
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
c     Summary:
c
c       - This routine is a DIRECTORY routine that gets a directory
c         entry from the memory-resident directory structure.
c       - If DEN = DFAULT, then the directory entry used will be the
c         file's current directory entry
c
c ----------------------------------------------------------------------
c
c     Input:
c       den     -  Directory entry Number -- if DFAULT, use current
c                  directory pointer (dircur(fid))
c       fid     -  File ID of PFF file
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       lendir  -  length of dataset
c       locdir  -  pointer to dataset
c       sc      -  character string for dataset comment
c       st      -  character string for dataset type label
c       tapp    -  # associated with application dataset type
c       traw    -  # associated with raw dataset type
c       ierr    -  error status flag;
c                     0,   No error
c                     1,   Illegal file ID (FID)
c                     2,   File NOT open
c                     3,   Illegal Directory Entry # (DEN)
c                     4,   Error retrieving string from NAME SERVER
c
c ----------------------------------------------------------------------
c
c     Internals:
c       iblk    -  the file's block #
c       iloc    -  the block offset for the current directory pointer
c       mapblk  -  the block # for this entry in the directory structure
c       tden    -  temporary directory entry #
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  DBLKSZ, MXFILS
c     pfstfg  -  DFAULT, NOPEN
c
c ... Included common decks:
c ... pfbufd
c       r:    dirinfo(*,*,*), dblkpr(*,*)
c ... pfbufm
c       r:    dircnt(*), dircur(*), wrflag(*)
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pfbufd.inc"
#include "pfbufm.inc"
#include "pferrs.inc"
c
#include "mdfpos.h"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   den, fid, ierr, traw, tapp, lendir
      HU_FPOS_TYPE locdir
      character st*(*), sc*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFDGET' )
c
c     Internal
      integer   iblk, iloc, mapblk, tden
c
c     External functions
      integer   NS_getname
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
c ... do we use the current pointer ??
c
      if ( den.eq.DFAULT ) then
        tden = dircur(fid)
      else
        tden = den
      endif
c
c ... Is the DEN legal ??
c
      if ( tden.lt.1 .OR. tden.gt.dircnt(fid) ) then
        ierr = 3
        call pfserr ( PFMODU, ierr, fid,
     &                'Illegal Directory Entry # (DEN)' )
        goto 999
      endif
c
c ... find the block and offset for current directory entry
c
      iblk = (tden - 1)/DBLKSZ
      iloc = tden - DBLKSZ*iblk
c
c ... map file block # to actual block used
c
      mapblk = dblkpr(iblk,fid)
c
c ... Now get all the directory entries from their actual location
c
      traw   = dirinfo(PDRAW,iloc,mapblk)
      tapp   = dirinfo(PDAPP,iloc,mapblk)
      lendir = dirinfo(PDLEN,iloc,mapblk)
      locdir = dirinfo(PDRLOC,iloc,mapblk)
      locdir = LBUF*locdir + dirinfo(PDOLOC,iloc,mapblk)
      ierr   = NS_getname(dirinfo(PDTTL,iloc,mapblk),sc)
      if ( ierr.ge.0 ) ierr = NS_getname(dirinfo(PDTYP,iloc,mapblk),st)
      if ( ierr.lt.0 ) then
        ierr = 4
        call pfserr ( PFMODU, ierr, fid,
     &                'Error retrieving string from NAME SERVER' )
        goto 999
      endif
      ierr = 0
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
