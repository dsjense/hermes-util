      subroutine pfdput ( fid, traw, tapp, lendir, locdir, st, sc,
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
c       - This routine is a DIRECTORY routine that puts a directory
c         entry into the memory-resident directory structure.
c       - The entry is placed at the current directory pointer position
c         for FID
c       - current directory pointer is incremented upon completion
c       - If the current directory pointer has the value DFAULT, nothing
c         is put into the structure and the pointer is not incremented
c
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file
c       lendir  -  length of dataset
c       locdir  -  pointer to dataset
c       sc      -  character string containing dataset comment
c       st      -  character string containing dataset type label
c       tapp    -  # associated with application dataset type
c       traw    -  # associated with raw dataset type
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                     0,   No error
c                     1,   Illegal file ID (FID)
c                     2,   File NOT open
c                     3,   Error retrieving string from NAME SERVER
c
c ----------------------------------------------------------------------
c
c     Internals:
c       iblk    -  the file's block #
c       iloc    -  the block offset for the current directory pointer
c       mapblk  -  the block # for this entry in the directory structure
c       type    -  pointer to raw dataset type name
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  DBLKSZ, MXFILS
c     pfetyp  -  PFNTYP
c     pfstfg  -  DFAULT, NOPEN
c
c ... Included common decks:
c ... pfbtyp
c       r:    rtypna(*)
c ... pfbufd
c       r:    dblkpr(*,*)
c       w:    dirinfo(*,*,*)
c ... pfbufm
c       r:    wrflag(*)
c       r/w:  dircnt(*), dircur(*)
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pfetyp.inc"
#include "pfstfg.inc"
#include "pfbtyp.inc"
#include "pfbufd.inc"
#include "pfbufm.inc"
#include "pferrs.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ierr, traw, tapp, lendir, locdir
      character st*(*), sc*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFDPUT' )
c
c     Internal
      integer   iblk, iloc, mapblk, type
c
c     External functions
      integer   NS_putname
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
c ... if the current directory pointer is set to DFAULT, then NO_OP
c
      if ( dircur(fid).eq.DFAULT ) goto 999
c
c ... find the block and offset for current directory entry
c
      iblk = (dircur(fid) - 1)/DBLKSZ
      iloc = dircur(fid) - DBLKSZ*iblk
c
c ... If this is the start of a new block that hasn't been allocated,
c ... need to allocate a block
c
      if ( iloc.eq.1 .AND. dircur(fid).gt.dircnt(fid) ) then
c
        call pfdabk ( fid, iblk, ierr )
        if ( ierr.ne.0 ) goto 999
c
      endif
c
c ... Get/Set block for this directory entry
c
      mapblk = dblkpr(iblk,fid)
c
c ... Now put all the directory entries in their actual storage location
c
      dirinfo(PDRAW,iloc,mapblk) = traw
      dirinfo(PDAPP,iloc,mapblk) = tapp
      dirinfo(PDLEN,iloc,mapblk) = lendir
      dirinfo(PDLOC,iloc,mapblk) = locdir
      dirinfo(PDTTL,iloc,mapblk) = NS_putname(sc)
      if (dirinfo(PDTTL,iloc,mapblk).le.0 ) then
        ierr = 3
        call pfserr ( PFMODU, ierr, fid,
     &                'Error retrieving string from NAME SERVER' )
        goto 999
      endif
c
      if ( st.eq.' ' ) then
c
        type = min ( traw, PFNTYP )
        if ( type.lt.0 ) type = PFNTYP
c
        dirinfo(PDTYP,iloc,mapblk) = NS_putname(rtypna(type))
c
      else
c
        dirinfo(PDTYP,iloc,mapblk) = NS_putname(st)
c
      endif
      if (dirinfo(PDTYP,iloc,mapblk).le.0 ) then
        ierr = 3
        call pfserr ( PFMODU, ierr, fid,
     &                'Error retrieving string from NAME SERVER' )
        goto 999
      endif
c
c
c ... if the # of entries for this file have been increased, increment
c ... the count; then advance the current directory pointer for file
c
      dircnt(fid) = max( dircnt(fid), dircur(fid) )
      dircur(fid) = dircur(fid) + 1
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
