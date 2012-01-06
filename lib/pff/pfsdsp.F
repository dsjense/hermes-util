      subroutine pfsdsp (fid, den, ierr)
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
c       - This routine is a SET routine that sets the current directory
c         pointer to "den" for the PFF file with ID "fid".  The PFF file
c         itself is positioned to the FWA address of the dataset
c         indicated by "den".
c       - In the special case where "den" is zero, leave the current
c         directory pointer as is, but insure that the file is
c         positioned at the FWA of that dataset.
c
c ----------------------------------------------------------------------
c
c     Input:
c       den     -  directory entry # -- points to selected dataset
c                  IF den = 0, use currently selected directory entry
c       fid     -  File ID of PFF file
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   Illegal File ID (FID)
c                      2,   File NOT Open
c                      3,   Unit in NOT in READ mode
c                      4,   Illegal directory entry (DEN)
c
c ----------------------------------------------------------------------
c
c     Internals:
c       iblk    -  the file's block #
c       iloc    -  the block offset for the current directory pointer
c       locwrd  -  word address to position PFF file pointer so that
c                  next word returned by PFUSIO corresponds to "locwrd+1"
c       tden    -  temporary directory entry #
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  DBLKSZ, LFHDR, MXFILS
c     pfstfg  -  NOPEN
c
c ... Included common decks:
c ... pfbufd
c       r:    dirinfo(*,*,*), dblkpr(*,*)
c ... pfbufm
c       r:    dircnt(*), wrflag(*)
c       r/w:  dircur(*)
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pfbufd.inc"
#include "pfbufm.inc"
#include "pferrs.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   den, fid, ierr
c
c     Internal
      integer   iblk, iloc, locwrd, mapblk, tden
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFSDSP' )
c
c ... External Block Data
      external  pffbd
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
c ... is FID legal ?
c
      if( fid.lt.1 .OR. fid.gt.MXFILS )then
c
        ierr = 1
        call pfserr ( PFMODU, ierr, fid, 'Illegal File ID (FID)' )
        goto 999
c
      endif
c
c ... is file open and in RE or RW state ?
c
      if(wrflag(fid).eq.NOPEN)then
c
        ierr = 2
        call pfserr ( PFMODU, ierr, fid, 'File NOT Open' )
        goto 999
c
      else if(wrflag(fid).eq.WR)then
c
        ierr = 3
        call pfserr ( PFMODU, ierr, fid, 'Unit in NOT in READ mode' )
        goto 999
c
      endif
c
c ... is DEN legal ?
c
      if( den.lt.0 .OR. den.gt.dircnt(fid)+1)then
c
        ierr = 4
        call pfserr ( PFMODU, ierr, fid,
     &                'Illegal directory entry (DEN)' )
        goto 999
c
      else if (den.ne.0) then
c
c ..... set current directory pointer for this file
c
        dircur(fid) = den
c
      endif
c
      tden = min( dircur(fid), dircnt(fid) )
c
      if (tden.le.0) then
c
c ..... the file is empty, go to end of header
c
        locwrd = LFHDR
c
      else
c
c ..... find the block and offset for current directory entry
c
        iblk = (tden - 1)/DBLKSZ
        iloc = tden - DBLKSZ*iblk
c
c ..... set pointer to FWA of dataset
c
        mapblk = dblkpr(iblk,fid)
c
        locwrd = dirinfo( PDLOC, iloc, mapblk )
c
c ..... Is request for End-Of-Data (EOD); if so, position to EOD
c
        if ( dircur(fid).eq.dircnt(fid)+1 ) then
          locwrd = locwrd + dirinfo( PDLEN, iloc, mapblk )
        endif
c
      endif
c
c ... Now position file
c
      call pfusek ( fid, locwrd, ierr )
c
c ----------------------------------------------------------------------
c
  999 continue
c
c ... write status message if DEBUG is on and return
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
