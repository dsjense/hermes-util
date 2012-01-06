      subroutine pfgfin ( fid, rwmode, fname, count, dspntr, ierr )
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
c       - This routine is a GET routine to find misc. file info
c         associated with a user-supplied  FID.
c
c     Input:
c       fid     -  File ID of PFF file
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       count   -  # of datasets in file
c       dspntr  -  current dataset pointer
c       fname   -  file name
c       rwmode  -  Read/Write Mode (RE, RW, WR)
c       ierr    -  error status flag;
c                      0,   No error
c                      1,  Illegal File ID (FID)
c                      2,  File NOT Open
c
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
c     pfbsiz  -  MXFILS, MCHTTL
c     pfstfg  -  NOPEN
c
c ... Included common decks:
c ... pfbufm
c       r:    dircnt(*),dircur(*),wrflag(*)
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pffils
c       r:    files(*)
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pfbufm.inc"
#include "pferrs.inc"
#include "pffils.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   count, dspntr, fid, rwmode, ierr
      character fname*(*)
c
c     Internal
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFGFIN' )
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
c ... is FID legal ?
c
      if( fid.lt.1 .OR. fid.gt.MXFILS )then
c
        ierr = 1
        call pfserr ( PFMODU, ierr, fid,
     &                             'Illegal File ID (FID)' )
        goto 999
c
      endif
c
c ... is file open ?
c
      if(wrflag(fid).eq.NOPEN)then
c
        ierr = 2
        call pfserr ( PFMODU, ierr, fid, 'Input File NOT Open' )
        goto 999
c
      endif
c
      count  = dircnt(fid)
      dspntr = dircur(fid)
      rwmode = wrflag(fid)
      fname  = files(fid)
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
