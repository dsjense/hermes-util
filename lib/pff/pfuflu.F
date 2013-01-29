      subroutine pfuflu (fid, ierr)
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
c       - This routine is a UTILITY routine that flushes the current
c         output buffer  for the PFF file "fid".
c       - This operation is ONLY ALLOWED in WRITE mode !!!
c
c ----------------------------------------------------------------------
c
c     Input:
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
c                      3,   Unit in NOT in WRITE mode
c
c ----------------------------------------------------------------------
c
c     Internals:
c       floc    -  file address of final overwrite location
c       i       -  loop index over padded portion of output buffer
c       locp    -  buffer pointer to 1st word of EOF padding
c       lrec    -  file record # associated with last word in file
c       pad     -  logical flag indicating if EOF padding is needed
c       rec     -  file record # associated with current active buffer
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  LBUF, MXFILS
c     pfstfg  -  RW, WR, NOPEN, EOFFLG
c
c ... Included common decks:
c ... pfbufm
c       r:    currec(*), fillun(*), lstwrd(*), locbuf(*), wrflag(*)
c       r/w:  modrec(*), ubuf(*,*)
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
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
      integer   fid, ierr
c
c     Internal
      integer   i, locp, lrec, rec
      HU_FPOS_TYPE floc
      logical   pad
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFUFLU' )
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
c ... is file open ?
c
      if(wrflag(fid).eq.NOPEN)then
c
        ierr = 2
        call pfserr ( PFMODU, ierr, fid, 'File NOT Open' )
        goto 999
c
      endif
c
c ... If in WRITEONLY mode, we are at end-of-file
c
      if ( wrflag(fid).eq.WR ) then
c
c ..... is anything in the buffer?  If not, do nothing
c
        if ( locbuf(fid).le.0 ) goto 999
c
c ..... find record # and see if EOF padding is needed
c
        rec  = currec(fid) + 1
c
        if ( ubuf(locbuf(fid)+1,fid).ne.EOFFLG ) then
          pad  = .TRUE.
          locp = locbuf(fid) + 1
        else
          pad  = .FALSE.
        endif
c
c ... If in READ/WRITE mode, need to worry about more things
c
      else if ( wrflag(fid).eq.RW ) then
c
c ..... have we modified the current buffer?  If not, do nothing
c
        if ( .NOT. modrec(fid) ) goto 999
c
c ..... find record # and see if EOF padding is needed
c
        rec  = currec(fid)
c
        lrec = lstwrd(fid)/LBUF + 1
        if ( lrec.eq.rec .AND. ubuf(locbuf(fid)+1,fid).ne.EOFFLG ) then
          pad  = .TRUE.
          floc = rec - 1
          locp = lstwrd(fid) - floc*LBUF + 1
        else
          pad  = .FALSE.
        endif
c
      else
c
c     READONLY OPERATION
c
c ..... Can't flush buffer of a file in READONLY mode
c
        ierr = 3
        call pfserr ( PFMODU, ierr, fid, 'Unit in NOT in WRITE mode' )
c
      endif
c
c ... if necessary, pad to end of buffer with EOF flag
c
      if ( pad ) then
c
        do 1 i=locp,LBUF
          ubuf(i,fid) = EOFFLG
    1   continue
c
      endif
c
c ... write out current record; turn off modified-record flag
c
      call pfmio(WR, fillun(fid), rec, ubuf(1,fid), ierr )
      modrec(fid) = .FALSE.
c
c ... force system to flush this unit
c
      call pfmflu( fillun(fid), ierr )
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
