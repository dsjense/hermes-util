      subroutine pfurwd ( fid, ierr )
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
c       - This routine is a UTILITY routine to "rewind" a PFF file.
c         All buffer management pointers, etc. are reset to their proper
c         values.
c       - The file's status (RE, WR, or RW) remains the same UNLESS:
c            if in WR mode, the mode is reset to RW.
c       - If the file is in WR mode, "lstwrd" is set to current file
c         position.
c
c
c     Input:
c       fid     -  File ID of PFF file to be rewound.
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   Illegal PFF File ID
c                      2,   File Not Open
c
c ----------------------------------------------------------------------
c
c     Internals:
c       i       -  do loop index words in I/O buffer
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  LBUF, LFHDR, MXFILS
c     pfstfg  -  RW, WR, NOPEN, EOFFLG
c
c ... Included common decks:
c ... pfbufm
c       r:    locbuf(*)
c       w:    lstwrd(*), modrec(*), ubuf(*,*)
c       r/w:  wrflag(*)
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pfbufm.inc"
#include "pferrs.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ierr
c
c     Internal
      integer   i
      integer   pfutel
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFURWD' )
c
c ... External Block Data
      external  pffbd
c
c ----------------------------------------------------------------------
c
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
c ... is fid an illegal PFF File ID ?
c
      if(fid.lt.1 .or. fid.gt.MXFILS)then
c
        ierr = 1
        call pfserr ( PFMODU, ierr, fid, 'Illegal PFF File ID' )
        goto 999
c
      endif
c
c ... is file open ?
c
      if ( wrflag(fid).eq.NOPEN ) then
c
        ierr = 2
        call pfserr ( PFMODU, ierr, fid, 'File Not Open' )
        goto 999
c
      endif
c
c ... if in WR mode:  reset mode to Read-Write
c ...                 increment current record counter because READ and
c ...                    WRITE modes are one record out of sync
c ...                 save last word value
c ...                 fill rest of buffer with EOF flag
c ...                 set record-modified flag
c
      if ( wrflag(fid).eq.WR ) then
c
        wrflag(fid) = RW
        currec(fid) = currec(fid) + 1
        lstwrd(fid) = pfutel( fid, ierr )
        do 1 i=locbuf(fid)+1,LBUF
          ubuf(i,fid) = EOFFLG
    1   continue
        modrec(fid) = .TRUE.
c
      endif
c
c ... now go to location of 1st dataset
c
      call pfsdsp( fid, 1, ierr )
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
