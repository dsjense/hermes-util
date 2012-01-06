      subroutine pfuwbk (fid, offset, nw, iarr, ierr)
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
c       - This routine is a UTILITY routine to write over a portion of
c         a PFF file that has been previously written to.  The overwrite
c         may extend over multiple file records.  There are two possible
c         cases:
c           1)  overwrite of record that has already been written to
c               disk -- if partial record, old record must be read from
c               disk into buffer, new values placed in buffer, and
c               entire buffer written back to disk.  If full record,
c               old record does not have to be read back in.
c           2)  overwrite of partial record that is currently in buffer
c               and has not yet gone to disk -- simply overwrite I/O
c               buffer.  If in Read/Write mode, also need to set MODREC.
c       - This routine reads and writes 16-bit signed integers (-32768
c         to 32767).  All data is read/written from/to files in fixed
c         length records.  The buffer is packed to 16-bit and written
c         to an ANSI-standard direct access file by the machine-dependent
c         routine PFMIO.  The buffer is read from the file and then
c         unpacked by the machine-dependent routine PFMIO.
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file for i/o operation
c       offset  -  Offset position in file, in 16-bit words
c       nw      -  # of integers to be overwritten
c       iarr    -  array of integers to be written over old file contents
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                   0,   No error
c                   1,   Illegal File ID (FID)
c                   2,   File NOT Open
c                   3,   Attempt to Overwrite File Open in READONLY Mode
c                   4,   Attempt to Overwrite File Outside Its Extent
c                   5,   EOF Encountered While Trying to Overwrite Data
c
c ----------------------------------------------------------------------
c
c     Internals:
c       crec    -  current active record # on FID
c       floc    -  file address of final overwrite location
c       i       -  do loop index over integer array
c       i1      -  initial pointer in current buffer
c       i2      -  final pointer in current buffer
c       ie      -  final pointer in extended/virtual current buffer
c       ipu     -  offset between input array and work buffer
c       ir      -  loop index over I/O records
c       ireci   -  initial record # of overwrite
c       irecf   -  final record # of overwrite
c       lloc    -  current file offset address on FID
c       locoff  -  record/buffer offset of first overwrite location
c       wbkbuf  -  (*)  temporary work buffer
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  LBUF, MXFILS
c     pfstfg  -  RE, WR, RW, NOPEN
c
c ... Included common decks:
c ... pfbufm
c       r:    currec(*), locbuf(*), wrflag(*), lstwrd(*)
c       w:    modrec(*), ubuf(*,*)
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
      integer   fid,offset,nw,iarr(nw),ierr
c
c     Internal
      integer   crec,floc,i1,i2,ie,i,ireci,irecf,ipu,ir,lloc,locoff
      integer   wbkbuf(LBUF)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFUWBK' )
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
c ----------------------------------------------------------------------
c
c     return if # of words is not positive
c
      if(nw.le.0)goto 999
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
c     is file open ?
c
      if(wrflag(fid).eq.NOPEN)then
        ierr = 2
        call pfserr ( PFMODU, ierr, fid, 'File NOT Open' )
        goto 999
      endif
c
c ... is file open for write (WR or RW) ?
c
      if(wrflag(fid).eq.RE)then
        ierr = 3
        call pfserr ( PFMODU, ierr, fid,
     &              'Attempt to Overwrite File Open in READONLY Mode' )
        goto 999
      endif
c
c ... Compute current file address and Record #
c ... Remember that CURREC is the # of records already WRITTEN to file
c
      if (wrflag(fid).eq.WR) then
        crec = currec(fid) + 1
        lloc = currec(fid)*LBUF + locbuf(fid)
      else
        crec = currec(fid)
        lloc = lstwrd(fid)
      endif
c
c ... compute overwrite final file address
c
      floc = offset + nw
c
c ... Is overwrite out of current file extent ??
c
      if( offset.lt.0 .OR. floc.gt.lloc )then
        ierr = 4
        call pfserr ( PFMODU, ierr, fid,
     &              'Attempt to Overwrite File Outside Its Extent')
        goto 999
      endif
c
c ... find record limits for initial and final points
c
      ireci = offset/LBUF + 1
      irecf = (floc - 1)/LBUF + 1
c
c ... find offset of first position in initial record
c
      locoff = offset - (ireci - 1)*LBUF
c
c ... get first and last address in virtual buffer and truncate last
c ... address to actual buffer
c
      i1 = locoff + 1
      ie = locoff + nw
      i2 = min(ie,LBUF)
c
c ... compute array-buffer offset
c
      ipu = -locoff
c
c ... loop over all records involved in overwrite
c
      do 3 ir=ireci,irecf
c
        if (ir.eq.crec) then
c
c ....... overwrite is in current active (unwritten) buffer --
c ....... transfer overwrite directly to I/O buffer
c
          do 1 i=i1,i2
            ubuf(i,fid) = iarr(i+ipu)
    1     continue
c
          if (wrflag(fid).eq.RW) modrec(fid) = .TRUE.
c
        else
c
c ....... overwrite is in record already dumped to file
c
          if (i1.eq.1 .AND. i2.eq.LBUF) then
c
c ......... overwrite of full buffer; don't need to get old contents
c
            call pfmio ( WR, fillun(fid), ir, iarr(1+ipu), ierr )
c
          else
c
c ......... pull old record values from disk to work buffer
c
            call pfmio ( RE, fillun(fid), ir, wbkbuf, ierr )
c
            if(ierr.eq.-1)then
c
c ........... process EOF error and return
c
              ierr = 5
              call pfserr ( PFMODU, ierr, fid,
     &                 'EOF Encountered While Trying to Overwrite Data')
              goto 999
c
            else if(ierr.ne.0)then
c
c ........... return if PFMIO call returns non-EOF error
c
              goto 999
c
            endif
c
c ......... overwrite data in the work buffer
c
            do 2 i=i1,i2
              wbkbuf(i) = iarr(i+ipu)
    2       continue
c
c ......... overwrite modified data in work buffer back to disk
c
            call pfmio ( WR, fillun(fid), ir, wbkbuf, ierr )
c
          endif
c
c ....... return if PFMIO call returns an error
c
          if(ierr.ne.0) goto 999
c
        endif
c
c ..... increment array-buffer offset
c ......recompute limits for next buffer
c
        ipu = ipu + LBUF
        i1  = 1
        ie  = ie - LBUF
        i2  = min(LBUF,ie)
c
    3 continue
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
