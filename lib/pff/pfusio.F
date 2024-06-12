      subroutine pfusio (fid, iop, nw, iarr, ierr)
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
c       - This routine is a UTILITY routine to perform sequential I/O to
c         a PFF file.
c       - This routine reads and writes 16-bit signed integers (-32768
c         to 32767).  All data is internally buffered and read/written
c         from/to files buffer by buffer in fixed length records.  Full
c         buffers are packed to 16-bit and written to an ANSI-standard
c         direct access file by the machine-dependent routine PFMIO.
c         Buffers are read from the file and then unpacked by the
c         machine-dependent routine PFMIO.
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file
c       iop     -  operation code, 0 for read, otherwise write
c       nw      -  # of integers to be read/written
c       iarr    -  array of integers to be read if IOP = RE
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       iarr    -  array of integers to be written if IOP = WR
c       ierr    -  error status flag;
c                    0,   No error
c                    1,   Illegal File ID (FID)
c                    2,   File NOT Open
c                    3,   Attempt to Read File Open in WRITE Mode
c                    4,   Attempt to Read Past End-Of-File
c                    5,   EOF Encountered While Trying to Read Data
c                    6,   Attempt to Write to File Open in READONLY Mode
c
c ----------------------------------------------------------------------
c
c     Internals:
c       dftarr  -  (*) array of three default integers
c       floc    -  temporary file offet value
c       i       -  do loop index over integer array
c       i1      -  initial pointer in buffer
c       i2      -  final pointer of array in extended buffer
c       ipu     -  offset in user-supplied array for full-buffer transfer
c       ir      -  loop index over I/O records
c       irecs   -  # of full buffers to be read/written
c       l       -  final pointer of array in current buffer
c       nowwrd  -  current file address (in 2-byte words)
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  LBUF, MXFILS
c     pfstfg  -  DFAULT, RE, WR, NOPEN
c
c ... Included common decks:
c ... pfbufm
c       r:    fillun(*), wrflag(*)
c       w:    extflg(*)
c       r/w:  currec(*), locbuf(*), ubuf(*,*), eofre(*),
c             modrec(*), lstwrd(*)
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
      integer   fid,iop,nw,iarr(*),ierr
c
c     Internal
      integer   i1,i2,l,i,irecs,ipu,ir
      HU_FPOS_TYPE floc, nowwrd
      integer   dftarr(3)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFUSIO' )
c
c ... External Block Data
      external  pffbd
c
      data dftarr / 3*DFAULT /
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
      if(iop.eq.RE)then
c
c     READ OPERATION
c
c       is file open for read ?
c
        if(wrflag(fid).eq.WR)then
          ierr = 3
          call pfserr ( PFMODU, ierr, fid,
     &                  'Attempt to Read File Open in WRITE Mode' )
          goto 999
        endif
c
        if(eofre(fid))then
          ierr = 4
          call pfserr ( PFMODU, ierr, fid,
     &                  'Attempt to Read Past End-Of-File' )
          goto 999
        endif
c
c       get first and last address in virtual buffer and truncate last
c       address to actual buffer
c
        i1 = locbuf(fid) + 1
        i2 = locbuf(fid) + nw
        l  = min(i2,LBUF)
c
c       get what we need that's in current buffer
c
        do 1 i=i1,l
          iarr(i-locbuf(fid)) = ubuf(i,fid)
    1   continue
c
c       if buffer is not exhausted, then return
c
        if(l.lt.LBUF) then
           locbuf(fid)=l
           goto 999
        end if
c
c ..... if current record has been modified, need to write it out
c ..... before reading in a new one (RW only)
c
        if (modrec(fid)) then
          call pfmio ( WR, fillun(fid), currec(fid), ubuf(1,fid), ierr )
          modrec(fid) = .FALSE.
        endif
c
c       find out how many full buffer's worth are left in request,
c       read them directly without using the buffer.
c
        irecs = i2/LBUF
        ipu = LBUF-locbuf(fid)
c
        do 2 ir=2,irecs
c
          currec(fid) = currec(fid) + 1
c
          call pfmio ( RE, fillun(fid), currec(fid), iarr(ipu+1), ierr )
c
          if(ierr.eq.-1)then
            eofre(fid) = .TRUE.
            ierr = 5
            call pfserr ( PFMODU, ierr, fid,
     &                  'EOF Encountered While Trying to Read Data')
            locbuf(fid) = 0
            goto 999
          else if(ierr.ne.0)then
            goto 999
          endif
c
          ipu = ipu + LBUF
    2   continue
c
c       read in next buffer full
c
        currec(fid) = currec(fid) + 1
c
        call pfmio ( RE, fillun(fid), currec(fid), ubuf(1,fid), ierr )
c
        if(ierr.eq.-1)then
          eofre(fid) = .TRUE.
          if(nw-ipu.gt.0)then
            ierr = 5
            call pfserr ( PFMODU, ierr, fid,
     &                  'EOF Encountered While Trying to Read Data')
            locbuf(fid) = 0
            goto 999
          endif
          ierr = 0
        else if(ierr.ne.0)then
          goto 999
        endif
c
c       reset buffer pointer and transfer from buffer any partial
c       buffer amount that remains
c
        locbuf(fid) = nw - ipu
c
        do 3 i=1,locbuf(fid)
          iarr(i+ipu) = ubuf(i,fid)
    3   continue

      else
c
c     WRITE OPERATION
c
c       is file open for write (WR or RW)
        if( wrflag(fid).eq.RE )then
          ierr = 6
          call pfserr ( PFMODU, ierr, fid,
     &                'Attempt to Write to File Open in READONLY Mode' )
          goto 999
        endif
c
c       get first and last address in virtual buffer and truncate last
c       address to actual buffer
c
        i1 = locbuf(fid) + 1
        i2 = locbuf(fid) + nw
        l  = min(i2,LBUF)
c
c       put into current buffer as much as will fit
c
        do 11 i=i1,l
          ubuf(i,fid) = iarr(i-locbuf(fid))
   11   continue
c
c       if buffer is full, need to keep going
c
        if(l.eq.LBUF) then
c
c         write out current (now full) buffer
c
          if ( wrflag(fid).eq.WR ) currec(fid) = currec(fid) + 1
c
          call pfmio ( WR, fillun(fid), currec(fid), ubuf(1,fid), ierr )
c
          if(ierr.ne.0) goto 999
c
c         find out how many full buffer's worth are left in request,
c         write them directly without using the buffer.
c
          irecs = i2/LBUF
          ipu = LBUF - locbuf(fid)
c
          do 12 ir=2,irecs
c
            currec(fid) = currec(fid) + 1
c
            call pfmio(WR, fillun(fid), currec(fid), iarr(ipu+1), ierr )
c
            if(ierr.ne.0) goto 999
c
            ipu = ipu + LBUF
   12     continue
c
c ....... if READ/WRITE mode, need to load in next record
c
          if ( wrflag(fid).eq.RW ) then
            currec(fid) = currec(fid) + 1
            call pfmio(RE, fillun(fid), currec(fid), ubuf(1,fid), ierr )
            if(ierr.eq.-1)then
c ........... if past EOF, that's okay
              ierr = 0
            else if(ierr.ne.0)then
              ierr = 5
              call pfserr ( PFMODU, ierr, fid,
     &                     'EOF Encountered While Trying to Read Data')
              locbuf(fid) = 0
              goto 999
            endif
          endif
c
c         reset buffer pointer and transfer to buffer any partial
c         buffer amount that remains
c
          locbuf(fid) = nw - ipu
c
          do 13 i=1,locbuf(fid)
            ubuf(i,fid) = iarr(i+ipu)
   13     continue
c
        else
c
          locbuf(fid) = l
c
        endif
c
c ..... if in RW mode, need to worry about some extra things
c
        if (wrflag(fid).eq.RW) then
c
          eofre(fid)  = .FALSE.
c
c ....... if we have have written anything and have a partial buffer,
c ....... need to set flag indicating this record has been modified
c
          if (locbuf(fid).gt.0) then
            modrec(fid) = .TRUE.
          else
            modrec(fid) = .FALSE.
          endif
c
c ....... have we extended the file ?
c
          nowwrd = currec(fid) - 1
          nowwrd = nowwrd*LBUF + locbuf(fid)
          if (nowwrd.gt.lstwrd(fid)) then
c
            if ( .NOT. extflg(fid) ) then
              extflg(fid) = .TRUE.
              floc = 1
              call pfuwbk(fid, floc, 3, dftarr, ierr)
            endif
c
            lstwrd(fid) = nowwrd
c
c ......... fill rest of buffer with EOF if not already there
c
            if (ubuf(locbuf(fid)+1,fid).ne.EOFFLG) then
              do 21 i=locbuf(fid)+1,LBUF
                ubuf(i,fid) = EOFFLG
   21         continue
              modrec(fid) = .TRUE.
            endif
c
          endif
c
        endif
c
      endif
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

      subroutine pfusio_scalar (fid, iop, nw, i, ierr)
        integer   fid,iop,nw,i,ierr
        integer iarr(1)
        iarr(1) = i
        call pfusio(fid, iop, nw, iarr, ierr)
        i = iarr(1)
      end subroutine
