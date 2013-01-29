      subroutine pfusek (fid, locwrd, ierr)
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
c       - This routine is a UTILITY routine that positions the PFF file
c         associated with PFF file "fid" to the word address "locwrd".
c       - This operation is ONLY ALLOWED in READ mode !!!
c       - The address is supplied in units of 16-bit words.
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file
c       locwrd  -  word address to position PFF file pointer so that
c                  next word returned by PFUSIO corresponds to "locwrd+1"
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   Illegal File ID (FID)
c                      2,   File NOT Open
c                      3,   Illegal file address (negative)
c                      4,   Unit in NOT in READ mode
c                      5,   Attempt to position file beyond EOF
c
c ----------------------------------------------------------------------
c
c     Internals:
c       locb    -  buffer pointer position associated with "locwrd"
c       locrec  -  offset of FW of record containing current file position
c       rec     -  file record # associated with "locwrd"
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  LBUF, MXFILS
c     pfstfg  -  RE, WR, NOPEN
c
c ... Included common decks:
c ... pfbufm
c       r:    locbuf(*), wrflag(*), fillun(*)
c       r/w:  currec(*), eofre(*), modrec(*), ubuf(*,*)
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
      HU_FPOS_TYPE locwrd
c
c     Internal
      integer   locb, rec
      HU_FPOS_TYPE locrec
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFUSEK' )
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
c ... is address legal ?
c
      if(locwrd.lt.0)then
c
        ierr = 3
        call pfserr ( PFMODU, ierr, fid,
     &                'Illegal file address (negative)' )
        goto 999
c
      endif
c
c ... Otherwise, position file ONLY IF in READ mode (RE or RW)
c
      if ( wrflag(fid).ne.WR ) then
c
c ..... translate word address to record # and buffer pointer
c
        locrec = locwrd/LBUF
        rec = locrec + 1
        locrec = locrec*LBUF
        locb = locwrd - locrec
c
c ..... is target record different that current record ?
c ..... if so, need to read and unpack that record
c
        if ( rec.ne.currec(fid) ) then
c
c ....... if current record has been modified, need to write it out
c ....... before reading in new record (RW only)
c
          if (modrec(fid)) then
            call pfmio(WR, fillun(fid), currec(fid), ubuf(1,fid), ierr )
            modrec(fid) = .FALSE.
          endif
c
          call pfmio ( RE, fillun(fid), rec, ubuf(1,fid), ierr )
c
          if ( ierr.eq.-1 ) then
            ierr = 0
c
c ......... Are we attempting to position file BEYOND the EOF ?
c
            if ( locb.gt.0 ) goto 10
c
c ......... does the immediately preceeding record have data ?
c
            call pfmio ( RE, fillun(fid), rec-1, ubuf(1,fid), ierr )
c
c ......... if not, we're still beyond EOF
c
            if ( ierr.ne.0 ) goto 10
c
c ......... otherwise, not an error, but entry into EOF-READ mode
c
            eofre(fid) = .TRUE.
c
          else if ( ierr.ne.0 ) then
c
            goto 999
c
          else
c
            eofre(fid) = .FALSE.
c
          endif
c
        else
c
c ....... Are we attempting to position file BEYOND the EOF ?
c
          if ( eofre(fid) .and. locb.ne.0 ) goto 10
c
        endif
c
c ..... set record and buffer pointers to target values
c ..... set return value
c
        locbuf(fid) = locb
        currec(fid) = rec
c
      else
c
c     WRITE OPERATION
c
c ..... Since true random buffering is not supported by PFUSIO, a seek
c ..... operation in WRITE mode is dicey at best.  Consequently, make
c ..... this an error.  To perform a "write-back" while in WRITE mode,
c ..... use subroutine PFUWBK
c
        ierr = 4
        call pfserr ( PFMODU, ierr, fid, 'Unit in NOT in READ mode' )
c
      endif
c
      goto 999
c
   10 continue
c
c ..... Process attempts to position file BEYOND the EOF
c
        ierr = 5
        call pfserr ( PFMODU, ierr, fid,
     &                    'Attempt to position file beyond EOF' )
        locb = 0
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
