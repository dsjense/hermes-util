      subroutine pfwnul ( fid, lnul, nulloc, ierr )
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
c       - This routine is a WRITE routine that writes a NULL region to
c         a PFF file.
c       - This operation is ONLY ALLOWED in WRITE mode !!!
c       - In Read/Write mode ONLY, if the input pointer (nulloc) is
c         non-positive, the  First Word Address (FWA) of the NULL region
c         is taken to be the position of the current file pointer.
c       - If the final position of the NULL region is before the last
c         word written to the file, the operation is completed only if
c         the word immediately following the region is a dataset framing
c         word or a NULL region indicator flag.  In the latter case, the
c         two NULL regions are combined into a single NULL region.
c       - NULL Region Format:  (Lnul is length of NULL region in 16-bit
c                               words)
c           if (Lnul .lt. 4) :
c              (Lnul)x<INT>       Lnul*NULFLG
c
c           if (Lnul .ge. 4) :
c              <INT>              NULFLG      NULL Region Flag
c              <LONG>             Lnul        NULL Region length
c              (Lnul-4)x<INT>     JUNK
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file
c       lnul    -  length of NULL Region
c       nulloc  -  FW address of NULL region
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   Attempt to write to file not open for WRITE
c                      2,   File Framing Error
c
c ----------------------------------------------------------------------
c
c     Internals:
c       cloc    -  current file pointer address
c       fill    -  length of padding required at end of file
c       floc    -  address of word after NULL region
c       i       -  loop index over buffer
c       ifrm    -  provisional framing word
c       nloc    -  address of first word of NULL region
c       ploc    -  provisional file address to start of padding region
c       tlen    -  provisional length of NULL length
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfstfg  -  RW, WR, DFAULT, DFRAME, NULLFG
c
c ... Included common decks:
c ... pfbufm
c       r:    lstwrd(*), wrflag(*)
c ... pfbufp
c       r:    LPRBUF
c       w:    prmbuf
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pfbufm.inc"
#include "pfbufp.inc"
#include "pferrs.inc"
c
#include "mdfpos.h"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ierr, lnul
      HU_FPOS_TYPE nulloc
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFWNUL' )
c
c     Internal
      integer   fill, i, ifrm, tlen
      HU_FPOS_TYPE cloc, floc, nloc, ploc
      HU_FPOS_TYPE pfutel
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
      tlen = lnul
c
      if ( wrflag(fid).eq.WR ) then
c
        ploc = 0
        floc = pfutel( fid, ierr) + lnul
c
      else if ( wrflag(fid).eq.RW ) then
c
c ..... Find FWA of NULL region
c
        if ( nulloc.lt.0) then
          nloc = pfutel ( fid, ierr )
        else
          nloc = nulloc
        endif
c
c ..... Find pointer to 1st word AFTER region
c
        floc = nloc + lnul
c
c ..... if we are writing beyond EOF, need to know how much to pad
c
        ploc = lstwrd(fid)
c
c ..... if write is not beyond EOF, need to check that ends at a file
c ..... marker
c
        if (floc.lt.lstwrd(fid)) then
c
          call pfusek ( fid, floc, ierr )
          call pfusio ( fid, RE, 1, ifrm, ierr )
          if (ierr.ne.0) goto 999
c
c ....... if a NULL region, need to combine into one region
c
          if (ifrm.eq.NULFLG) then
            call pfusio ( fid, RE, 3, prmbuf, ierr )
            if (prmbuf(1).ge.0) then
              call pfui2l ( prmbuf, tlen, ierr)
              tlen = tlen + lnul
            else
              do 1 i=1,3
                if ( prmbuf(i).eq.DFRAME .OR. prmbuf(i).eq.EOFFLG ) then
                  tlen = lnul + i
                  goto 2
                endif
    1         continue
              goto 20
    2         continue
            endif
c
c ....... if a Dataset framing word, okay; otherwise a framing error
c
          else if (ifrm.ne.DFRAME) then
            goto 20
          endif
c
        endif
c
c ..... Position to FWA of NULL region
c
        call pfusek ( fid, nloc, ierr )
c
      else
c
        ierr = 1
        call pfserr ( PFMODU, ierr, fid,
     &                'Attempt to write to file not open for WRITE' )
c
        goto 999
c
      endif
c
c ... Write framing word and length
c
      prmbuf(1) = NULFLG
      if ( tlen.lt.4 ) then
        do 11 i=2,tlen
          prmbuf(i) = NULFLG
   11   continue
      else
        call pful2i ( tlen, prmbuf(2), ierr )
        tlen = 4
      endif
c
      call pfusio (fid, WR, tlen, prmbuf, ierr)
      if ( ierr.ne.0 ) goto 999
c
c ... If necessary, pad rest of null region with "Default" flag
c
      if ( ploc.lt.floc ) then
c
        cloc = pfutel(fid,ierr)
        if ( ploc.gt.cloc) then
          cloc = ploc
          call pfusek( fid, cloc, ierr )
        endif
c
        fill = floc - cloc
c
        if ( fill.gt.0 ) then
c
          do 12 i=1,min( fill, LPRBUF)
            prmbuf(i) = DFAULT
   12     continue
c
          call pfusio (fid, WR, mod(fill,LPRBUF), prmbuf, ierr)
          do 13 i=1,fill/LPRBUF
            call pfusio (fid, WR, LPRBUF, prmbuf, ierr)
   13     continue
c
        endif
c
      endif
c
      goto 999
c
   20 continue
c
        ierr = 2
        call pfserr ( PFMODU, ierr, fid, 'File Framing Error' )
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
