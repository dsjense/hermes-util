      subroutine pfurep ( fname, ierr, dirflg )
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
c       - This routine is a UTILITY routine to repair a PFF DIRECTORIED
c         file that has been improperly closed.
c         This function finds an available open FID, assigns the file
c         "fname" to that FID, opens the file, attempts to load its
c         directory.  Failing this, it attempts to rebuild the directory
c         from the header info in each dataset, one dataset at a time.
c         If it cannot reconstruct the entire directory, it truncates
c         the file after the last dataset for which a directory entry
c         was successfully loaded.  It then closes the file, rewriting
c         the directory if necessary.
c
c ----------------------------------------------------------------------
c
c     Input:
c       fname   -  character variable containing name of file to be
c                  opened
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       dirflg --  # of directory entries loaded from file (RE-RW mode)
c       ierr    -  error status flag;
c                     -2,   File repaired with potential loss of data
c                     -1,   File repaired with no loss of data
c                      0,   File not in need of repair
c                      1,   No Available PFF File ID's
c                      2,   No Available Logical Units
c                      3,   Error Opening File
c                      4,   File opened for read is empty
c                      5,   File has no PFF header
c
c ----------------------------------------------------------------------
c
c     Internals:
c       dlen    -  dataset length
c       dloc    -  dataset pointer
c       fid     -  File ID of PFF file opened by this routine
c       i       -  do loop index over PFF file ID's
c       ldptr   -  pointer to start of directory entries on file
c       lun     -  Unit # of PFF file opened by this routine
c       mc,mt,  \  unused integers
c        nc,nt  /    for PFRHED call
c       nrfu    -  # of RFU header words used  (unused)
c       repmod  -  logical flag indicating whether directory is to be
c                  loaded in "repair" mode (i.e., directory is
c                  unreadable and/or unaccessible)
c       sc      -  character string -- dataset comment (title)
c       st      -  character string -- dataset type label
c       tapp    -  # associated with application dataset type
c       traw    -  # associated with raw dataset type
c       vds     -  dataset version #  (unused)
c
c     Parameters:
c
c       PFIRST -  first available PFF file logical unit #
c       PLAST  -  last available PFF file logical unit #
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  MCHTTL, MCHTYP, MXFILS, LFHDR
c     pfstfg  -  RE, WR, RW, NOPEN, DFAULT
c
c ... Included common decks:
c ... pfbufm
c       w:    currec(*), locbuf(*), fillum(*), lstwrd(*), extflg(*),
c             eofre(*), maxrec(*), modrec(*), ubuf(*,*), dircnt(*),
c             dircur(*)
c       r/w:  wrflag(*)
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pffils
c       r:    frstlu, lastlu
c       w:    filcnt, filcur, fidmap(*), files(*)
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pfbufm.inc"
#include "pferrs.inc"
#include "pffils.inc"
c
#include "mdfpos.h"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   ierr, dirflg
      character fname*(*)
c
c     Internal
      integer   dlen, fid, i, lun, tapp, traw, ilong(3)
      integer   mc, mt, nc, nt, nrfu, vds, lusave, ierrx
      HU_FPOS_TYPE ldptr, locsav, dloc
      character st*(MCHTYP), sc*(MCHTTL)
      logical   repmod, did_rep
      integer   pfg1lu
c
      HU_FPOS_TYPE pfutel
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFUREP' )
c
c ... External Block Data
      external  pffbd
c
c ----------------------------------------------------------------------
c
      lusave = luverb
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
c     search for 1st available File ID (FID)
c
      do 1 i=1,MXFILS
        fid = i
        if(wrflag(i).eq.NOPEN) goto 2
    1 continue
c
c     no available files, return
c
        ierr   = 1
        call pfserr ( PFMODU, ierr, 0, 'No Available PFF File ID''s' )
        goto 999
c
    2 continue
c
c     search for 1st available logical unit
c
      lun = pfg1lu ( frstlu, lastlu, ierr )
c
c     no available files, return
c
      if ( ierr .ne. 0 ) then
        ierr   = 2
        call pfserr ( PFMODU, ierr, 0, 'No Available Logical Units' )
        goto 999
      endif
c
c     open file in RW mode (machine dependent)
c     preset filename so error handling will list it
c
      files (fid) = fname
      call pfmopn ( fid, lun, RW, fname, ierr )
c
c     if error encountered when opening file, set filename back to ' '
c     and return.  (PFMOPN has already handled error processing)
c
      if(ierr.ne.0)then
        files (fid) = ' '
        ierr   = 3
        goto 999
      endif
c
      did_rep = .FALSE.
c
c ..... load in first buffer
c
      call pfmio (RE, lun, 1, ubuf(1,fid), ierr )
c
c ... if EOF, set EOF-Read status, if error encountered when
c ... loading buffer, set error flag and return
c
      if(ierr.eq.-1)then
        ierr = 4
        call pfserr ( PFMODU, ierr, 0,
     &               'File opened for read is empty' )
        goto 990
      else if(ierr.ne.0)then
        goto 990
      endif
c
c ... Does file have a proper header; if so, read it in from buffer
c
      if (ubuf(1,fid).ne.FFRAME) then
        ierr   = 5
        call pfserr ( PFMODU, ierr, 0,
     &               'File has no PFF header' )
        goto 990
      else
        luverb = OFFLUN
        if ( ubuf(2,fid).eq.DFAULT ) then
          repmod = .TRUE.
        else
          call pfui2l64 (ubuf(2,fid), ldptr, ierr)
          if ( ierr.ne.0 ) then
            ierr = 0
            repmod = .TRUE.
          else
            lstwrd(fid) = ldptr - 1
            repmod = .FALSE.
          endif
        endif
      endif
c
      currec(fid) = 1
      extflg(fid) = .FALSE.
c

c ... initialize pointer values
c
      locbuf(fid) = LFHDR
      wrflag(fid) = RW
      fillun(fid) = lun
      modrec(fid) = .FALSE.
      eofre(fid)  = .FALSE.
      dircnt(fid) = 0
      dircur(fid) = 1
c
c ... load the file's directory
c
   30 continue
c
      if ( repmod ) then
c
c ..... if can't use directory datasets, initialize pointers for
c ..... sequential header reads
c
        dlen = 0
        dloc = LFHDR
c
      else
c
c ..... position file at FWA of directory info
c
        call pfusek ( fid, ldptr, ierr )
        if ( ierr.ne.0 ) then
          repmod = .TRUE.
          goto 30
        endif
c
      endif
c
c ... DO WHILE ( not EOF of read-directory call )
c
   40 continue
c
        if ( repmod ) then
c
c ....... if can't use directory datasets, advance pointer and read
c ....... next header
c
          call pfusek ( fid, dloc + dlen, ierr )
          ierrx = ierr
          call pfrhed ( fid, dloc, traw, tapp, vds, dlen, nrfu,
     &                  st, nt, mt, sc, nc, mc, ierr )
          if ( ierr.gt.0 ) then
            if (ierrx.ne.0) dircnt(fid) = dircnt(fid) - 1
            ierr = 0
            call pfdget(fid,dircnt(fid),traw,tapp,dlen,dloc,
     &                  st,sc,ierr)
            dloc = dloc + dlen
            ierr = -1
            goto 50
          endif
c
        else
c
          call pfrdir ( fid, traw, tapp, st, sc, dlen, dloc, ierr )
          if ( ierr.eq.0 ) then
            locsav = pfutel (fid, ierr)
            call pfusek ( fid, dloc + 1, ierr )
            call pfusio_scalar (fid, RE, 1, i, ierr)
            if ( i.eq.DFAULT ) then
              call pful2i ( dlen, ilong, ierr )
              call pfusek ( fid, dloc + 1, ierr )
              call pfusio (fid, WR, 3, ilong, ierr)
              did_rep = .TRUE.
            endif
            call pfusek ( fid, locsav, ierr )
          endif
c
        endif
c
c ..... if EOF, then exit this do-while loop
c
        if ( ierr.eq.-1 ) then
          ierr = 0
       goto 50
        endif
c
c ..... if any other error, start again in "repair" mode or exit
c
        if ( ierr.ne.0 ) then
c
          if ( repmod ) then
            goto 990
          else
            repmod = .TRUE.
            ierr = 0
            dircnt(fid) = 0
            dircur(fid) = 1
            goto 30
          endif
c
        endif
c
c ..... put the directory info in memory-resident directory structure
c
        call pfdput ( fid, traw, tapp, dlen, dloc, st, sc, ierr )
c
        goto 40
c
c ..... END DO WHILE
c
   50 continue
c
      maxrec(fid) = currec(fid)
c
      if ( repmod ) then
        ierr = ierr - 1
        if ( dircnt(fid).eq.0) then
          lstwrd(fid) = LFHDR
        else
          lstwrd(fid) = dloc
        endif
        extflg(fid) = .TRUE.
      else
        if ( did_rep ) then
          ierr = -1
        else
          ierr = 0
        endif
      endif
c
c ... position file at FWA of 1st dataset; return # of dir. entries
c
      ierrx = 0
      dloc = LFHDR
      call pfusek ( fid, dloc, ierrx )
      dircur(fid) = 1
c
      dirflg = dircnt(fid)
c
c ... Add file to file map for menu management
c
      filcnt = filcnt + 1
      filcur = filcnt
      fidmap(filcnt) = fid
c
      ierrx = 0
      call pfucls(fid,ierrx)
c
      goto 999
c
  990 continue
c
c ..... error after opening file; close file and reset buffer
c ..... management values
c
        close ( unit=lun )
c
        files (fid) = ' '
        wrflag(fid) = NOPEN
        fillun(fid) = -1
        currec(fid) = 0
        maxrec(fid) = 0
        locbuf(fid) = 0
        lstwrd(fid) = 0
        extflg(fid) = .FALSE.
        dircnt(fid) = 0
        dircur(fid) = 0
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
      luverb = lusave
      return
c
 1000 format(' %PFF-',a,'-DBG:  ',a,' Error Status = ',i5)
c
      end
