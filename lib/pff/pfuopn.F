      function pfuopn ( fname, status, ierr, dirflg )
      integer  pfuopn
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
c       - This routine is a UTILITY routine to open a DIRECTORIED file.
c         This function finds an available open FID, assigns the file
c         "fname" to that FID, opens the file, loads in its directory,
c         and returns the PFF File ID.
c
c ----------------------------------------------------------------------
c
c     Input:
c       fname   -  character variable containing name of file to be
c                  opened
c       status  -  file status, 0 for read (RE),
c                               1 for write (WR),
c                               2 for read/write (RW)
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       dirflg --  # of directory entries loaded from file (RE-RW mode)
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   No Available PFF File ID's
c                      2,   No Available Logical Units
c                      3,   Error Opening File
c                      4,   Invalid Status
c                      5,   File opened for read is empty
c                      6,   File has no PFF header
c
c     Return Value:
c       pfuopn  =  < 0,    error has occurred, returns (-IERR)
c                    0,    error has occurred in lower level PFF routine
c                  > 0,    returns PFF File ID # of file
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
c     pfbsiz  -  MCHTTL, MCHTYP, MXFILS, LFHDR, DBLKSZ
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
c       w:    filcnt, filcur, fidmap(*), fpprec(*), files(*)
c       r/w:  fpprec(*)
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
      integer   status, ierr, dirflg
      character fname*(*)
c
c     Internal
      integer   dlen, dloc, fid, i, iblk, iden, ldptr, lun, tapp, traw
      integer   mc, mt, nc, nt, nrfu, vds, errtmp
      character st*(MCHTYP), sc*(MCHTTL)
      logical   repmod
      integer   pfg1lu
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFUOPN' )
c
      integer   PFIRST, PLAST
      parameter (PFIRST = 20 )
      parameter (PLAST  = 90 )
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
      pfuopn = 0
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
        pfuopn = -ierr
        goto 999
c
    2 continue
c
c     search for 1st available logical unit
c
      lun = pfg1lu ( PFIRST, PLAST, ierr )
c
c     no available files, return
c
      if ( ierr .ne. 0 ) then
        ierr   = 2
        call pfserr ( PFMODU, ierr, 0, 'No Available Logical Units' )
        pfuopn = -ierr
        goto 999
      endif
c
c     set file status; error if invalid file status supplied
c
      if( status.ne.RE .AND. status.ne.WR .AND. status.ne.RW )then
        ierr   = 4
        call pfserr ( PFMODU, ierr, 0, 'Invalid Status' )
        pfuopn = -ierr
        goto 999
      endif
c
c     open file (machine dependent)
c     preset filename so error handling will list it
c     reset filename in case pfmopn modifies it
c
      files (fid) = fname
      call pfmopn ( fid, lun, status, fname, ierr )
      files (fid) = fname
c
c     if error encountered when opening file, set filename back to ' '
c     and return.  (PFMOPN has already handled error processing)
c
      if(ierr.ne.0)then
        files (fid) = ' '
        ierr   = 3
        pfuopn = -ierr
        goto 999
      endif
 
      if( status.eq.RE .OR. status.eq.RW ) then
c
c ..... load in first buffer
c
        call pfmio (RE, lun, 1, ubuf(1,fid), ierr )
c
c ..... if EOF, set EOF-Read status, if error encountered when
c ..... loading buffer, set error flag and return
c
        if(ierr.eq.-1)then
          ierr   = 5
          call pfserr ( PFMODU, ierr, 0,
     &                 'File opened for read is empty' )
          pfuopn = -ierr
          goto 990
        else if(ierr.ne.0)then
          goto 990
        endif
c
c ..... Does file have a proper header; if so, read it in from buffer
c
        if (ubuf(1,fid).ne.FFRAME) then
          ierr   = 6
          call pfserr ( PFMODU, ierr, 0,
     &                 'File has no PFF header' )
          pfuopn = -ierr
          goto 990
        else
          if ( ubuf(2,fid).eq.DFAULT ) then
            repmod = .TRUE.
          else
            call pfui2l (ubuf(2,fid), ldptr, ierr)
            if ( ierr.ne.0 ) goto 990
            lstwrd(fid) = ldptr - 1
            repmod = .FALSE.
          endif
        endif
c
        currec(fid) = 1
        extflg(fid) = .FALSE.
c
      else
c
c ..... load file header into buffer and set flags
c
        ubuf(1,fid) = FFRAME
        do 20 i=2,LFHDR
          ubuf(i,fid) = DFAULT
   20   continue
c
        currec(fid) = 0
        extflg(fid) = .TRUE.
        lstwrd(fid) = LFHDR
c
      endif
c
c ... initialize pointer values
c
      locbuf(fid) = LFHDR
      wrflag(fid) = status
      fillun(fid) = lun
      modrec(fid) = .FALSE.
      eofre(fid)  = .FALSE.
      dircnt(fid) = 0
      dircur(fid) = 1
c
c ... set <FARRAY> precision for file
c
      fpprec(fid) = fpprec(0)
c
c ... if in read mode, load the file's directory
c
      if ( status.eq.RE .OR. status.eq.RW ) then
c
   30   continue
c
        if ( repmod ) then
c
c ....... if can't use directory datasets, initialize pointers for
c ....... sequential header reads
c
          dlen = 0
          dloc = LFHDR
c
        else
c
c ....... position file at FWA of directory info
c
          call pfusek ( fid, ldptr, ierr )
          if ( ierr.ne.0 ) goto 990
c
        endif
c
c ..... DO WHILE ( not EOF of read-directory call )
c
   40   continue
c
          if ( repmod ) then
c
c ......... if can't use directory datasets, advance pointer and read
c ......... next header
c
            call pfusek ( fid, dloc + dlen, ierr )
            call pfrhed ( fid, dloc, traw, tapp, vds, dlen, nrfu,
     &                    st, nt, mt, sc, nc, mc, ierr )
c
          else
c
            call pfrdir ( fid, traw, tapp, st, sc, dlen, dloc, ierr )
c
          endif
c
c ....... if EOF, then exit this do-while loop
c
          if ( ierr.eq.-1 ) then
            ierr = 0
         goto 50
          endif
c
c ....... if any other error, start again in "repair" mode or exit
c
          if ( ierr.ne.0 ) then
c
            if ( repmod ) then
              goto 990
            else
              repmod = .TRUE.
              ierr = 0
c
c ........... return any directory blocks used to free block stack; 
c ........... reset block pointers
c
              iblk = 0
              do iden=1,dircnt(fid),DBLKSZ
                call pfdfbk ( fid, iblk, ierr )
                iblk = iblk + 1
              end do
              dircnt(fid) = 0
              dircur(fid) = 1
              goto 30
            endif
c
          endif
c
c ....... put the directory info in memory-resident directory structure
c
          call pfdput ( fid, traw, tapp, dlen, dloc, st, sc, ierr )
          if ( ierr.ne.0 ) goto 990
c
          goto 40
c
c ....... END DO WHILE
c
   50   continue
c
        maxrec(fid) = currec(fid)
c
        if ( repmod ) then
          if ( dircnt(fid).eq.0) then
            lstwrd(fid) = LFHDR
          else
            lstwrd(fid) = dloc
          endif
          if ( wrflag(fid).eq.RW ) then
            extflg(fid) = .TRUE.
          endif
        endif
c
c ..... position file at FWA of 1st dataset; return # of dir. entries
c
        call pfusek ( fid, LFHDR, ierr )
        dircur(fid) = 1
c
        dirflg = dircnt(fid)
c
      else
c
        dirflg = 0
c
      endif
c
c ... Add file to file map for menu management
c
      filcnt = filcnt + 1
      filcur = filcnt
      fidmap(filcnt) = fid
c
c ... set return value to File ID
c
      pfuopn = fid
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
c ..... return any directory blocks used to free block stack; reset 
c ..... block pointers
c
        errtmp = 0
        iblk = 0
        do iden=1,dircnt(fid),DBLKSZ
          call pfdfbk ( fid, iblk, errtmp )
          iblk = iblk + 1
        end do
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
      return
c
 1000 format(' %PFF-',a,'-DBG:  ',a,' Error Status = ',i5)
c
      end
