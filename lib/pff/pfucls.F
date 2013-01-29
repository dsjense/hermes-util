      subroutine pfucls ( fid, ierr )
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
c       - This routine is a UTILITY routine to close a DIRECTORIED PFF
c         file.  This routine closes PFF file "fid" and resets all the
c         buffer management pointers, etc.  For a file in write mode, it
c         also pads and flushes the buffer if it is not empty.
c       - If the extend flag has been set, the file's directory is
c         appended to the end of the file
c
c
c     Input:
c       fid     -  File ID of PFF file to be closed.  If fid = 0,
c                  then close all open PFF files.
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   Illegal PFF File ID
c                      2,   File Not Open
c                      3,   File not on FID map
c                      4,   Error retrieving string from NAME SERVER
c                      5,   Error freeing string to NAME SERVER
c
c ----------------------------------------------------------------------
c
c     Internals:
c       ctype   -  dataset type string for output
c       first   -  first file ID # in loop
c       floc    -  temporary file offet value
c       i       -  do loop index words in I/O buffer
c       iblk    -  current file directory block #
c       iden    -  loop index over file directory blocks
c       ifid    -  Loop index over PFF file ID's
c       ifil    -  file # that maps to FID
c       ilast   -  last directory entry for current directory block
c       iloc    -  the block offset for the current directory entry
c       im      -  do loop index over FID map
c       last    -  last file ID # in loop
c       lbytes  -  Number of bytes in full file
c       ldptr   -  pointer to start of directory entrys on file
c       mapblk  -  the block # for this entry in the directory structure
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  DBLKSZ, LBUF, MCHTYP, MXFILS, PRECLB
c     pfstfg  -  WR, RW, NOPEN, EOFFLG
c
c ... Included common decks:
c ... pfbufd
c       r:    dirinfo(*,*,*), dblkpr(*,*)
c ... pfbufm
c       w:    eofre(*), modrec(*), ubuf(*,*), dircur(*)
c       r/w:  currec(*), locbuf(*), wrflag(*), fillun(*), extflg(*),
c             lstwrd(*), dircnt(*), maxrec(*)
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pffils
c       w:    filcnt, filcur, fidmap(*), files(*)
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pfbufm.inc"
#include "pfbufd.inc"
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
      integer   fid, ierr
c
c     Internal
      integer   first, i, iblk, iden, ifid, ifil, ilast, iloc, im
      integer   last, lbytes, mapblk
      HU_FPOS_TYPE floc, ldptr
      character ctype*(MCHTYP), ctitle*(MCHTTL)
c
c     External functions
      integer   NS_freename, NS_getname
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFUCLS' )
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
c ... is this a request that all files be closed ?
c
      if(fid.eq.0)then
c
        first = 1
        last  = MXFILS
c
c ... is fid an illegal PFF File ID ?
c
      else if(fid.lt.1 .or. fid.gt.MXFILS)then
c
        ierr = 1
        call pfserr ( PFMODU, ierr, fid, 'Illegal PFF File ID' )
        goto 999
c
c ... collapse loop to include only the requested (legal) file ID
c
      else
c
        first = fid
        last  = fid
c
      endif
c
c ... loop over all files in request
c
      do 25 ifid=first,last
c
c ..... is file open ?
c
        if ( wrflag(ifid).eq.NOPEN ) then
c
c ....... only an error in request was for a specific file
c ....... if all files were requested, just loop to next file
c
          if ( fid.ne.0 ) then
            ierr = 2
            call pfserr ( PFMODU, ierr, fid, 'File Not Open' )
            goto 999
          endif
c
        else
c
c ....... if extend flag is set, need to write EOFFLG, then directory
c
          if ( extflg(ifid) ) then
c
c ......... if in read/write mode, need to go to end-of-data
c
            if (wrflag(ifid).eq.RW) then
              call pfusek ( ifid, lstwrd(ifid), ierr )
            endif
c
c ......... put one word of 'fffe' (-2) to indicate end-of-information
c
            locbuf(ifid) = locbuf(ifid) + 1
            ubuf(locbuf(ifid),ifid) = EOFFLG
c
c ......... if buffer is full, need to dump it
c
            if (locbuf(ifid).eq.LBUF) then
              if (wrflag(ifid).eq.WR) currec(ifid) = currec(ifid) + 1
              call pfmio (WR, fillun(ifid), currec(ifid), ubuf(1,ifid),
     &                    ierr)
              if (wrflag(ifid).eq.RW) currec(ifid) = currec(ifid) + 1
              locbuf(ifid) = 0
            endif
c
c ......... write pointer to directory info into file header
c
            floc = 0
            call pfwlds ( ifid, floc, ldptr, ierr )
c
c ......... Write directory to file now --
c ......... loop over all directory blocks used by this file
c
            iblk = 0
            do 2 iden=1,dircnt(ifid),DBLKSZ
c
c ........... find the actual block that this file block is mapped to
c
              mapblk = dblkpr(iblk,ifid)
c
c ........... loop over all entries in this block
c
              ilast = min ( DBLKSZ, dircnt(ifid) - iden + 1 )
              do 1 iloc=1,ilast
c
c ............. if type is 'unknown', map back to ' '
c
                ierr = NS_getname(dirinfo(PDTTL,iloc,mapblk),ctitle)
                if ( ierr.ge.0 )
     &            ierr = NS_getname(dirinfo(PDTYP,iloc,mapblk),ctype)
                if ( ierr.lt.0 ) then
                  ierr = 4
                  call pfserr ( PFMODU, ierr, fid,
     &                 'Error retrieving string from NAME SERVER' )
                  goto 999
                endif
                ierr = 0
                if ( ctype.eq.'Unknown' ) then
                  ctype = ' '
                endif
c
c ............. write out this directory entry
c
                floc = dirinfo(PDRLOC,iloc,mapblk)
                floc = LBUF*floc + dirinfo(PDOLOC,iloc,mapblk)
                call pfwdir ( ifid, dirinfo(PDRAW,iloc,mapblk),
     &                        dirinfo(PDAPP,iloc,mapblk), ctype,
     &                        ctitle, dirinfo(PDLEN,iloc,mapblk),
     &                        floc, ierr )
                if ( ierr.ne.0 ) goto 999
c
    1         continue
c
c ........... increment file block counter
c
              iblk = iblk + 1
c
    2       continue
c
c ......... put one word of 'fffe' (-2) after directory data
c
            locbuf(ifid) = locbuf(ifid) + 1
            ubuf(locbuf(ifid),ifid) = EOFFLG
c
c ......... fill remainder of buffer (if any) with 'fffe' (-2)
c
            do 3 i=locbuf(ifid)+1,LBUF
              ubuf(i,ifid) = EOFFLG
    3       continue
c
c ......... write out current (now full) buffer
c
            if (wrflag(ifid).eq.WR) currec(ifid) = currec(ifid) + 1
c
            call pfmio (WR,fillun(ifid),currec(ifid),ubuf(1,ifid),ierr)
c
            if (ierr.ne.0) goto 999
c
            maxrec(ifid) = currec(ifid)
c
          else if (modrec(ifid)) then
c
            call pfmio (WR,fillun(ifid),currec(ifid),ubuf(1,ifid),ierr)
            modrec(ifid) = .FALSE.
c
          endif
c
c ....... close file
c
          lbytes = maxrec(ifid)*PRECLB
          call pfmcls ( ifid, fillun(ifid), lbytes, ierr )
c
          if(ierr.ne.0) goto 999
c
c ....... free nameserver handles for directory strings, return any
c ....... directory blocks used to free block stack; reset block pointers
c
          iblk = 0
          do 11 iden=1,dircnt(ifid),DBLKSZ
            mapblk = dblkpr(iblk,ifid)
            ilast = min ( DBLKSZ, dircnt(ifid) - iden + 1 )
            do iloc=1,ilast
              ierr = NS_freename(dirinfo(PDTTL,iloc,mapblk))
              if ( ierr.eq.0 )
     &          ierr = NS_freename(dirinfo(PDTYP,iloc,mapblk))
              if ( ierr.ne.0 ) then
                ierr = 5
                call pfserr ( PFMODU, ierr, fid,
     &               'Error freeing string to NAME SERVER' )
                goto 999
              endif
            end do
            call pfdfbk ( ifid, iblk, ierr )
            iblk = iblk + 1
   11     continue
c
c ....... reset buffer management values
c
          files (ifid) = ' '
          wrflag(ifid) = NOPEN
          fillun(ifid) = -1
          currec(ifid) = 0
          maxrec(ifid) = 0
          locbuf(ifid) = 0
          lstwrd(ifid) = 0
          extflg(ifid) = .FALSE.
          modrec(ifid) = .FALSE.
          eofre(ifid)  = .FALSE.
          dircnt(ifid) = 0
          dircur(ifid) = 0
c
c ....... remove file from FID management map
c
c           if closed file is not current file:
c              1) preserve current file
c           if closed file is current file:
c              1) set current file pointer to next file, UNLESS
c              2) closed file was last file; set pointer to previous
c                 file
c
c ....... Find the FID map entry matching this FID
c
          do 12 im=1,filcnt
            if(fidmap(im).eq.ifid)then
              ifil = im
              goto 13
            endif
   12     continue
c
c ....... Not found !!!
c
          ierr = 3
          call pfserr ( PFMODU, ierr, fid, 'File not on FID map' )
          goto 999
c
   13     continue
c
c ....... decrement file counter and repack FID map
c
          filcnt = filcnt - 1
          do 14 im=ifil,filcnt
            fidmap(im) = fidmap(im+1)
   14     continue
c
c ....... if pointer is higher than removed file or last file,
c ....... decrement it by one
c
          if( filcur.gt.min(filcnt,ifil) ) then
            filcur = filcur - 1
          endif
c
        endif
c
   25 continue
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
