      subroutine pfupck ( fid, status, ierr )
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
c       - This routine is a UTILITY routine that attempts to pack a PFF
c         file that has for some reason developed empty spots.
c       - The file will be packed only if the datasets are located in
c         ascending order in the file; if this is not the case, PFUCPY
c         must be used to copy the datasets into a new file, where they
c         will be both packed and properly ordered.
c       - A status flag is used to inform the calling program of the
c         outcome of this call.  There are three possiblities:
c              status =  0 -- File was already packed.
c              status =  1 -- File was successfully packed.
c              status = -1 -- Datasets are out of order.
c                             File was not modified !
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
c       status  -  outcome status flag;
c                        0,  File was already packed
c                        1,  File was successfully packed
c                       -1,  Datasets out of order; File not modified
c       ierr    -  error status flag;
c                        0,  No error
c                        1,  Illegal File ID (FID)
c                        2,  File NOT Open
c
c ----------------------------------------------------------------------
c
c     Internals:
c       dslen   -  dataset length
c       dsloc   -  dataset file position pointer  (UNPACKED)
c       ids     -  loop index over datasets on file
c       ifirst  -  # of first unpacked dataset
c       ilast   -  # of words used in work buffer for data transfer
c       ip      -  loop index over words in dataset
c       locpck  -  dataset file position pointer  (PACKED)
c       maxloc  -  largest file pointer encountered for any dataset
c       next    -  location where next dataset should be if it is packed
c       sc      -  character string for dataset comment
c       st      -  character string for dataset type label
c       tapp    -  # associated with application dataset type
c       traw    -  # associated with raw dataset type
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  LFHDR, LPRBUF, MXFILS
c     pfstfg  -  NOPEN, RE
c
c ... Included common decks:
c ... pfbufm
c       r:    dircnt(*), wrflag(*)
c ... pfbufp
c       w:    prmbuf(*)
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
      integer   fid, status, ierr
c
c     Internal
      integer   dslen, ids, ifirst, ilast, ip ,tapp, traw
      HU_FPOS_TYPE dsloc, locpck, maxloc, next
      character sc*1, st*1
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFUPCK' )
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
c ... loop over datasets to see if packing is necessary and/or possible
c
      next = LFHDR
      ifirst = 0
      maxloc = 0
      status = 0
c
      do 1 ids=1,dircnt(fid)
c
c ..... get directory info for this dataset
c
        call pfdget ( fid, ids, traw, tapp, dslen, dsloc, st, sc, ierr )
c
c ..... Is the file packed so far ?
c
        if ( ifirst.eq.0 ) then
c
c ....... if file has become unpacked, save dataset # and pointer to hole
c
          if ( dsloc.ne.next ) then
            ifirst = ids
            locpck = next
          endif
c
c ....... increment pointer to where next dataset should be if packed
c
          next = next + dslen
c
        else
c
c ....... the file is unpacked; is it out of order as well ?
c
          if (dsloc.lt.maxloc) then
            status = -1
            goto 999
          endif
c
        endif
c
c ..... save largest dataset pointer so far
c
        maxloc = dsloc
c
    1 continue
      if (ierr.ne.0) goto 999
c
c ... if file is already packed, do nothing; otherwise, pack the file
c
      if (ifirst.ne.0) then
c
c ..... loop over datasets to pack file
c
        do 5 ids=ifirst,dircnt(fid)
c
c ....... get directory info for this dataset & position file
c
          call pfdget (fid, ids, traw, tapp, dslen, dsloc, st, sc, ierr)
          call pfusek ( fid, dsloc, ierr )
          if (ierr.ne.0) goto 999
c
c ....... copy dataset one work buffer at a time
c
          do 3 ip=1,dslen,LPRBUF
c
c ......... check for partial last buffer
c
            ilast = min( LPRBUF, dslen - ip + 1 )
c
c ......... read in a bufferful from dataset
c
            call pfusio ( fid, RE, ilast, prmbuf, ierr )
c
c ......... write bufferful of data back to file; this will not clobber
c ......... needed data since we are packing down !
c
            call pfuwbk ( fid, locpck, ilast, prmbuf, ierr )
c
            if (ierr.ne.0) goto 999
c
c ......... increment address for packed data
c
            locpck = locpck + ilast
c
    3     continue
c
    5   continue
c
        status = 1
c
      endif
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
