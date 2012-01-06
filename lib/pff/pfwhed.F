      subroutine pfwhed ( fid, traw, tapp, vds, lds, rfuarr,
     &                    st, sc, lstadr, ierr )
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
c       - This routine is a WRITE routine that writes a dataset's header
c         to a PFF file.
c       - This operation is ONLY ALLOWED in WRITE mode !!!
c       - If the input length (lds) is non-positive, the length is NOT
c         written to the file by this routine.  This length must be
c         filled in by calling the routine PFWLDS after the entire
c         dataset associated with this header has been written.  The
c         value must returned as "lstadr" from this routine be passed as
c         input to PFWLDS.  NOTE THAT this is the preferred way to write
c         dataset length into the header because it insures that the
c         length is correct.
c       - If the RFU fields are not being used, the parameter DFAULT
c         (defined in the INCLUDE File PFSTFG) can be passed in the
c         place of the "rfuarr" array, since the array is accessed only
c         up to its first element containing DFAULT, after which the
c         field is padded with DFAULT. Currently, the 1 lowest-order bit
c         of RFUARR(1) are reserved.
c       - Dataset Header Format:
c           <INT>          DFRAME       dataset framing word  (-1)
c           <LONG>         Lds          dataset length (in 2-byte words)
c           <INT>          Traw         raw dataset type
c           <INT>          Vds          raw dataset type version #
c           <INT>          Tapp         application dataset type
c           10x<INT>       RFU          reserved for future use
c           <STRING>       TYPE         application dataset type label
c           <STRING>       TITLE        dataset title/comment
c       - RFU array format:
c           word 1,  bit 1   -- indicates FP_FULL precision on all 
c                               <FARRAY>'s in this dataset
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file
c       lds     -  length of dataset in 16-bit words
c       rfuarr  -  array of of non-default reserved integers  (maximum
c                  array element accessed is the lesser of RFUMAX or the
c                  first word containing DFAULT)
c       sc      -  character string to be loaded with dataset type label
c       st      -  character string to be loaded with dataset type label
c       tapp    -  # associated with application dataset type
c       traw    -  # associated with raw dataset type
c       vds     -  dataset version #
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       lstadr  -  file position pointer on subroutine entry
c
c ----------------------------------------------------------------------
c
c     Internals:
c       buf     -  buffer space for encoding <LONG> and RFU stream values
c       i       -  loop index over RFU array
c       rfmode  -  flag indicating whether RFU field is being padded
c                  with DFAULT
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfstfg  -  WR, DFAULT, DFRAME
c
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pffils
c       r:    fpprec(*)
c ... pfhrfu
c       r:    RFUMAX
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pferrs.inc"
#include "pffils.inc"
#include "pfhrfu.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ierr, lds, lstadr, tapp, traw, vds
      integer   rfuarr(*)
      character st*(*), sc*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFWHED' )
c
c     Internal
      integer   i, rfmode
      integer   buf(RFUMAX)
      integer   pfutel
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
c ... Find entry (current) file position
c
      lstadr = pfutel ( fid, ierr )
c
c ... Write framing word
c
      call pfusio (fid, WR, 1, DFRAME, ierr)
      if ( ierr.ne.0 ) goto 999
c
      if ( lds.gt.0 ) then
c
c ..... Encode dataset length to <LONG> if it's greater than zero
c
        call pful2i ( lds, buf, ierr )
        if ( ierr.ne.0 ) goto 999
c
      else
c
c ..... otherwise, write DFAULT in encoded integers
c
        buf(1) = DFAULT
        buf(2) = DFAULT
        buf(3) = DFAULT
c
      endif
c
c ... Write encoded length to file
c
      call pfusio (fid, WR, 3, buf, ierr)
      if ( ierr.ne.0 ) goto 999
c
c ... Write out the raw dataset type, version #, and app. dataset type
c
      buf(1) = traw
      buf(2) = vds
      buf(3) = tapp
c
      call pfusio (fid, WR, 3, buf, ierr)
      if ( ierr.ne.0 ) goto 999
c
c ... Write out the Reserved words (max of RFUMAX); pad with DFAULT
c ... after 1st occurence of DFAULT
c
      rfmode = 0
      do 1 i=1,RFUMAX
c
        if ( rfmode.eq.0 ) then
          buf(i) = rfuarr(i)
          if ( rfuarr(i).eq.DFAULT ) rfmode = 1
        else
          buf(i) = DFAULT
        endif
c
    1 continue
c
c ... set RFU's controlled by PFWHED:
c
c ... FP_FULL precision on <FARRAY>'s
      if ( buf(1).ne.DFAULT ) then
        buf(1) = (buf(1)/4)*4
      else
        buf(1) = 0
      endif
      if ( fpprec(fid).eq.FP_FULL ) buf(1) = buf(1) + 1
c
      call pfusio (fid, WR, RFUMAX, buf, ierr)
      if ( ierr.ne.0 ) goto 999
c
c ... Write dataset type label
c
      call pfwchr ( fid, st, ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... Write dataset comment string
c
      call pfwchr ( fid, sc, ierr )
      if ( ierr.ne.0 ) goto 999
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
