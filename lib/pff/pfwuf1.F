      subroutine pfwuf1 ( fid, tapp, st, sc, nblks, ispare,
     &                    x0, dx, xlabel, blabel, locb, low,
     &                    imax, idlist, farray,     ierr )
c
c***********************************************************************
c   $Id$
c
c   Copyright (2008) Sandia Corporation. Under the terms of
c   Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
c   Government retains certain rights in this software.
c
c   Hermes is free software: you can redistribute it and/or modify
c   it under the terms of the GNU Lesser General Public License as
c   published by the Free Software Foundation, either version 3 of
c   the License, or (at your option) any later version.
c
c   Hermes is distributed in the hope that it will be useful, but
c   WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU Lesser General Public License for more details.
c
c   You should have received a copy of the GNU Lesser General
c   Public License along with Hermes.  If not, see
c   <http://www.gnu.org/licenses/>.
c
C_Groups @(#)
c***********************************************************************
c
c
c ----------------------------------------------------------------------
c
c     Summary:
c
c       - This routine is a WRITE routine that writes a UF1 (uniform,
c         1D, Floating) dataset to a PFF file.
c       - The input array is assumed to be blocked.  Dimensioned size
c         of each block is passed as well as a do-list for each block
c         for what is to be written to file.
c       - NOTE:  X0 is associated with the lowest dimensioned point
c         ("low") in "farray" for each block.  Similarly, DX is the
c         grid separation of "farray" for each block.  If "idlist(1)"
c         .gt. "low", the X0 written to the file is adjusted to
c         correspond grid location grid location "idlist(1)".
c         Similarly, if "idlist(3)" .gt. 1, the DX written to the file
c         is adjusted to be "idlist(3)" times the input DX.
c       - This operation is ONLY ALLOWED in WRITE mode !!!
c       - Dataset Format:
c           <HEADER>       PFTUF1
c           <INT>          NBLKS  (# of data blocks)
c           LOOP 1,NBLKS
c             <LONG>         NX
c             <INT>x5        Reserved for application (ISPARE)
c             <FLOAT>        X0
c             <FLOAT>        DX
c             <STRING>       XLABEL
c             <STRING>       BLABEL
c             <FARRAY>       FARRAY
c           ENDLOOP
c
c
c ----------------------------------------------------------------------
c
c     Input:
c       blabel  -  (*) Title label for each block. If 1st character of
c                  blabel(1) is '&', then the strings in blabel(1)
c                  and xlabel(1) are used for all blocks. In this case,
c                  blabel is assumed to be all characters after the
c                  leading '&'.
c       dx      -  (*) grid separation in X direction for each block
c       farray  -  (*) linear array containing floating point data
c       fid     -  File ID of PFF file
c       ispare  -  (*,*) spare words reserved for application for each block
c       idlist  -  (*,*) array do-list for mth block of "farray", i.e.,
c                  (do i=idlist(1,m),idlist(2,m),idlist(3,m))
c       imax    -  (*) upper dimension of "farray" for each block
c       locb    -  (*) pointer to FWA of each block in "farray"
c       low     -  lower dimension of each block of "farray"
c       nblks   -  # of data blocks to be written to file
c       sc      -  character string to be loaded with dataset comment
c       st      -  character string to be loaded with dataset type label
c       tapp    -  # associated with application dataset type
c       x0      -  (*) initial grid value for each block
c       xlabel  -  (*) X Label for each block (see blabel)
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                      0,  No error
c                      1,  Illegal DO-List
c
c ----------------------------------------------------------------------
c
c     Internals:
c       bl1     -  1st character of blabel string to write to file
c       bloc    -  buffer pointer
c       buf     -  buffer space for encoding of <LONG>'s, etc.
c       iblk    -  loop index over data blocks
c       lbblk   -  index of [bx]label array to write to file for each block
c       lbmax   -  maximum index to use for lbblk
c       lds     -  length of dataset written (in 16-bit words)
c       lstadr  -  file position pointer for beginning of dataset
c       nx      -  size of output array in "x" direction
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfetyp  -  PFTUF1
c     pfstfg  -  FP_REDU, FP_FULL, WR, DFAULT
c
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pffils
c       r:    fpprec(*)
c
#include "pfbsiz.inc"
#include "pfetyp.inc"
#include "pfstfg.inc"
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
      integer   fid, ierr, low, nblks, tapp
      integer   ispare(5,nblks), locb(nblks)
      integer   imax(nblks), idlist(3,nblks)
      real      farray(low:*)
      real      dx(nblks), x0(nblks)
      character st*(*), sc*(*)
      character xlabel(nblks)*(*), blabel(nblks)*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFWUF1' )
      integer   BUFLEN
      parameter ( BUFLEN = 5 )
c
c     Internal
      integer   bl1, bloc, iblk, lbblk, lbmax, nx, tlen
      HU_FPOS_TYPE lds, lstadr
      integer   buf(0:BUFLEN)
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
c ... Write header
c
      call pfwhed ( fid, PFTUF1, tapp, DFAULT, DFAULT, DFAULT, st, sc,
     &              lstadr, ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... Write # of blocks
c
      call pfusio_scalar (fid, WR, 1, nblks, ierr)
      if ( ierr.ne.0 ) goto 999
c
c ... handle special case of '&' in 1st character of blabel(1)
c ...   1) use labels in xlabel(1) for corresponding label in all blocks
c ...   2) use remainder (after '&') of blabel(1) for all blocks
c
      if ( blabel(1)(1:1) .eq. '&' ) then
        bl1 = 2
        lbmax = 1
      else
        bl1 = 1
        lbmax = nblks
      endif
c
c ... Loop over all blocks
c
      do 1 iblk=1,nblks
c
c ..... check for illegal do-list (don't allow zero-trip)
c
        if( (idlist(2,iblk) - idlist(1,iblk))*idlist(3,iblk).lt.0 .OR.
     &      idlist(3,iblk).eq.0                             ) then
          ierr = 1
          call pfserr ( PFMODU, ierr, fid, 'Illegal DO-List' )
          goto 999
        endif
c
c ..... Load buffer with grid size and write to file
c
        nx = (idlist(2,iblk) - idlist(1,iblk))/idlist(3,iblk) + 1
c
        call pful2i ( nx, buf, ierr )
        call pfusio (fid, WR, 3, buf, ierr)
c
c ..... write spare words to file
c
        call pfusio (fid, WR, 5, ispare(1,iblk), ierr)
c
c ..... Load buffer with real grid info (X0, DX, etc.)
c
        bloc = 0
        call pfuf2i ( x0(iblk) + (idlist(1,iblk)-low)*dx(iblk),
     &                0, buf(bloc), ierr)
        bloc = bloc + 3
        call pfuf2i ( idlist(3,iblk)*dx(iblk), 0, buf(bloc), ierr)
        bloc = bloc + 3
c
c ..... Write out buffer
c
        call pfusio (fid, WR, bloc, buf, ierr)
c
c ..... Write out grid & block labels
c
        lbblk = min(lbmax,iblk)
c
        call pfwchr (fid, xlabel(lbblk), ierr)
        call pfwchr (fid, blabel(lbblk)(bl1:), ierr)
c
        if ( ierr.ne.0 ) goto 999
c
c ..... Write out array -- fake array as 3D to sparsify by do-list
c
        buf(0) = low
        buf(1) = low
        buf(2) = 1
c
        if ( fpprec(fid).ne.FP_ALLFULL ) then
          call pfwfa3 ( fid, FP_REDU, low, imax(iblk), low, low,
     &                  idlist(1,iblk), buf, buf,
     &                  farray(locb(iblk)), 0, ierr)
        else
          call pfwfa3 ( fid, FP_FULL, low, imax(iblk), low, low,
     &                  idlist(1,iblk), buf, buf,
     &                  farray(locb(iblk)), 0, ierr)
        endif
        if ( ierr.ne.0 ) goto 999
c
    1 continue
c
c ----------------------------------------------------------------------
c
c ... now call routine to write dataset length into the header
c
      call pfwlds ( fid, lstadr, lds, ierr )
c
c ... put directory info into memory-resident directory structure
c
      tlen = lds
      call pfdput ( fid, PFTUF1, tapp, tlen, lstadr, st, sc, ierr )
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
