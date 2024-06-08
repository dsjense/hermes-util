      subroutine pfwni3 ( fid, tapp, st, sc, nblks, mspare, nspare,
     &                    ispare, x, y, z, xlabel, ylabel,
     &                    zlabel, blabel, locx, locy, locz, locb, low,
     &                    imax, jmax, kmax, idlist, jdlist, kdlist,
     &                    mskbits, shbits, iarray, ierr )
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
c       - This routine is a WRITE routine that writes a NI3
c         (non-uniform, 3D, Integer) dataset to a PFF file.
c       - The input array is assumed to be block-3D.  Dimensioned size
c         of each 3D block is passed as well as a do-list in each
c         direction of each block for what is to be written to file.
c       - If mskbits is zero, the supplied input array is simply
c         truncated to 16-bits and all integers in the array must be in
c         the range -32768 to 32767.
c       - If mskbits is nonzero, the value written to the file is shifted
c         and masked from the supplied input iarray, i.e.,
c               ival = mod( iarray/(2**shbits),2**mskbits)
c         Note that in this mode, the supplied values in iarray must be
c         non-negative.
c       - This operation is ONLY ALLOWED in WRITE mode !!!
c       - Dataset Format:
c           <HEADER>       PFTNI3
c           <INT>          NBLKS  (# of data blocks)
c           LOOP 1,NBLKS
c             <LONG>         NX
c             <LONG>         NY
c             <LONG>         NZ
c             <IARRAY>       ISPARE(1:NSPARE) (Reserved for application)
c             <FARRAY>       X(1:NX)
c             <FARRAY>       Y(1:NY)
c             <FARRAY>       Z(1:NZ)
c             <STRING>       XLABEL
c             <STRING>       YLABEL
c             <STRING>       ZLABEL
c             <STRING>       BLABEL
c             <IARRAY>       IARRAY(1:NX*NY*NZ)
c           ENDLOOP
c
c
c ----------------------------------------------------------------------
c
c     Input:
c       blabel  -  (*) Title label for each block. If 1st character of
c                  blabel(1) is '&', then the strings in blabel(1),
c                  xlabel(1), ylabel(1), and zlabel(1) are used for all
c                  blocks. In this case, blabel is assumed to be all
c                  characters after the leading '&'.
c       iarray  -  (*) linear array containing integer data
c       fid     -  File ID of PFF file
c       ispare  -  (*,*) spare words reserved for application for each block
c       idlist  -  (*,*) array do-list in "x" dimension for mth block of
c                  "iarray", (do i=idlist(1,m),idlist(2,m),idlist(3,m))
c       imax    -  (*) upper "x" dimension of "iarray" for each block
c       jdlist  -  (*,*) array do-list in "y" dimension for mth block of
c                  "iarray", (do j=jdlist(1,m),jdlist(2,m),jdlist(3,m))
c       jmax    -  (*) upper "y" dimension of "iarray" for each block
c       kdlist  -  (*,*) array do-list in "z" dimension for mth block of
c                  "iarray", (do k=kdlist(1,m),kdlist(2,m),kdlist(3,m))
c       kmax    -  (*) upper "z" dimension of "iarray" for each block
c       locb    -  (*) pointer to FWA of each block in "iarray"
c       locx    -  (*) FWA of block in the X grid array "x"
c       locy    -  (*) FWA of block in the Y grid array "y"
c       locz    -  (*) FWA of block in the Z grid array "z"
c       low     -  lower dimension for all "i,j,k" directions for each
c                  block of "iarray" and lower dimension for "x", "y",
c                  and "z" arrays
c       nblks   -  # of data blocks to be written to file
c       mspare  -  1st dimension of ispare array ( mspare .ge. nspare)
c       nspare  -  # of spare words used in each block
c       mskbits -  # of bits from input word to be written to dataset
c       shbits  -  # of bits to be shifted in input word before applying
c                  bit mask (mskbits)
c       sc      -  character string to be loaded with dataset comment
c       st      -  character string to be loaded with dataset type label
c       tapp    -  # associated with application dataset type
c       x       -  (*) grid array in X direction for each block
c       xlabel  -  (*) X Label for each block (see blabel)
c       y       -  (*) grid array in Y direction for each block
c       ylabel  -  (*) Y Label for each block (see blabel)
c       z       -  (*) grid array in Z direction for each block
c       zlabel  -  (*) Z Label for each block (see blabel)
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
c       lbblk   -  index of [bxyz]label array to write to file for each block
c       lbmax   -  maximum index to use for lbblk
c       lds     -  length of dataset written (in 16-bit words)
c       lstadr  -  file position pointer for beginning of dataset
c       nx      -  size of output array in "x" direction
c       ny      -  size of output array in "y" direction
c       nz      -  size of output array in "z" direction
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfetyp  -  PFTNI3
c     pfstfg  -  FP_REDU, FP_FULL, FP_ORDFULL, WR, DFAULT
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
      integer   fid, ierr, low, mskbits, mspare, nspare, nblks,
     &          nblks_arr(1), shbits, tapp
      integer   ispare(mspare,nblks)
      integer   locb(nblks), locx(nblks), locy(nblks), locz(nblks)
      integer   imax(nblks), jmax(nblks), kmax(nblks)
      integer   idlist(3,nblks), jdlist(3,nblks), kdlist(3,nblks)
      integer      iarray(low:*)
      real      x(low:*), y(low:*), z(low:*)
      character st*(*), sc*(*)
      character xlabel(nblks)*(*), ylabel(nblks)*(*)
      character zlabel(nblks)*(*), blabel(nblks)*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFWNI3' )
      integer   BUFLEN
      parameter ( BUFLEN = 8 )
c
c     Internal
      integer   bl1, bloc, iblk, lbblk, lbmax, nx, ny, nz, prec_x, tlen
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
      call pfwhed ( fid, PFTNI3, tapp, DFAULT, DFAULT, DFAULT, st, sc,
     &              lstadr, ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... Write # of blocks
c
      nblks_arr = nblks
      call pfusio (fid, WR, 1, nblks_arr, ierr)
      nblks = nblks_arr(1)
      if ( ierr.ne.0 ) goto 999
c
c ... handle special case of '&' in 1st character of blabel(1)
c ...   1) use labels in [xyz]label(1) for corresponding label in all blocks
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
c ... Set precision of ordinate arrays
c
      if ( fpprec(fid).ne.FP_ORDFULL ) then
        prec_x = fpprec(fid)
      else
        prec_x = FP_FULL
      endif
c
c ... Loop over all blocks
c
      do 1 iblk=1,nblks
c
c ..... check for illegal do-list (don't allow zero-trip)
c
        if( (idlist(2,iblk) - idlist(1,iblk))*idlist(3,iblk).lt.0 .OR.
     &      (jdlist(2,iblk) - jdlist(1,iblk))*jdlist(3,iblk).lt.0 .OR.
     &      (kdlist(2,iblk) - kdlist(1,iblk))*kdlist(3,iblk).lt.0 .OR.
     &      idlist(3,iblk).eq.0  .OR. jdlist(3,iblk).eq.0   .OR.
     &      kdlist(3,iblk).eq.0                             ) then
          ierr = 1
          call pfserr ( PFMODU, ierr, fid, 'Illegal DO-List' )
          goto 999
        endif
c
c ..... Load buffer with grid sizes and write to file
c
        nx = (idlist(2,iblk) - idlist(1,iblk))/idlist(3,iblk) + 1
        ny = (jdlist(2,iblk) - jdlist(1,iblk))/jdlist(3,iblk) + 1
        nz = (kdlist(2,iblk) - kdlist(1,iblk))/kdlist(3,iblk) + 1
c
        bloc = 0
        call pful2i ( nx, buf(bloc), ierr )
        bloc = bloc + 3
        call pful2i ( ny, buf(bloc), ierr )
        bloc = bloc + 3
        call pful2i ( nz, buf(bloc), ierr )
        bloc = bloc + 3
        call pfusio (fid, WR, bloc, buf, ierr)
c
c ..... Write out spare word integer array
c
        call pfwia (fid, nspare, ispare(1,iblk), ierr)
c
c ..... write out grid arrays -- fake arrays as 3D to sparsify by do-list
c
        buf(0) = low
        buf(1) = low
        buf(2) = 1
c
        call pfwfa3 ( fid, prec_x, low, imax(iblk), low, low,
     &                idlist(1,iblk),   buf,  buf,
     &                x(locx(iblk)), 0, ierr)
c
        call pfwfa3 ( fid, prec_x, low, jmax(iblk), low, low,
     &                jdlist(1,iblk),   buf,  buf,
     &                y(locy(iblk)), 0, ierr)
c
        call pfwfa3 ( fid, prec_x, low, kmax(iblk), low, low,
     &                kdlist(1,iblk),   buf,  buf,
     &                z(locz(iblk)), 0, ierr)
c
c ..... Write out grid & block labels
c
        lbblk = min(lbmax,iblk)
c
        call pfwchr (fid, xlabel(lbblk), ierr)
        call pfwchr (fid, ylabel(lbblk), ierr)
        call pfwchr (fid, zlabel(lbblk), ierr)
        call pfwchr (fid, blabel(lbblk)(bl1:), ierr)
c
        if ( ierr.ne.0 ) goto 999
c
c ..... Write out array
c
        call pfwia3 ( fid, low, imax(iblk), jmax(iblk), kmax(iblk),
     &                idlist(1,iblk), jdlist(1,iblk), kdlist(1,iblk),
     &                mskbits, shbits, iarray(locb(iblk)), ierr)
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
      call pfdput ( fid, PFTNI3, tapp, tlen, lstadr, st, sc, ierr )
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
