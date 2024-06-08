      subroutine pfrni364 ( fid, mode, keep, mblks, mgrdx, mgrdy, mgrdz,
     &                    mspare, mdim, tapp, st, sc, nblks, nx, ny, nz,
     &                    nspare, ispare, x, y, z, goff10,
     &                    xlabel, ylabel, zlabel, blabel, locx, locy,
     &                    locz, locia, lenia, iarray, ierr )
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
c       - This routine is a READ routine that reads a NI3 (nonuniform,
c         3D, Integer) dataset from a PFF file.
c       - This operation is ONLY ALLOWED in READ mode !!!
c       - This routine operates in two modes:
c           0)  The Integer array is returned in "iarray"
c           1)  The integer array is NOT returned; instead, a
c               PFF file pointer ("locia") and array length ("lenia")
c               is returned for each block.
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
c       blabel  -  (*) If blabel(1) = '&' on input, the dimesioned size of
c                  blabel, xlabel, ylabel, and zlabel is assumed to be 1,
c                  and the returned values for each will be those for the
c                  last (nblks) block in the dataset
c       fid     -  File ID of PFF file
c       keep    -  logical flag indicating whether or not to keep a
c                  non-zero value in the case of underflow
c       mblks   -  dimension size of arrays over blocks
c       mdim    -  dimension size of integer array "iarray"
c       mgrdx   -  dimension size of X grid array "x"
c       mgrdy   -  dimension size of Y grid array "y"
c       mgrdz   -  dimension size of Z grid array "z"
c       mspare  -  1st dimension of ispare array ( mspare .ge. nspare)
c       mode    -  subroutine mode flag --
c                   = 0,        return integer array in "iarray"
c                   otherwise,  return pointer and length in "locia"
c                               and "lenia"
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       blabel  -  (*) Title label for each block
c       goff10  -  (*) power-of-ten offset for grid arrays for each block
c       iarray  -  (*) linear array containing integer data (Mode 0
c                  only)
c       ispare  -  (*,*) spare words reserved for application for each block
c       lenia   -  (*) length of integer array for each block
c       locia   -  (*) pointer to integer array for each block:
c                        Mode 0:  FWA of block in "iarray"
c                        Mode 1:  PFF file pointer to <IARRAY> "iarray"
c       locx    -  (*) FWA of block in the X grid array "x"
c       locy    -  (*) FWA of block in the Y grid array "y"
c       locz    -  (*) FWA of block in the Z grid array "z"
c       nblks   -  # of data blocks to be read from file
c       nspare  -  # of spare words used in each block
c       nx      -  (*) # of values in X direction for each block
c       ny      -  (*) # of values in Y direction for each block
c       nz      -  (*) # of values in Z direction for each block
c       sc      -  character string to be loaded with dataset comment
c       st      -  character string to be loaded with dataset type label
c       tapp    -  # associated with application dataset type
c       x       -  (*) grid array in X direction for each block
c       xlabel  -  (*) X Label for each block
c       y       -  (*) grid array in Y direction for each block
c       ylabel  -  (*) Y Label for each block
c       z       -  (*) grid array in Z direction for each block
c       zlabel  -  (*) Z Label for each block
c       ierr    -  error status flag;
c                     -1,   EOF marker encountered (Not really an error)
c                      0,   No error
c                      1,   Incorrect dataset type
c                      2,   Insufficient array dimensions
c                      3,   File offset (locia) overflow
c
c ----------------------------------------------------------------------
c
c     Internals:
c       buf     -  (*) work buffer for decoding <FLOAT>s, etc.
c       i       -  misc. loop index
c       iblk    -  loop index over data blocks
c       iloc    -  pointer to FWA of "iarray" in each block
c       ilocx   -  pointer to FWA of "x" array in each block
c       ilocy   -  pointer to FWA of "y" array in each block
c       ilocz   -  pointer to FWA of "z" array in each block
c       io1-3   -  power-of-10 offsets for grid arrays
c       lbblk   -  index of [bxyz]label array to read from file for each block
c       lbmax   -  maximum index to use for lbblk
c       lds     -  length of dataset read (in 16-bit words)
c       lstadr  -  file pointer on subroutine entry
c       nblen   -  non-blank character string length
c       nchrsc  -  non-blank length of header comment string
c       nchrst  -  non-blank length of header datatype label string
c       nrfu    -  # of non-default header reserved integers
c       off     -  power-of-ten multiplier for grid arrays
c       slen    -  truncated character string length
c       tnx     -  # of values in X direction for current block
c       tny     -  # of values in Y direction for current block
c       tnz     -  # of values in Z direction for current block
c       traw    -  # associated with raw datatype from input file
c       trunsc  -  # of characters truncated from header comment string
c       trunst  -  # of characters truncated from header datatype label
c                  string
c       vds     -  dataset version #
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfetyp  -  PFTNI3
c     pfstfg  -  RE
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pfetyp.inc"
#include "pfstfg.inc"
#include "pferrs.inc"
c
#include "mdfpos.h"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      logical   keep
      integer   fid, ierr, mblks, mdim, mgrdx, mgrdy, mgrdz, mode
      integer   mspare, nblks, nblks_arr(1), nspare, tapp
      integer   goff10(mblks)
      integer   ispare(mspare,mblks), lenia(mblks)
      integer   locx(mblks), locy(mblks), locz(mblks)
      integer   nx(mblks), ny(mblks), nz(mblks)
      integer   iarray(mdim)
      HU_FPOS_TYPE locia(mblks)
      real      x(mgrdx), y(mgrdy), z(mgrdz)
      character st*(*), sc*(*)
      character xlabel(mblks)*(*), ylabel(mblks)*(*)
      character zlabel(mblks)*(*), blabel(mblks)*(*)
c
c ... Parameters:
      character PFMODU*8
      parameter ( PFMODU = 'PFRNI364' )
      integer   BUFLEN
      parameter ( BUFLEN = 8 )
c
c     Internal
      integer   i, io1, io2, io3, iloc, ilocx, ilocy, ilocz, iblk
      integer   lbblk, lbmax, lds, vds, traw, nrfu, off, tnx
      integer   tny, tnz, slen, nblen, nchrst, nchrsc, trunst, trunsc
      HU_FPOS_TYPE lstadr
      integer   buf(0:BUFLEN)
      HU_FPOS_TYPE pfutel
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
c ... Read header
c
      call pfrhed ( fid, lstadr, traw, tapp, vds, lds, nrfu,
     &              st, nchrst, trunst, sc, nchrsc, trunsc,
     &              ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... return if this is not the right dataset type  (PFTNI3)
c
      if( traw.ne.PFTNI3 ) then
        ierr = 1
        call pfserr ( PFMODU, ierr, fid,
     &                          'Incorrect dataset type' )
        goto 999
      endif
c
c ... Read # of blocks
c
      nblks_arr = nblks
      call pfusio (fid, RE, 1, nblks_arr, ierr)
      nblks = nblks_arr(1)
      if ( ierr.ne.0 ) goto 999
c
c ... return if # of blocks is larger than array dimension size
c
      if(nblks.gt.mblks)then
        ierr = 2
        call pfserr ( PFMODU, ierr, fid,
     &                          'Insufficient array dimensions' )
        goto 999
      endif
c
c ... handle special case of '&' in 1st character of blabel(1) on input:
c ... return labels for last block in the input dataset in [bxyz]label(1)
c
      if ( blabel(1) .eq. '&' ) then
        lbmax = 1
      else
        lbmax = nblks
      endif
c
c ... Loop over all blocks
c
      iloc  = 1
      ilocx = 1
      ilocy = 1
      ilocz = 1
c
      do 50 iblk=1,nblks
c
c ..... Read in grid size
c
        call pfusio (fid, RE, 9, buf, ierr)
        call pfui2l ( buf(0), nx(iblk), ierr)
        call pfui2l ( buf(3), ny(iblk), ierr)
        call pfui2l ( buf(6), nz(iblk), ierr)
c
c ..... Read spare word integer array
c
        call pfria ( fid, mspare, nspare, ispare(1,iblk), ierr )
c
c ..... read in grid arrays
c
        call pfrfa ( fid, mgrdx-ilocx+1, keep, tnx,
     &                  x(ilocx), io1, x(ilocx), ierr)
        call pfrfa ( fid, mgrdy-ilocy+1, keep, tny,
     &                  y(ilocy), io2, y(ilocy), ierr)
        call pfrfa ( fid, mgrdz-ilocz+1, keep, tnz,
     &                  z(ilocz), io3, z(ilocz), ierr)
c
        if ( ierr.ne.0 ) goto 999
c
c ..... Find maximum power-of-ten offset and normalize to that offset
c
        goff10(iblk) = max ( io1, io2, io3 )
c
        if ( io1.ne.goff10(iblk) ) then
          off = 10.**(io1 - goff10(iblk))
          do 1 i=0,tnx-1
            x(i+ilocx) = x(i+ilocx) * off
    1     continue
        endif
c
        if ( io2.ne.goff10(iblk) ) then
          off = 10.**(io2 - goff10(iblk))
          do 2 i=0,tny-1
            y(i+ilocy) = y(i+ilocy) * off
    2     continue
        endif
c
        if ( io3.ne.goff10(iblk) ) then
          off = 10.**(io3 - goff10(iblk))
          do 3 i=0,tnz-1
            z(i+ilocz) = z(i+ilocz) * off
    3     continue
        endif
c
c ..... Read in grid & block labels;
c .....   don't worry about lengths & truncation
c
        lbblk = min(lbmax,iblk)
c
        call pfrchr (fid, xlabel(lbblk), slen, nblen, ierr)
        call pfrchr (fid, ylabel(lbblk), slen, nblen, ierr)
        call pfrchr (fid, zlabel(lbblk), slen, nblen, ierr)
        call pfrchr (fid, blabel(lbblk), slen, nblen, ierr)
c
c       What mode are we in ??
c
        if ( mode.eq.0 ) then
c
c ....... set FWA array pointer; Read out array
c
          call pfria ( fid, mdim-iloc+1, lenia(iblk),
     &                 iarray(iloc), ierr)
c
          locia(iblk) = iloc
          iloc  = iloc  + lenia(iblk)
c
        else
c
c ....... find out where we are in file; skip over scale and offset;
c .......    read in the array length; then reposition file pointer
c .......    to end of array
c
          locia(iblk) = pfutel (fid, ierr )
          call pfusio (fid, RE, 3, buf, ierr)
          call pfui2l ( buf, lenia(iblk), ierr)
          call pfusek (fid, locia(iblk)+lenia(iblk)+3, ierr )
c
        endif
c
        if ( ierr.ne.0 ) goto 999
c
        locx(iblk) = ilocx
        locy(iblk) = ilocy
        locz(iblk) = ilocz
c
        ilocx = ilocx + nx(iblk)
        ilocy = ilocy + ny(iblk)
        ilocz = ilocz + nz(iblk)
c
   50 continue
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
