      subroutine pfruf3 ( fid, mode, keep, mblks, mdim, tapp, st, sc,
     &                    nblks, nx, ny, nz, ispare,
     &                    x0, dx, y0, dy, z0, dz, goff10,
     &                    xlabel, ylabel, zlabel, blabel,
     &                    locfa, lenfa, farray, foff10, ierr )
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
c       - This routine is a READ routine that reads a UF3 (uniform,
c         3D, Floating) dataset from a PFF file.
c       - This operation is ONLY ALLOWED in READ mode !!!
c       - This routine operates in two modes:
c           0)  The floating array is returned in "farray"
c           1)  The floating array is NOT returned; instead, a
c               PFF file pointer ("locfa") and array length ("lenfa")
c               is returned for each block.
c       - Dataset Format:
c           <HEADER>       PFTUF3
c           <INT>          NBLKS  (# of data blocks)
c           LOOP 1,NBLKS
c             <LONG>         NX
c             <LONG>         NY
c             <LONG>         NZ
c             <INT>x5        Reserved for application (ISPARE)
c             <FLOAT>        X0
c             <FLOAT>        DX
c             <FLOAT>        Y0
c             <FLOAT>        DY
c             <FLOAT>        Z0
c             <FLOAT>        DZ
c             <STRING>       XLABEL
c             <STRING>       YLABEL
c             <STRING>       ZLABEL
c             <STRING>       BLABEL
c             <FARRAY>       FARRAY
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
c       mdim    -  dimension size of integer array "farray"
c       mode    -  subroutine mode flag --
c                   = 0,        return integer array in "farray"
c                   otherwise,  return pointer and length in "locfa"
c                               and "lenfa"
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       blabel  -  (*) Title label for each block
c       dx      -  (*) grid separation in X direction for each block
c       dy      -  (*) grid separation in Y direction for each block
c       dz      -  (*) grid separation in Z direction for each block
c       foff10  -  (*) power-of-ten offset for floating array for each
c                  block
c       goff10  -  (*) power-of-ten offset for grid info for each block
c       farray  -  (*) linear array containing floating data (Mode 0
c                  only)
c       ispare  -  (*,*) spare words reserved for application for each block
c       lenfa   -  (*) length of integer array for each block
c       locfa   -  (*) pointer to floating array for each block:
c                        Mode 0:  FWA of block in "farray"
c                        Mode 1:  PFF file pointer to <FARRAY> "farray"
c       nblks   -  # of data blocks to be read from file
c       nx      -  (*) # of values in X direction for each block
c       ny      -  (*) # of values in Y direction for each block
c       nz      -  (*) # of values in Z direction for each block
c       sc      -  character string to be loaded with dataset comment
c       st      -  character string to be loaded with dataset type label
c       tapp    -  # associated with application dataset type
c       x0      -  (*) initial X grid value for each block
c       xlabel  -  (*) X Label for each block
c       y0      -  (*) initial Y grid value for each block
c       ylabel  -  (*) Y Label for each block
c       z0      -  (*) initial Z grid value for each block
c       zlabel  -  (*) Z Label for each block
c       ierr    -  error status flag;
c                     -1,   EOF marker encountered (Not really an error)
c                      0,   No error
c                      1,   Incorrect dataset type
c                      2,   Insufficient array dimensions
c                      3,   File offset (locfa) overflow
c
c ----------------------------------------------------------------------
c
c     Internals:
c       buf     -  work buffer for decoding <FLOAT>s, etc.
c       floc    -  temporary file offet value
c       iblk    -  loop index over data blocks
c       iloc    -  pointer to FWA of "farray" in each block
c       io1-6   -  power-of-10 offsets for floats
c       lbblk   -  index of [bxyz]label array to read from file for each block
c       lbmax   -  maximum index to use for lbblk
c       lds     -  length of dataset read (in 16-bit words)
c       lstadr  -  file pointer on subroutine entry
c       nblen   -  non-blank character string length
c       nchrsc  -  non-blank length of header comment string
c       nchrst  -  non-blank length of header datatype label string
c       nrfu    -  # of non-default header reserved integers
c       slen    -  truncated character string length
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
c     pfetyp  -  PFTUF3
c     pfstfg  -  RE
c
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
      integer   fid, ierr, mblks, mdim, mode, nblks, nblks_arr(1), tapp
      integer   foff10(mblks), goff10(mblks)
      integer   ispare(5,mblks), lenfa(mblks), locfa(mblks), nx(mblks)
      integer   ny(mblks), nz(mblks)
      real      farray(mdim)
      real      dx(mblks), dy(mblks), dz(mblks)
      real      x0(mblks), y0(mblks), z0(mblks)
      character st*(*), sc*(*)
      character xlabel(mblks)*(*), ylabel(mblks)*(*)
      character zlabel(mblks)*(*), blabel(mblks)*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFRUF3' )
      integer   BUFLEN
      parameter ( BUFLEN = 31 )
c
c     Internal
      integer   io1, io2, io3, io4, io5, io6, iloc, iblk, lbblk, lbmax
      integer   lds, vds, traw, nrfu
      integer   slen, nblen, nchrst, nchrsc, trunst, trunsc
      HU_FPOS_TYPE lstadr, floc
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
c ... return if this is not the right dataset type  (PFTUF3)
c
      if( traw.ne.PFTUF3 ) then
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
      iloc = 1
c
      do 1 iblk=1,nblks
c
c ..... Read in grid size and spare words (5)
c
        call pfusio (fid, RE, 9, buf, ierr)
        call pfui2l ( buf(0), nx(iblk), ierr)
        call pfui2l ( buf(3), ny(iblk), ierr)
        call pfui2l ( buf(6), nz(iblk), ierr)
        call pfusio (fid, RE, 5, ispare(1,iblk), ierr)
c
c ..... Load buffer with real grid info (X0, DX, etc.); then decode it
c
        call pfusio (fid, RE, 18, buf, ierr)
        call pfui2f (.FALSE., buf(0),  x0(iblk), io1, ierr)
        call pfui2f (.FALSE., buf(3),  dx(iblk), io2, ierr)
        call pfui2f (.FALSE., buf(6),  y0(iblk), io3, ierr)
        call pfui2f (.FALSE., buf(9),  dy(iblk), io4, ierr)
        call pfui2f (.FALSE., buf(12), z0(iblk), io5, ierr)
        call pfui2f (.FALSE., buf(15), dz(iblk), io6, ierr)
c
c ..... Find maximum power-of-ten offset and normalize to that offset
c
        goff10(iblk) = max ( io1, io2, io3, io4, io5, io6 )
c
        if ( goff10(iblk).ne.io1) then
          x0(iblk) = x0(iblk)*10.**(io1 - goff10(iblk))
        endif
        if ( goff10(iblk).ne.io2) then
          dx(iblk) = dx(iblk)*10.**(io2 - goff10(iblk))
        endif
        if ( goff10(iblk).ne.io3) then
          y0(iblk) = y0(iblk)*10.**(io3 - goff10(iblk))
        endif
        if ( goff10(iblk).ne.io4) then
          dy(iblk) = dy(iblk)*10.**(io4 - goff10(iblk))
        endif
        if ( goff10(iblk).ne.io5) then
          z0(iblk) = z0(iblk)*10.**(io5 - goff10(iblk))
        endif
        if ( goff10(iblk).ne.io6) then
          dz(iblk) = dz(iblk)*10.**(io6 - goff10(iblk))
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
          call pfrfa ( fid, mdim-iloc+1, keep, lenfa(iblk),
     &                  farray(iloc), foff10(iblk), farray(iloc), ierr)
c
          locfa(iblk) = iloc
          iloc = iloc + lenfa(iblk)
c
        else
c
c ....... Get pointer to first word of the array, and verify that it is
c ....... in range for an INTEGER value. Then use pfuskiparr to skip
c ....... over the array, returning the length
c
          floc = pfutel (fid, ierr )
          locfa(iblk) = floc
          if( floc.eq.locfa(iblk) ) then
            call pfuskiparr(fid, floc, lenfa(iblk), ierr)
          else
            ierr = 3
            call pfserr ( PFMODU, ierr, fid,
     &                          'File offset (locfa) overflow' )
          endif
c
        endif
c
        if ( ierr.ne.0 ) goto 999
c
    1 continue
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
