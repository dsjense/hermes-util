      subroutine pfwvtx ( fid, tapp, st, sc, m, n, nv, ispare,
     &                    vlabel, alabel, vdim, vert, attr,   ierr )
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
c       - This routine is a WRITE routine that writes a VTX (Vertex
c         List) dataset to a PFF file.
c       - Verticies have dimensionality "m"
c       - Each vertex has "n" attributes
c       - This operation is ONLY ALLOWED in WRITE mode !!!
c       - Dataset Format:
c           <HEADER>       PFTVTX
c           <INT>          M                 (vertex dimensionality)
c           <INT>          N                 (attribute dimensionality)
c           <LONG>         NV                (# of verticies)
c           <INT>x5        ISPARE            (Reserved for application)
c           <STRING>xM     VLABEL            (vertex coordinate labels)
c           <STRING>xN     ALABEL            (attribute labels)
c           LOOP i=1,M                       (DATASET VERSION 1 !!!)
c             <FARRAY>       VERT(i,1:NV)  (mD vertex list)
c           ENDLOOP
c           LOOP i=1,N
c             <FARRAY>       Ai(1:NV)        (ith attribute list)
c           ENDLOOP
c
c
c ----------------------------------------------------------------------
c
c     Input:
c       alabel  -  (*) attribute labels
c       attr    -  (*,*) 2D array containing attribute data
c       ispare  -  (*) spare words reserved for application
c       fid     -  File ID of PFF file
c       m       -  vertex dimensionality
c       n       -  attribute dimensionality
c       nv      -  # of verticies in vertex list
c       sc      -  character string to be loaded with dataset comment
c       st      -  character string to be loaded with dataset type label
c       tapp    -  # associated with application dataset type
c       vdim    -  dimensioned vertex list length [attr(1:vdim,*)]
c       vert    -  (*) mD vertex list array [packed--(1:m,1:nv)]
c       vlabel  -  (*) Vertex coordinate labels
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                      0,          No error
c                      otherwise,  Error in called PFF routine
c
c ----------------------------------------------------------------------
c
c     Internals:
c       buf     -  buffer space for encoding of <LONG>'s, etc.
c       i       -  loop index
c       lds     -  length of dataset written (in 16-bit words)
c       lstadr  -  file position pointer for beginning of dataset
c       prec_x  -  precision type of the ordinate float arrays
c       prec_a  -  precision type of the attribute float arrays
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfetyp  -  PFTVTX
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
      integer   fid, ierr, m, n, nv, tapp, vdim
      integer   ispare(5)
      real      vert(*), attr(1:vdim,*)
      character st*(*), sc*(*)
      character vlabel(*)*(*), alabel(*)*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFWVTX' )
      integer   BUFLEN
      parameter ( BUFLEN = 4 )
c
c     Internal
      integer   i, prec_x, prec_a, tlen
      HU_FPOS_TYPE lds, lstadr
      integer   buf(0:BUFLEN), idlist(3), jdlist(3), kdlist(3)
c
c ----------------------------------------------------------------------
c
      data idlist, jdlist, kdlist / 9*1 /
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
c ... Write header  (dataset version (vds) = 1)
c
      call pfwhed ( fid, PFTVTX, tapp, 1, DFAULT, DFAULT, st, sc,
     &              lstadr, ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... Write out M, N, and NV
c
      if ( ierr.ne.0 ) goto 999
      buf(0) = m
      buf(1) = n
      call pful2i ( nv, buf(2), ierr)
      call pfusio (fid, WR, 5, buf, ierr)
c
c ... write spare words to file
c
      call pfusio (fid, WR, 5, ispare, ierr)
c
c ... Write out vertex labels
c
      do 1 i=1,m
        call pfwchr (fid, vlabel(i), ierr)
    1 continue
c
c ... Write out attribute labels
c
      do 2 i=1,n
        call pfwchr (fid, alabel(i), ierr)
    2 continue
c
c ... Set precision of ordinate and attribute float arrays
c
      if ( fpprec(fid).ne.FP_ORDFULL ) then
        prec_x = fpprec(fid)
        prec_a = fpprec(fid)
      else
        prec_x = FP_FULL
        prec_a = FP_REDU
      endif
c
ccc START ORIGINAL VERSION 
ccc
ccc ... write out vertex list only if M is positive
ccc
cc      if (m.gt.0) then
ccc
cc        call pfwfa ( fid, prec_x, m*nv, vert, 0, ierr)
cc        if ( ierr.ne.0 ) goto 999
ccc
cc      endif
c
cc START Version 1 (current version)
c
c ... loop over vertex dimensions and write each separately
c
      jdlist(2) = nv
      do i=1,m
c
        call pfwfa3 ( fid, prec_x, 1, m, nv, 1,
     &                idlist, jdlist, kdlist, vert(i), 0, ierr)
        if ( ierr.ne.0 ) goto 999
c
      end do
c
c ... now loop over attribute arrays (N)
c
      do 10 i=1,n
c
c ..... write out each attribute array
c
        call pfwfa ( fid, prec_a, nv, attr(1,i), 0, ierr)
        if ( ierr.ne.0 ) goto 999
c
   10 continue
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
      call pfdput ( fid, PFTVTX, tapp, tlen, lstadr, st, sc, ierr )
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
