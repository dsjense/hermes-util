      subroutine pfrvtx64 ( fid, mode, keep, mdim, ndim, vdim, adim,
     &                    m, n, nv, nvr, tapp, st, sc, ispare,
     &                    vlabel, alabel, vert, voff10, attr, aoff10,
     &                    locv, loca,    ierr )
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
c       - This routine is a READ routine that reads a VTX (Vertex List)
c         dataset from a PFF file.
c       - This routine supports two dataset versions (vds=DFAULT & vds=1)
c       - Verticies have dimensionality "m"
c       - Each vertex has "n" attributes
c       - This operation is ONLY ALLOWED in READ mode !!!
c       - This routine operates in three modes:
c           0)  The attribute array ("attr") is assumed to be a 2D array
c               with dimension  --  ATTR(1:ADIM,1:NDIM)
c           1)  The attribute array ("attr") is assumed to be a 1D array
c               with dimension  --  ATTR(1:ADIM); into which the
c               attribute data is densely packed
c           2)  The vertex and attribute arrays are NOT returned;
c               instead, PFF file pointers to the vertex array and the
c               attribute arrays ("locv" and "loca", respectively) are
c               returned
c       - Dataset Format:
c           <HEADER>       PFTVTX
c           <INT>          M                 (vertex dimensionality)
c           <INT>          N                 (attribute dimensionality)
c           <LONG>         NV                (# of verticies)
c           <INT>x5        ISPARE            (Reserved for application)
c           <STRING>xM     VLABEL            (vertex coordinate labels)
c           <STRING>xN     ALABEL            (attribute labels)
c           IF (VDS.eq.DFAULT)
c             IF (M.GT.0)
c               <FARRAY>       VERT(1:M,1:NV)  (mD vertex list)
c             ENDIF
c           ELSE IF (VDS.eq.1)
c             LOOP i=1,M
c               <FARRAY>       VERT(i,1:NV)    (i comp. of vertex list)
c             ENDLOOP
c           ENDIF
c           LOOP i=1,N
c             <FARRAY>       Ai(1:NV)        (ith attribute list)
c           ENDLOOP
c
c
c ----------------------------------------------------------------------
c
c     Input:
c       adim    -  if mode=0, maximum # of vertices dimensioned in 2D
c                             attribute array  (max. value of "nv")
c                  if mode=1, dimensioned length of 1D packed attribute
c                             array  (max. value of "n"*"nv")
c       fid     -  File ID of PFF file
c       keep    -  logical flag indicating whether or not to keep a
c                  non-zero value in the case of underflow
c       mdim    -  dimensioned vertex dimensionality (max. value of "m")
c       mode    -  subroutine mode flag --
c                   = 0,        return attributes into 2D array
c                   = 1,        return attributes into packed 1D array
c                   otherwise,  return pointers "locv" and "loca(1:N)"
c       ndim    -  dimensioned # of attributes (max. value of "n")
c       vdim    -  dimensioned vertex list length (max. value of
c                  "m"*"nv")
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       alabel  -  (*) attribute labels
c       aoff10  -  (*) power-of-ten offset for each array of attribute
c                  data ("attr")
c       attr    -  (*) linear array containing attribute data  (see mode)
c       ispare  -  (*) spare words reserved for application
c       loca    -  (*) PFF file pointers to the attribute arrays ("attr")
c                  (mode 2 only)
c       locv    -  PFF file pointer to the vertex array ("attr")  (mode
c                  2 only)
c       m       -  vertex dimensionality
c       n       -  attribute dimensionality
c       nv      -  # of verticies in vertex list in dataset
c       nvr     -  # of verticies in vertex list actually returned
c       sc      -  character string to be loaded with dataset comment
c       st      -  character string to be loaded with dataset type label
c       tapp    -  # associated with application dataset type
c       vert    -  (*) mD vertex list array (packed) (not used for mode 2)
c       vlabel  -  (*) Vertex coordinate labels
c       voff10  -  power-of-ten offset for vertex data ("vert")
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
c       i       -  loop index over attributes
c       j       -  loop index over spatial dimension of vertex
c       k       -  loop index over vertices
c       iloc    -  pointer to FWA of each attribute array
c       lds     -  length of dataset read (in 16-bit words)
c       lstadr  -  file pointer on subroutine entry
c       nblen   -  non-blank character string length
c       nchrsc  -  non-blank length of header comment string
c       nchrst  -  non-blank length of header datatype label string
c       nrfu    -  # of non-default header reserved integers
c       slen    -  truncated character string length
c       tlen    -  length of floating point array read in
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
c     pfetyp  -  PFTVTX
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
      integer   adim, fid, ierr, m, mdim, mode
      integer   n, ndim, nv, nvr, tapp, vdim, voff10
      integer   ispare(1:5), aoff10(1:ndim)
      HU_FPOS_TYPE locv, loca(1:ndim)
      real      attr(*), vert(1:vdim)
      character st*(*), sc*(*)
      character vlabel(1:mdim)*(*), alabel(1:ndim)*(*)
c
c ... Parameters:
      character PFMODU*8
      parameter ( PFMODU = 'PFRVTX64' )
      integer   BUFLEN
      parameter ( BUFLEN = 8 )
c
c     Internal
      integer   i, j, k, iloc, lds, tlen
      integer   vds, traw, nrfu, vofftmp, voffmax
      integer   slen, nblen, nchrst, nchrsc, trunst, trunsc
      HU_FPOS_TYPE llocv, lstadr
      integer   buf(0:BUFLEN), idlist(3), jdlist(3), kdlist(3)
      real      xfact
c
c ----------------------------------------------------------------------
c
      data idlist, jdlist, kdlist / 9*1 /
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
c ... Read header
c
      call pfrhed ( fid, lstadr, traw, tapp, vds, lds, nrfu,
     &              st, nchrst, trunst, sc, nchrsc, trunsc,
     &              ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... return if this is not the right dataset type  (PFTVTX)
c
      if( traw.ne.PFTVTX ) then
        ierr = 1
        call pfserr ( PFMODU, ierr, fid,
     &                          'Incorrect dataset type' )
        goto 999
      endif
c
c ... Read in M, N, and NV
c
      call pfusio (fid, RE, 5, buf, ierr)
      if ( ierr.ne.0 ) goto 999
      m = buf(0)
      n = buf(1)
      call pfui2l ( buf(2), nv, ierr)
c
c ... return if any array dimension sizes are exceeded
c
      if( n.gt.ndim .OR. m.gt.mdim )then
        ierr = 2
        call pfserr ( PFMODU, ierr, fid,
     &                          'Insufficient array dimensions' )
        goto 999
      endif
c
c ... check to see if array limits will cause us to truncate the
c ... returned vertex list
c
      if (m.gt.0) then
        nvr = min( nv, vdim/m )
      else
        nvr = nv
      endif
c
      if (n.gt.0) then
        if (mode.eq.0 ) then
          nvr = min( nvr, adim )
        else
          nvr = min( nvr, adim/n )
        endif
      endif
c
c ... Read in spare words (5)
c
      call pfusio (fid, RE, 5, ispare, ierr)
c
c ... Read in vertex labels
c
      do 1 i=1,m
        call pfrchr (fid, vlabel(i), slen, nblen, ierr)
    1 continue
c
c ... Read in attribute labels
c
      do 2 i=1,n
        call pfrchr (fid, alabel(i), slen, nblen, ierr)
    2 continue
c
c ... read in vertex list (two versions supported)
c
      if ( vds.eq.DFAULT ) then
c
c ..... Original Dataset version (vds = DFAULT)
c
c ..... read in vertex list only if M is positive
c
        if (m.gt.0) then
c
          if ( mode.eq.0 .OR. mode.eq.1 ) then
c
c ......... actually read the data into the supplied arrays
c
            call pfrfa ( fid, vdim, keep, tlen,
     &                   vert, voff10, vert, ierr)
c
c ......... ignore truncation error -- we've already trapped it
c
            if ( ierr.eq.1 ) ierr = 0
c
          else
c
c ....... Skip over array, returning pointer to first word and length
c
          call pfuskiparr(fid, locv, tlen, ierr)
          if ( ierr.ne.0 ) go to 999
c
          endif
c
        endif
c
      else
c
c ..... Current Dataset version (vds = 1)
c
c ..... loop over vertex dimensions and read each separately
c
        voff10  = 0
        jdlist(2) = nv
        do j=1,m
c
          if ( mode.eq.0 .OR. mode.eq.1 ) then
c
c ......... actually read the data into the supplied arrays
c
            call pfrfa3 ( fid, keep, 1, m, nv, 1, idlist, jdlist,
     &                    kdlist, vert(j), vofftmp, ierr)
c
c ......... ignore truncation error -- we've already trapped it
c
            if ( ierr.eq.1 ) ierr = 0
c
            voffmax = max(voff10,vofftmp)
            if (vofftmp.gt.voff10) then
              xfact = 10.0**(voff10-vofftmp)
              do k=1,(j-1)*nv
                vert(k) = vert(k)*xfact
              end do
            else if (vofftmp.lt.voff10) then
              xfact = 10.0**(vofftmp-voff10)
              do k=(j-1)*nv+1,j*nv
                vert(k) = vert(k)*xfact
              end do
            endif
            voff10 = voffmax
c
          else
c
c ......... Skip over array, returning pointer to first word and length
c
            call pfuskiparr(fid, llocv, tlen, ierr)
            if ( ierr.ne.0 ) go to 999
            if (j .EQ. 1) locv = llocv
c
          endif
c
        end do
c
      endif
c
c ... now loop over attribute arrays (N)
c
      iloc   = 1
c
      do 13 i=1,n
c
        if ( mode.eq.0 .OR. mode.eq.1 ) then
c
c ....... actually read the data into the supplied arrays
c
c
          call pfrfa ( fid, nvr, keep, tlen,
     &                 attr(iloc), aoff10(i), attr(iloc), ierr)
c
c ....... ignore truncation error -- we've already trapped it
c
          if ( ierr.eq.1 ) ierr = 0
c
c ....... advance FWA for next array of attributes, either 2D or packed
c
          if (mode.eq.0) then
            iloc = iloc + adim
          else
            iloc = iloc + nvr
          endif
c
        else
c
c ....... Skip over array, returning pointer to first word and length
c
          call pfuskiparr(fid, loca(i), tlen, ierr)
c
        endif
c
        if ( ierr.ne.0 ) goto 999
c
   13 continue
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
