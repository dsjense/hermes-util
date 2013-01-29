      subroutine pfrngd ( fid, keep, mdim, ndim, xdim, vdim, sprdim,
     &                    tapp, st, sc, m, n, nx, nvp, nspare, ispare,
     &                    alabel, vlabel, x, goff10, v, voff10,
     &                    ioff, ierr )
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
c       - This routine is a READ routine that reads an NGD dataset
c         (n-dimensional vectors on a parallelpiped region of an
c         m-dimensional space, zoned with a single block using non-
c         uniform grids) from a PFF file.
c       - This routine supports two dataset versions (vds=DFAULT & vds=1)
c       - This operation is ONLY ALLOWED in READ mode !!!
c       - Dataset Format:
c           <HEADER>       PFTNGD
c           <INT>          M                 (Space dimensionality)
c           <INT>          N                 (Vector dimensionality)
c           IF (VDS.eq.DFAULT)
c             <INT>xM        NX              (# grid points for each
c           ELSE IF (VDS.eq.1)
c             <LONG>xM        NX             (# grid points for each
c           ENDIF
c                                             dimension of space)
c           <IARRAY>       ISPARE(1:NSPARE)  (Reserved for application)
c           <STRING>xM     ALABEL            (coordinate labels for
c                                             each axis of m-D space)
c           <STRING>xN     VLABEL            (labels for each component
c                                             of vector data)
c           LOOP i=1,M
c             <FARRAY>     Xi(1:NXi)         (Grid points for Xi axis)
c           ENDLOOP
c           LOOP j=1,N
c             <FARRAY>     Vj(1:MG)          (j'th component of vector at
c           ENDLOOP                          each grid point)
c
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file
c       keep    -  logical flag indicating whether or not to keep a
c                  non-zero value in the case of underflow
c       mdim    -  dimensioned space dimensionality (max. value of "m")
c       ndim    -  dimensioned vector dimensionality (max. value of "n")
c       sprdim  -  dimensioned length of ISPARE array
c       vdim    -  1st dimension size of vector array  (max. value of
c                  "nx(1) * ... * nx(m)")
c       xdim    -  dimension size of x-grid array (max. value of
c                  "nx(1) + ... + nx(m)" )
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       alabel  -  (*) spatial axis labels
c       goff10  -  power-of-ten offset for grid data
c       ispare  -  (*) spare words reserved for application
c       m       -  space dimensionality
c       n       -  vector dimensionality
c       nspare  -  # spare words in dataset
c       nvp     -  # vector grid points (nx(1)*...nx(m))
c       nx      -  (*) # grid points along each axis
c       sc      -  character string to be loaded with dataset comment
c       st      -  character string to be loaded with dataset type label
c       tapp    -  # associated with application dataset type
c       v       -  (*,*) Vector data
c       vlabel  -  (*) Vector component labels
c       voff10  -  power-of-ten offset for vector data
c       x       -  (*) Grid array
c       ierr    -  error status flag;
c                     -1,   EOF marker encountered (Not really an error)
c                      0,   No error
c                      1,   Incorrect dataset type
c                      2,   Insufficient array dimensions
c                      3,   Error reading ISPARE array
c                      4,   # grid points in floating array not equal
c                           to number of points in "nx" array
c                      5,   # vector points in floating array not equal
c                           to "nvp"
c
c ----------------------------------------------------------------------
c
c     Work Space:
c       ioff  -  integer array used to compute power-of-ten offsets for
c                "x" and "v" arrays.  Must be dimensioned at least as
c                large as max(mdim,ndim)
c
c ----------------------------------------------------------------------
c
c     Internals:
c       buf     -  buffer space for encoding of <LONG>'s
c       lds     -  length of dataset read (in 16-bit words)
c       lstadr  -  file pointer on subroutine entry
c       nblen   -  non-blank character string length
c       nchrsc  -  non-blank length of header comment string
c       nchrst  -  non-blank length of header datatype label string
c       nrfu    -  # of non-default header reserved integers
c       nvp     -  # vector grid points ("nx(1) * ... * nx(m)")
c       nvpj    -  # vector points in floating array for j'th component
c       nxgi    -  # grid points in floating array for i'th axis
c       slen    -  truncated character string length
c       tnxg    -  Total # axis grid points ("nx(1) + ... + nx(m)")
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
c     pfetyp  -  PFTNGD
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
      integer   fid, goff10, ierr, m, mdim, n, ndim, nspare, nvp,
     1          sprdim, tapp, vdim, voff10, xdim
      integer   ioff(1:*), ispare(1:sprdim), nx(1:mdim)
      real      x(1:xdim)
      real      v(1:vdim,1:ndim)
      character st*(*), sc*(*)
      character alabel(1:mdim)*(*), vlabel(1:ndim)*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFRNGD' )
c
c     Internal
      character erstrg*64
      integer   i, j, j0, lds, nvpj, nxgi, tnxg
      integer   vds, traw, nrfu
      integer   slen, nblen, nchrst, nchrsc, trunst, trunsc
      HU_FPOS_TYPE lstadr
      integer   buf(3)
      real      off
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
c ... return if this is not the right dataset type  (PFTNGD)
c
      if( traw.ne.PFTNGD ) then
        ierr = 1
        call pfserr ( PFMODU, ierr, fid,
     &                          'Incorrect dataset type' )
        goto 999
      endif
c
c ... Read in M and N
c
      call pfusio (fid, RE, 1, m, ierr)
      call pfusio (fid, RE, 1, n, ierr)
c
c ... Read in dimensions for each spatial axis (two versions supported)
c
      if ( vds.eq.DFAULT ) then
c
c ..... Original Dataset version (vds = DFAULT)
c
        call pfusio (fid, RE, m, nx, ierr)
c
      else
c
c ..... Current Dataset version (vds = 1)
c       <LONG>s are used instead of <INT>s for axis dimensions
c
      do i=1,m
        call pfusio (fid, RE, 3, buf, ierr)
        call pfui2l ( buf, nx(i), ierr)
      end do
c
      endif
c
c ... Compute total # spatial grid points, and # of vector points
c
      tnxg = 0
      nvp  = 1
c
      do 100 i=1,m
        tnxg = tnxg + nx(i)
        nvp  = nvp * nx(i)
  100 continue
c
c ... return if any array dimension sizes are exceeded
c
      if( m.GT.mdim    .OR. n.GT.ndim    .OR.
     1    tnxg.GT.xdim .OR. nvp.GT.vdim) then
        ierr = 2
        call pfserr ( PFMODU, ierr, fid,
     &                          'Insufficient array dimensions' )
        goto 999
      endif
c
c ... Read spare word integer array
c
      call pfria ( fid, sprdim, nspare, ispare, ierr )
c
      if (ierr .NE. 0) then
        ierr = 3
        call pfserr ( PFMODU, ierr, fid,
     &                          'Error reading spare word array')
        goto 999
      endif
c
c ... Read in spatial axis labels
c
      do 1 i=1,m
        call pfrchr (fid, alabel(i), slen, nblen, ierr)
    1 continue
c
c ... Read in vector component labels
c
      do 2 i=1,n
        call pfrchr (fid, vlabel(i), slen, nblen, ierr)
    2 continue
c
c ... Read in grid data
c
      j      = 1
      goff10 = 0
c
      do 200 i=1,m
        call pfrfa ( fid, xdim-j+1, keep, nxgi,
     1               x(j), ioff(i), x(j), ierr)
        if ( ierr.ne.0 ) goto 999
c
        if (nxgi .NE. nx(i)) then
          ierr = 4
          write(erstrg,195) i
  195     format('# grid points in float array .NE. nx(i) for i=',i2)
          call pfserr ( PFMODU, ierr, fid, erstrg)
          goto 999
        endif
c
        j      = j + nx(i)
        goff10 = max (goff10, ioff(i))
  200 continue
c
c ... Now normalize grid data to maximum power-of-ten offset
c
      j0 = 0
c
      do 220 i=1,m
        if (ioff(i) .NE. goff10) then
          off = 10.0**(ioff(i) - goff10)
          do 210 j=j0+1,j0+nx(i)
            x(j) = x(j) * off
  210     continue
        endif
        j0 = j0 + nx(i)
  220 continue
c
c ... Loop over vector components
c
      voff10 = 0
c
      do 300 j=1,n
c
c ..... read each component
c
        call pfrfa ( fid, vdim, keep, nvpj,
     1               v(1,j), ioff(j), v(1,j), ierr)
        if ( ierr.ne.0 ) goto 999
c
        if (nvpj .NE. nvp) then
          ierr = 5
          write(erstrg,295) j
  295     format('# vector points in float array .NE. nvp for j=',i2)
          call pfserr ( PFMODU, ierr, fid, erstrg)
          goto 999
        endif
c
        voff10 = max (voff10, ioff(j))
  300 continue
c
c ... Now normalize vector data to maximum power-of-ten offset
c
      do 320 j=1,n
        if (ioff(j) .NE. voff10) then
          off = 10.0**(ioff(j) - voff10)
          do 310 i=1,nvp
            v(i,j) = v(i,j) * off
  310     continue
        endif
  320 continue
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
