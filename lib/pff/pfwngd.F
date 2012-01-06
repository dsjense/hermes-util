      subroutine pfwngd ( fid, tapp, st, sc, m, n, nx, mg,
     &                    nspare, ispare, alabel, vlabel, x, v, ierr )
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
c       - This routine is a WRITE routine that writes an NGD dataset
c         (n-dimensional vectors on a parallelpiped region of an
c         m-dimensional space, zoned with a single block using non-
c         uniform grids) to a PFF file.
c       - This operation is ONLY ALLOWED in WRITE mode !!!
c       - Dataset Format:
c           <HEADER>       PFTNGD
c           <INT>          M                 (Space dimensionality)
c           <INT>          N                 (Vector dimensionality)
c           <LONG>xM       NX                (# grid points for each
c                                             dimension of space --
c                                             DATASET VERSION 1 !!!)
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
c       alabel  -  (*) axis labels for each axis of m-D space
c       ispare  -  (*) spare words reserved for application
c       fid     -  File ID of PFF file
c       m       -  space dimensionality
c       n       -  vector dimensionality
c       mg      -  Dimension for # points on which vectors are defined
c       nspare  -  # spare words
c       nx      -  (*) # grid points along each axis of space
c       sc      -  character string to be loaded with dataset comment
c       st      -  character string to be loaded with dataset type label
c       tapp    -  # associated with application dataset type
c       x       -  (*,*) Grid array for each dimension
c       v       -  (*,*) Vector data
c       vlabel  -  (*) labels for each component of vector data
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                      0,          No error
c                      1,          Vector array dimension too small
c                                  mg < nx(1) * ... * nx(m)
c                      otherwise,  Error in called PFF routine
c
c ----------------------------------------------------------------------
c
c     Internals:
c       buf     -  buffer space for encoding of <LONG>'s
c       i       -  loop index
c       lds     -  length of dataset written (in 16-bit words)
c       lstadr  -  file position pointer for beginning of dataset
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfetyp  -  PFTNGD
c     pfstfg  -  WR, DFAULT
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
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ierr, m, mg, n, nspare, tapp
      integer   ispare(1:*), nx(1:m)
      real      x(1:*)
      real      v(1:mg,1:n)
      character st*(*), sc*(*)
      character alabel(*)*(*), vlabel(*)*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFWNGD' )
c
c     Internal
      integer   i, j, lds, lstadr, ng
      integer   buf(3)
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
c ... Get total # grid points for vector data
c
      ng = 1
      do 100 i=1,m
        ng = ng * nx(i)
  100 continue
c
      if (ng .GT. mg) then
        ierr = 1
        call pfserr (PFMODU, ierr, fid, 'Vector dimension too small')
        go to 999
      endif
c
c ----------------------------------------------------------------------
c
c ... Write header  (dataset version (vds) = 1)
c
      call pfwhed ( fid, PFTNGD, tapp, 1, DFAULT, DFAULT, st, sc,
     &              lstadr, ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... Write out M and N
c
      call pfusio (fid, WR, 1, m, ierr)
      call pfusio (fid, WR, 1, n, ierr)
c
c ... Write out dimensions for each axis
c
ccc START ORIGINAL VERSION 
ccc
cc      call pfusio (fid, WR, m, nx, ierr)
c
cc START Version 1 (current version)
cc   Use <LONG>s instead of <INT>s to write axis dimensions
c
      do i=1,m
        call pful2i ( nx(i), buf, ierr )
        call pfusio (fid, WR, 3, buf, ierr)
      end do
c
c ... Write out spare word integer array
c
      call pfwia (fid, nspare, ispare, ierr)
c
c ... Write out axis labels labels
c
      do 1 i=1,m
        call pfwchr (fid, alabel(i), ierr)
    1 continue
c
c ... Write out vector component labels
c
      do 2 i=1,n
        call pfwchr (fid, vlabel(i), ierr)
    2 continue
c
c ... write out grid data
c
      j = 1
      do 200 i=1,m
        call pfwfa ( fid, nx(i), x(j), 0, ierr)
        if ( ierr.ne.0 ) goto 999
c
        j = j + nx(i)
  200 continue
c
c ... Now loop over vector components
c
      do 300 i=1,n
c
c ..... write out each component
c
        call pfwfa ( fid, ng, v(1,i), 0, ierr)
        if ( ierr.ne.0 ) goto 999
c
  300 continue
c
c ----------------------------------------------------------------------
c
c ... now call routine to write dataset length into the header
c
      call pfwlds ( fid, lstadr, lds, ierr )
c
c ... put directory info into memory-resident directory structure
c
      call pfdput ( fid, PFTNGD, tapp, lds, lstadr, st, sc, ierr )
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
