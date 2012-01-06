      subroutine p2dbh1(a,b1off,b2off,c,cim,fw,g
     &               ,imax,jmax,pass,phi,b1,b2,h)
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
      implicit none
c
c ----------------------------------------------------------------------
c
c     Summary:
c
c       - This routine initializes b1, b2, and h for each ADI pass
c       - It should be called at the beginning of each pass by P2DSLV
c       - This routine performs the following specific functions:
c          1)  Redefines non-conductor b1 and b2 to account for
c              Wachspress iteration parameter
c          2)  Computes the h appropriate to this pass
c
c ----------------------------------------------------------------------
c
c     Input:
c       a       -  "A" differencing coefficient array in the opposite
c                  direction of pass  (block form)
c       b1off   -  offset to be added to b1 array to properly include
c                  Wachspress iteration parameter for this pass
c       b2off   -  offset to be added to b2 array to properly include
c                  Wachspress iteration parameter for this pass
c       c       -  "C" differencing coefficient array in the opposite
c                  direction of pass  (block form)
c       cim     -  cell information matrix (block form)
c       fw      -  scale factor array for wachpress multiplier
c       g       -  source array (block form)
c       imax    -  maximum grid index for this block
c       jmax    -  maximum grid index for this block
c       pass    -  1 or 2, denoting direct solve in "1" or "2" direction
c       phi     -  solution potential from last pass  (block form)
c
c ----------------------------------------------------------------------
c
c     Output:
c       b1,b2   -  "B" differencing coefficient arrays in i- and j-
c                  directions modified for iteration parameters  (block
c                  form)
c       h       -  array containing source and alternate direction
c                  differences  (block form)
c
c ----------------------------------------------------------------------
c
c     Internals:
c       i,j     -  loop indices
c       e0inv   -  -1./(permittivity of free space)
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c ... include MD parameters
c
#include "mdr8.h"
c
c ... Passed variables:
c
      integer imax,jmax,pass
      REAL_8 b1off,b2off
      integer cim(0:imax,0:jmax)
      REAL_8 a(0:imax,0:jmax),c(0:imax,0:jmax),fw(0:imax,0:jmax)
      real g(0:imax,0:jmax)
      REAL_8 phi(0:imax,0:jmax)
      REAL_8 b1(0:imax,0:jmax),b2(0:imax,0:jmax),h(0:imax,0:jmax)
c
c ... internal variables:
c
c ... loop indices:
      integer i,j
c ... scalars:
      REAL_8 e0inv
c
c ... Parameters:
c
#ifdef USE_DP
      parameter (e0inv= 1.129409066758147d+11)
#else
      parameter (e0inv= 1.129409066758147e+11)
#endif
c
c ----------------------------------------------------------------------
c     Loop over all cells in block (not including lower buffer cell)
c      to recompute b1 and b2
c ----------------------------------------------------------------------
c
      do 2 j=1,jmax
        do 1 i=1,imax
c
c ....... ANSI-standard FORTRAN-77 version
c
          if(cim(i,j).eq.0)then
            b1(i,j)=b1(i,j)+b1off*fw(i,j)
            b2(i,j)=b2(i,j)+b2off*fw(i,j)
          else
            b1(i,j)=b1(i,j)
            b2(i,j)=b2(i,j)
          endif
c
c ....... non-ANSI-standard Cray "vector merge" version
c
c         b1(i,j) = cvmgz( b1(i,j)+b1off*fw(i,j), b1(i,j), cim(i,j) )
c         b2(i,j) = cvmgz( b2(i,j)+b2off*fw(i,j), b2(i,j), cim(i,j) )
c
    1   continue
    2 continue
c
c ----------------------------------------------------------------------
c     Compute h as appropriate for pass
c ----------------------------------------------------------------------
c ... Note that row 1, rather than row MAX, is the primary row for block
c ...  and periodic boundaries.  In those cases, C1 is not zero, and
c ...  proper values of PHI must already be loaded into buffer row zero.
c ...  In all other cases, C1 should be zero.  Note that the coding
c ...  implicitly forces Amax = 0
c
      if(pass.eq.1)then
c
        do 4 j=1,jmax-1
          do 3 i=1,imax
            h(i,j) = e0inv*g(i,j) -  a(i,j)*phi(i,j+1)
     &                      - b2(i,j)*phi(i,j)
     &                      -  c(i,j)*phi(i,j-1)
    3     continue
    4   continue
c
        do 5 i=1,imax
          h(i,jmax) = e0inv*g(i,jmax) - b2(i,jmax)*phi(i,jmax)
     &                          -  c(i,jmax)*phi(i,jmax-1)
    5   continue
c
      else
c
        do 7 j=1,jmax
          do 6 i=1,imax-1
            h(i,j) = e0inv*g(i,j) -  a(i,j)*phi(i+1,j)
     &                      - b1(i,j)*phi(i  ,j)
     &                      -  c(i,j)*phi(i-1,j)
    6     continue
    7   continue
c
        do 8 j=1,jmax
            h(imax,j) = e0inv*g(imax,j) - b1(imax,j)*phi(imax  ,j)
     &                            -  c(imax,j)*phi(imax-1,j)
    8   continue
c
      endif
c
c ----------------------------------------------------------------------
c     Loop over all cells in block (not including lower buffer cell)
c      to handle conductor effects on h
c ----------------------------------------------------------------------
c
      do 10 j=1,jmax
        do 9 i=1,imax
c
c ....... ANSI-standard FORTRAN-77 version
c
          if(cim(i,j).ne.0)then
            h(i,j)=g(i,j)
          endif
c
c ....... non-ANSI-standard Cray "vector merge" version
c
c         h(i,j) = cvmgz( h(i,j), g(i,j), cim(i,j) )
c
    9   continue
   10 continue
c
      return
      end
