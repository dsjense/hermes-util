      subroutine p2derr(blk,imax,jmax,phi,work,errmax,phimax,phimin)
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
c       - This routine checks for convergence of solution
c       - It should be called at the end of each iteration by P2DSLV
c       - This routine performs the following specific functions:  
c          1)  If BLK = 1, initialize error check parameters
c          2)  For all blocks, search mesh for new error check parameters
c
c ----------------------------------------------------------------------
c
c     Input:  
c       blk     -  block #
c       imax    -  maximum grid index for this block
c       jmax    -  maximum grid index for this block
c       phi     -  potential array
c       work    -  array in which old values of PHI are saved
c
c ----------------------------------------------------------------------
c
c     Output:  
c       errmax  -  variable for storing maximum local difference between 
c                  old and new PHI solutions
c       phimax  -  variable for storing maximum value of PHI for all 
c                  after this iteration
c       phimin  -  variable for storing minimum value of PHI for all 
c                  after this iteration
c
c ----------------------------------------------------------------------
c
c     Internals:  
c       i,j     -  do loop indices
c
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
      integer blk,imax,jmax
      REAL_8 errmax,phimax,phimin
      REAL_8 phi(0:imax,0:jmax),work(0:imax,0:jmax)
c
c ... internal variables:  
c
c ... loop indices:
      integer i,j
c
c
c
c ... update convergence variables
c
      if(blk.eq.1)then
        errmax=0.
        phimax=phi(1,1)
        phimin=phi(1,1)
      endif
c
      do 2 j=1,jmax
        do 1 i=1,imax
          errmax = max( errmax, abs(work(i,j)-phi(i,j)) )
          phimax = max( phimax, phi(i,j) )
          phimin = min( phimin, phi(i,j) )
    1   continue
    2 continue
c
      return
      end
