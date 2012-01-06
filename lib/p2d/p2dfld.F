        subroutine p2dfld
c
c ---------------------------- input -----------------------------------
c
     &             ( blk,cim,deli,delj,imax,jmax, nneu,neupdi
     &              ,phi,system,xi,xj
c
c ---------------------------- output ----------------------------------
c
     &              ,ei,ej,luout, ier               )
c
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
c       - This routine finds the field (gradient) associated with the
c         potential obtained by the Poisson solver.
c
c ----------------------------------------------------------------------
c
c     Input:
c       blk     -  Index of THIS block (use for Neumann BCs)
c       cim     -  cell information matrix (linear form)
c       deli    -  delta grid array in i-direction
c       delj    -  delta grid array in j-direction
c       imax    -  maximum "i" grid index in this block
c       jmax    -  maximum "j" grid index in this block
c       luout   -  logical unit # for printed output (error messages)
c       phi     -  potential
c       system  -  coordinate system flag:
c                  the problem is x3 independent
c                                     (x1,x2,x3)  (h1,h2,h3)
c                     1 - cartesian   (x ,y ,z )  (1 ,1 ,1 )
c                     2 - cylindrical (z ,r,phi)  (1 ,1 ,r )
c                     3 - cylindrical (r,phi,z)   (1 ,r ,1 )
c                         r=0 singularity not implemented!
c                     4 - spherical (theta,phi,r) (r,r*sin(theta),1)
c                         (solves r**2*LaPlacian; returns phi and r*field)
c                         theta=0,pi singularities not implemented!
c                     5 - spherical (r,theta,phi) (1,r,r*sin(theta))
c                         r=0 singularity not implemented!
c                         theta=0,pi singularities not implemented!
c                     6 - spherical (r,sin(theta) phi,theta) (1,r*sin(theta),r)
c                         r=0 singularity not implemented!
c       xi      -  grid array in i-direction
c       xj      -  grid array in j-direction
c       nneu    -  # Neumann boundary conditions
c       neupdi  -  plane definition indices for Neumann BCs
c
c ----------------------------------------------------------------------
c
c     Output:
c       ei      -  "i" component of field
c       ej      -  "j" component of field
c       ier     -  error parameter indicating completion status
c                     0 - Normal Successful completion
c                    -1 - Coordinate system not supported in present version
c                    -2 - Code does not handle cyl radius=0
c                    -3 - Code does not handle sph radius=0
c                    -4 - Code does not handle sph theta=0,pi
c
c ----------------------------------------------------------------------
c
c     Internals:
c       fact    -  metric weighting factor for gradient difference
c       i,j     -  loop indices
c       sxi     - sin( xi(i) )
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
      integer blk,imax,jmax,luout,system, ier, nneu
      integer neupdi(1:5,1)
      integer cim(0:imax,0:jmax)
      REAL_8 phi(0:imax,0:jmax)
      real xi(0:imax),xj(0:jmax),deli(0:imax),delj(0:jmax)
      real ei(0:imax,0:jmax),ej(0:imax,0:jmax)
c
c ... internal variables:
c
c ... loop indices:
      integer i,j,neu
c ... scalars:
      real fact,sxi
c
      ier = 0
c
c ... Ex component
c
      do 2 j=1,jmax
c
c       Tentatively set buffer fields to 0.
        ei(0,j)=0.
        ei(imax,j)=0.
c
        do 1 i=1,imax-1
          ei(i,j) = - (phi(i+1,j) - phi(i,j))/deli(i)
    1   continue
    2 continue
c
c ... Ey component
c
      do 4 i=1,imax
c
c       Tentatively set buffer fields to 0.
        ej(i,0)=0.
        ej(i,jmax)=0.
c
c ..... correct gradient for coordinate system metric
c
        if (system .eq. 1 .or. system .eq. 2) then
          fact = 1.
        else if(system .eq. 3 .or. system .eq. 5
     &          .or. system .eq.6 )then
                  if(xi(i).eq.0)then
                    if (luout.ge.0) then
                      write(luout,*)'p2dfld: Warning -- r = 0 ',
     &                              'singularity is not implemented'
                      write(luout,*)'        system = ',system
                    endif
                    if ( system .eq. 3 )then
                      ier = -2
                    else
                      ier = -3
                    endif
                    return
                  else
                    fact = 1./xi(i)
                  endif
        else if(system .eq. 4)then
                  sxi=sin(xi(i))
                  if(abs(sxi).le.1.e-25)then
                    if (luout.ge.0) then
                      write(luout,*)'p2dfld: Warning -- sin(theta) = 0',
     &                              ' singularity is not implemented'
                      write(luout,*)'        system = ',system
                    endif
                    ier = -4
                    return
                  else
                   fact = 1./sxi
                  endif
        else
              if (luout.ge.0) then
                write(luout,*)'p2dfld: Invalid coordinate system: ',
     &                         system
              endif
              ier = -1
              return
         endif
c
        do 3 j=1,jmax-1
          ej(i,j) = - fact*(phi(i,j+1) - phi(i,j))/delj(j)
    3   continue
    4 continue
c
c ... Set buffer field values for Neumann boundary conditions
c
      do neu=1,nneu
        if (neupdi(5,neu) .EQ. blk) then
          if (neupdi(4,neu) .EQ. 1) then
            do j=neupdi(1,neu),neupdi(2,neu)
              ei(0,j) = -ei(1,j)
            enddo
          elseif (neupdi(4,neu) .EQ. -1) then
            do j=neupdi(1,neu),neupdi(2,neu)
              ei(imax,j) = -ei(imax-1,j)
            enddo
          elseif (neupdi(4,neu) .EQ. 2) then
            do i=neupdi(1,neu),neupdi(2,neu)
              ej(i,0) = -ej(i,1)
            enddo
          else
            do i=neupdi(1,neu),neupdi(2,neu)
              ej(i,jmax) = -ej(i,jmax-1)
            enddo
          endif
        endif
      enddo
c
      return
      end
