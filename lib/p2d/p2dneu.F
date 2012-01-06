      subroutine p2dneu
     &      ( cim, deli, delj, imax, jmax, luout, system
     &      ,maxsys,kb,ke,kn,normal
     &      ,  xi,   xj,   a1,   a2,    b1,  b2,  c1,   c2 , ier)
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
c       - This routine modifies the difference coefficients in
c         the 2-D Poisson solve to account for the Neuman Boundary
c         condition denoted neu.
c         If both Neuman and Dirichlet boundary conditions are specified
c         the Dirichlet boundary conditions take precident.
c ----------------------------------------------------------------------
c
c     Input:
c       a1,a2   -  "A" differencing coefficient arrays in i- and j-
c                  directions
c       b1,b2   -  "B" differencing coefficient arrays in i- and j-
c                  directions
c       c1,c2   -  "C" differencing coefficient arrays in i- and j-
c                  directions
c       cim     -  cell information matrix (single block form)
c       deli    -  delta grid array in i-direction (single block form)
c       delj    -  delta grid array in j-direction (single block form)
c       imax    -  maximum grid index in i-direction
c       jmax    -  maximum grid index in j-direction
c       kb,ke   -  beginning and ending points for periodic and Neumann
c                  surfaces
c       kn      -  index of normal plane along Neumann and periodic
c                  surfaces
c       luout   -  logical unit # for printed output (error messages)
c       maxsys  -  # of valid coordinate systems
c       normal  -  normal direction indicator for Neumann and periodic
c                  surfaces
c       system  -  coordinate system flag:
c                  the problem is x3 independent
c                                     (x1,x2,x3)  (h1,h2,h3)
c                     1 - cartesian   (x ,y ,z )  (1 ,1 ,1 )
c                     2 - cylindrical (z ,r,phi)  (1 ,1 ,r )
c                     3 - cylindrical (r,phi,z)   (1 ,r ,1 )
c                         r=0 singularity not implemented!
c                     4 - spherical (theta,phi,r) (r,r*sin(theta),1)
c                         (solves r**2*LaPlacian; returns phi and r*field)
c                         theta=0 singularity not implemented!
c                     5 - spherical (r,theta,phi) (1,r,r*sin(theta))
c                         r=0 singularity not implemented!
c                     6 - spherical (r,sin(theta) phi,theta) (1,r*sin(theta),r)
c                         r=0 singularity not implemented!
c       xi      -  grid array in i-direction (single block form)
c       xj      -  grid array in j-direction (single block form)
c
c ----------------------------------------------------------------------
c
c     Output:
c       a1,a2   -  "A" differencing coefficient arrays in i- and j-
c                  directions
c       b1,b2   -  "B" differencing coefficient arrays in i- and j-
c                  directions
c       c1,c2   -  "C" differencing coefficient arrays in i- and j-
c                  directions
c       ier     -  error parameter indicating completion status
c                     0 - Normal Successful completion
c                   -10 - Neumann BC not implemented for this system
c
c ----------------------------------------------------------------------
c
c     Internals:
c       cxj     -  cos( xj(kn) )
c       cxjp    -  cos( xj(kn)+.5*delj(kn) )
c       cxjm    -  cos( xj(kn)-.5*delj(kn-1) )
c       xjps    -  (xj(kn)+.5*delj(kn))**2
c       xjms    -  (xj(kn)-.5*delj(kn-1))**2
c       xjs     -  xj(kn)**2
c       k       -  loop index
c       xxi     -  xi(kn)
c       xip     -  xi(kn)+.5*deli(kn)
c       xim     -  xi(kn)-.5*deli(kn-1)
c       xis     -  xxi**2
c       xips    -  xip**2
c       xims    -  xim**2
c       xic     -  xxi**3
c       xipc    -  xip**3
c       ximc    -  xim**3
c       cxi     -  cos( xi(kn) )
c       cxip    -  cos( xip )
c       cxim    -  cos( xim )
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
      integer imax,jmax,kb,ke,kn,luout,maxsys,normal,system, ier
      integer cim(0:imax,0:jmax)
      REAL_8 a1(0:imax,0:jmax),a2(0:imax,0:jmax),b1(0:imax,0:jmax)
      REAL_8 b2(0:imax,0:jmax),c1(0:imax,0:jmax),c2(0:imax,0:jmax)
      real deli(0:imax),xi(0:imax),delj(0:jmax),xj(0:jmax)
c
c ... internal variables:
c
c ... loop indices:
      integer k
c ... scalars:
      REAL_8 xxi,xip,xim,xis,xips,xims,xic,xipc,ximc,cxi,cxip,cxim
      REAL_8  t,u,v
      REAL_8  cxjm,cxjp,cxj,xjs,xjms,xjps
c
c ... Statement functions
c
       ier = 0
c
c
c ..... loop over surface
c
        do 51 k=kb,ke
c
c ....... normal in "i" direction
c
          if(abs(normal).eq.1)then
c
c ......... compute linear index
c
            if(cim(kn,k).eq.0)then
c.............no Dirichlet boundary condition
c
c
              if(system.eq.1 .or. system.eq.2)then
                if(normal.gt.0)then
c.. ............. BC at lower end of grid
                  t=(1.+(deli(kn-1)/deli(kn)) )
                  a1(kn,k)=a1(kn,k)*t
                  b1(kn,k)=-a1(kn,k)
                  c1(kn,k)=0.
                else
c ............... BC at upper end of grid
                  t=(1.+(deli(kn)/deli(kn-1)) )
                  c1(kn,k)=c1(kn,k)*t
                  b1(kn,k)=-c1(kn,k)
                  a1(kn,k)=0.
                endif
c
              elseif(system.eq.3)then
                  xip=(xi(kn)+.5*deli(kn))
                  xim=(xi(kn)-.5*deli(kn-1))
                  xxi=xi(kn)
                  xips=xip*xip
                  xims=xim*xim
                  xis=xxi*xxi
                if(normal.gt.0)then
c................ BC at lower end of grid
                  t=(xips-xims)/(xips-xis)
                  u=(xip+xim)/(xip+xxi)
                  v=u
                  a1(kn,k)=a1(kn,k)*t
                  b1(kn,k)=-a1(kn,k)
                  c1(kn,k)=0.
                  a2(kn,k)=a2(kn,k)*u
                  c2(kn,k)=c2(kn,k)*v
                  b2(kn,k)=-(a2(kn,k)+c2(kn,k))
                else
c ............... BC at upper end of grid
                  t=(xips-xims)/(xis-xims)
                  u=(xip+xim)/(xim+xxi)
                  v=u
                  c1(kn,k)=c1(kn,k)*t
                  b1(kn,k)=-c1(kn,k)
                  a1(kn,k)=0.
                  a2(kn,k)=a2(kn,k)*u
                  c2(kn,k)=c2(kn,k)*v
                  b2(kn,k)=-(a2(kn,k)+c2(kn,k))
                endif
c
              elseif(system.eq.4)then
                cxip=cos( xi(kn)+.5*deli(kn) )
                cxim=cos( xi(kn)-.5*deli(kn-1) )
                cxi=cos( xi(kn) )
                if(normal.gt.0)then
c.. ............. BC at lower end of grid
                  t=(cxim-cxip)/(cxi-cxip)
                  u=t*deli(kn)/( deli(kn)+deli(kn-1) )
                  v=u
                  a1(kn,k)=a1(kn,k)*t
                  b1(kn,k)=-a1(kn,k)
                  c1(kn,k)=0.
                  a2(kn,k)=a2(kn,k)*u
                  c2(kn,k)=c2(kn,k)*v
                  b2(kn,k)=-(a2(kn,k)+c2(kn,k))
                else
c ............... BC at upper end of grid
                  t=(cxim-cxip)/(cxim-cxi)
                  u=t*deli(kn-1)/( deli(kn)+deli(kn-1) )
                  v=u
                  c1(kn,k)=c1(kn,k)*t
                  b1(kn,k)=-c1(kn,k)
                  a1(kn,k)=0.
                  a2(kn,k)=a2(kn,k)*u
                  c2(kn,k)=c2(kn,k)*v
                  b2(kn,k)=-(a2(kn,k)+c2(kn,k))
                endif
              elseif(system.eq.5 .or. system.eq.6)then
                  xip=(xi(kn)+.5*deli(kn))
                  xim=(xi(kn)-.5*deli(kn-1))
                  xxi=xi(kn)
                  xips=xip*xip
                  xims=xim*xim
                  xis=xxi*xxi
                  xipc=xips*xip
                  ximc=xims*xim
                  xic=xis*xxi
                if(normal.gt.0)then
c................ BC at lower end of grid
                  t=(xipc-ximc)/(xipc-xic)
                  u=t*(xips-xis)/(xips-xims)
                  v=u
                  a1(kn,k)=a1(kn,k)*t
                  b1(kn,k)=-a1(kn,k)
                  c1(kn,k)=0.
                  a2(kn,k)=a2(kn,k)*u
                  c2(kn,k)=c2(kn,k)*v
                  b2(kn,k)=-(a2(kn,k)+c2(kn,k))
                else
c ............... BC at upper end of grid
                  t=(xipc-ximc)/(xic-ximc)
                  u=t*(xis-xims)/(xips-xims)
                  v=u
                  c1(kn,k)=c1(kn,k)*t
                  b1(kn,k)=-c1(kn,k)
                  a1(kn,k)=0.
                  a2(kn,k)=a2(kn,k)*u
                  c2(kn,k)=c2(kn,k)*v
                  b2(kn,k)=-(a2(kn,k)+c2(kn,k))
                endif
c
              else
                if (luout.ge.0) then
                  write(luout,*)'p2dneu: Neuman boundary condition ',
     &                          'not implemented for this ',
     &                          'coordinate system'
                  write(luout,*)'        system = ',system
                endif
                ier = -10
                return
c
              endif
            endif
          else
c
c ....... normal in "j" direction
c
            if(cim(k,kn).eq.0)then
c.............no Dirichlet boundary condition
c
              if(system.eq.1 .or. system.eq.3 .or. system.eq.4
     &           .or. system.eq.6)then
                if(normal.gt.0)then
c................ BC at lower end of grid
                  v=1.+delj(kn-1)/delj(kn)
                  a2(k,kn)=a2(k,kn)*v
                  c2(k,kn)=0.
                  b2(k,kn)=-a2(k,kn)
                else
c................ BC at upper end of grid
                  v=1.+delj(kn)/delj(kn-1)
                  a2(k,kn)=0.
                  c2(k,kn)=c2(k,kn)*v
                  b2(k,kn)=-c2(k,kn)
                endif
c
              elseif(system.eq.2)then
                xjps=(xj(kn)+.5*delj(kn))**2
                xjms=(xj(kn)-.5*delj(kn-1))**2
                xjs=xj(kn)**2
                if(normal.gt.0)then
c................ BC at lower end of grid
                  v=(xjps-xjms)/(xjps-xjs)
                  a2(k,kn)=a2(k,kn)*v
                  c2(k,kn)=0.
                  b2(k,kn)=-a2(k,kn)
                else
c................ BC at upper end of grid
                  v=(xjps-xjms)/(xjs-xjms)
                  a2(k,kn)=0.
                  c2(k,kn)=c2(k,kn)*v
                  b2(k,kn)=-c2(k,kn)
                endif
c
              elseif(system.eq.5)then
                cxj=cos( xj(kn) )
                cxjp=cos( xj(kn)+.5*delj(kn) )
                cxjm=cos( xj(kn)-.5*delj(kn-1) )
                if(normal.gt.0)then
c................ BC at lower end of grid
                  v=(cxjm-cxjp)/(cxj-cxjp)
                  a2(k,kn)=a2(k,kn)*v
                  c2(k,kn)=0.
                  b2(k,kn)=-a2(k,kn)
                else
c................ BC at upper end of grid
                  v=(cxjm-cxjp)/(cxjm-cxj)
                  a2(k,kn)=0.
                  c2(k,kn)=c2(k,kn)*v
                  b2(k,kn)=-c2(k,kn)
                endif
c
              else
                if (luout.ge.0) then
                  write(luout,*)'p2dneu: Neuman boundary condition ',
     &                          'not implemented for this ',
     &                          'coordinate system'
                  write(luout,*)'        system = ',system
                endif
                ier = -10
                return
              endif
c
            endif
          endif
c
 51     continue
 52   continue
c
      return
      end
