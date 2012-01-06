      subroutine p2ddif
     &      ( cim, deli, delj, er, imax, jmax, luout, ncond, pot
     &      , system, xi, xj, g, a1, a2, b1, b2,  c1, c2, maxsys, ier)
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
c       - This routine performs initialization for the 2-D Poisson solve
c         differencing coefficients for one block of problem
c       - This routine accounts for conductors
c       - It should be called before correcting for Neumann and periodic
c         boundary conditions
c         Differencing coefficients can be computed in one cartesian,
c         two cylindrical, or three spherical coordinate systems.
c       - This routine performs the following specific functions:
c          1)  Computes differencing coefficients A, B, and C for both
c              directions for one block of solution
c          2)  Modifies A, B, and C arrays and the source array (G) as
c              necessary for conductors (for that block)
c
c ----------------------------------------------------------------------
c
c     Input:
c       cim     -  cell information matrix (single block form)
c       deli    -  delta grid array in i-direction (single block form)
c       delj    -  delta grid array in j-direction (single block form)
c       er(0)   -  relative dielectric constant
c       imax    -  maximum grid index in i-direction
c       jmax    -  maximum grid index in j-direction
c       luout   -  logical unit # for printed output (error messages)
c       maxsys  -  # of valid coordinate systems
c       ncond   -  # of conductors in problem
c       pot     -  potential values for each conductor
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
c       xi      -  grid array in i-direction (single block form)
c       xj      -  grid array in j-direction (single block form)
c       g       -  source array (single block form)
c
c ----------------------------------------------------------------------
c
c     Output:
c       g       -  source array corrected for conductors (single block
c                  form)
c       a1,a2   -  "A" differencing coefficient arrays in i- and j-
c                  directions
c       b1,b2   -  "B" differencing coefficient arrays in i- and j-
c                  directions
c       c1,c2   -  "C" differencing coefficient arrays in i- and j-
c                  directions
c       ier     -  error parameter indicating completion status:
c                     0 - Normal Successful completion
c                    -1 - Coordinate system is not supported
c                    -2 - Code does not handle cyl radius=0
c                    -3 - Code does not handle sph radius=0
c                    -4 - Code does not handle sph theta=0,pi
c 
c
c ----------------------------------------------------------------------
c
c     Internals:
c       i,j     -  loop indices
c       cdif    -  cos(xi(i)-.5*deli(i-1))-cos(xi(i)+.5*deli(i))
c       cdif    -  cos(xj(j)-.5*delj(j-1))-cos(xj(j)+.5*delj(j))
c       dipdim1 -  deli(i)+deli(i-1)
c       djpdjm1 -  delj(j)+delj(j-1)
c       djmdjm1 -  (delj(j)-delj(j-1))/4.
c       dimdim1 -  (deli(i)-deli(i-1))/4.
c       es      -  er(0)
c       pi      -   4.*atan(1.)
c       si      -   sin(xi(i))
c       sip     -   sin(xi(i)+deli(i)*.5)
c       sim     -   sin(xi(i)-deli(i-1)*.5)
c       sjp     -   sin(xj(j)+delj(j)*.5)
c       sjm     -   sin(xj(j)-delj(j-1)*.5)
c       xip2    -   (xi(i)+deli(i)*.5)**2
c       xim2    -   (xi(i)-deli(i-1)*.5)**2
c       dx2     -    xip2-xim2
c       dx3     -   (xi(i)+deli(i)*.5)**3-(xi(i)-deli(i-1)*.5)**3
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
      integer imax,jmax,luout,maxsys,ncond,system, ier
      integer cim(0:imax,0:jmax)
      REAL_8 a1(0:imax,0:jmax),a2(0:imax,0:jmax),b1(0:imax,0:jmax)
      REAL_8 b2(0:imax,0:jmax),c1(0:imax,0:jmax),c2(0:imax,0:jmax)
      real g(0:imax,0:jmax),pot(1)
      real deli(0:imax),er(0:0),xi(0:imax),delj(0:jmax),xj(0:jmax)
c
c ... internal variables:
c
c ... loop indices:
      integer i,j
c ... scalars:
      REAL_8 cdif,dipdim1,djpdjm1,djmdjm1,dimdim1,es,pi
     &     ,si,sip,sim,sjp,sjm,xip2,xim2,dx2,dx3
c
#ifdef USE_DP
      parameter (pi = 3.14159265358979324d0)
#else
      parameter (pi = 3.14159265358979324)
#endif
      ier = 0
      es=er(0)
c
c
c ----------------------------------------------------------------------
c     Loop over all cells in block (not including lower buffer cell)
c ----------------------------------------------------------------------
c
      do 2 j=1,jmax
        do 1 i=1,imax
c
c ....... if not a conductor --
c
          if(cim(i,j).eq.0)then
c
            dipdim1=deli(i)+deli(i-1)
            djpdjm1=delj(j)+delj(j-1)
c
            if (system.eq.1) then
c
c ......... Cartesian  (i,j) = (x,y)
c
              a1(i,j)=es*2./(deli(i)*dipdim1)
              c1(i,j)=es*2./(deli(i-1)*dipdim1)
              a2(i,j)=es*2./(delj(j)*djpdjm1)
              c2(i,j)=es*2./(delj(j-1)*djpdjm1)
c
            else if (system.eq.2) then
c
c ......... Cylindrical (i,j) = (z,r)
c
              a1(i,j)=es*2./(deli(i)*dipdim1)
              c1(i,j)=es*2./(deli(i-1)*dipdim1)
c
c ........... check for coordinate system singularity in r
c
              if(xj(j).eq.0.)then
                a2(i,j)=es*4./delj(j)**2
                c2(i,j)=0.
              else
                djmdjm1=(delj(j)-delj(j-1))/4.
                a2(i,j)=es*(xj(j)+.5*delj(j))*
     &                  2./(delj(j)*djpdjm1*(xj(j)+djmdjm1))
                c2(i,j)=es*(xj(j)-.5*delj(j-1))*
     &                  2./(delj(j-1)*djpdjm1*(xj(j)+djmdjm1))
              endif
            else if (system.eq.3) then
c
c ......... Cylindrical (i,j) = (r,phi)
                  if(xi(i).eq.0)then
                    if (luout.ge.0) then
                      write(luout,*)'p2ddif: Warning -- r = 0 ',
     &                              'singularity is not implemented'
                      write(luout,*)'        system = ',system
                    endif
                    ier = -2
                    return
                  else
                    dimdim1=(deli(i)-deli(i-1))/4.
                    a1(i,j)=es*(2.*xi(i)+deli(i))
     &                      /(deli(i)*dipdim1*(xi(i)+dimdim1))
                    c1(i,j)=es*(2.*xi(i)-deli(i-1))
     &                      /(deli(i-1)*dipdim1*(xi(i)+dimdim1))
                    a2(i,j)=es*2.
     &                      /(xi(i)*delj(j)*djpdjm1*(xi(i)+dimdim1))
                    c2(i,j)=es*2.
     &                      /(xi(i)*delj(j-1)*djpdjm1*(xi(i)+dimdim1))
c
                  endif
            else if (system.eq.4) then
c
c ......... spherical (i,j) = (theta,phi)
                  if(xi(i).eq.0 .or .xi(i).eq.pi)then
                    if (luout.ge.0) then
                      write(luout,*)'p2ddif: Warning -- sin(theta) = 0',
     &                              ' singularity is not implemented'
                      write(luout,*)'        system = ',system
                    endif
                    ier = -4
                    return
                  else
                    si=sin(xi(i))
                    sip=sin(xi(i)+deli(i)*.5)
                    sim=sin(xi(i)-deli(i-1)*.5)
                    cdif=cos(xi(i)-.5*deli(i-1))-cos(xi(i)+.5*deli(i))
                    a1(i,j)=es*sip/(deli(i)*cdif)
                    c1(i,j)=es*sim/(deli(i-1)*cdif)
                    a2(i,j)=es*dipdim1/(delj(j)*si*djpdjm1*cdif)
                    c2(i,j)=es*dipdim1/(delj(j-1)*si*djpdjm1*cdif)
                  endif
            else if (system.eq.5) then
c ......... spherical (i,j) = (r,theta)
                  if(xi(i).eq.0.)then
                    if (luout.ge.0) then
                      write(luout,*)'p2ddif: Warning -- r = 0 ',
     &                              'singularity is not implemented'
                      write(luout,*)'        system = ',system
                    endif
                    ier = -3
                    return
                  elseif(xj(j).eq.0. .or .xj(j).eq.pi)then
                    if (luout.ge.0) then
                      write(luout,*)'p2ddif: Warning -- sin(theta) = 0',
     &                              ' singularity is not implemented'
                      write(luout,*)'        system = ',system
                    endif
                    ier = -4
                    return
                  else
                    sjp=sin(xj(j)+delj(j)*.5)
                    sjm=sin(xj(j)-delj(j-1)*.5)
                    cdif=cos(xj(j)-.5*delj(j-1))-cos(xj(j)+.5*delj(j))
                    xip2=(xi(i)+deli(i)*.5)**2
                    xim2=(xi(i)-deli(i-1)*.5)**2
                    dx2=xip2-xim2
                    dx3=(xi(i)+deli(i)*.5)**3-(xi(i)-deli(i-1)*.5)**3
                    a1(i,j)=es*3.*xip2/(deli(i)*dx3)
                    c1(i,j)=es*3.*xim2/(deli(i-1)*dx3)
                    a2(i,j)=es*1.5*sjp*dx2/(xi(i)*delj(j)*cdif*dx3)
                    c2(i,j)=es*1.5*sjm*dx2/(xi(i)*delj(j-1)*cdif*dx3)
                  endif
            else if (system.eq.6) then
c ......... spherical (i,j) = (r,sin(theta)*phi)
                  if(xi(i).eq.0)then
                    if (luout.ge.0) then
                      write(luout,*)'p2ddif: Warning -- r = 0 ',
     &                              'singularity is not implemented'
                      write(luout,*)'        system = ',system
                    endif
                    ier = -3
                    return
                  else
                    xip2=(xi(i)+deli(i)*.5)**2
                    xim2=(xi(i)-deli(i-1)*.5)**2
                    dx2=xip2-xim2
                    dx3=(xi(i)+deli(i)*.5)**3-(xi(i)-deli(i-1)*.5)**3
                    a1(i,j)=es*3.*xip2/(deli(i)*dx3)
                    c1(i,j)=es*3.*xim2/(deli(i-1)*dx3)
                    a2(i,j)=es*3.*dx2/(xi(i)*delj(j)*djpdjm1*dx3)
                    c2(i,j)=es*3.*dx2/(xi(i)*delj(j-1)*djpdjm1*dx3)
                  endif
            else
              if (luout.ge.0) then
                if(system.ge.1 .and. system.le.maxsys)then
                  write(luout,100) system,'1 and 2'
                else
                  write(luout,200) system,maxsys
                endif
              endif
              ier = -1
              return
            endif
c
c ......... set center-point diff. coeff. independently of system
c
            b1(i,j)= - ( a1(i,j) + c1(i,j) )
            b2(i,j)= - ( a2(i,j) + c2(i,j) )
c
c ....... if a conductor --
c
          else
c
            a1(i,j)=0.
            a2(i,j)=0.
            b1(i,j)=1.
            b2(i,j)=1.
            c1(i,j)=0.
            c2(i,j)=0.
            g(i,j)=pot(cim(i,j))
c
          endif
c
    1   continue
    2 continue
c
       return
c
  100 format(1x,27('*'),' p2ddif: WARNING ',27('*'),//
     &      ,10x,'Differencing coefficients for system ',i2
     &      ,' currently not available',/
     &      ,10x,'Systems ',a,' are currently available',//
     &      ,1x,71('*'),//)
c
  200 format(1x,27('*'),' p2ddif: WARNING ',27('*'),//
     &      ,10x,'Requested coordinate system # ',i2
     &      ,' is not valid',/
     &      ,10x,'Systems 1 through ',i2,' are valid ',//
     &      ,1x,71('*'),//)
c
      end
