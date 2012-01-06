      subroutine p2d
c
c        ---------------------- input ----------------------------------
c
     &   ( bcm, bcmb, bcme, bgla, bgle, bgli, bglj, blkdim, cim, conv
     &   , deli, delj, er, g, imax, itrmax, jmax, locb
     &   , loci, locj, luout, lwork, mwachs, nblk, ncond, neupdi, nneu
     &   , nper, perpdi, pot, system, xi, xj
c
c        ---------------------- working --------------------------------
c
     &   ,work
c
c        ---------------------- output ---------------------------------
c
     &   , ei, ej, phi, niter, ier                             )
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
c       - This routine performs a block-grid 2-D Poisson solve using an
c         ADI solver with Wachspress convergence acceleration.
c       - Boundary conditions supported are:
c          1)  Direchlet,
c          2)  Neumann, and
c          3)  periodic.
c       - This routine performs the following specific functions:
c          1)  Allocates user-supplied working space
c          2)  calls P2DINI to initialize the algorithm
c          3)  calls P2DSLV to obtain the potential
c          4)  calls P2DFLD to obtain the fields associated with the
c              potential, i.e., field = - grad(potential).
c
c ----------------------------------------------------------------------
c
c     Input:
c       bcm     -  block connectivity matrix  (+/- 1 or 2)
c       bcmb    -  beginning indicies for block connectivity line
c       bcme    -  ending indicies for block connectivity line
c       bgla    -  dimension of linear potential and related arrays
c       bgle    -  dimension of relative dielectric array
c       bgli    -  dimension of linear grid arrays in i-direction
c       bglj    -  dimension of linear grid arrays in j-direction
c       blkdim  -  upper dimension of block arrays
c       cim     -  cell information matrix (linear form)
c       conv    -  convergence ratio for solution
c       deli    -  delta grid array in i-direction (linear form)
c       delj    -  delta grid array in j-direction (linear form)
c       er      -  relative dielectric array
c       g       -  charge density (coul / meter**2)
c       imax    -  maximum grid index in each block
c       itrmax  -  maximum # of iterations allowed
c       jmax    -  maximum grid index in each block
c       locb    -  pointer to FWA for each block of linear potential
c                  and related arrays
c       loci    -  pointer to FWA for each block of linear i-grid arrays
c       locj    -  pointer to FWA for each block of linear j-grid arrays
c       luout   -  logical unit # for printed output (error messages)
c                  used only if luout >= 0
c       lwork   -  length of working array
c       mwachs  -  dimensioned size of array for Wachspress iteration
c                  parameters
c       nblk    -  # of blocks in problem
c       ncond   -  # of conductors in problem
c       neupdi  -  plane definition indices for Neumann BCs
c       nneu    -  # of Neumann boundary conditions
c       nper    -  # of periodic BCs
c       perpdi  -  plane definition indices for periodic BCs (second
c                  index is 1 or 2 for lower and upper planes,
c                  respectively)
c       pot     -  potential values for each conductor
c       system  -  coordinate system flag:
c                     1 - cartesian   (  x  ,  y  )
c                     2 - cylindrical (  z  ,  r  )
c                     3 - cylindrical (  r  , phi )
c                     4 - spherical   (theta, phi )  times r**2
c                         (solves r**2*LaPlacian; returns phi and r*field)
c                     5 - spherical   (  r  ,theta)
c                     6 - spherical   (sin(theta) phi , r)
c       xi      -  grid array in i-direction (linear form)
c       xj      -  grid array in j-direction (linear form)
c
c ----------------------------------------------------------------------
c
c     Passed working array:
c       work    -  Working array
c
c ----------------------------------------------------------------------
c
c     Output:
c       ei      -  "i" component of field  (linear form)
c       ej      -  "j" component of field  (linear form)
c       phi     -  potential  (linear form)
c       niter   -  # of iterations required for solution
c       ier     -  error parameter indicating completion status
c                     0 - Normal Successful completion
c                    +1 - Failure to converge in specified # of iterations
c                         Fields will be calculated.
c                    -1 - Coordinate system is not supported in present version
c                    -2 - Code does not handle cylindrical radius = 0
c                    -3 - Code does not handle spherical radius = 0
c                    -4 - Code does not handle spherical theta = 0,pi
c                   -10 - Neumann BC not implemented with this system
c                   -98 - Dimensioning for Wachspress iteration parameters
c                         is insufficient
c                   -99 - Insufficient size for WORK array 
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c ... Passed variables:
c
      integer bgla,bgle,bgli,bglj,blkdim,itrmax,luout,lwork,mwachs,nblk
      integer ncond,nneu,nper,system,niter,ier, iertmp
      integer bcm (1:blkdim,1:blkdim),bcmb(1:2,1:blkdim,1:blkdim)
      integer bcme(1:2,1:blkdim,1:blkdim)
      integer cim(0:bgla),imax(1:blkdim), jmax(1:blkdim)
      integer locb(1:blkdim),loci(1:blkdim),locj(1:blkdim)
      integer neupdi(1:5,1),perpdi(1:5,1:2,1)
      real conv
      real g(0:bgla), er(0:bgle)
      real ei(0:bgla), ej(0:bgla), phi(0:bgla)
      real deli(0:bgli),delj(0:bglj)
      real xi(0:bgli),xj(0:bglj)
      real pot(1:ncond)
      real work(0:lwork)
c
c ... include MD parameters
c
#include "mdr8.h"
c
c ----------------------------------------------------------------------
c
c     Internals:
c       bglap   -  total storage required for linear potential arrays
c       count   -  counter for first free word in working array
c       fwachs  -  multiplier for Wachspress parameters for differencing
c                  scheme
c       i       -  loop index over work array
c       iertmp  -  temporary value of ier used to calculate fields only
c       nb      -  loop index over field blocks
c       nwachs  -  # of Wachspress iteration parameters needed for this
c                  problem
c       qa1     -  FWA of A1 differencing coefficient array
c       qa2     -  FWA of A2 differencing coefficient array
c       qb1     -  FWA of B1 differencing coefficient array
c       qb2     -  FWA of B2 differencing coefficient array
c       qfw     -  FWA of wachpress scale array 
c       qc1     -  FWA of C1 differencing coefficient array
c       qc2     -  FWA of C2 differencing coefficient array
c       qd      -  FWA of D working array
#ifdef USE_DP
c       qei     -  FWA of Ei working array
c       qej     -  FWA of Ej working array
c       qphi    -  FWA of PHI working array
#endif
c       qh      -  FWA of H working array
c       qiord   -  FWA of IORDER block ordering array
c       qjoff   -  FWA of JOFF index offset array
c       qjord   -  FWA of JORDER block ordering array
c       qperbl  -  FWA of PERBLK periodic BC working array
c       qphiold -  FWA of PHI old value for the error check
c       qwach   -  FWA of WACHS acceleration parameter array
c
c ----------------------------------------------------------------------
c
c ... internal parameters:
c
      integer hi,low
      parameter (low = 1, hi = 2)
c
c ... internal variables:
c
c ... loop indices:
      integer i, nb, nb2, per
c ... scalars:
      integer bglap,bglap2,count,dir,dir2,nwachs
      integer qa1,qa2,qb1,qb2,qc1,qc2,qfw,qh,qd
      integer qiord,qjoff,qjord,qperbl,qwachs, qphiold
#ifdef USE_DP
      integer qei, qej, qphi
#endif
      REAL_8 fwachs
c
c ... statement functions:
      integer p2dr8o
c
c ----------------------------------------------------------------------
c     allocate working space and check to see if it is long enough
c ----------------------------------------------------------------------
c
c ... initialize working space offset pointer -- set each working space
c ...  address pointer and then increment counter as required
c
      ier = 0
      count  = p2dr8o(work)
c
      bglap  = bgla + 1
      bglap2 = bglap *MDSCL
c
c ... FWA for array A1
      qa1    = count
      count  = count + bglap2
c
c ... FWA for array A2
      qa2    = count
      count  = count + bglap2
c
c ... FWA for array B1
      qb1    = count
      count  = count + bglap2
c
c ... FWA for array B2
      qb2    = count
      count  = count + bglap2
c
c ... FWA for array C1
      qc1    = count
      count  = count + bglap2
c
c ... FWA for array C2
      qc2    = count
      count  = count + bglap2
c
      if(bgle.eq.0)then
        qfw = p2dr8o(work)
      else
c
c ..... FWA for array fw
        qfw    = count
        count  = count + bglap2
      endif
c
c ... FWA for array H
      qh     = count
      count  = count + bglap2
c
c ... FWA for array D
      qd     = count
      count  = count + bglap2
c
#ifdef USE_DP
c ... FWA for array Ei
      qei    = count
      count  = count + bglap2
c
c ... FWA for array Ej
      qej    = count
      count  = count + bglap2
c
c ... FWA for array PHI
      qphi   = count
      count  = count + bglap2
c
#endif
c ... FWA for array PHIOLD
      qphiold= count
      count  = count + bglap2
c
c ... FWA for array WACHS
      qwachs = count
      count  = count + (mwachs+1)*MDSCL
c
c ... FWA for array IORDER
      qiord  = count
      count  = count + blkdim
c
c ... FWA for array JORDER
      qjord  = count
      count  = count + blkdim
c
c ... FWA for array JOFF
      qjoff  = count
      count  = count + blkdim
c
c ... FWA for array PERBLK
      qperbl = count
      count  = count + 2*blkdim
c
      if(count.gt.lwork)then
        if(luout.ge.0) write(luout,100)count,lwork
        ier = -99
        return
      endif
c
c ... initialize workspace to zero
c
      do 1 i=0,lwork
        work(i) = 0.0
    1 continue
c
c ... call Poisson algorithm's initializer routine
c
      call p2dini
     &     ( bcm,bcmb,bcme,bgla,bgle,bgli,bglj,blkdim,cim,deli,delj
     &      ,er,imax,jmax,work(qjoff),locb,loci,locj,luout,mwachs,nblk
     &      ,ncond,neupdi,nneu,nper,perpdi,pot,system,xi,xj
#ifdef USE_DP
     &      ,g,work(qd),work(qh),work(qei),work(qej),work(qphi)
#else
     &      ,g,work(qd),work(qh),ei,ej,phi
#endif
     &      ,work(qa1),work(qa2),work(qb1),work(qb2),work(qc1),work(qc2)
     &      ,work(qfw),fwachs
     &      ,work(qiord),work(qjord),nwachs,work(qperbl)
     &      ,work(qwachs), ier   )
c **********************************************************************
      if (ier .ne. 0) return
c
c ... call Poisson algorithm's solver routine
c
      if(bgle.eq.0)then
        call p2dslv
     &      (work(qa1),work(qa2),work(qb1),work(qb2),bcm,bcmb,bcme,bgla
     &      ,blkdim,work(qc1),work(qc2),cim,conv,fwachs,g,imax
     &      ,work(qiord),itrmax,jmax
     &      ,work(qjoff),work(qjord),locb,luout,nblk,nper,nwachs
     &      ,work(qperbl),perpdi,system,work(qwachs)
#ifdef USE_DP
     &      ,work(qd),work(qh),work(qej),work(qei)
     &      ,niter,work(qphi), work(qphiold), ier )
#else
     &      ,work(qd),work(qh),ej,ei ,niter,phi,work(qphiold), ier )
#endif
      else
        call p2dsl1
     &     ( work(qa1),work(qa2),work(qb1),work(qb2),bcm,bcmb,bcme,bgla
     &      ,blkdim,work(qc1),work(qc2),cim,conv,work(qfw)
     &      ,fwachs,g,imax,work(qiord),itrmax,jmax
     &      ,work(qjoff),work(qjord),locb,luout,nblk,nper,nwachs
     &      ,work(qperbl),perpdi,system,work(qwachs)
#ifdef USE_DP
     &      ,work(qd),work(qh),work(qej),work(qei)
     &      ,niter,work(qphi), work(qphiold), ier )
#else
     &      ,work(qd),work(qh),ej,ei ,niter,phi,work(qphiold), ier )
#endif
      endif
      if (ier .lt. 0) return
      iertmp = ier
c
c ... call Poisson algorithm's field (gradient) routine for each block
c
      do 2 nb=1,nblk
c
        call p2dfld
     &     ( nb, cim(locb(nb)),deli(loci(nb)),delj(locj(nb))
     &      ,imax(nb),jmax(nb), nneu,neupdi
#ifdef USE_DP
     &      ,work(qphi+MDSCL*locb(nb)),system,xi(loci(nb)),xj(locj(nb))
#else
     &      ,phi(locb(nb)),system,xi(loci(nb)),xj(locj(nb))
#endif
     &      ,ei(locb(nb)),ej(locb(nb)), luout, ier  )
c
        if (ier .ne. 0) return
    2 continue
c
c ... Set E-field in buffer cells for block and periodic connections
c
      do nb=1,nblk
        do nb2=nb+1,nblk
          if (bcm(nb,nb2) .NE. 0) then
            dir  = abs(bcm(nb,nb2))
            dir2 = 3 - dir
            call p2dseb(imax(nb),imax(nb2),jmax(nb),jmax(nb2)
     &                 ,bcmb(dir,nb,nb2),bcmb(dir,nb2,nb)
     &                 ,bcmb(dir2,nb,nb2),bcmb(dir2,nb2,nb)
     &                 ,bcme(dir2,nb,nb2),dir
     &                 ,ei(locb(nb)) ,ej(locb(nb))
     &                 ,ei(locb(nb2)),ej(locb(nb2)) )
          endif
        enddo
      enddo
c
      do per=1,nper
        nb  = perpdi(5,low,per)
        nb2 = perpdi(5,hi,per)
        dir = perpdi(4,low,per)
        call p2dseb(imax(nb),imax(nb2),jmax(nb),jmax(nb2)
     &    ,perpdi(3,low,per),perpdi(3,hi,per)
     &    ,perpdi(1,low,per),perpdi(1,hi,per)
     &    ,perpdi(2,low,per),dir
     &    ,ei(locb(nb)) ,ej(locb(nb))
     &    ,ei(locb(nb2)),ej(locb(nb2)) )
      enddo
c
#ifdef USE_DP
c ... Move the double precision phi to the output array
      call p2dphi(phi, work(qphi), bglap)
c
#endif
      ier = iertmp
c
      return
c
  100 format(1x,21('*'),' WARNING from subroutine P2D ',21('*'),//
     &      ,10x,'Insufficient working space provided by calling '
     &      ,'routine',/
     &      ,10x,'Needed:    ',i10,/
     &      ,10x,'Provided:  ',i10,/
     &      ,1x,71('*'),//)
c
      end
#ifdef USE_DP

      subroutine p2dphi (phi, phidouble, np)
c
c     This copies the double precision phi to the return value PHI.
c 
c     Passed
      integer np
      real phi(*)
      REAL_8 phidouble(*)
c
c     Internal
      integer i
      do i = 1, np
        phi(i) = phidouble(i)
      enddo
      return
      end
#endif
