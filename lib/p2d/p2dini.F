      subroutine p2dini
     &          ( bcm,bcmb,bcme,bgla,bgle,bgli,bglj,blkdim,cim,deli
     &           ,delj,er,imax,jmax,joff,locb,loci,locj,luout,mwachs
     &           ,nblk,ncond,neupdi,nneu,nper,perpdi,pot,system,xi,xj
     &           ,g,d,h,s,p,phi
     &           ,a1,a2,b1,b2,c1,c2,fw,fwachs,iorder,jorder
     &           ,nwachs, perblk,wachs, ier)
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
c       - It should be called before performing iteration over the
c         Wachpress iteration parameters
c       - This routine performs the following specific functions:
c          1)  Computes the Wachspress iteration parameters.
c          2)  Determines block ordering for two-pass direct solve
c          3)  Computes differencing coefficients A, B, and C for both
c              directions
c          4)  Modifies A, B, and C arrays and the source array (G) as
c              necessary for conductors
c          5)  Modifies A, B, and C arrays as necessary for Neumann BCs
c          6)  Modifies A, B, and C arrays and stores appropriate
c              pointers as necessary for periodic BCs
c
c ----------------------------------------------------------------------
c
c     Input:
c       bcm     -  block connectivity matrix  (+/- 1 or 2)
c       bcmb    -  beginning indicies for block connectivity line
c       bcme    -  ending indicies for block connectivity line
c       bgla    -  dimension of linear potential and related arrays
c       bgle    -  dimension of relative dielectric constant array
c       bgli    -  dimension of linear grid arrays in i-direction
c       bglj    -  dimension of linear grid arrays in j-direction
c       blkdim  -  upper dimension of block arrays
c       cim     -  cell information matrix (linear form)
c       deli    -  delta grid array in i-direction (linear form)
c       delj    -  delta grid array in j-direction (linear form)
c       er      -  relative dielectric constant array
c       imax    -  maximum grid index in each block
c       jmax    -  maximum grid index in each block
c       joff    -  "j" offset array used to compute addresses in linear
c                  arrays
c       locb    -  pointer to FWA for each block of linear potential
c                  and related arrays
c       loci    -  pointer to FWA for each block of linear i-grid arrays
c       locj    -  pointer to FWA for each block of linear j-grid arrays
c       luout   -  logical unit # for printed output (error messages)
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
c       xi      -  grid array in i-direction (linear form)
c       xj      -  grid array in j-direction (linear form)
c       g       -  source array (linear form)
c
c ----------------------------------------------------------------------
c
c     Passed working arrays:
c       d       \
c       h       -
c       p       -  Working arrays for solver to be initialized to zero
c       s       -
c       phi     /
c
c ----------------------------------------------------------------------
c
c     Output:
c       g       -  source array corrected for conductors (linear form)
c       a1,a2   -  "A" differencing coefficient arrays in i- and j-
c                  directions
c       b1,b2   -  "B" differencing coefficient arrays in i- and j-
c                  directions
c       c1,c2   -  "C" differencing coefficient arrays in i- and j-
c                  directions
c       fw      -  scale factor array for wachpress multiplier
c       fwachs  -  multiplier for Wachspress parameters for differencing
c                  scheme
c       iorder  -  block ordering array for i-component direct solve
c       jorder  -  block ordering array for j-component direct solve
c       nwachs  -  # of Wachspress iteration parameters needed for this
c                  problem
c       p       -  integer array equivalenced by this call to the "P"
c                  recursion array (linear form).  Periodic pointers are
c                  loaded into the zero-row slots as appropriate
c       perblk  -  flag indicating if current block contains a portion
c                  of a periodic string
c       wachs   -  Wachspress iteration parameters for this grid
c       ier     -  error parameter indicating completion status
c                     0 - Normal Successful completion
c                   -98 - Dimensioning for Wachspress iteration parameters
c                         is insufficient
c
c ----------------------------------------------------------------------
c
c     Internals:
c       blk     -  block #
c       blkl    -  block # for lower end of periodic BC
c       blkp    -  block # in primed coordinate
c       dxmin   -  minimum delta grid over entire mesh in i-direction
c       dymin   -  minimum delta grid over entire mesh in j-direction
c       meshmx  -  maximum average number of mesh nodes
c       i,j,k   -  loop indices
c       iblk    -  block # for blocks ordered for pass #1
c       icnt    -  counter used in ordering block arrays
c       ip      -  index for "i" direction in a connecting block
c       jblk    -  block # for blocks ordered for pass #2
c       jp      -  index for "j" direction in a connecting block
c       kb,ke   -  beginning and ending points for periodic and Neumann
c                  surfaces
c       kl      -  grid index along lower periodic boundary
c       kloff   -  offset between grid indices on upper and lower
c                  periodic surfaces
c       kn      -  index of normal plane along Neumann and periodic
c                  surfaces
c     	knl     -  index of normal plane along lower periodic surface
c       l       -  linear index for location in big arrays
c       ll,lu   -  linear indices for locations in big arrays in the
c                  lower and upper periodic blocks
c       ll0,lu0 -  linear indices for the zero-row locations in big
c                  arrays in the lower and upper periodic blocks
c       lnsqrt  -  ln(sqrt(2)-1)
c       m       -  dummy argument for block # in statement function
c       maxsys  -  # of valid coordinate systems
c       neu     -  loop index over Neumann boundary conditions
c       normal  -  normal direction indicator for Neumann and periodic
c                  surfaces
c       per     -  loop index over periodic boundaries
c       pmax    -  maximum value of pointer in a block-to-block pointer
c                  transfer
c       ti      -  temporary storage for Wachspress fraction exponent
c       tn      -  temporary storage for computing Wachspress fraction
c                  exponent
c       xmax    -  maximum value of grid in i-direction
c       xmin    -  minimum value of grid in i-direction
c       ymax    -  maximum value of grid in j-direction
c       ymin    -  minimum value of grid in j-direction
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c ... include MD parameters
c
#include "mdr8.h"
#ifdef USE_DP
# define  PROMOTE(A)    dble(A)
#else
# define  PROMOTE(A)        (A)
#endif
c
c ... Passed variables:
c
      integer bgla,bgle,bgli,bglj,blkdim,luout,mwachs,nblk,ncond,nneu
      integer nper,nwachs,system, ier
      integer bcm (1:blkdim,1:blkdim),bcmb(1:2,1:blkdim,1:blkdim)
      integer bcme(1:2,1:blkdim,1:blkdim)
      integer cim(0:bgla),imax(1:blkdim),iorder(1:blkdim)
      integer jmax(1:blkdim),joff(1:blkdim),jorder(1:blkdim)
      integer locb(1:blkdim),loci(1:blkdim),locj(1:blkdim)
      integer p(1:MDSCL,0:bgla)
      integer neupdi(1:5,1),perblk(1:2,1:blkdim),perpdi(1:5,1:2,1)
      REAL_8 fw(0:bgla), fwachs
      REAL_8 d(0:bgla), h(0:bgla), phi(0:bgla),s(0:bgla)
      real er(0:bgle), g(0:bgla), pot(1)
      REAL_8 a1(0:bgla),a2(0:bgla),b1(0:bgla),b2(0:bgla)
      REAL_8 c1(0:bgla),c2(0:bgla),wachs(0:mwachs)
      real deli(0:bgli),xi(0:bgli),delj(0:bglj),xj(0:bglj)
c
c ... internal variables:
c
c ... loop indices:
      integer blk,blkp,i,j,k,neu,per
c ... scalars:
      REAL_8 dxmin,dymin,meshmx,xmin,xmax,ymin,ymax,ti,tn
      integer blkl,iblk,icnt,ip,jblk,jp,kb,ke,kl,kloff,kn,knl
      integer l,ll,ll0,lu,lu0,normal,pmax
c
c ... Parameters:
      REAL_8 lnsqrt,pio2
      integer maxsys
      parameter (maxsys = 6)
#ifdef USE_DP
      parameter (lnsqrt = -0.881373587019543d0)
      parameter (pio2   =  1.570796326794897d0)
#else
      parameter (lnsqrt = -0.881373587019543)
      parameter (pio2   =  1.570796326794897)
#endif
c
c ... Statement functions
      integer locfun,m
c
c ----------------------------------------------------------------------
c     Statement function definitions
c ----------------------------------------------------------------------
c
      locfun(m,i,j) = locb(m) + i + joff(m)*j
c
c
c ----------------------------------------------------------------------
c     Initialize output and working arrays to zero
c ----------------------------------------------------------------------
c
c ... loop over linear array length
c
      ier = 0
      do 1 l=0,bgla
        d(l)   = 0.
        h(l)   = 0.
        do j=1,MDSCL
          p(j,l)   = 0
        end do
        phi(l) = 0.
        s(l)   = 0.
    1 continue
c
c ----------------------------------------------------------------------
c     Compute Wachspress iteration parameters
c ----------------------------------------------------------------------
c
c ... search entire grid for minimum delta and spatial grid limits
c
      do 4 blk=1,nblk
c
        if(blk.eq.1)then
          dxmin=deli(1)
          dymin=delj(1)
          xmin=xi(1)
          xmax=xi(imax(1))
          ymin=xj(1)
          ymax=xj(jmax(1))
        else
          xmin=min( xmin , PROMOTE(xi(loci(blk)+1))         )
          xmax=max( xmax , PROMOTE(xi(loci(blk)+imax(blk))) )
          ymin=min( ymin , PROMOTE(xj(locj(blk)+1))         )
          ymax=max( ymax , PROMOTE(xj(locj(blk)+jmax(blk))) )
        endif
c
        do 2 i=0,imax(blk)
          dxmin=min( dxmin , PROMOTE(deli(loci(blk)+i)) )
    2   continue
c
        do 3 j=0,jmax(blk)
          dymin=min( dymin , PROMOTE(delj(locj(blk)+j)) )
    3   continue
c
    4 continue
c
c ... find effective mesh length
c
      meshmx=max( (xmax-xmin)/dxmin , (ymax-ymin)/dymin )
c
c ... compute # of Wachspress parameters and check to see if
c ...  dimensioning is adequate
c
      nwachs=log(tan(pio2/meshmx))/lnsqrt  +  1
      if(nwachs.gt.mwachs)then
        if (luout.ge.0) write(luout,100) ' mwachs', nwachs, mwachs
        ier = -98
        return
      endif
c
c ... compute Wachspress iteration parameters
c
      tn=2./nwachs
      fwachs=( 2./min(dxmin,dymin) )**2
      wachs(0)     =cos(pio2/meshmx)**2
      wachs(nwachs)=sin(pio2/meshmx)**2
c     if homogeneous dielectric multiply fwachs by dielectric constant
      if(bgle.eq.0)fwachs=er(0)*fwachs
c
      do 11 i=1,nwachs-1
        ti=i*tn
        wachs(i)=wachs(0)*tan(pio2/meshmx)**(ti)
   11 continue
c
c ----------------------------------------------------------------------
c     Determine block ordering for 2-pass direct solve
c ----------------------------------------------------------------------
c
c ... i-direction
c
c
c ..... set ordered counter and temporary flags (1. = not ordered,
c .....                                               0. = ordered)
c
      icnt=0
      do 21 blk=1,nblk
        a1(blk)=1.
   21 continue
c
c ... if all blocks are ordered skip ahead to next task
c
c     do while (icnt.lt.nblk)
   22 if(icnt.ge.nblk)goto 29
c
c ... otherwise, keep looping over all the blocks that haven't been
c ...  ordered
c
      do 24 blk=1,nblk
        if(a1(blk).eq.1.)then
c
c ....... loop over rest of blocks, looking for a lower connection
c .......  to a block that has not yet been ordered
c
          do 23 blkp=1,nblk
            if(a1(blkp).eq.1. .and. bcm(blk,blkp).eq.-1)goto 24
   23     continue
c
c ..... didn't find connection, save this block on order stack and clear
c .....  temporary flag
c
          icnt=icnt+1
          iorder(icnt)=blk
          a1(blk)=0.
c
        endif
c
   24 continue
c
c ... go back and loop over the blocks again
c
c     end do
      goto 22
c
c ... all blocks ordered in i-direction
c
   29 continue
c
c
c ... j-direction
c
c ..... set ordered counter and temporary flags (1. = not ordered,
c .....                                               0. = ordered)
c
      icnt=0
      do 31 blk=1,nblk
        a1(blk)=1.
   31 continue
c
c ... if all blocks are ordered skip ahead to next task
c
c     do while (icnt.lt.nblk)
   32 if(icnt.ge.nblk)goto 39
c
c ... otherwise, keep looping over all the blocks that haven't been
c ...  ordered
c
      do 34 blk=1,nblk
        if(a1(blk).eq.1.)then
c
c ....... loop over rest of blocks, looking for a lower connection
c .......  to a block that has not yet been ordered
c
          do 33 blkp=1,nblk
            if(a1(blkp).eq.1. .and. bcm(blk,blkp).eq.-2)goto 34
   33     continue
c
c ..... didn't find connection, save this block on order stack and clear
c .....  temporary flag
c
          icnt=icnt+1
          jorder(icnt)=blk
          a1(blk)=0.
c
        endif
c
   34 continue
c
c ... go back and loop over the blocks again
c
c     end do
      goto 32
c
c ... all blocks ordered in j-direction
c
   39 continue
c
      if(bgle.gt.0)then
c-------------------------------------------------------------------------
c.......non-homogeneous dielectric constant
c.......preset relative dielectric constants in buffer cells
c.......for Neumann boundary conditions
c-------------------------------------------------------------------------
c ..... loop over Neumann BCs
c
        do 41 neu=1,nneu
c
c ..... ..define indicies for surface
c
          kb=neupdi(1,neu)
          ke=neupdi(2,neu)
          kn=neupdi(3,neu)
          normal=neupdi(4,neu)
          blk=neupdi(5,neu)
          call p2derm( cim(locb(blk)),imax(blk),jmax(blk),kb,ke,kn
     &                ,normal,er(locb(blk)) )
c
 41     continue
      endif
c
c ----------------------------------------------------------------------
c     Initialize the JOFF array
c     Compute differencing coefficients in both directions
c     Incorporate conductor information in differencing coefficients and
c        source
c ----------------------------------------------------------------------
c
      do 49 blk=1,nblk
c
c ..... Initialize the JOFF array
c
        joff(blk) = imax(blk) + 1
c
c ..... pass array addresses for each block in subroutine call
c
        if(bgle.eq.0)then
c.........homogeneous dielectric constant throughout
          call p2ddif( cim(locb(blk)), deli(loci(blk)), delj(locj(blk))
     &             ,er(0), imax(blk), jmax(blk), luout, ncond, pot
     &             , system, xi(loci(blk)), xj(locj(blk)), g(locb(blk))
     &             , a1(locb(blk)), a2(locb(blk)), b1(locb(blk))
     &             , b2(locb(blk)), c1(locb(blk)), c2(locb(blk))
     &             , maxsys, ier)
        else
c.........dielectric constant is not homogeneous over all blocks
          call p2ddi1( cim(locb(blk)), deli(loci(blk)), delj(locj(blk))
     &               , er(locb(blk)), imax(blk), jmax(blk), luout, ncond
     &               , pot, system, xi(loci(blk)), xj(locj(blk))
     &               , g(locb(blk)), a1(locb(blk)), a2(locb(blk))
     &               , b1(locb(blk)), b2(locb(blk)), c1(locb(blk))
     &               , c2(locb(blk)), fw(locb(blk))
     &               , maxsys, ier)
        endif
        if (ier .ne. 0) return
c
   49 continue
c
c ----------------------------------------------------------------------
c     Modify differencing coefficients for Neumann BCs
c ----------------------------------------------------------------------
c
c ... loop over Neumann BCs
c
      do 52 neu=1,nneu
c
c ..... define indicies for surface
c
        kb=neupdi(1,neu)
        ke=neupdi(2,neu)
        kn=neupdi(3,neu)
        normal=neupdi(4,neu)
        blk=neupdi(5,neu)
 
        call p2dneu( cim(locb(blk)), deli(loci(blk)), delj(locj(blk))
     &             , imax(blk), jmax(blk), luout, system
     &             , maxsys,kb,ke,kn,normal
     &             , xi(loci(blk)), xj(locj(blk))
     &             , a1(locb(blk)), a2(locb(blk)), b1(locb(blk))
     &             , b2(locb(blk)), c1(locb(blk)), c2(locb(blk)), ier )
        if (ier .ne. 0) return
c
 52   continue
c
c ----------------------------------------------------------------------
c     Modify differencing coefficients for periodic BCs and load
c      pointers is appropriate
c ----------------------------------------------------------------------
c
c ... initialize flags that show whether a block contains a portion of
c ...  a periodic string
c
      do 60 blk=1,nblk
        perblk(1,blk)=0
        perblk(2,blk)=0
   60 continue
c
c ... loop over periodic BCs
c
      do 62 per=1,nper
c
c ..... define indicies for upper surface
c
        kb=perpdi(1,2,per)
        ke=perpdi(2,2,per)
        kn=perpdi(3,2,per)
        normal=abs(perpdi(4,2,per))
        blk=perpdi(5,2,per)
c
c ..... set flag to show that this block is an upper periodic block
c
        perblk(normal,blk)=2
c
c ..... define indicies for lower surface
c
        kloff=perpdi(1,1,per) - kb
        knl=perpdi(3,1,per)
        blkl=perpdi(5,1,per)
c
c ..... loop over upper block surface
c
        do 61 k=kb,ke
c
c ....... compute index in lower block surface
c
          kl=k+kloff
c
c ....... normal in "i" direction
c
          if(normal.eq.1)then
c
c ......... compute linear index in upper and lower blocks
c
            lu=locfun(blk,kn,k)
            ll=locfun(blkl,knl,kl)
c
c ......... if upper boundary is Dirichlet or Neumann, transfer it to
c .........  the lower boundary
c
            if(a1(lu).eq.0.)then
              c1(ll)=0.
              b1(ll)=b1(lu)
              fw(ll)=fw(lu)
              a1(ll)=c1(lu)
            endif
c
c ......... if lower boundary is not Dirichlet or Neumann, load pointer
c .........  and C1 value
c
            if(c1(ll).ne.0.)then
              lu0=locfun(blk,0,k)
              ll0=locfun(blkl,0,kl)
              c1(lu0)=c1(ll)
              p(1,lu0)=ll0
              p(1,ll0)=ll0
            endif
c
c ....... normal in "j" direction
c
          else
c
c ......... compute linear index in upper and lower blocks
c
            lu=locfun(blk,k,kn)
            ll=locfun(blkl,kl,knl)
c
c ......... if upper boundary is Dirichlet or Neumann, transfer it to
c .........  the lower boundary
c
            if(a2(lu).eq.0.)then
              c2(ll)=0.
              b2(ll)=b2(lu)
              fw(ll)=fw(lu)
              a2(ll)=c2(lu)
            endif
c
c ......... if lower boundary is not Dirichlet or Neumann, load pointer
c .........  and C1 value
c
            if(c2(ll).ne.0.)then
              lu0=locfun(blk,k,0)
              ll0=locfun(blkl,kl,0)
              c2(lu0)=c2(ll)
              p(1,lu0)=ll0
              p(1,ll0)=ll0
            endif
          endif
c
   61   continue
   62 continue
c
c ... set flag to show that this block contains a portion of a
c ...  periodic string
c
      do 74 blk=nblk,1,-1
c
c ..... process blocks in downsweep order for each direction
c
        iblk=iorder(blk)
        jblk=jorder(blk)
c
c ..... loop over connecting blocks to see if they connect above (in
c .....  either direction) to a block with a periodic string
c .....  if so, map pointer array from upper to lower block
c .....  if there are any non-zero pointers passed, then set flag for
c .....  lower block to 1
c
        do 73 blkp=1,nblk
c
          if(bcm(iblk,blkp).eq.1)then
            if(perblk(1,blkp).ne.0)then
              pmax=0
              jp=bcmb(2,blkp,iblk)
              do 71 j=bcmb(2,iblk,blkp),bcme(2,iblk,blkp)
                lu0=locfun(blkp,0,jp)
                ll0=locfun(iblk,0,j)
                p(1,ll0)=p(1,lu0)
                pmax=max(pmax,p(1,ll0))
                jp = jp + 1
   71         continue
              if(pmax.gt.0) perblk(1,iblk)=1
            endif
          endif
c
          if(bcm(jblk,blkp).eq.2)then
            if(perblk(2,blkp).ne.0)then
              pmax=0
              ip=bcmb(1,blkp,jblk)
              do 72 i=bcmb(1,jblk,blkp),bcme(1,jblk,blkp)
                lu0=locfun(blkp,ip,0)
                ll0=locfun(jblk,i,0)
                p(1,ll0)=p(1,lu0)
                pmax=max(pmax,p(1,ll0))
                ip = ip + 1
   72         continue
              if(pmax.gt.0) perblk(2,jblk)=1
            endif
          endif
c
   73   continue
   74 continue
c
      return
c
  100 format(1x,20('*'),' WARNING from subroutine P2DINI ',19('*'),//
     &      ,10x,'Wachspress iteration parameter dimensioning is'
     &      ,' inadequate',//
     &      ,10x,'Parameter ',a,' needs to be at least ',i8,/
     &      ,10x,'Its current value is ',i8,//
     &      ,1x,71('*'),//)
c
      end
