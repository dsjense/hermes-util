      subroutine p2dsl1
     &          ( a1,a2,b1,b2,bcm,bcmb,bcme,bgla
     &           ,blkdim,c1,c2,cim,conv,fw,fwachs
     &           ,g,imax,iorder,itrmax
     &           ,jmax,joff,jorder,locb,luout,nblk,nper,nwachs,perblk
     &           ,perpdi,system,wachs,d,h,p,s, niter,phi 
     &           ,phiold, ier )
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
c       - This routine performs the 2-D Poisson solve
c       - It should be called after calling P2DINI
c       - This routine performs the following specific functions:
c          1)  Iterates over the Wachspress parameters, solving for PHI
c          2)  Uses Peaceman-Rachford ADI formula
c          3)  Periodic conditions are handled by specially accounting
c              for the additional recursion terms required
c          4)  Convergence is checked for after each complete iteration
c
c ----------------------------------------------------------------------
c
c     Input:
c       a1,a2   -  "A" differencing coefficient arrays in i- and j-
c                  directions  (linear form)
c       b1,b2   -  "B" differencing coefficient arrays in i- and j-
c                  directions  (linear form)
c       bcm     -  block connectivity matrix  (+/- 1 or 2)
c       bcmb    -  beginning indicies for block connectivity line
c       bcme    -  ending indicies for block connectivity line
c       bgla    -  dimension of linear potential and related arrays
c       blkdim  -  upper dimension of block arrays
c       c1,c2   -  "C" differencing coefficient arrays in i- and j-
c                  directions  (linear form)
c       cim     -  cell information matrix (linear form)
c       conv    -  convergence ratio for solution
c       fw      -  wachpress scaling factor array
c       fwachs  -  multiplier for Wachspress parameters for differencing
c                  scheme
c       g       -  source array (linear form)
c       imax    -  maximum grid index in each block
c       iorder  -  block ordering array for i-component direct solve
c       itrmax  -  maximum # of iterations allowed
c       jmax    -  maximum grid index in each block
c       joff    -  "j" offset array used to compute addresses in linear
c                  arrays
c       jorder  -  block ordering array for j-component direct solve
c       locb    -  pointer to FWA for each block of linear potential
c                  and related arrays
c       luout   -  logical unit # for printed output (error messages)
c       nblk    -  # of blocks in problem
c       nper    -  # of periodic BCs
c       nwachs  -  # of Wachspress iteration parameters needed for this
c                  problem
c       perblk  -  flag indicating if block contains a portion
c                  of a periodic string
c       perpdi  -  plane definition indices for periodic BCs (second
c                  index is 1 or 2 for lower and upper planes,
c                  respectively)
c       system  -  coordinate system flag:
c                     1 - cartesian   (  x  ,  y  )
c                     2 - cylindrical (  z  ,  r  )
c                     3 - cylindrical (  r  , phi )
c                     4 - spherical   (theta, phi )
c                         (solves r**2*LaPlacian; returns phi and r*field)
c                     5 - spherical   (  r  ,theta)
c                     6 - spherical   (sin(theta) phi , r)
c       wachs   -  Wachspress iteration parameters for this grid
c
c ----------------------------------------------------------------------
c
c     Passed working arrays:
c       h       -  working array containing source and alternate
c                  direction differences  (linear form)
c       p,s,d   -  working arrays (linear form) used in recursion, i.e.,
c                     phi(i) = s(i)*phi(i-1) + p(i) + d(i)*phi0
c                  NOTE:  Periodic pointers have been stored into the
c                         zero-row slots of p in P2DINI as appropriate
c
c ----------------------------------------------------------------------
c
c     Output:
c       niter   -  # of iterations required to reach requested
c                  convergence
c       phi     -  solution potential  (linear form)
c       ier     -  error parameter indicating completion status
c                     0 - Normal Successful completion
c                    +1 - Failure to converge in specified # of iterations
c
c ----------------------------------------------------------------------
c
c     Internals:
c       altpas  -  1 or 2, denoting direction opposite to solve
c       b1off   -  offset to correct b1 to include iteration parameter
c       b2off   -  offset to correct b2 to include iteration parameter
c       blk     -  loop index denoting block #
c       blkp    -  loop index denoting block #
c       errmax  -  maximum value of error in phi over entire mesh
c       flg1st  -  flag indicating whether or not (0 or 1) this is the
c                  first block processed in this pass
c       iblk    -  ordered block # for pass 1
c       iter    -  loop index denoting iteration count
c       jblk    -  ordered block # for pass 2
c       kwach   -  Wachspress parameter # for this iteration
c       pass    -  1 or 2, denoting direct solve in "1" or "2" direction
c       per     -  loop index over periodic boundary conditions
c       phimax  -  maximum value of phi over entire mesh
c       phimin  -  minimum value of phi over entire mesh
c       pwachs  -  Wachspress iteration parameter for this iteration
c       pwach0  -  Wachspress iteration parameter for last iteration
c
c ----------------------------------------------------------------------
c
c     Parameters:
c       hi      -  Index indicating upper periodic boundary
c       low     -  Index indicating lower periodic boundary
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
      integer bgla,blkdim,itrmax,niter,luout,nblk,nper,nwachs,system
      integer ier
      integer bcm (    1:blkdim,1:blkdim),bcmb(1:2,1:blkdim,1:blkdim)
      integer bcme(1:2,1:blkdim,1:blkdim)
      integer cim(0:bgla),imax(1:blkdim),jmax(1:blkdim),joff(1:blkdim)
      integer iorder(1:blkdim),jorder(1:blkdim),locb(1:blkdim)
      integer perblk(1:2,1:blkdim),perpdi(1:5,1:2,1)
      real conv, g(0:bgla)
      REAL_8 a1(0:bgla),a2(0:bgla),b1(0:bgla),b2(0:bgla)
      REAL_8 c1(0:bgla),c2(0:bgla),fw(0:bgla),fwachs
      REAL_8 wachs(0:nwachs)
      REAL_8 d(0:bgla), h(0:bgla), p(0:bgla),s(0:bgla)
      REAL_8 phi(0:bgla), phiold(0:bgla)
c
c ... internal variables:
c
c ... loop indices:
      integer i,blk,blkp,iter,per
c ... scalars:
      REAL_8 b1off,b2off,errmax,phimax,phimin,pwachs,pwach0
      integer altpas,flg1st,iblk,jblk,kwach,pass
c
c ... Parameters:
c
      integer hi,low
      parameter (low = 1, hi = 2)
c
c
c ----------------------------------------------------------------------
c     Start iteration loop over Wachspress iteration parameters
c ----------------------------------------------------------------------
c
c ... initialize value of last iteration parameter
c
      ier = 0
      pwach0=0.
c
c ... start iteration loop
c
      do 50 iter=0,itrmax
c
        niter=iter
c       save the old value of phi
        do i = 0, bgla
          phiold(i) = phi(i)
        enddo
c
c ..... compute # of, and  Wachspress parameter for this iteration
c
        kwach=mod(iter,nwachs+1)
        pwachs=fwachs*wachs(kwach)
c
c ..... compute offsets needed to make b1  =  b1 - pwachs  ,
c .....                                b2  =  b2 + pwachs
c
        b2off = pwachs+pwach0
        b1off = -b2off
c
c ..... save the iteration parameter for the next iteration
c
        pwach0=pwachs
c
c ..... pass = 1 denotes direct solve in "i" or "1" direction
c
        pass = 1
        altpas = 2
c
c ..... modify b1 and b2 and compute h for pass 1  (block by block)
c
        do 1 blk=1,nblk
c
          call p2dbh1(a2(locb(blk)),b1off,b2off,c2(locb(blk))
     &               ,cim(locb(blk)),fw(locb(blk)),g(locb(blk))
     &               ,imax(blk),jmax(blk),pass,phi(locb(blk))
     &               ,b1(locb(blk)),b2(locb(blk)),h(locb(blk))  )
c
    1   continue
c
c ..... Block boundaries
c .....  pass upper block H(1) to lower block H(N) if boundary
c .....  has ALTPAS normal direction to avoid corruption of this pass
c
        do 3 blk=1,nblk,1
          do 2 blkp=1,nblk,1
            if(bcm(blk,blkp).eq.altpas)then
              call p2dpat(imax(blkp),imax(blk),jmax(blkp),jmax(blk)
     &                   ,bcmb(altpas,blkp,blk),bcmb(altpas,blk,blkp)
     &                   ,bcmb(pass,blkp,blk),bcmb(pass,blk,blkp)
     &                   ,bcme(pass,blkp,blk),pass
     &                   ,h(locb(blkp)),h(locb(blk)) )
            endif
    2     continue
    3   continue
c
c ..... compute recursion coefficients (block by block) for pass 1
c
        do 4 blk=nblk,1,-1
c
c ....... process blocks in pass 1 downsweep order
c
          iblk=iorder(blk)
c
          call p2ddwn(a1(locb(iblk)),b1(locb(iblk)),c1(locb(iblk))
     &               ,bcm,bcmb,bcme,bgla,iblk,blkdim,h(locb(iblk))
     &               ,imax(iblk),jmax(iblk),joff,locb,nblk,nper
     &               ,pass,perblk,perpdi,p(locb(iblk))
     &               ,b1,d(locb(iblk)),d,h,p(locb(iblk)),p
     &               ,s(locb(iblk)),s  )
c
    4   continue
c
c ..... compute potentials (phi) (block by block) for pass 1
c
        flg1st=0
        do 5 blk=1,nblk,1
c
c ....... process blocks in pass 1 upsweep order
c
          iblk=iorder(blk)
c
          call p2dup(a1(locb(iblk)),b1(locb(iblk)),bcm,bcmb,bcme,bgla
     &              ,iblk,blkdim,d(locb(iblk)),d,flg1st
     &              ,imax(iblk),jmax(iblk)
     &              ,joff,locb,nblk,nper,p(locb(iblk)),pass,perblk
     &              ,perpdi,p(locb(iblk)),s(locb(iblk))
     &              ,phi(locb(iblk))      ,h(locb(iblk)) ,phi )
c
    5   continue
c
c .....  Periodic boundaries
c
        do 6 per=1,nper
          blk  = perpdi(5,hi,per)
          blkp = perpdi(5,low,per)
c
c ....... pass lower boundary PHI(1) to upper boundary PHI(N) if boundary
c .......  has ALTPAS normal direction to fix corruption of this pass
c
          if(abs(perpdi(4,low,per)).eq.altpas)then
            call p2dpat(imax(blkp),imax(blk),jmax(blkp),jmax(blk)
     &                 ,perpdi(3,low,per),perpdi(3,hi,per)
     &                 ,perpdi(1,low,per),perpdi(1,hi,per)
     &                 ,perpdi(2,low,per),pass
     &                 ,phi(locb(blkp)),phi(locb(blk)) )
c
c ....... if boundary has PASS normal direction, then . . .
c
          else if(abs(perpdi(4,low,per)).eq.pass)then
c
c ......... pass upper boundary PHI(N-1) to lower boundary PHI(0)
c .........  to prepare for next pass
c
            call p2dpat(imax(blk),imax(blkp),jmax(blk),jmax(blkp)
     &                 ,perpdi(3,hi,per)-1,perpdi(3,low,per)-1
     &                 ,perpdi(1,hi,per),perpdi(1,low,per)
     &                 ,perpdi(2,hi,per),altpas
     &                 ,phi(locb(blk)),phi(locb(blkp)) )
c
c ......... pass lower boundary PHI(1) to upper boundary PHI(N)
c .........  to fix corruption of this pass
c
            call p2dpat(imax(blkp),imax(blk),jmax(blkp),jmax(blk)
     &                 ,perpdi(3,low,per),perpdi(3,hi,per)
     &                 ,perpdi(1,low,per),perpdi(1,hi,per)
     &                 ,perpdi(2,low,per),altpas
     &                 ,phi(locb(blkp)),phi(locb(blk)) )
c
          endif
c
    6   continue
c
c ..... Block boundaries
c
        do 8 blk=1,nblk,1
          do 7 blkp=1,nblk,1
            if(bcm(blk,blkp).eq.altpas)then
c
c ..........  pass upper block PHI(1) to lower block PHI(N) if boundary
c ..........  has ALTPAS normal direction to fix corruption of this pass
c
              call p2dpat(imax(blkp),imax(blk),jmax(blkp),jmax(blk)
     &                   ,bcmb(altpas,blkp,blk),bcmb(altpas,blk,blkp)
     &                   ,bcmb(pass,blkp,blk),bcmb(pass,blk,blkp)
     &                   ,bcme(pass,blkp,blk),pass
     &                   ,phi(locb(blkp)),phi(locb(blk)) )
c
c ......... if boundary has PASS normal direction, then . . .
c
            else if(bcm(blk,blkp).eq.pass)then
c
c ........... pass upper boundary PHI(N-1) to lower boundary PHI(0)
c ...........  to prepare for next pass
c
              call p2dpat(imax(blk),imax(blkp),jmax(blk),jmax(blkp)
     &                   ,bcmb(pass,blk,blkp)-1,bcmb(pass,blkp,blk)-1
     &                   ,bcmb(altpas,blk,blkp),bcmb(altpas,blkp,blk)
     &                   ,bcme(altpas,blk,blkp),altpas
     &                   ,phi(locb(blk)),phi(locb(blkp)) )
c
c ........... pass lower boundary PHI(1) to upper boundary PHI(N)
c ...........  to fix corruption of this pass
c
              call p2dpat(imax(blkp),imax(blk),jmax(blkp),jmax(blk)
     &                   ,bcmb(pass,blkp,blk),bcmb(pass,blk,blkp)
     &                   ,bcmb(altpas,blkp,blk),bcmb(altpas,blk,blkp)
     &                   ,bcme(altpas,blkp,blk),altpas
     &                   ,phi(locb(blkp)),phi(locb(blk)) )
c
            endif
    7     continue
    8   continue
c
c
c ..... pass = 2 denotes direct solve in "j" or "2" direction
c
        pass = 2
        altpas = 1
c
c ..... compute offsets needed to make b1  =  b1 + pwachs  ,
c .....                                b2  =  b2 - pwachs
c
        b1off = 2.*pwachs
        b2off = -b1off
c
c ..... modify b1 and b2 and compute h for pass 2  (block by block)
c
        do 11 blk=1,nblk
c
          call p2dbh1(a1(locb(blk)),b1off,b2off,c1(locb(blk))
     &               ,cim(locb(blk)),fw(locb(blk)),g(locb(blk))
     &               ,imax(blk),jmax(blk),pass,phi(locb(blk))
     &               ,b1(locb(blk)),b2(locb(blk)),h(locb(blk))  )
c
   11   continue
c
c ..... Block boundaries
c .....  pass lower block H(1) to upper block H(N) if boundary
c .....  has ALTPAS normal direction to avoid corruption of this pass
c
        do 13 blk=1,nblk,1
          do 12 blkp=1,nblk,1
            if(bcm(blk,blkp).eq.altpas)then
              call p2dpat(imax(blkp),imax(blk),jmax(blkp),jmax(blk)
     &                   ,bcmb(altpas,blkp,blk),bcmb(altpas,blk,blkp)
     &                   ,bcmb(pass,blkp,blk),bcmb(pass,blk,blkp)
     &                   ,bcme(pass,blkp,blk),pass
     &                   ,h(locb(blkp)),h(locb(blk)) )
            endif
   12     continue
   13   continue
c
c ..... compute recursion coefficients (block by block) for pass 2
c
        do 14 blk=nblk,1,-1
c
c ....... process blocks in pass 2 downsweep order
c
          jblk=jorder(blk)
c
          call p2ddwn(a2(locb(jblk)),b2(locb(jblk)),c2(locb(jblk))
     &               ,bcm,bcmb,bcme,bgla,jblk,blkdim,h(locb(jblk))
     &               ,imax(jblk),jmax(jblk),joff,locb,nblk,nper
     &               ,pass,perblk,perpdi,p(locb(jblk))
     &               ,b2,d(locb(jblk)),d,h,p(locb(jblk)),p
     &               ,s(locb(jblk)),s  )
c
   14   continue
c
c ..... compute potentials (phi) (block by block) for pass 2
c ..... along the way, updates convergence parameters
c
        do 15 blk=1,nblk,1
c
c ....... set flag if this is the first block processed in upsweep
c
          if(blk.eq.1)then
            flg1st = 1
          else
            flg1st = 0
          endif
c
c ....... process blocks in pass 2 upsweep order
c
          jblk=jorder(blk)
c
          call p2dup(a2(locb(jblk)),b2(locb(jblk)),bcm,bcmb,bcme,bgla
     &              ,jblk,blkdim,d(locb(jblk)),d,flg1st
     &              ,imax(jblk),jmax(jblk)
     &              ,joff,locb,nblk,nper,p(locb(jblk)),pass,perblk
     &              ,perpdi, p(locb(jblk)),s(locb(jblk))
     &              ,phi(locb(jblk))      ,h(locb(jblk)) ,phi )
c
   15   continue
c
c .....  Periodic boundaries
c
        do 16 per=1,nper
          blk  = perpdi(5,hi,per)
          blkp = perpdi(5,low,per)
c
c ....... pass lower block PHI(1) to upper block PHI(N) if boundary
c .......  has ALTPAS normal direction to fix corruption of this pass
c
          if(abs(perpdi(4,low,per)).eq.altpas)then
            call p2dpat(imax(blkp),imax(blk),jmax(blkp),jmax(blk)
     &                 ,perpdi(3,low,per),perpdi(3,hi,per)
     &                 ,perpdi(1,low,per),perpdi(1,hi,per)
     &                 ,perpdi(2,low,per),pass
     &                 ,phi(locb(blkp)),phi(locb(blk)) )
c
c ....... if boundary has PASS normal direction, then . . .
c
          else if(abs(perpdi(4,low,per)).eq.pass)then
c
c ......... pass upper boundary PHI(N-1) to lower boundary PHI(0)
c .........  to prepare for next pass
c
            call p2dpat(imax(blk),imax(blkp),jmax(blk),jmax(blkp)
     &                 ,perpdi(3,hi,per)-1,perpdi(3,low,per)-1
     &                 ,perpdi(1,hi,per),perpdi(1,low,per)
     &                 ,perpdi(2,hi,per),altpas
     &                 ,phi(locb(blk)),phi(locb(blkp)) )
c
c ......... pass lower boundary PHI(1) to upper boundary PHI(N)
c .........  to fix corruption of this pass
c
            call p2dpat(imax(blkp),imax(blk),jmax(blkp),jmax(blk)
     &                 ,perpdi(3,low,per),perpdi(3,hi,per)
     &                 ,perpdi(1,low,per),perpdi(1,hi,per)
     &                 ,perpdi(2,low,per),altpas
     &                 ,phi(locb(blkp)),phi(locb(blk)) )
c
          endif
c
   16   continue
c
c ..... Block boundaries
c
        do 18 blk=1,nblk,1
          do 17 blkp=1,nblk,1
            if(bcm(blk,blkp).eq.altpas)then
c
c ........... pass lower block PHI(1) to upper block PHI(N) if boundary
c ...........  has ALTPAS normal direction to fix corruption of this pass
c
              call p2dpat(imax(blkp),imax(blk),jmax(blkp),jmax(blk)
     &                   ,bcmb(altpas,blkp,blk),bcmb(altpas,blk,blkp)
     &                   ,bcmb(pass,blkp,blk),bcmb(pass,blk,blkp)
     &                   ,bcme(pass,blkp,blk),pass
     &                   ,phi(locb(blkp)),phi(locb(blk)) )
c
c ......... if boundary has PASS normal direction, then . . .
c
            else if(bcm(blk,blkp).eq.pass)then
c
c ........... pass upper boundary PHI(N-1) to lower boundary PHI(0)
c ...........  to prepare for next pass
c
              call p2dpat(imax(blk),imax(blkp),jmax(blk),jmax(blkp)
     &                   ,bcmb(pass,blk,blkp)-1,bcmb(pass,blkp,blk)-1
     &                   ,bcmb(altpas,blk,blkp),bcmb(altpas,blkp,blk)
     &                   ,bcme(altpas,blk,blkp),altpas
     &                   ,phi(locb(blk)),phi(locb(blkp)) )
c
c ........... pass lower boundary PHI(1) to upper boundary PHI(N)
c ...........  to fix corruption of this pass
c
              call p2dpat(imax(blkp),imax(blk),jmax(blkp),jmax(blk)
     &                   ,bcmb(pass,blkp,blk),bcmb(pass,blk,blkp)
     &                   ,bcmb(altpas,blkp,blk),bcmb(altpas,blk,blkp)
     &                   ,bcme(altpas,blkp,blk),altpas
     &                   ,phi(locb(blkp)),phi(locb(blk)) )
c
            endif
   17     continue
   18   continue
c
c
c ..... test convergence -- if converged, then return
c
        if (iter .gt. 2) then
          do 21 blk=1,nblk,1
            call p2derr(blk,imax(blk),jmax(blk),phi(locb(blk)),
     &              phiold(locb(blk)) ,errmax,phimax,phimin )
   21     continue
c
          if ( errmax.le.conv*(phimax-phimin) ) return
        endif
c
   50 continue
c
c ... if not converged in "itrmax"iterations, write warning and stop
c
      if (luout.ge.0) write(luout,100)itrmax,errmax,phimax-phimin,conv
c
      ier = +1
      return
c
  100 format(1x,20('*'),' WARNING from subroutine P2DSL1 ',19('*'),//
     &      ,7x,'POISSON solve did not converge after ',i10
     &      ,' iterations',//
     &      ,7x,'The ratio of the error (',1pe10.3,') to the range ('
     &      ,e10.3,')',/,7x,'is greater than the specified '
     &      ,'convergence criterion',/, 7x, '(',e10.3,')',//
     &      ,1x,71('*'),//)
c
      end
