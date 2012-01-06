      subroutine p2dup(a,b,bcm,bcmb,bcme,bgla,blk,blkdim,d,dbig,flg1st
     &                 ,imax,jmax,joff,locb,nblk,nper,p
     &                 ,pass,perblk,perpdi,pointr,s     ,phi     ,work
     &                 ,phibig   )
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
c       - This routine performs the back-substitution upsweep for the
c         ADI solver for one block
c       - It should be called for each pass by P2DSLV, after P2DDWN
c       - This routine performs the following specific functions:
c          1)  If PASS = 2, then save old PHI in work array
c          2)  If block has an lower block boundary, PHI(0) is taken
c              from matching block
c          3)  Use back-substitution to compute PHI(k) for k=1,N
c          4)  If block has an upper periodic boundary, PHI(N) and
c              PHI(N-1) are modified using value of PHI(1) in matching
c              block
c          5)  If PASS = 2, then compute variables used in convergence
c              testing
c
c ----------------------------------------------------------------------
c
c     Input:
c       a       -  "A" differencing coefficient array in the direction
c                  of pass  (block form)
c       b       -  "B" differencing coefficient array in the direction
c                  of pass  (block form)
c       bcm     -  block connectivity matrix  (+/- 1 or 2)
c       bcmb    -  beginning indicies for block connectivity line
c       bcme    -  ending indicies for block connectivity line
c       bgla    -  dimension of linear potential and related arrays
c       blk     -  # of block being prcessed
c       blkdim  -  upper dimension of block arrays
c       d       -  "D" direct solve recursion array   (block form)
c       dbig    -  "D" direct solve recursion array   (linear form)
c       flg1st  -  flag indicating whether or not (0 or 1) this is the
c                  first block processed in this pass
c       imax    -  maximum grid index for this block
c       jmax    -  maximum grid index for this block
c       joff    -  "j" offset array used to compute addresses in linear
c                  arrays
c       locb    -  pointer to FWA for each block of linear potential
c                  and related arrays
c       nblk    -  # of blocks in problem
c       nper    -  # of periodic BCs
c       p       -  "P" direct solve recursion array   (block form)
c       pass    -  1 or 2, denoting direct solve in "1" or "2" direction
c       perblk  -  flag indicating if current block contains a portion
c                  of a periodic string
c       perpdi  -  plane definition indices for periodic BCs (second
c                  index is 1 or 2 for lower and upper planes,
c                  respectively)
c       pointr  -  integer array, equivalanced to the p array through
c                  the subroutine call, used to store periodic pointers
c       s       -  "S" direct solve recursion array   (block form)
c       phi     -  potential array before this iteration pass  (block
c                  form)
c
c ----------------------------------------------------------------------
c
c     Working Arrays:
c       work    -  array to save old values of PHI for convergence check
c
c ----------------------------------------------------------------------
c
c     Output:
c       phi     -  potential array after this iteration pass  (block
c                  form)
c       phibig  -  potential array after this iteration pass  (linear
c                  form)
c
c ----------------------------------------------------------------------
c
c     Internals:
c       altpas  -  alternate direction to pass direction
c       blkp    -  loop index over blocks
c       k       -  loop index over direction of pass
c       kmax    -  maximum index in direction of pass
c       l1      -  linear array pointers to addresses in other blocks
c       ladd    -  linear array pointer do loop increment amount
c       m       -  loop index over direction alternate to pass direction
c       mmax    -  maximum index in direction alternate to pass
c                  direction
c       mp      -  index in direction alternate to pass direction in a
c                  connecting block
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
      integer bgla,blk,blkdim,flg1st,imax,jmax,nblk,nper,pass
      integer bcm (    1:blkdim,1:blkdim),bcmb(1:2,1:blkdim,1:blkdim)
      integer bcme(1:2,1:blkdim,1:blkdim)
      integer joff(1:blkdim),locb(1:blkdim),perblk(1:2,1:blkdim)
      integer perpdi(1:5,1:2,1),pointr(1:MDSCL,0:imax,0:jmax)
      REAL_8 a(0:imax,0:jmax),b(0:imax,0:jmax),p(0:imax,0:jmax)
      REAL_8 d(0:imax,0:jmax)
      REAL_8 phi(0:imax,0:jmax),s(0:imax,0:jmax),work(0:imax,0:jmax)
      REAL_8 phibig(0:bgla),dbig(0:bgla)
c
c ... internal variables:
c
c ... loop indices:
      integer blkp,i,j,k,m
c ... scalars:
      integer altpas,kmax,kp,l1,ladd,mmax,mp
c
c ... Parameters:
c
c ... Statement functions
      integer locfun
c
c ----------------------------------------------------------------------
c     Statement function definitions
c ----------------------------------------------------------------------
c
      locfun(m,i,j) = locb(m) + i + joff(m)*j
c
c
c ----------------------------------------------------------------------
c     Check to see which pass is requested and begin upsweep
c ----------------------------------------------------------------------
c
      if(pass.eq.1)then
c
c ..... pass = 1 denotes direct solve in "i" or "1" direction
c
        mmax=jmax
        kmax=imax
        altpas=3-pass
c
c ..... preload PHI(1) from lower periodic boundary into D(0)
c .....  Note that this is okay for non-periodic strings, since in that
c .....  case, the D coefficients should be zero
c
        do 2 m=1,mmax
          d(0,m) = dbig(pointr(1,0,m))
    2   continue
c
c ..... loop over blocks
c
        k=0
        do 4 blkp=1,nblk
          if(bcm(blk,blkp).eq.-pass)then
c
c ......... if lower connection, then get PHI(0) from matching block
c
            kp=bcmb(  pass,blkp,blk)
            mp=bcmb(altpas,blkp,blk)
            l1=locfun(blkp,kp-1,mp)
            ladd=joff(blkp)
            do 3 m=bcmb(altpas,blk,blkp),bcme(altpas,blk,blkp)
              phi(k,m)=phibig(l1)
              l1=l1+ladd
    3       continue
          endif
    4   continue
c
c ..... use back-substitution to compute PHI(k), k=1,N
c
        do 6 k=1,kmax
          do 5 m=1,mmax
            phi(k,m) = s(k,m)*phi(k-1,m) + d(k,m)*d(0,m) + p(k,m)
    5     continue
    6   continue
c
      else
c
c ..... pass = 2 denotes direct solve in "j" or "2" direction
c
        mmax=imax
        kmax=jmax
        altpas=3-pass
c
c ..... preload PHI(1) from lower periodic boundary into D(0)
c .....  Note that this is okay for non-periodic strings, since in that
c .....  case, the D coefficients should be zero
c
        do 12 m=1,mmax
          d(m,0) = dbig(pointr(1,m,0))
   12   continue
c
c ..... loop over blocks
c
        k=0
        do 14 blkp=1,nblk
          if(bcm(blk,blkp).eq.-pass)then
c
c ......... if lower connection, then get PHI(0) from matching block
c
            kp=bcmb(  pass,blkp,blk)
            mp=bcmb(altpas,blkp,blk)
            l1=locfun(blkp,mp,kp-1)
            ladd=1
            do 13 m=bcmb(altpas,blk,blkp),bcme(altpas,blk,blkp)
              phi(m,k)=phibig(l1)
              l1=l1 + ladd
   13       continue
          endif
   14   continue
c
c ..... use back-substitution to compute PHI(k), k=1,N
c
        do 16 k=1,kmax
          do 15 m=1,mmax
            phi(m,k) = s(m,k)*phi(m,k-1) + d(m,k)*d(m,0) + p(m,k)
   15     continue
   16   continue
c
      endif
c
      return
      end
