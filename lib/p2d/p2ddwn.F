      subroutine p2ddwn(a,b,c,bcm,bcmb,bcme,bgla,blk,blkdim,h,imax
     &                 ,jmax,joff,locb,nblk,nper,pass,perblk,perpdi
     &                 ,pointr,       bbig,d,dbig,hbig,p,pbig,s,sbig  )
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
c       - This routine performs the recursive downsweep for the ADI
c         solver for one block
c       - It should be called at the for each pass by P2DSLV
c       - Values in the upper row are patched to account for block and
c         periodic boundaries
c       - For vectorization, all columns are treated as if they are in
c       - a periodic string
c       - This routine performs the following specific functions:
c          1)  Computes S(N) and P(N) for each column (1-M)
c          2)  If block has an upper periodic boundary, D(N), S(N) and
c              P(N) are modified; and R(N-1), T(N-1), and Q(n-1) are
c              initialized
c          3)  If block has an upper block boundary, D(N), S(N) and P(N)
c              are taken from matching block, as well as running values
c              of T(N-1)
c          4)  Recurse down through block (k=n-1,2,-1) computing D(k),
c              S(k), P(k), T(k-1), Q(k-1), and R(k-1).
c          5)  Compute D(1), P(1) and S(1) using D(2), S(2), P(2), R(1),
c              T(1), and Q(1)
c          6)  Compute R(0), T(0) and Q(0)
c       - Storage conventions:
c          1)  C(1) of lower periodic block is stored in C(0) of upper
c              periodic block (already done by P2DINI)
c          2)  T(k-1) is stored in S(0)
c          3)  Q(k-1) is stored in H(0) of lower periodic block
c          4)  R(k-1) is stored in B(0) of lower periodic block
c          5)  Pointer to H(0) and B(0) in lower periodic block is
c              stored in P(0).  If not periodic, this value is zero, and
c              points to an unused location in arrays.  (These pointers
c              are precomputed in P2DINI.)
c
c ----------------------------------------------------------------------
c
c     Input:
c       a       -  "A" differencing coefficient array in the direction
c                  of pass  (block form)
c       b       -  "B" differencing coefficient array in the direction
c                  of pass  (block form)
c       c       -  "C" differencing coefficient array in the direction
c                  of pass  (block form)
c       bcm     -  block connectivity matrix  (+/- 1 or 2)
c       bcmb    -  beginning indicies for block connectivity line
c       bcme    -  ending indicies for block connectivity line
c       bgla    -  dimension of linear potential and related arrays
c       blk     -  # of block being prcessed
c       blkdim  -  upper dimension of block arrays
c       h       -  array containing source and alternate direction
c                  differences  (block form)
c       imax    -  maximum grid index for this block
c       jmax    -  maximum grid index for this block
c       joff    -  "j" offset array used to compute addresses in linear
c                  arrays
c       locb    -  pointer to FWA for each block of linear potential
c                  and related arrays
c       nblk    -  # of blocks in problem
c       nper    -  # of periodic BCs
c       pass    -  1 or 2, denoting direct solve in "1" or "2" direction
c       perblk  -  flag indicating if current block contains a portion
c                  of a periodic string
c       perpdi  -  plane definition indices for periodic BCs (second
c                  index is 1 or 2 for lower and upper planes,
c                  respectively)
c       pointr  -  integer array, equivalanced to the p array through
c                  the subroutine call, used to store periodic pointers
c
c ----------------------------------------------------------------------
c
c     Output:
c       bbig    -  "B" differencing coefficient arrays in pass direction
c                  (linear form)
c       d       -  "D" direct solve recursion array   (block form)
c       dbig    -  "D" direct solve recursion array   (linear form)
c       hbig    -  "H" direct solve source array   (linear form)
c       p       -  "P" direct solve recursion array   (block form)
c       pbig    -  "P" direct solve recursion array   (linear form)
c       s       -  "S" direct solve recursion array   (block form)
c       sbig    -  "S" direct solve recursion array   (linear form)
c
c ----------------------------------------------------------------------
c
c     Internals:
c       altpas  -  alternate direction to pass direction
c       blkp    -  loop index over blocks
c       k       -  loop index over direction of pass
c       kmax    -  maximum index in direction of pass
c       l0,l1   -  linear array pointers to addresses in other blocks
c       ladd    -  linear array pointer do loop increment amount
c       m       -  loop index over direction alternate to pass direction
c       mmax    -  maximum index in direction alternate to pass
c                  direction
c       mp      -  index in direction alternate to pass direction in a
c                  connecting block
c       per     -  loop index over periodic boundaries
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
      integer bgla,blk,blkdim,imax,jmax,nblk,nper,pass
      integer bcm (    1:blkdim,1:blkdim),bcmb(1:2,1:blkdim,1:blkdim)
      integer bcme(1:2,1:blkdim,1:blkdim)
      integer joff(1:blkdim),locb(1:blkdim),perblk(1:2,1:blkdim)
      integer perpdi(1:5,1:2,1),pointr(1:MDSCL,0:imax,0:jmax)
      REAL_8 a(0:imax,0:jmax),b(0:imax,0:jmax),c(0:imax,0:jmax)
      REAL_8 h(0:imax,0:jmax),p(0:imax,0:jmax),s(0:imax,0:jmax)
      REAL_8 d(0:imax,0:jmax),dbig(0:bgla)
      REAL_8 bbig(0:bgla),hbig(0:bgla),pbig(0:bgla),sbig(0:bgla)
c
c ... internal variables:
c
c ... loop indices:
      integer blkp,k,m,per
c ... scalars:
      integer altpas,kmax,l0,l1,ladd,mmax,mp
c
c ... Parameters:
c
c ... Statement functions and dummy arguments
      integer locfun,i,j
c
c ----------------------------------------------------------------------
c     Statement function definitions
c ----------------------------------------------------------------------
c
      locfun(m,i,j) = locb(m) + i + joff(m)*j
c
c
c ----------------------------------------------------------------------
c     Check to see which pass is requested and begin downsweep
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
c ..... initialize top row for all but block and periodic boundaries
c
        k=kmax
        do 1 m=1,mmax
          d(k,m) = 0.
          s(k,m) = -c(k,m)/b(k,m)
          p(k,m) =  h(k,m)/b(k,m)
          s(0,m) = 0.
    1   continue
c
c ..... loop over blocks
c
        do 3 blkp=1,nblk
          if(bcm(blk,blkp).eq.pass)then
c
c ......... if upper connection, then modify top row and pass T array
c
            mp=bcmb(altpas,blkp,blk)
            l1=locfun(blkp,1,mp)
            l0=locfun(blkp,0,mp)
            ladd=joff(blkp)
            do 2 m=bcmb(altpas,blk,blkp),bcme(altpas,blk,blkp)
              s(0,m)=sbig(l0)
              s(k,m)=sbig(l1)
              p(k,m)=pbig(l1)
              d(k,m)=dbig(l1)
              l1=l1+ladd
              l0=l0+ladd
    2       continue
          endif
    3   continue
c
c ..... loop over periodic boundaries if this block contains an upper
c .....  periodic boundary
c
        if(perblk(pass,blk).eq.2)then
c
          do 6 per=1,nper
            if(perpdi(5,2,per).eq.blk .and.
     &         abs(perpdi(4,2,per)).eq.pass)then
c
c .........   if upper connection, then modify top row, define T,
c .........    and initialize R and Q to zero
c
              do 4 m=perpdi(1,2,per),perpdi(2,2,per)
                d(k,m)=1.
                s(k,m)=0.
                p(k,m)=0.
                s(0,m)=c(0,m)
                l0=pointr(1,0,m)
                bbig(l0)=0.
                hbig(l0)=0.
    4         continue
            endif
    6     continue
c
        endif
c
c ..... recursively define D, S, and P
c
        do 9 k=kmax-1,2,-1
          do 7 m=1,mmax
            d(k,m) = -a(k,m)*d(k+1,m) / ( b(k,m) + a(k,m)*s(k+1,m) )
            s(k,m) = -c(k,m) / ( b(k,m) + a(k,m)*s(k+1,m) )
            p(k,m) = ( h(k,m) - a(k,m)*p(k+1,m) ) /
     &               ( b(k,m) + a(k,m)*s(k+1,m) )
    7     continue
c
c ....... recursively increment Q, R, and T if this block contains a
c .......  portion of a periodic string
c
          if(perblk(pass,blk).ne.0)then
c
cdir$       ivdep
            do 8 m=1,mmax
              hbig(pointr(1,0,m))=hbig(pointr(1,0,m))+s(0,m)*p(k,m)
              bbig(pointr(1,0,m))=bbig(pointr(1,0,m))+s(0,m)*d(k,m)
              s(0,m)=s(0,m)*s(k,m)
    8       continue
c
          endif
c
    9   continue
c
c ..... define D(1), S(1), P(1), Q(0), R(0), and T(0)
c
        k=1
        do 10 m=1,mmax
          d(k,m) = -a(k,m)*d(k+1,m) / ( b(k,m) + a(k,m)*s(k+1,m) )
          s(k,m) = -c(k,m) / ( b(k,m) + a(k,m)*s(k+1,m) )
          p(k,m) = ( h(k,m) - a(k,m)*p(k+1,m) ) /
     &             ( b(k,m) + a(k,m)*s(k+1,m) )
   10   continue
c
c ..... Loop over all periodic boundaries
c
        do 12 per=1,nper
          if(perpdi(5,1,per).eq.blk .and.
     &       abs(perpdi(4,1,per)).eq.pass)then
c
c ......... if lower connection, set S(1) = 0, modify P(1) and D(1)
c .........  Store lower periodic PHI value in D(0)
c
            do 11 m=perpdi(1,1,per),perpdi(2,1,per)
              p(k,m) = ( h(k,m) - h(0,m) - a(k,m)*p(k+1,m) ) /
     &                 ( b(k,m) + b(0,m) +
     &                   a(k,m)*(s(k+1,m)+d(2,m)) + s(0,m) )
              d(0,m) = p(k,m)
              d(k,m) = 0.
              s(k,m) = 0.
   11       continue
          endif
   12   continue
c
c ..... increment Q(0), R(0), and T(0) if this block contains a
c .......  portion of a periodic string
c
        if(perblk(pass,blk).ne.0)then
c
cdir$     ivdep
          do 13 m=1,mmax
              hbig(pointr(1,0,m))=hbig(pointr(1,0,m))+s(0,m)*p(k,m)
              bbig(pointr(1,0,m))=bbig(pointr(1,0,m))+s(0,m)*d(k,m)
              s(0,m)=s(0,m)*s(k,m)
   13     continue
c
        endif
c
      else
c
c ..... pass = 2 denotes direct solve in "j" or "2" direction
c
        mmax=imax
        kmax=jmax
        altpas=3-pass
c
c ..... initialize top row for all but block and periodic boundaries
c
        k=kmax
        do 21 m=1,mmax
          d(m,k) = 0.
          s(m,k) = -c(m,k)/b(m,k)
          p(m,k) =  h(m,k)/b(m,k)
          s(m,0) = 0.
   21   continue
c
c ..... loop over blocks
c
        do 23 blkp=1,nblk
          if(bcm(blk,blkp).eq.pass)then
c
c ......... if upper connection, then modify top row and pass T array
c
            mp=bcmb(altpas,blkp,blk)
            l1=locfun(blkp,mp,1)
            l0=locfun(blkp,mp,0)
            ladd=1
            do 22 m=bcmb(altpas,blk,blkp),bcme(altpas,blk,blkp)
              s(m,0)=sbig(l0)
              s(m,k)=sbig(l1)
              p(m,k)=pbig(l1)
              d(m,k)=dbig(l1)
              l1=l1+ladd
              l0=l0+ladd
   22       continue
          endif
   23   continue
c
c ..... loop over periodic boundaries if this block contains an upper
c .....  periodic boundary
c
        if(perblk(pass,blk).eq.2)then
c
          do 26 per=1,nper
            if(perpdi(5,2,per).eq.blk .and.
     &         abs(perpdi(4,2,per)).eq.pass)then
c
c .........   if upper connection, then modify top row, define T,
c .........    and initialize R and Q to zero
c
              do 24 m=perpdi(1,2,per),perpdi(2,2,per)
                d(m,k)=1.
                s(m,k)=0.
                p(m,k)=0.
                s(m,0)=c(m,0)
                l0=pointr(1,m,0)
                bbig(l0)=0.
                hbig(l0)=0.
   24         continue
            endif
   26     continue
c
        endif
c
c ..... recursively define D, S, and P
c
        do 29 k=kmax-1,2,-1
          do 27 m=1,mmax
            d(m,k) = -a(m,k)*d(m,k+1) / ( b(m,k) + a(m,k)*s(m,k+1) )
            s(m,k) = -c(m,k) / ( b(m,k) + a(m,k)*s(m,k+1) )
            p(m,k) = ( h(m,k) - a(m,k)*p(m,k+1) ) /
     &               ( b(m,k) + a(m,k)*s(m,k+1) )
   27     continue
c
c ....... recursively increment Q, R, and T if this block contains a
c .......  portion of a periodic string
c
          if(perblk(pass,blk).ne.0)then
c
cdir$       ivdep
            do 28 m=1,mmax
              hbig(pointr(1,m,0))=hbig(pointr(1,m,0))+s(m,0)*p(m,k)
              bbig(pointr(1,m,0))=bbig(pointr(1,m,0))+s(m,0)*d(m,k)
              s(m,0)=s(m,0)*s(m,k)
   28       continue
c
          endif
c
c
   29   continue
c
c ..... define D(1), S(1), P(1), Q(0), R(0), and T(0)
c
        k=1
        do 30 m=1,mmax
          d(m,k) = -a(m,k)*d(m,k+1) / ( b(m,k) + a(m,k)*s(m,k+1) )
          s(m,k) = -c(m,k) / ( b(m,k) + a(m,k)*s(m,k+1) )
          p(m,k) = ( h(m,k) - a(m,k)*p(m,k+1) ) /
     &             ( b(m,k) + a(m,k)*s(m,k+1) )
   30   continue
c
c ..... Loop over all periodic boundaries
c
        do 32 per=1,nper
          if(perpdi(5,1,per).eq.blk .and.
     &       abs(perpdi(4,1,per)).eq.pass)then
c
c ......... if lower connection, set S(1) = 0, modify P(1) and D(1)
c .........  Store lower periodic PHI value in D(0)
c
            do 31 m=perpdi(1,1,per),perpdi(2,1,per)
              p(m,k) = ( h(m,k) - h(m,0) - a(m,k)*p(m,k+1) ) /
     &                 ( b(m,k) + b(m,0) +
     &                   a(m,k)*(s(m,k+1)+d(m,2)) + s(m,0) )
              d(m,0) = p(m,k)
              d(m,k) = 0.
              s(m,k) = 0.
   31       continue
          endif
   32   continue
c
c ..... increment Q(0), R(0), and T(0) if this block contains a
c .......  portion of a periodic string
c
        if(perblk(pass,blk).ne.0)then
c
cdir$     ivdep
          do 33 m=1,mmax
            hbig(pointr(1,m,0))=hbig(pointr(1,m,0))+s(m,0)*p(m,k)
            bbig(pointr(1,m,0))=bbig(pointr(1,m,0))+s(m,0)*d(m,k)
            s(m,0)=s(m,0)*s(m,k)
   33     continue
c
        endif
c
      endif
c
      return
      end
