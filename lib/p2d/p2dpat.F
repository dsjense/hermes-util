      subroutine p2dpat(imax1,imax2,jmax1,jmax2,k1,k2,m1l,m2l,m1u,pass
     &                 ,a1,a2 )
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
c       - This routine passes one row (or column) of array A1 to one row
c         (or column) of A2
c       - This routine performs the following specific functions:
c          1)  if PASS = 1:
c                 sets A2(m2,k2) = A1(m1,k1) for m1 = m1l,m1u and
c                 m2 = m1 + m2l - m1l
c          2)  if PASS = 2:
c                 sets A2(k2,m2) = A1(k1,m1) for m1 = m1l,m1u and
c                 m2 = m1 + m2l - m1l
c
c ----------------------------------------------------------------------
c
c     Input:
c       imax1   -  maximum "i" grid index for block 1
c       imax2   -  maximum "j" grid index for block 2
c       jmax1   -  maximum "i" grid index for block 1
c       jmax2   -  maximum "j" grid index for block 2
c       k1      -  "i" coordinate in block 1
c       k2      -  "i" coordinate in block 2
c       m1l     -  lower "j" coordinate in block 1
c       m2l     -  lower "j" coordinate in block 2
c       m1u     -  upper "j" coordinate in block 1
c       pass    -  1 or 2, denoting tangential direction "i" or "j",
c                  respectively
c       a1      -  potential array in block 1
c
c ----------------------------------------------------------------------
c
c     Output:
c       a2      -  potential array in block 2
c
c ----------------------------------------------------------------------
c
c     Internals:
c       m       -  loop index
c       moff    -  block offset  (moff = m2l - m1l)
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
      integer imax1,imax2,jmax1,jmax2,k1,k2,m1l,m2l,m1u,pass
      REAL_8 a1(0:imax1,0:jmax1),a2(0:imax2,0:jmax2)
c
c ... internal variables:
c
c ... loop indices:
      integer m
c ... scalars:
      integer moff
c
      moff = m2l - m1l
c
      if(pass.eq.1)then
c
        do 1 m=m1l,m1u
          a2(m+moff,k2) = a1(m,k1)
    1   continue
c
      else if(pass.eq.2)then
c
        do 2 m=m1l,m1u
          a2(k2,m+moff) = a1(k1,m)
    2   continue
c
      endif
c
      return
      end
