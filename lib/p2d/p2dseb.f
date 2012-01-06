      subroutine p2dseb(imax1,imax2,jmax1,jmax2,k1,k2,m1l,m2l,m1u,dir
     &                 ,ei1,ej1, ei2,ej2 )
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
c       - This routine passes longitudinal E-fields from interior points
c         to buffer cells at block connections /or periodic boundaries,
c         where "longitudinal" means the component normal to the
c         connection edge. For example, for a block connection in i,
c         with block #1 as the upper block, this routine sets
c            Ei1(0,*)     = Ei2(imax2-1,*)
c            Ei2(imax2,*) = Ei1(1,*)
c         For a block connection in j, with block #2 as the upper block
c            Ej2(*,0)     = Ej1(*,jmax2-1)
c            Ej1(jmax2,*) = Ej2(1,*)
c         NOTE that there is no mapping of the transverse field components
c
c ----------------------------------------------------------------------
c
c     Input:
c       imax1   -  maximum "i" grid index for block 1
c       imax2   -  maximum "j" grid index for block 2
c       jmax1   -  maximum "i" grid index for block 1
c       jmax2   -  maximum "j" grid index for block 2
c       k1      -  "dir" coordinate in block 1 \ Indices for connection
c       k2      -  "dir" coordinate in block 2 / of POTENTIAL
c       m1l     -  lower "tdir" coordinate in block 1
c       m2l     -  lower "tdir" coordinate in block 2
c       m1u     -  upper "tdir" coordinate in block 1
c       dir     -  Direction of block connection: 1 ="i", 2 = "j"
c
c ----------------------------------------------------------------------
c
c     Input/Output:
c       ei1     -  Ei field in block 1
c       ej1     -  Ej field in block 1
c       ei2     -  Ei field in block 2
c       ej2     -  Ej field in block 2
c
c ----------------------------------------------------------------------
c
c     Internals:
c       k*i     -  "dir" index for block * INSIDE  point \ * = 1, 2
c       k*o     -  "dir" index for block * OUTSIDE point /
c       m       -  loop index
c       moff    -  block offset  (moff = m2l - m1l)
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c ... Passed variables:
c
      integer imax1,imax2,jmax1,jmax2,k1,k2,m1l,m2l,m1u,dir
      real    ei1(0:imax1,0:jmax1), ej1(0:imax1,0:jmax1)
      real    ei2(0:imax2,0:jmax2), ej2(0:imax2,0:jmax2)
c
c ... internal variables:
c
c ... loop indices:
      integer m
c ... scalars:
      integer k1i,k1o, k2i,k2o, moff
c
      moff = m2l - m1l
c
      if (k1 .EQ. 1) then
        k1i = k1
        k1o = k1 - 1
        k2i = k2 - 1
        k2o = k2
      else
        k1i = k1 - 1
        k1o = k1
        k2i = k2
        k2o = k2 - 1
      endif
c
      if(dir.eq.1)then
c
        do 1 m=m1l,m1u
          ei1(k1o,m)      = ei2(k2i,m+moff)
          ei2(k2o,m+moff) = ei1(k1i,m)
    1   continue
c
      else if(dir.eq.2)then
c
        do 2 m=m1l,m1u
          ej1(m     ,k1o) = ej2(m+moff,k2i)
          ej2(m+moff,k2o) = ej1(m     ,k1i)
    2   continue
c
      endif
c
      return
      end
