      program tstak
c
c ----------------------------------------------------------------------
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
C_Groups main standalone
c ----------------------------------------------------------------------
c
c     This routine tests the funcionality of the stak library
c
c ----------------------------------------------------------------------
c
      implicit none
c
c     Machine dependent include file
#include "mdptr.h"
c
      integer INT, FLT
      parameter ( INT = 0 , FLT = -1 )
c
      integer BNDWRDS
      parameter ( BNDWRDS = HU_PTR_BYTES/4 )
c
      integer ierr, k, n, ilist(5), itmp, ng, ngd, nsize, nsize2
cc      integer i_ptr, c3_ptr, c4_ptr, c5_ptr, f_ptr
      integer i(1)
      integer isave(1:50)
      integer bin(5), nguard(5), numb(5)
      HU_SIZE_T last(5), size(5)
      real    f(0:1)
      logical nsave
      integer lua
      character c3(5)*(3),c4(5)*(4),c5(5)*(5)
      character cdum*10
c
      integer stkinit, stkfptr, stkchk, stklist,
     &        stkrbin, stkdmp, stkload, stkrclr

      HU_PTR_TYPE pgrow
      HU_PTR_TYPE stkgrow, stkptr
c
      pointer ( i_ptr, i )
      pointer ( f_ptr, f )
      pointer ( c3_ptr, c3 )
      pointer ( c4_ptr, c4 )
      pointer ( c5_ptr, c5 )
c
cc      nsave = .FALSE.
      nsave = .TRUE.
      lua = 10
cc      lua = 6
      if ( lua.ne.6 ) open(unit=lua,file='stkinfo',status='unknown')
c
      print'(a,$)','Enter ng: '
      read*,ng
c
      ngd = (ng -1)/BNDWRDS + 1
c
      cdum = '0123456789'
c
      i_ptr = stkptr(1,25,INT,'i')
c
      ierr  = stkinit(1,0,.TRUE.,ng,nsave,lua,6)
      print 200,'ierr = ',ierr
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
c
      i_ptr = stkptr(1,25,INT,'i')
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
      c3_ptr = stkptr(1,5,3,'c3')
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
      c4_ptr = stkptr(1,5,4,'c4')
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
      c5_ptr = stkptr(1,5,5,'c5')
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
c
      nsize = 40 + (4+ngd)*BNDWRDS + 1
      ierr  = stkinit(5,nsize,.TRUE.,ng,nsave,lua,6)
      print 200,'ierr = ',ierr
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
c
      ierr  = stkinit(1,0,.TRUE.,ng,nsave,lua,6)
      print 100,'ierr = ',ierr
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
c
      i_ptr = stkptr(1,25,INT,'i')
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
      c3_ptr = stkptr(1,5,3,'c3')
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
c
      f_ptr = stkptr(5,20,FLT,'f')
      ierr = stkdmp(6,'(10z10)',0,ilist)
      do k=0,19
        f(k) = k
      end do
c
      c4_ptr = stkptr(1,5,4,'c4')
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
      c5_ptr = stkptr(1,5,5,'c5')
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
c
      print 100, 'int_array:',(i(k),k=1,25)
c
cc      i_ptr = stkptr(6,25,INT,'i')
cc      print 200,'iptr = ',i_ptr
c
      do k=1,5
        c3(k) = cdum(k:)
        c4(k) = cdum(k:)
        c5(k) = cdum(k:)
      end do
c
      ierr = stkdmp(6,'(10z10)',0,ilist)
c
      ierr = stkchk(0)
      print 200,'stkchk: ',ierr
      ierr = stklist(0,6)
      print 200,'stklist: ',ierr
c
      ierr = stkfptr(1,2)
      print 200,'ierr = ',ierr
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
c
      open(unit=1,file='saveit',status='unknown',form='unformatted')
      ilist(1) = 1
      ilist(2) = 5
      ilist(3) = 3
c
      ierr = stkdmp(1,' ',3,ilist)
      print 200,'ierr = ',ierr
      close(1)
c
      ierr = stkdmp(6,'(10z10)',0,ilist)
      ierr = stkchk(0)
      print 200,'stkchk: ',ierr
      ierr = stklist(0,6)
      print 200,'stklist: ',ierr
c
c
      do k=1,(ngd+2)*BNDWRDS
        isave(k) = i(k+25)
      end do
      do k=1,25+(ngd+2)*BNDWRDS
        i(k) = k
      end do
      ierr = stkchk(0)
      ierr = stkdmp(6,'(10z10)',0,ilist)
c
      do k=(ngd+1)*BNDWRDS+1,(ngd+2)*BNDWRDS
        i(25+k) = isave(k)
      end do
      ierr = stkchk(0)

      do k=BNDWRDS+1,(ngd+1)*BNDWRDS
        i(25+k) = isave(k)
      end do
      ierr = stkchk(0)
c
      ierr = stkrbin(1)
      print 200,'stkrbin error: ',ierr
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
      ierr = stkdmp(6,'(10z10)',0,ilist)
      ierr = stkchk(0)
c
      ierr= stkinit(1,40,.TRUE.,ng,nsave,lua,6)
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
c
      ierr = stkrbin(0)
      print 200,'ierr = ',ierr
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
c
      open(unit=1,file='saveit',status='old',form='unformatted')
      print *,'saveit opened'
c
      k = 1
    3 continue
c
        read(1,end=5,err=6) bin(k),size(k),last(k),nguard(k)
      print 200,'saveit read ',k,size(k),last(k),nguard(k)
        if( size(k).ge.0 ) then
          itmp = -size(k)
          ierr = stkinit(bin(k),itmp,.TRUE.,nguard(k),nsave,lua,6)
          print 200,'ierr = ',ierr
          ierr = stkload(1,' ',bin(k),last(k))
      print 200,'saveit load ',k
          print 200,'ierr = ',ierr
        endif
c
        k = k+1
        goto 3
c
    5 continue
c
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
c
      i_ptr = stkptr(1,25,INT,'i')
      if (i_ptr.ne.0) print 100,'i_array:',(i(k),k=1,25)
      c3_ptr = stkptr(1,5,3,'c3')
      if (c3_ptr.ne.0) print*,(c3(k),k=1,5)
      c4_ptr = stkptr(1,5,4,'c4')
      if (c4_ptr.ne.0) print*,(c4(k),k=1,5)

      f_ptr = stkptr(5,20,FLT,'f')
      if (f_ptr.ne.0) print 110,'f_array:',(f(k),k=0,19)
c
      ierr = stkrclr(0)
      print 200,'ierr = ',ierr
c
      c5_ptr = stkptr(1,5,5,'c5')
      if (c5_ptr.ne.0) then
        do k=1,5
          c5(k) = cdum(k+1:)
        end do
      endif
c
      f_ptr = stkptr(5,20,FLT,'f')
      if (f_ptr.ne.0) then
        do k=0,19
          f(k) = k+0.5
        end do
      endif
c
      ierr = stkdmp(6,'(10z10)',0,ilist)
      ierr = stkchk(0)
      ierr = stklist(0,6)
c
      nsize = 0
      pgrow = stkgrow(1,5000,nsize)
      print *,'grow error: ',pgrow
c
      print*,'before 50000'
      read'(a)',cdum
      ierr  = stkinit(3,50000,.TRUE.,ng,nsave,lua,6)
      nsize = 0
      i_ptr = stkptr(3,nsize,INT,'i')
      print 200,'nsize: ',nsize
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
c
      ierr = stklist(0,6)
c
      do k=1,nsize
        i(k) = k
      enddo
c
      ierr = 0
      nsize2 = 0
      print*,'before 100000'
      read'(a)',cdum
      pgrow = stkgrow(3,100000,nsize2)
      print *,'grow error: ',pgrow
      print 200,'nsize2: ',nsize2
      print *,'i_ptr,pgrow: ',i_ptr,pgrow
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
      if  ( pgrow.gt. 0 ) then
        print *,'i_ptr,pgrow: ',i_ptr,pgrow
        i_ptr = pgrow
        do k=1,nsize
          if ( i(k).ne.k ) ierr = ierr + 1
        enddo
        if ( ierr.ne.0 ) print 200,'Count ERROR: ',ierr
        do k=nsize+1,nsize2
          i(k) = k
        enddo
      endif
c
      ierr = stkchk(0)
      ierr = stklist(0,6)
c
      ierr = stkfptr(3,1)
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
c
      print*,'before 10'
      read'(a)',cdum
      pgrow = stkgrow(3,10,10)
      print *,'grow error: ',pgrow
      call stkqry(5,n,bin,numb,size,last)
      print 100,'qry ',(bin(k),numb(k),size(k),last(k),k=1,n)
      print *,'after 10'
      read'(a)',cdum
c
      ierr = stkchk(0)
      ierr = stklist(0,6)
c
      goto 50
c
    6 continue
        print 200,'error on read',k
   50 continue
c
  100 format(a,/,(8i9))
  110 format(a,/,(1p6e13.5))
  200 format(a,8i8)
c
      end
