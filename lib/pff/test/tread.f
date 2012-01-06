      program tread
c
c***********************************************************************
c   $Id$
c   
c   Copyright (2008) Sandia Corporation. Under the terms of
c   Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
c   Government retains certain rights in this software.
c   
c   Hermes is free software: you can redistribute it and/or modify
c   it under the terms of the GNU Lesser General Public License as
c   published by the Free Software Foundation, either version 3 of
c   the License, or (at your option) any later version.
c   
c   Hermes is distributed in the hope that it will be useful, but
c   WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU Lesser General Public License for more details.
c   
c   You should have received a copy of the GNU Lesser General
c   Public License along with Hermes.  If not, see
c   <http://www.gnu.org/licenses/>.
c   
C_Groups main standalone
c***********************************************************************
c
      character file*80
      integer   a(0:2047)
      integer FCOUNT
      parameter (FCOUNT = 1024)
      integer   i4(1:FCOUNT)
      real      x(1:FCOUNT), xtmp(1:FCOUNT)
c
      integer fid, lun, ierr, irec, i, j, irec0, i4strt, ioff, itmp
      integer RE, WR, RW
c
      parameter ( RE = 0 , WR = 1 , RW = 2 )
c
      data  fid,lun / 1,3 /
c
      ierr = 0
c
      print*,'File to read '
      read'(a)',file
c
      call pfmopn( fid, lun, RE, file, ierr )
c
      if ( ierr.ne.0 ) then
        print*,'mopn error = ',ierr
        stop
      endif
c
      irec = 0

c ... start TEST of 4-byte integer MD code
c
      i4strt = 32768
      do j=0,2*(FCOUNT-1),FCOUNT
        irec = irec + 1
        call pfmio ( RE, lun, irec, a, ierr )
        if ( ierr.ne.0 ) then
          print*,'mio error = ',ierr,'  irec = ',irec
          goto 20
        endif
        call pfmi4( RE, FCOUNT, a, i4, ierr )
        if ( ierr.ne.0 ) then
          print*,'mf4 error = ',ierr,'  irec = ',irec
          stop
        endif
        ioff = i4strt - FCOUNT/2
        do i=1,FCOUNT
          itmp = i+ioff
          if ( i4(i).ne.i+ioff ) then
            print'(a,2i8,2x,2z10)', 'ERROR -- ',i, j, itmp, i4(i)
          else if ( i.eq.1 ) then
            print'(2i8,2i15,2z10)',irec,i+j, itmp, i4(i), itmp, i4(i)
          endif
        end do
        i4strt = 2*i4strt
      end do

c ... end TEST of 4-byte integer MD code
c
c ... start TEST of FULL-PRECISION MD code
c
      do j=0,2*(FCOUNT-1),FCOUNT
        irec = irec + 1
        call pfmio ( RE, lun, irec, a, ierr )
        if ( ierr.ne.0 ) then
          print*,'mio error = ',ierr,'  irec = ',irec
          goto 20
        endif
        call pfmf4( RE, FCOUNT, a, x, ierr )
        if ( ierr.ne.0 ) then
          print*,'mf4 error = ',ierr,'  irec = ',irec
          stop
        endif
        do i=1,FCOUNT
          xtmp(i) = 1.0/real(i+j)
        end do
        do i=1,FCOUNT
          if ( xtmp(i).ne.x(i) ) then
            print'(a,2i8,2x,2z10)', 'ERROR -- ',i, i+j, xtmp(i), x(i)
          else if ( i.eq.1 ) then
            print'(2i8,2e15.8,2z10)',irec,j+i,xtmp(i),x(i),xtmp(i),x(i)
          endif
        end do
      end do

c ... end TEST of FULL-PRECISION MD code
c
      irec0 = irec
c
   10 continue
        j = (irec-irec0)*2048 + 1
        irec = irec + 1
        call pfmio ( RE, lun, irec, a, ierr )
        if ( ierr.ne.0 ) then
          print'(a,i5,a,i5)',' mio error = ',ierr,'  irec = ',irec
          goto 20
        endif
        do 2 i=0,2047
          if ( a(i).ne. i + j ) print*,'ERROR -- ',irec,i,j,a(i)
    2   continue
c
        print'(3i8)',irec,j,a(0)
c
        goto 10
c
   20 continue
c
      ierr = 0
c
      call pfmcls( fid, lun, 40960, ierr)
      if ( ierr.ne.0 ) then
        print*,'mcls error = ',ierr
        stop
      endif
c
      stop
      end
