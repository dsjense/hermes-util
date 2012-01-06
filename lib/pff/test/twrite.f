      program twrite
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
C_Groups main standalone
c***********************************************************************
c
      character file*80
      integer   a(0:2047)
      integer FCOUNT
      parameter (FCOUNT = 1024)
      integer   i4(1:FCOUNT)
      real      x(1:FCOUNT)
c
      integer fid, lun, ierr, irec, i, j, i4strt, ioff
      integer RE, WR, RW
c
      parameter ( RE = 0 , WR = 1 , RW = 2 )
c
      data  fid,lun / 1,3 /
c
      ierr = 0
c
      print*,'File to write '
      read'(a)',file
c
      call pfmopn( fid, lun, WR, file, ierr )
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
        ioff = i4strt - FCOUNT/2
        do i=1,FCOUNT
          i4(i) = ioff + i
        end do
        call pfmi4( WR, FCOUNT, a, i4, ierr )
        if ( ierr.ne.0 ) then
          print*,'mi4 error = ',ierr,'  irec = ',irec
          stop
        endif
        call pfmio ( WR, lun, irec, a, ierr )
        if ( ierr.ne.0 ) then
          print*,'mio error = ',ierr,'  irec = ',irec
          stop
        endif
        i4strt = 2*i4strt
      end do

c ... end TEST of 4-byte integer MD code

c ... start TEST of FULL-PRECISION MD code
c
      do j=0,2*(FCOUNT-1),FCOUNT
        irec = irec + 1
        do i=1,FCOUNT
          x(i) = 1.0/real(i+j)
        end do
        call pfmf4( WR, FCOUNT, a, x, ierr )
        if ( ierr.ne.0 ) then
          print*,'mf4 error = ',ierr,'  irec = ',irec
          stop
        endif
        call pfmio ( WR, lun, irec, a, ierr )
        if ( ierr.ne.0 ) then
          print*,'mio error = ',ierr,'  irec = ',irec
          stop
        endif
      end do

c ... end TEST of FULL-PRECISION MD code
c
      do 1 j=1,20480,2048
        do 2 i=0,2047
          a(i) = i + j
    2   continue
        irec = irec + 1
        call pfmio ( WR, lun, irec, a, ierr )
        if ( ierr.ne.0 ) then
          print*,'mio error = ',ierr,'  irec = ',irec
          stop
        endif
    1 continue
c
      call pfmcls( fid, lun, 40960+8*FCOUNT, ierr)
      if ( ierr.ne.0 ) then
        print*,'mcls error = ',ierr
        stop
      endif
c
      stop
      end
