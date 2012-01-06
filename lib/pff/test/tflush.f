      program tflush
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
      character file*80, ctmp*1
      integer   a(0:2047)
c
      integer fid, lun, ierr, irec, i, j
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
      do 1 j=1,20480,2048
        do 2 i=0,2047
          a(i) = i + j
    2   continue
        irec = irec + 1
        print*,'Writing buffer ',irec
        call pfmio ( WR, lun, irec, a, ierr )
        if ( ierr.ne.0 ) then
          print*,'mio error = ',ierr,'  irec = ',irec
          stop
        endif
        read '(a)', ctmp
        print*,'Calling PFMFLU'
        call pfmflu(lun,ierr)
        if ( ierr.ne.0 ) then
          print*,'mflu error = ',ierr,'  irec = ',irec
          stop
        endif
        read '(a)', ctmp
        if ( ctmp.eq.'q' ) goto 5
    1 continue
c
    5 continue
c
      call pfmcls( fid, lun, 40960, ierr)
      if ( ierr.ne.0 ) then
        print*,'mcls error = ',ierr
        stop
      endif
c
      stop
      end
