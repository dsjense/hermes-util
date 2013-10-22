      program tdouble
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
      integer   i, ierr, ival(5), j
      double precision dpi, dval, dval2
      character*32 efmt, ffmt, ifmt
c        
      dpi  = 4.0D0*atan(1.0D0)
      efmt = '(i1,a)'
      ffmt = '(i1,1pe21.12,/,1x,e21.12,e14.3)'
      ifmt = '(i1,5i7)'
c
      do i=0,2
        ierr = 0
        dval = dpi**((2*i+1)**(i+1))
c
        call pfud2i(dval,ival,ierr)
        if (ierr .EQ. 0) then
          print ifmt, i, (ival(j),j=1,5)
          call pfui2d(ival,dval2,ierr)
          if (ierr .EQ. 0) then
            print ffmt, i, dval,dval2,1.0D0-dval2/dval
          else
            print efmt, i,ierr,' pfui2d error'
          endif
        else
          print efmt, i,ierr,' pfud2i error'
        endif
c
        dval = -1.0D0/dval
        call pfud2i(dval,ival,ierr)
        if (ierr .EQ. 0) then
          call pfui2d(ival,dval2,ierr)
          print ifmt, i, (ival(j),j=1,5)
          if (ierr .EQ. 0) then
            print ffmt, i, dval,dval2,1.0D0-dval2/dval
          else
            print efmt, i,ierr,' pfui2d error'
          endif
        else
          print efmt, i,ierr,' pfud2i error'
        endif
      enddo
c
      stop
      end
