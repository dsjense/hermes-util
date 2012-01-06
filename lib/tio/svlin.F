      subroutine svlin (
c ... INPUT
     1 lunit, comment,
c ... OUTPUT
     2 ierr )
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
c     SVLIN writes out the internal TIOLIB input line (i.e. unexpanded)
c     to unit LUNIT.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT
c       lunit   -  Unit to write to
c       comment -  Flag to add comment character before line
c
c     OUTPUT:
c       ierr    -  Error flag:-
c                            0  = normal return
c                            -1 = Error on write to output unit
c
      integer       lunit, ierr
      logical       comment
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "ioctrl.inc"
#include "ioinln.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer      tioerh
c
c-----------------------------------------------------------------------
c
c ... Local variables:
c
      character errmsg*132
      integer  i
c
c=======================================================================
c     BEGIN: Write TIOLIB internal input line
c=======================================================================
c
      if(comment) then
        do i=1,nuiln
          write(lunit,'(2a)',err=9000)
     1          cmtchr, uinptl(locuil(i):locuil(i)+lenuil(i)-1)
        enddo
      else
        do i=1,nuiln
          write(lunit,'(a)',err=9000)
     1          uinptl(locuil(i):locuil(i)+lenuil(i)-1)
        enddo
      endif

c
      return
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9000 continue
      write(errmsg,9800) lunit
      ierr = tioerh ('TIO', 'SVLIN', 2, errmsg, -1)
      return
c
 9800 format('Error writing archive lines to unit #',i3)
c
      end
