      subroutine ljusti (
c ... INPUT
     & int, width,
c ... OUTPUT
     & string, nch, ierr )
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
c     LJUSTI returns a character string corresponding to integer INT,
c     in one of two modes:-
c
c       1. WIDTH .LE. 0: String is simply left justified
c
c       2. WIDTH .GT. 0: String is returned  with a field width of WIDTH
c                        with leading zeroes as required.  If integer
c                        has a field width > WIDTH, an error occurs, and
c                        LJUSTI returns a value 'ERROR'
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
      character*(*) string
      integer       ierr, int, nch, width
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character*10 temp
      integer      i, j, lstzro, lwidth
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer lennb
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      ierr = 0
      write(temp,'(i10)') int
c
c ... Locate first non-blank character
c
      do i=1,10
        if (temp(i:i) .NE. ' ') go to 10
      enddo
c
   10 continue
c
      if (width .LE. 0) then
        string = temp(i:)
      else
        lwidth = 11 - i
        if (lwidth .GT. width) then
          ierr = -1
          string = ' '
        else
          lstzro = width - lwidth
          do j=1,lstzro
            string(j:j) = '0'
          enddo
          string(lstzro+1:) = temp(i:)
        endif
      endif
c
      nch = lennb(string)
      return
c
      end
