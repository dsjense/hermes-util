      character*(*) function iogioc (
c ... INPUT
     1 line )
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
c     IOGIOC is a TIO utility routine that returns the I/O command if
c     the input line is an I/O command, or a blank string if not.
c     Any number of blanks can come before or after the I/O command
c     character.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments
c
c     INPUT
c       line -  Input line to be tested
c
      character*(*) line
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "ioctrl.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer      lennb
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERs and variables
c
      integer i, j, k, ki, kf, len
c
c
c=======================================================================
c=======================================================================
c
      len = lennb(line)
c
c ... Find first non-blank character
c
      do 1 i=1,len
        if (line(i:i) .NE. ' ') then
          if (line(i:i) .EQ. cmdchr(1:1)) then
            go to 10
          else
            go to 2
          endif
        endif
    1 continue
c
c ... This is not an I/O command line, return blank string
c
    2 continue
      iogioc = ' '
      return
c
c ... This is an I/O command line, locate command
c
   10 continue
      do 11 j=i+1,len
        if (line(j:j) .NE. ' ') then
          ki = j
          go to 20
        endif
   11 continue
c
      iogioc = ' '
      return
c
   20 continue
      do 21 k=ki,len
        if (index(delim(1:ndelim), line(k:k)) .GT. 0) then
          kf = k
          go to 30
        endif
   21 continue
      kf = len
c
   30 continue
      iogioc = line(ki:kf)
c
      return
      end
