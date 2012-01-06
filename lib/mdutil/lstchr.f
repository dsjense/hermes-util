      integer function lstchr(c)
c
c **********************************************************************
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
c **********************************************************************
c
c     This routine returns the position of the last nonblank character 
c     in the character variable "c"
c
c ----------------------------------------------------------------------
c *************************  ANSI Version  **************************
c ----------------------------------------------------------------------
c
c     Input:
c       c       -  character variable to be converted
c
c     Output:  NONE
c
c     Return Value:
c        0          -  the string is blank
c        otherwise  -  the position of the last nonblank character in 
c                      the string is returned
c
c     Internal:
c       i       -  loop index over characters in string
c
c ----------------------------------------------------------------------
c
      character c*(*)
c
      integer   i
c
c ----------------------------------------------------------------------
c
c ... find maximum # of characters and integer words, check for array 
c ... overflow
c
      do 1 i=len(c),1,-1
        if ( c(i:i).ne.' ' ) then
          lstchr = i
          return
        endif
    1 continue
c
      lstchr = 0
c
      return
      end
