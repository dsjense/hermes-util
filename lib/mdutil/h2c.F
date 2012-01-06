      integer function h2c(h,hmax,c)
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
c     This routine converts an integer array with a null-byte terminator 
c     (such as is returned by C as a string pointer) to a FORTRAN 
c     character variable 
c
c     - Gets # of bytes per integer from "nbpi.inc" file
c
c ----------------------------------------------------------------------
c
c     Input:
c       h       -  integer array to be converted
c       hmax    -  # of words dimensioned in "h" integer array
c
c     Output:
c       c       -  character variable into which decoded string is placed
c
c     Return Value:
c        0  -  Normal return
c        1  -  Insufficient space in destination string to store decoded 
c              value, or, reached end of integer array without finding 
c              NULL byte
c
c     Internal:
c       i       -  running index over words in integer array
c       j       -  running index over characters in input string
c       k       -  maximum string length that can be decoded
c       jmax    -  # of of characters in trimmed input string
c       isav    -  (*)  array of bytes extracted from integer word
c       ival    \  temporaries to hold integer word as characters are 
c       inew    /   shifted out of it
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     nbpi    -  NBPI
c
#include "nbpi.inc"
c
c ----------------------------------------------------------------------
c
      character c*(*)
      integer   hmax
      integer   h(hmax)
c
      integer   i,j,k,jmax,ival,inew
      integer   isav(NBPI)
c
c ----------------------------------------------------------------------
c
c ... find maximum # of characters
c
      jmax = min(len(c),NBPI*hmax - 1)
c
      j = 0
      i = 0
      c = ' '
c
c ... loop over words in integer array, stopping when NULL byte is found
c
   10 continue
c
        i = i + 1
c
        ival = h(i)
#ifdef HU_ENDIAN_IS_LSB_FIRST
c
c       1st byte occupies least significant 8 bits of integer word
c
c ..... decode bytes in forward order
c
        do 1 k=1,NBPI
#else
c
c       1st byte occupies most significant 8 bits of integer word
c
c ..... decode bytes in reverse order
c
        do 1 k=NBPI,1,-1
#endif
          inew = ival/256
          isav(k) = ival - 256*inew
          ival = inew
    1   continue
c
c ..... loop over bytes
c
        do 2 k=1,NBPI
c
c ....... if NULL byte found, then return
c
          if(isav(k).eq.0) then
            h2c = 0
            return
          endif
c
c ....... if we've run out of space in destination string or source array, 
c ....... return with error
c
          j = j + 1
          if ( j.gt.jmax ) then
            h2c = 1
            return
          endif
c
c ....... otherwise, store character in destination string
c
          c(j:j) = char(isav(k))
c
    2   continue
c
      goto 10
c
      end
