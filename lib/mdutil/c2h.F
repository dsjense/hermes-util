      integer function c2h(c,h,hmax,imax)
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
c     This routine converts a FORTRAN character variable into an integer 
c     array will an null terminator that can be passed to C as a string 
c     pointer
c
c     - Gets # of bytes per integer from "nbpi.inc" file
c
c ----------------------------------------------------------------------
c
c     Input:
c       c       -  character variable to be converted
c       hmax    -  # of words dimensioned in "h" integer array
c
c     Output:
c       h       -  integer array into which converted character is put
c       imax    -  # of word actually used to encode the character string
c
c     Return Value:
c        0  -  Normal return
c        1  -  Insufficient space in "h" array to encode string
c
c     Internal:
c       i       -  loop index over words in integer array
c       j       -  running index over characters in input string
c       k       -  loop index over bytes in integer word
c       jmax    -  # of of characters in trimmed input string
c       iadd    -  integer ASCII value of a character
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
      integer   hmax,imax
      integer   h(hmax)
c
      integer   i,j,k,jmax,iadd
      integer   lstchr
c
c ----------------------------------------------------------------------
c
c ... find maximum # of characters and integer words, check for array 
c ... overflow
c
      jmax = lstchr(c)
      imax = jmax/NBPI + 1
      if ( imax.gt.hmax ) then
        imax = hmax
        c2h = 1
      else
        c2h = 0
      endif
#ifdef HU_ENDIAN_IS_LSB_FIRST
c
c     1st byte occupies least significant 8 bits of integer word
c
c ... loop over all words (bytes are in reverse order)
c
      do 2 i=1,imax
c
c ..... loop over bytes per word, packing & shifting
c
        h(i) = 0
        do 1 j=min(jmax,i*NBPI),(i-1)*NBPI+1,-1
          h(i) = 256*h(i) + ichar(c(j:j))
    1   continue
    2 continue
#else
c
c     1st byte occupies most significant 8 bits of integer word
c
c ... loop over all but last integer word
c
      j = 0
      do 2 i=1,imax-1
        j = j + 1
c
c ..... loop over bytes per word, packing & shifting
c
        h(i) = ichar(c(j:j))
        do 1 k=2,NBPI
          j = j + 1
          h(i) = 256*h(i) + ichar(c(j:j))
    1   continue
    2 continue
c
c ... now do last integer word
c
      h(imax) = 0
c
c .. loop over bytes per word, packing & shifting
c
      do 3 k=1,NBPI
c
c ..... if we've reached end of character string, need to NULL pad to 
c ..... end-of-word boundary
c
        if ( j.eq.jmax ) then
          iadd = 0
        else
          j = j + 1
          iadd = ichar(c(j:j))
        endif
c
        h(imax) = 256*h(imax) + iadd
    3 continue
#endif
c
      return
      end
