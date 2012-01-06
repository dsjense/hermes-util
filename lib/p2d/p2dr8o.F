#include "mdptr.h"
      integer function p2dr8o(word)
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
      implicit none
c
c ----------------------------------------------------------------------
c
c     Summary:
c
c       - This function returns a word shift to insure byte alignment
c         when allocating a real*8 array from a real*4 array
c
c ----------------------------------------------------------------------
c
c     Input:
c       word    -  FWA of location to be allocated
c
c     Return Value:  offset to "word" that will be properly aligned
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c ... Passed variables:
c
      integer word
c
#ifdef USE_DP
# if defined(CRAsys)
c ... Crays are word addressable only (w/ 8-byte words only)
c
      p2dr8o = 0
# else
c
c ... Local variables:
c
      HU_PTR_TYPE addr
c
c ... Byte-addressable machines with default 4-byte reals may need an
c ... offset
c
      addr = loc(word)
      if ( (addr/8)*8 .ne. addr ) then
        p2dr8o = 1
      else
        p2dr8o = 0
      endif
# endif
#else
c ... all allocations are default real -- no shift required
c
      p2dr8o = 0
#endif
c
      return
      end
