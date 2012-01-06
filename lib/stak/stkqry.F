      subroutine stkqry(kmax,n,bin,num,size,last)
c
c ----------------------------------------------------------------------
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
c ----------------------------------------------------------------------
c
c     This routine queries the stack management counters, providing 
c     information on memory usage.
c
c ----------------------------------------------------------------------
c
c     Passed variables:
c
c     Input:
c       kmax      -  maximum dimension of bin, size, and last arrays
c
c     Output:
c       n         -  number of bins currently in use
c       bin       -  (*) bin numbers of the currently-in-use bins
c       num       -  (*) # of active allocations for in-use bins
c       size      -  (*) allocated size of the currently-in-use bins
c       last      -  (*) current length of the currently-in-use bins
c
c ----------------------------------------------------------------------
c
      implicit none
c
c ----------------------------------------------------------------------
c
c     Machine dependent include file
#include "mdptr.h"
c
c     Passed variables
      integer kmax, n, bin(1:kmax), num(1:kmax)
      HU_SIZE_T last(1:kmax), size(1:kmax)
c
c     Common blocks
#include "stak.inc"
c
c     Local variables
      integer i
c
c ... loop over bins, looking for ones that are in-use
c
      n = 0
      do i=1,NBINS
        if ( biggest(i).ne.NOT_SET ) then
c
c ....... if been is in-use, increment counter and store info if room
c
          n = n + 1
          if ( n.le.kmax) then
            bin(n)  = i
            num(n) = count(i)
            last(n) = next(i)
            size(n) = biggest(i)
          endif
        endif
      end do
c
      return
      end
