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
c> \file stkqry.F
c> \brief File containing subroutine \ref stkqry of the STAK library.
c>
c> \addtogroup PublicInterface
c>   \{
c>
c ----------------------------------------------------------------------
c
c> \brief This routine queries the stack management counters, providing 
c>        information on memory usage.
c>
c> \param[in]  kmax  Maximum dimension of bin, size, and last arrays
c> \param[out] n     Number of bins currently in use
c> \param[out] bin   Array of bin numbers of the currently-in-use bins
c> \param[out] num   Array of active allocation counts for in-use bins
c> \param[out] size  Array of allocated sizes of the currently-in-use bins
c> \param[out] last  Array of current lengths of the currently-in-use bins
c>
c>  \note Returned values of \e size and \e last are in units of integer words.
c
      subroutine stkqry(kmax,n,bin,num,size,last)
c
      implicit none
c
c ----------------------------------------------------------------------
c
c     Machine dependent include file
#include "mdptr.h"
c following defines are related to doxygen
#define HU_SIZE_t  HU_SIZE_T
c
c     Passed variables
      integer kmax, n, bin(1:kmax), num(1:kmax)
      HU_SIZE_t last(1:kmax), size(1:kmax)
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
c>  \} END of PublicInterface group
