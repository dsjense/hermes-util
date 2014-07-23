      subroutine mdrget(seed)
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
c     Subroutine MDRGET returns the current value of the random # seed.
c
c     Output only:
c       seed   -  (*) current value of random # seed
c
c
c ----------------------------------------------------------------------
c
c ... passed variables:
c
      integer seed(1:*)
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included common blocks:
c ... crseed  (non-Cray, non-F90)
c       r:    NUM_SEEDS
c       r:    rseed(1:NUM_SEEDS)
c
#if defined(CRAsys)
c
c     !!!!!!!!!!!!!!!!!!!!!! UNICOS VERSION !!!!!!!!!!!!!!!!!!!!!
c
c     This calls the CRAY-specific routine RANGET
c
      call ranget(seed(1))
#elif defined(HU_RSEED_F90)
c
c     Use F90 random_seed procedure
c
      integer k
c
      call random_seed(SIZE = k)
      call random_seed(GET  = seed(1:k))
#else
c     !!!!!!!!! All others  !!!!!!!!!
c
# include "crseed.inc"

      integer i
c
c     seed is stored in rseed array in common block crseed!
c
      do i=1,NUM_SEEDS
        seed(i) = rseed(i)
      end do
#endif
c
      return
      end
