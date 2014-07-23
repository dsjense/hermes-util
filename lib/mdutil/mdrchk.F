      integer function mdrchk(mseeds, init, nseeds)
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
c     Subroutine MDRCHK checks that the dimensioned size of the random
c     number seed array is large enough for the random number algorithm
c     used, and if its dimensioned size matches the the supplied value
c     of `mseeds'. It also returns the size required by the algorithm in
c     `nseeds'. If requested, it will also initialize the seed array
c     with a specified or default starting value.
c
c     Common blocks:  crseed.inc
c
c     Input only:
c       mseeds  -  If > 0, current size of random number seed array
c       init    -  If = 0, seed array not initialized
c                  If < 0, for algorithms using a explicit seed ONLY,
c                          seed array will be initialized  with a
c                          default value
c                  If > 0, seed array will be initialized using the
c                          lowest odd integer >= `init'
c
c     Output only:
c       nseeds  -  (*) (1) Required size of random number seed array
c                      (2) Current dimension of array in this function
c
c     Return Value:
c       0,  No errors encountered
c       1,  The Hermes macro HU_RSEED_SIZE is smaller than the
c           algorithm-required size of the seed array.
c       2,  The supplied value of `mseeds' does not match the Hermes
c           macro HU_RSEED_SIZE (only if `mseeds' is greater than zero)
c       3,  Both error states 1 & 2 exist
c
c
c ----------------------------------------------------------------------
c
      implicit none
c
c ... passed variables:
c
      integer mseeds, init, nseeds(2)
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included common blocks:
c ... crseed
c       r:    NUM_SEEDS
c
#include "crseed.inc"
c
c ... internal variables:
c
      integer i, seed1
      logical always, seedfill
c
      mdrchk = 0
#if defined(HU_RSEED_F90)
      call random_seed(SIZE = nseeds(1))
      if ( nseeds(1).gt.NUM_SEEDS ) mdrchk = 1
#else
      nseeds(1) = NUM_SEEDS
#endif
      nseeds(2) = NUM_SEEDS
      if ( mseeds.gt. 0 .AND. NUM_SEEDS.ne.mseeds ) mdrchk = mdrchk + 2
c
#if defined(HU_RSEED_RAN) || defined(HU_RSEED_RAN_PLUS_EPS)
      always = .TRUE.
#else
      always = .FALSE.
#endif
      if ( mdrchk.eq.0 .AND. init.ne.0 ) then
        seedfill = .TRUE.
        if ( init.gt.0 ) then
          seed1 = 2*(init/2) + 1
        else
          seedfill = always
          seed1 = 13843845
        endif

        if ( seedfill ) then
c ....... initialize seed array with large odd integers
          do i=1,nseeds(1)
            rseed(i)  = seed1 + (i-1)*(i+4)
          end do
c ....... need to set random seed for those algorithms that do not use the
c         common block seed array
          if ( .NOT. always ) call mdrset(rseed)
        endif
      endif
c
      return
      end
