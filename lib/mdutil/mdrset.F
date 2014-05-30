      subroutine mdrset(seed)
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
c     Subroutine MDRSET sets the current value of the random # seed.
c     This is necessary for machines (such as a CRAY) whose random #
c     function does not give access to the seed.
c
c     On machines whose random # function provides the current seed,
c     make sure that function and this subroutine use
c     the same variable for the seed.  In that case, this routine should
c     be NO-OP.
c
c     Common blocks:  NONE
c
c     Input only:
c       seed   -  (*) current value of random # seed
c
c
c ----------------------------------------------------------------------
c
c ... passed variables:
c
      integer seed(1:*)
c
c ... internal variables:  NONE
c
#if defined(CRAsys)
c
c     !!!!!!!!!!!!!!!!!!!!!! UNICOS VERSION !!!!!!!!!!!!!!!!!!!!!
c
c     This calls the CRAY-specific routine RANSET
c
      call ranset(seed(1))
c
#elif defined(HU_RSEED_F90)
c
c     Use F90 random_seed procedure
c
      integer k
c
      call random_seed(SIZE = k)
      call random_seed(PUT  = seed(1:k))
#else
c
c     !!!!!!!!! All others  !!!!!!!!!
c
c     This call is NO-OP (use RAN function as described above)
c
#endif
c
      return
      end
