      subroutine p2dget
     &          ( verzun, date, nfac, bglfac, blkfac, wacfac, confac)
c
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
c       - This routine returns the version and working array size
c         factor parameters to the calling program. 
c
c       - Working space required    =
c                 bglfac*bglap + blkfac*nblk + wacfac*mwachs + confac

c ----------------------------------------------------------------------
c
c     Output:
c       verzun  -  Floating-point version no. (significant only for 2
c                   places to the right of the decimal, i.e., f5.2)
c       date    -  integer-encoded date (10000*month+100*day+year)
c       nfac    -  # of scaling factors passed to compute the size of
c                   work array
c       bglfac  -  factor times "bglap" needed in working array
c       blkfac  -  factor times "blkdim" needed in working array
c       wacfac  -  factor times "mwachs" needed in working array
c       confac  -  constant storage needed in working array
c
c ----------------------------------------------------------------------
c
c   Explanation of the parameters:
c
c     PVERZ   -  P2D version number (F5.2 resolution)
c     PDATE   -  date of P2D version number
c     PNFAC   -  # of factors needed for working space computation
c                (also, should equal the # of parameters remaining on
c                this list)
c     PBGL?   \
c     PBLK?   -  ? = I -- multiplier for integer workspace
c     PWAC?   -  ? = F -- multiplier for float workspace
c     PCON?   /
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c ... include MD parameters
c
#include "mdr8.h"
c
c ... Passed variables:
c
       real verzun
       integer date, nfac, bglfac, blkfac, wacfac, confac
c
c ----------------------------------------------------------------------
c
c   Declare and set the parameters:
c
      integer PDATE, PNFAC, PBGLI, PBGLF, PBLKI, PBLKF, PWACI, PWACF
      integer PCONI, PCONF
      real    PVERZ
c
      parameter ( PVERZ = 2.0 , PDATE = 110497 )
      parameter ( PNFAC = 4)
      parameter ( PBLKI = 5 , PBLKF = 0,  PWACI = 0 , PWACF = 1 )
      parameter ( PCONI = 0 , PCONF = 1 , PBGLI = 0 )
#ifdef USE_DP
      parameter ( PBGLF = 13 )
#else
      parameter ( PBGLF = 10 )
#endif
c
c ----------------------------------------------------------------------
c     Return parameter settings 
c ----------------------------------------------------------------------
c
       verzun = PVERZ
       date = PDATE
       nfac = PNFAC
       bglfac = PBGLF*MDSCL + PBGLI
       blkfac = PBLKF*MDSCL + PBLKI
       wacfac = PWACF*MDSCL + PWACI
       confac = PCONF*MDSCL + PCONI + (MDSCL - 1)
c
       return
       end
