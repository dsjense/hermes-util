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
c **********************************************************************
c
c ----------------------------------------------------------------------
c
c     Summary:
c
c       Common block containing random # seed.
c       It should be available to all routines calling the random #
c       function, as well as any routine calling MDRGET or MDRSET.
c
c ----------------------------------------------------------------------
c
c     Restart info:
c       - Save all variables in this common block
c       - Immediately before writing RESTART file, call mdrget(rseed)
c       - Immediately after reading RESTART file, call mdrset(rseed)
c
c     Parameters needed in this module, but not supplied:
c
c       - NUM_SEEDS (from mdranf.h)
c
c ----------------------------------------------------------------------
c
c     Explanation of the variables:
c
c       rseed   -  (*) current value of random # seed
c
c ----------------------------------------------------------------------
c
c     Declare variables and define the common block:
c
      integer rseed(1:NUM_SEEDS)
c
      common /crseed/ rseed
c
c***********************************************************************
