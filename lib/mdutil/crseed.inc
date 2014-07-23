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
c     Summary:
c
c       Common block containing random # seed.
c       It should be available to all routines calling the random #
c       function, as well as any routine calling MDRGET or MDRSET.
c
c ----------------------------------------------------------------------
c
c     Parameters needed in this module, but not supplied: NONE
c
c ----------------------------------------------------------------------
c
c     Explanation of the variables:
c
c       NUM_SEEDS - (parameter) size of random # seed array
c       rseed     -  (*) current value of random # seed array
c
c ----------------------------------------------------------------------
c
#define MDRANF_H
#ifndef HU_RSEED_SIZE
# define HU_RSEED_SIZE 1
#endif
c
      integer NUM_SEEDS
      parameter ( NUM_SEEDS = HU_RSEED_SIZE )
      
c     Declare variables and define the common block:
c
      integer rseed(1:NUM_SEEDS)
c
      common /crseed/ rseed
c
c***********************************************************************
