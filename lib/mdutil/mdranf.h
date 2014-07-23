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
c ... Machine-dependent random # function
c
c ... NOTE:  For machines whose random # function provides access to the 
c            seed, the seed is provided by the integer RSEED in common 
c            block CRSEED
c
#ifndef MDRANF_H
      
# if defined(CRAsys)
#  define  RANF(A)     A = ranf()
# endif

# if defined(HU_RSEED_F90)
c     Use F90 random number generator
#  define  RANF(A)     call random_number(A)
# endif

# if defined(HU_RSEED_RAN)
#  define  RANF(A)     A = ran(rseed(1))
#  if defined(IRIsys)
      real ran
#  endif
# endif

# if defined(HU_RSEED_RAN_PLUS_EPS)
c     Add a small number (less than the machine dependent precision
c     of ~6e-8 for IEEE 32-bit floats) to the value returned by ran()
c     to avoid returned values that are precisely zero.
c
#  define  RANF(A)     A = ran(rseed(1)) + 1.0e-10
# endif

#endif
c
