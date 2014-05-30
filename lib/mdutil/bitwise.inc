CMDEPFUN -- Begin machine-dependent statement function definitions
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
c ...  Bitwise statement functions:
c
      integer comp,iand,ior,ieor,lshift,rshift,extrct,lstore
      integer sfv1, sfv2, sfvval, sfvmsk, sfvshf
c
c ... define statement functions  (to minimize machine-dependency)
c
#if defined(CRAsys)
c
c     **********************  UNICOS Version   ************************
c
c      Use Cray-specific bitwise logical operations
c
c ----------------------------------------------------------------------
c
c
c BINARY COMPLEMENT
      comp(sfv1)     = .not. sfv1
c
c BITWISE AND of sfv1 and sfv2
      iand(sfv1,sfv2)   = and(sfv1,sfv2)
c
c BITWISE OR of sfv1 and sfv2
      ior(sfv1,sfv2)    = or(sfv1,sfv2)
c
c BITWISE EXCLUSIVE OR of sfv1 and sfv2
      ieor(sfv1,sfv2)   = xor(sfv1,sfv2)
c
c LEFT SHIFT of sfv1 by sfv2 bits
      lshift(sfv1,sfv2) = shiftl(sfv1,sfv2)
c
c RIGHT SHIFT of sfv1 by sfv2 bits
      rshift(sfv1,sfv2) = shiftr(sfv1,sfv2)
c
c EXTRACT value from sfvval using sfvmsk & sfvshf
      extrct(sfvval,sfvmsk,sfvshf) = and(sfvmsk,shiftr(sfvval,sfvshf))
c
c STORE sfv2 in low bits of sfv1 with mask sfvmsk
      lstore(sfv1,sfv2,sfvmsk) = or(sfv2,and(sfv1,sfvmsk))
c
#else
c
c ... Use quasi-standard bit intrinsics
c
# if defined(HPsys)
c     Explicitly type intrinsics for HPUX 10.0 compiler
c
      integer  ishft, not
# endif
c
c
c BINARY COMPLEMENT
      comp(sfv1)     = not(sfv1)
c
c  IAND, IOR, IEOR are intrinsic
c  LSHIFT and RSHIFT are sometimes intrinsic
c
# if defined(HU_NEED_LRSHIFT)
c
c  Need to define LSHIFT and RSHIFT in terms of ISHFT
c
c LEFT SHIFT of sfv1 by sfv2 bits
      lshift(sfv1,sfv2) = ishft(sfv1,sfv2)
c
c RIGHT SHIFT of sfv1 by sfv2 bits
      rshift(sfv1,sfv2) = ishft(sfv1,-sfv2)
c
c EXTRACT value from sfvval using sfvmsk & sfvshf
      extrct(sfvval,sfvmsk,sfvshf) = iand(sfvmsk,ishft(sfvval,-sfvshf))
# else

c EXTRACT value from sfvval using sfvmsk & sfvshf
      extrct(sfvval,sfvmsk,sfvshf) = iand(sfvmsk,rshift(sfvval,sfvshf))
# endif
c
c STORE sfv2 in low bits of sfv1 with mask sfvmsk
      lstore(sfv1,sfv2,sfvmsk) = ior(sfv2,iand(sfv1,sfvmsk))
#endif
c
CMDEPFUN -- End machine-dependent statement function definitions
