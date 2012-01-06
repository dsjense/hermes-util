#ifndef MDR8_H
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
c ----------------------------------------------------------------------
c
c     Summary:
c
c       - include file containing md parameters for handling double 
c         precision on 4-byte machines
c
c ----------------------------------------------------------------------
c
c     MDSCL   -  Scaling parameter for the size of a real
c                If real is 4 bytes and real*8 is used, then MDSCL = 2
c                If real is 8 bytes then MDSCL = 1
c
c ----------------------------------------------------------------------
c
      integer MDSCL
c
# ifdef USE_DP
#  if defined(CRAsys)
c ... Crays are word addressable only (w/ 8-byte words only)
c
      parameter ( MDSCL = 1 )
#   define REAL_8  real
#   undef USE_DP
#  else
c ... Byte-addressable machines with default 4-byte reals may need an
c ... offset
c
      parameter ( MDSCL = 2 )
#   define REAL_8  real*8
#  endif
# else
c ... all allocations are default real -- no shift required
c
      parameter ( MDSCL = 1 )
#   define REAL_8  real
# endif
c
#endif
