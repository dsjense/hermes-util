#ifndef MDPTR_H
# define MDPTR_H
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
c***********************************************************************
c
c ----------------------------------------------------------------------
c
c     Machine dependent include file containing pointer type definition
c
c     - used to specify the FORTRAN integer type that is large enough 
c       to hold Cray compatible pointers (addresses).
c
c     - used primarily for passing/returning pointers to/from subprograms. 
c       Pointers appearing in POINTER statements should not be explicitly 
c       typed.
c
c     - typically addresses are 32 bits long but 64 bit addresses
c       are becoming more common.
c
c ----------------------------------------------------------------------
c
c
c HU_PTR_BYTES and HU_PTR_TYPE must be defined macros that evaluate to the
c correct machine dependent types (e.g., HU_PTR_BYTES=4 and
c HU_PTR_TYPE=INTEGER*4)
c HU_SIZE_T must be an integer type with the same size as the C size_t type
c
c



# ifndef HU_PTR_BYTES
C many platforms dont have cpp/fpp error messages
C just make the compiler barf...
 error "ERROR: you must define HU_PTR_BYTES to be the number of bytes in memory addresses"
 error "       HU_PTR_BYTES=4 for 32-bit address platforms"
 error "       HU_PTR_BYTES=8 for 64-bit address platforms"
# endif

# ifndef HU_PTR_TYPE
#  define HU_PTR_TYPE INTEGER*HU_PTR_BYTES
# endif

# ifndef HU_SIZE_T
#  define HU_SIZE_T HU_PTR_TYPE
# endif
c
#endif
