#ifndef MDFPOS_H
# define MDFPOS_H
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
c     Machine dependent include file containing file addressing type
c     definition
c
c     - used to specify the FORTRAN integer type that is large enough 
c       to hold file offset values (in bytes).
c
c     - used primarily for randomly addressing data in files. 
c
c     - typically file offsets are 32 bits long but 64 bit offsets
c       are becoming more common.
c
c ----------------------------------------------------------------------
c
c
c HU_FPOS_BYTES and HU_FPOS_TYPE must be defined macros that evaluate to the
c correct machine dependent types (e.g., HU_FPOS_BYTES=4 and
c HU_FPOS_TYPE=INTEGER*4)
c
c
# ifndef HU_FPOS_TYPE
#  ifndef HU_FPOS_BYTES
c   if HU_FPOS_BYTES not defined, use default integer
#   define HU_FPOS_TYPE INTEGER*4
#   define HU_FPOS_BYTES 4
#  else
#   define HU_FPOS_TYPE INTEGER*HU_FPOS_BYTES
#  endif
# else
#  ifndef HU_FPOS_BYTES
C many platforms dont have cpp/fpp error messages
C just make the compiler barf...
 error "ERROR: you must define HU_FPOS_BYTES to be the number of bytes required to store file position (in bytes)"
 error "       HU_FPOS_BYTES=4 for platforms with 32-bit file addressing"
 error "       HU_FPOS_BYTES=8 for platforms with 64-bit file addressing"
#  endif

# endif

#endif
