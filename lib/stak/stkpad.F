c ----------------------------------------------------------------------
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
c ----------------------------------------------------------------------
c
c> \file stkpad.F
c> \brief File containing the functions \ref stkpad and \ref stkpad_ST of
c>        the STAK library.
c>
c> \addtogroup PublicInterface
c>   \{
c
c ----------------------------------------------------------------------
c
c> \brief This function performs pointer arithmetic (addition) with 
c>        pointers to integer or real variables.
c>
c> \param[in] ptr    starting pointer
c> \param[in] type   type of array pointed to by starting pointer \e ptr
c>                    \li \b INTPTR (0) for integer arrays,
c>                    \li \b FLTPTR (-1) for real arrays,
c>                    \li \b I2PTR  (-2) for integer*2 arrays,
c>                    \li \b I4PTR  (-3) for integer*4 arrays,
c>                    \li \b LOGPTR (-4) for for logical arrays,
c>                    \li \b L1PTR  (-5) for for logical*1 arrays,
c>                    \li \b DBLPTR (-6) for double precision arrays,
c>                    \li \b R4PTR  (-7) for real*4 arrays,
c>                    \li \b R8PTR  (-8) for real*8 arrays,
c>                    \li \b PTRPTR (-9) for pointer arrays,
c>                    \li length (\e type>0) for character*(\e type) variable
c> \param[in] offset number of array elements of type \e type to advance
c>                   pointer value
c>
c> \returns 
c>     \li 0, illegal value for \e type
c>     \li otherwise, a pointer (type \b HU_PTR_tYPE) to the offset memory
c>
c>  \note
c>   \li The maximum pointer value that can be returned is limited to the
c>       maximum positive value that can be represented in a default Fortran
c>       integer, typically 2 <sup>31</sup> - 1. If your hardware and operating
c>       system support 64-bit addressing, you can obtain larger pointers using 
c>       the \ref stkpad_ST function.
c>   \li This function is useful for the situation where multiple arrays must be
c>       contiguous in memory. A call to \ref stkptr can allocate enough memory
c>       for all the arrays, returning the pointer to the first array. Pointers
c>       to the remaining arrays can be obtained by successive calls to \ref
c>       stkpad.
c>   \sa stkpad_ST
c
      function stkpad ( ptr, type, offset )
c
      implicit none
c
c ----------------------------------------------------------------------
c
c     Machine dependent include file
#include "mdptr.h"
c following defines are related to doxygen
#define HU_PTR_tYPE  HU_PTR_TYPE
#define HU_SIZE_t  HU_SIZE_T
c
c     Return value
      HU_PTR_tYPE stkpad
c
c     Passed variables
      HU_PTR_tYPE ptr
      integer type, offset
c
c     Local variables
      HU_SIZE_T offset_st
c
c     Called functions
      HU_PTR_TYPE stkpad_ST

      offset_st = offset

      stkpad = stkpad_ST( ptr, type, offset_st )
c
      return
      end

c> \brief This function performs pointer arithmetic (addition) with pointers
c>        to integer or real variables, and is not limited to 32-bit
c>        pointers on platforms that support it.
c>
c>  The interface of this function is exactly the same as the \ref stkpad
c>  function, except:
c>    \li The 3rd parameter (\e offset) is of type \b HU_SIZE_t, rather
c>        than type \b integer in \ref stkpad.
c>
c> \returns Same return values as \ref stkpad
c>
c>  \note
c>   \li The maximum pointer value that can be returned is limited to the
c>       maximum positive value that can be represented by the platform's
c>       \b HU_SIZE_t type. If your hardware and operating system does not
c>       support 64-bit addressing, this function is exactly equivalent to the
c>       \ref stkpad function.
c>   \li This function is useful for the situation where multiple arrays must be
c>       contiguous in memory. A call to \ref stkptr_ST can allocate enough
c>       memory for all the arrays, returning the pointer to the first array.
c>       Pointers to the remaining arrays can be obtained by successive calls
c>       to \ref stkpad_ST.
c>   \sa stkpad
      function stkpad_ST ( ptr, type, offset )
c
c     Return value
      HU_PTR_tYPE stkpad_ST
c
c     Passed variables
      HU_PTR_tYPE ptr
      integer type
      HU_SIZE_t offset
c
c     Common blocks
#include "stak.inc"
#include "stkenum.inc"
c
c     Local variables
      logical badtype
c
c ----------------------------------------------------------------------
c
c ... add in the requested offset
c
      badtype = .FALSE.
      if (type.gt.0) then
        if ( NBPC.eq.0 ) then
          badtype = .TRUE.
        else
          stkpad_ST = ptr + type*offset*(NBPC/NBPAU)
        endif
      else if (type.gt.LASTPTR) then
        stkpad_ST = ptr + offset*(typebyts(type)/NBPAU)
      else
        badtype = .TRUE.
      endif
c
      if (badtype)then
        if ( lustk.gt.MINLU ) then
          write(lustk,*) 'stkpad:  invalid pointer type', type
        endif
        stkpad_ST = 0
        return
      endif

      return
      end
c>  \} END of PublicInterface group
