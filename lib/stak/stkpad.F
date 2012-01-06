      function stkpad ( ptr, type, offset )
c
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
c     This routine performs pointer arithmetic (addition) with 
c     pointers to integer or real variables
c
c     Return Value:
c       >0  -  result of pointer arithmetic
c        0  -  illegal value for "type"
c
c ----------------------------------------------------------------------
c
c     Passed variables:
c
c     Input:
c       ptr       -  starting pointer
c       type      -  INTPTR (0) for integer variables
c                    FLTPTR (-1) for real variables
c                    I2PTR  (-2) for integer*2 arrays,
c                    I4PTR  (-3) for integer*4 arrays,
c                    LOGPTR (-4) for logical arrays,
c                    L1PTR  (-5) for logical*1 arrays,
c                    DBLPTR (-6) for double precision arrays,
c                    R4PTR  (-7) for real*4 arrays,
c                    R8PTR  (-8) for real*8 arrays,
c                    PTRPTR (-9) for pointer arrays,
c                    length (nc>0) of character variable to be allocated
c       offset    -  number of array elements to advance pointer value
c
c ----------------------------------------------------------------------
c
      implicit none
c
c ----------------------------------------------------------------------
c
c     Machine dependent include file
#include "mdptr.h"
c
c     Return value
      HU_PTR_TYPE stkpad
c
c     Passed variables
      HU_PTR_TYPE ptr
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

      function stkpad_ST ( ptr, type, offset )
c
c     Return value
      HU_PTR_TYPE stkpad_ST
c
c     Passed variables
      HU_PTR_TYPE ptr
      integer type
      HU_SIZE_T offset
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
