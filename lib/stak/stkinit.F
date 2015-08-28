c     ------------------------------------------------------------------
c     ----
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
c> \file stkinit.F
c> \brief File containing integer functions \ref stkinit and \ref stkinit_ST of
c>        the STAK library.
c>
c> \addtogroup PublicInterface Fortran-callable Application (Public) Interface
c>
c> \brief Although not absolutely guaranteed, it is the intent that
c>        the functions in the STAK library's application layer
c>        will not change in terms of their functionality or interface
c>        specification. Any changes in future versions can be
c>        expected be upward compatible with earlier versions.
c>   \{
c>
c ----------------------------------------------------------------------
c
c> \brief This function initializes a dynamic memory bin.
c>
c>  It works in 3 modes:
c>  -# If size=0, it initializes the counters and will process 
c>     calls to the pointer allocation routine (\ref stkptr) to compute the
c>     total space required for the bin. If \ref stkinit is then called again
c>     (w/ size=0), it will dynamically allocate the computed space and
c>     initialize the  counters.  Then the same calls to the pointer
c>     allocation routines can be make to actually allocate the pointers.
c>  -# If size>0, then the dynamic memory bin is allocated w/ "size"
c>     integer words and the counters are initialized.
c>  -# If size<0, then the dynamic memory bin is allocated w/ "size"
c>     integer words and the counters are initialized. However, the bin
c>     is put in "reset" mode, which means that as pointers are subsequently
c>     instantiated, the control link words in the memory bin are not set,
c>     but instead checked for consistency. This mode is designed for
c>     reinstantiating pointers in a memory bin that has been repopulated 
c>     by loading it with data that was previously dumped from that bin to
c>     a file.
c>
c> \param[in] bin       bin to be initialized (1-\b NBINS) (if bin=0, only 
c>                      first-time stack initialization is performed)
c> \param[in] size      size (integer words) to be allocated. If zero, uses
c>                      2-pass system to compute, then allocate, needed 
c>                      space (see 1 above).  If size<0, -size words are 
c>                      allocated and the bin is put in "reset" mode.
c> \param[in] zero      logical flag indicating whether to initialize 
c>                      allocated space to zero
c> \param[in] nguard    # of guard words separating allocations for this bin
c> \param[in] namesave  logical flag indicating that a linked list of names 
c>                      and other info is to be maintained for this bin
c> \param[in] lualloc   If .ge. MINLU, unit # to be used for diagnostic 
c>                      messages concerning allocation and deallocation for
c>                      this bin
c> \param[in] luerr     If .ge. MINLU, unit # to be used for error messages
c>                      (ignored on ALL but first call to this routine)
c>
c> \returns
c>  \li  0, Successful completion
c>  \li  1, Bin out of range
c>  \li  2, Bin has already been allocated
c>  \li  3, Unable to allocate memory
c>  \li  4, error initializing linked list of names
c>
c>  \note
c>   \li The first time this routine is called, some initial stuff is done.
c>   \li The parameter MINLU is defined in stkenum.inc.
c>   \li The number of available bins is set by the parameter \b NBINS (see
c>       stak.inc).
c>   \li The maximum size bin that can be allocated is limited by the maximum
c>       positive value that can be represented in a default Fortran integer,
c>       typically 2 <sup>31</sup> - 1. If your hardware and operating system
c>       support 64-bit addressing, you can allocate larger blocks using the
c>       \ref stkinit_ST function.
c>   \sa stkinit_ST
c>
      integer function stkinit ( bin, size, zero, nguard, namesave,
     &                           lualloc, luerr )
c ----------------------------------------------------------------------
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
c     Passed variables
      integer bin, size, nguard, lualloc, luerr
      logical zero, namesave
c
c     Local variables
      HU_SIZE_T size_st
c
c     Called functions
      integer stkinit_ST

      size_st = size

      stkinit = stkinit_ST( bin, size_st, zero, nguard, namesave,
     &                      lualloc, luerr )

      return
      end
c
c> \brief This function initializes a dynamic memory bin, and is not limited
c>        to 32-bit memory allocations on platforms that support it.
c>
c>  The interface of this function is exactly the same as the \ref stkinit
c>  function, except:
c>    \li The 2nd parameter is of type \b HU_SIZE_t, rather
c>        than type \b integer (\e size) in \ref stkinit.
c>
c>  It works in same 3 modes as described in \ref stkinit.
c>
c> \returns Same return values as \ref stkinit
c>
c>  \note
c>   \li The first time this routine is called, some initial stuff is done.
c>   \li The parameter MINLU is defined in stkenum.inc.
c>   \li The number of available bins is set by the parameter NBINS (see
c>       stak.inc).
c>   \li The maximum size bin that can be allocated is limited by the maximum
c>       positive value that can be represented by the platform's \b HU_SIZE_t
c>       type. If your hardware and operating system does not support 64-bit
c>       addressing, this function is exactly equivalent to the \ref stkinit
c>       function.
c>   \sa stkinit
c>
      integer function stkinit_ST ( bin, size, zero, nguard, namesave,
     &                              lualloc, luerr )
c
c ----------------------------------------------------------------------
c
      implicit none
c
c ----------------------------------------------------------------------
c
c     Passed variables
      integer bin, nguard, lualloc, luerr
      HU_SIZE_t size
      logical zero, namesave
c
c     Common blocks
#include "stak.inc"
#include "stkenum.inc"
c
c     Local variables
      integer i
      HU_SIZE_T tsize
c
      integer first_time
      save    first_time
c
      integer a(0:1)
      pointer ( a_ptr, a )
c
c     Called functions
      HU_PTR_TYPE mdgmem_st
      integer name_init
c
c ----------------------------------------------------------------------
c
      data first_time / 0 /
c
c ----------------------------------------------------------------------
c
c ... Is this the first call to STAK software; need to set cookie, and
c ... initialize counters for all bins
c
      if ( first_time.eq.0 ) then
        first_time = 1
        lustk = luerr
        cookie = MAGIC
        do i=1,NBINS
          ngard(i) = 0
          next(i) = 0
          count(i) = 0
          biggest(i) = NOT_SET
        end do
        typebyts(INTPTR) = NBPI
        typebyts(I2PTR)  = NBPI2
        typebyts(I4PTR)  = NBPI4
        typebyts(LOGPTR) = NBPL
        typebyts(L1PTR)  = NBPL1
        typebyts(FLTPTR) = NBPF
        typebyts(DBLPTR) = NBPD
        typebyts(R4PTR)  = NBPR4
        typebyts(R8PTR)  = NBPR8
        typebyts(PTRPTR) = NBPPTR
        alignbyts(INTPTR) = BA_I
        alignbyts(I2PTR)  = BA_I2
        alignbyts(I4PTR)  = BA_I4
        alignbyts(LOGPTR) = BA_L
        alignbyts(L1PTR)  = BA_L1
        alignbyts(FLTPTR) = BA_F
        alignbyts(DBLPTR) = BA_D
        alignbyts(R4PTR)  = BA_R4
        alignbyts(R8PTR)  = BA_R8
        alignbyts(PTRPTR) = BA_PTR
      endif
c
      stkinit_ST = 0
c
c ... If bin is zero, just return
c
      if ( bin.eq.0 ) return
c
c ... Make sure bin is in legal range
c
      if ( bin.lt.1 .OR. bin.gt.NBINS ) then
        stkinit_ST = 1
        if ( luerr.gt.MINLU ) then
          write(luerr,*)'stkinit:  bin out of range - ',bin
        endif
        return
      endif
c
c ... Has memory for this been already been allocated??
c
      if ( biggest(bin).gt.0 ) then
        stkinit_ST = 2
        if ( lustk.gt.MINLU ) then
          write(lustk,100)'stkinit:  bin has already been allocated - ',
     &                   bin,biggest(bin)
        endif
        return
      else if ( biggest(bin).lt.0 ) then
c ..... set # of guard words for NOT_SET or RESRVED cases
        ngard(bin) = nguard
      endif
c
c ... if size is negative, set "reset" mode and flip sign
c
      if ( size.lt.0 ) then
        tsize = -size
        reset(bin) = .TRUE.
      else
        tsize = size
        reset(bin) = .FALSE.
      endif
c
c ... set biggest to size to allocate, will allocate IF size>0 or 
c ... next counter is > 0.
c
      biggest(bin) = max(next(bin),tsize)
      if ( biggest(bin).gt.0 ) then
c
c ..... get local pointer to required memory, process error in allocate
c
        a_ptr = mdgmem_st(biggest(bin))
        if ( a_ptr.eq.0 ) then
          stkinit_ST = 3
          if ( lustk.gt.MINLU ) then
            write(lustk,100)'stkinit:  unable to allocate memory - ',
     &                    bin,biggest(bin)
          endif
          return
        endif
c
c ..... if zero initialization requested, do it
c
        if ( zero ) then
          do i=0,biggest(bin)-1
            a(i) = 0
          end do
        endif
c
c ..... set pointer for this bin to local pointer to allocated space
c ..... reset counter to next address for bin
c
        call setbptr(bin,a_ptr)
        next(bin) = 0
        count(bin) = 0
c
        luall(bin) = lualloc
        snames(bin) = namesave
c
        if ( namesave ) then
          i = name_init(bin,nguard)
          if ( i.ne.0 ) then
            stkinit_ST = 4
            write(lustk,*)'stkinit:  name_init error -- ',i
            return
          endif
        endif
c
        if ( lualloc.gt.MINLU ) then
          write(lualloc,200) bin, biggest(bin)
          call mdflsh(lualloc)
        endif
      endif
c
      return
c
  100 format(a,/,'  bin: ',i5,/,'  allocated size: ',i10)
  200 format('stkinit:  bin ',i5,' allocated with ',i10,' words')
c
      end
c>  \} END of PublicInterface group
