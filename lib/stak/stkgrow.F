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
c> \file stkgrow.F
c> \brief File containing the function \ref stkgrow of the STAK library.
c>
c> \addtogroup PublicInterface
c>   \{
c
c ----------------------------------------------------------------------
c
c> \brief This function grows (or shrinks) a dynamic memory bin.
c>
c> It is only legal to grow a bin that has been allocated, but from which no
c> more than one allocation has been made (<TT>count(bin)</TT> < 2). The bin
c> can be shrunk ONLY IF no allocations have been made (<TT>count(bin)</TT>
c> = 0).
c>
c> \note
c>  \li Note that the count is determined by the number of allocations made
c>      made from the bin via calls to \ref stkptr MINUS the number of
c>      allocations released from the bin via calls to \ref stkfptr.
c>  \li The \c count array referred to here is defined in stak.inc.
c>  \li The maximum size bin that can be allocated is limited by the maximum
c>      positive value that can be represented in a default Fortran integer,
c>      typically 2 <sup>31</sup> - 1. If your hardware and operating system
c>      support 64-bit addressing, you can grow a bin's size larger than this
c>      using the \ref stkgrow_ST function.
c>
c> \param[in] bin
c>      Bin from which to allocate pointer (1-\b NBINS)
c> \param[in] newbsize
c>      New size (integer words) to be reallocated. If less than current size,
c>      no pointers can be currently allocated from the bin (i.e.,
c>      <TT>count(bin)</TT> = 0).
c> \param[in,out] newasize
c>      New size (in words) of 1st array allocated. This is ignored if no arrays
c>      are currently allocated from this bin. If \e newasize is non-positive,
c>      then all remaining space is to be used for this 1st array. In this
c>      case, the size allocated for the array is returned in \e newasize.
c>
c> \returns
c>   \li   0, Successful completion (no current allocations from bin)
c>   \li  -1, Bin out of range
c>   \li  -2, Stack has not been initialized
c>   \li  -3, Bin has not been allocated
c>   \li  -4, Illegal request for current allocations for this bin
c>   \li  -5, Unable to reallocate memory
c>   \li  -6, Bin integrity violated
c>   \li  otherwise, a pointer (type \b HU_PTR_tYPE) to the one allocated array
c>            in the bin (it may have been necessary to move the bin in the
c>            process of growing it).
c>
c> \sa stkptr, stkfptr, stkgrow_ST
c
      function stkgrow ( bin, newbsize, newasize )
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
c
      HU_PTR_tYPE stkgrow
c
c     Passed variables
c
      integer bin, newbsize, newasize
c
c     Local variables
      HU_SIZE_t newbsize_st, newasize_st
c
c     Called functions
      HU_PTR_tYPE stkgrow_ST

      newbsize_st =  newbsize
      newasize_st =  newasize

      stkgrow = stkgrow_ST( bin, newbsize_st, newasize_st )

      if ( newasize.le.0 ) newasize =  newasize_st
c
      return
      end
c
c> \brief This function grows (or shrinks) a dynamic memory bin, and is not
c>        limited to 32-bit memory allocations on platforms that support it.
c>
c>  The interface of this function is exactly the same as the \ref stkgrow
c>  function, except:
c>    \li The 2nd and 3rd parameters are of type \b HU_SIZE_t, rather
c>        than type \b integer (\e size) in \ref stkgrow.
c>
c> \returns Same return values as \ref stkgrow
c>
c> \sa stkptr, stkfptr, stkgrow
c
      function stkgrow_ST ( bin, newbsize, newasize )
c
c     Return value
c
      HU_PTR_tYPE stkgrow_ST
c
c     Passed variables
c
      integer bin
      HU_SIZE_t newbsize, newasize
c
c     Common blocks
c
#include "stak.inc"
#include "stkenum.inc"
c
c     Local variables
c
      integer   ierr, type, clen, bndwrds
      HU_SIZE_T k, off, leng

      parameter ( bndwrds = HU_PTR_BYTES/4 )
c
      integer a(0:1)
      pointer ( a_ptr, a )
c
      integer b(0:1)
      pointer ( b_ptr, b )
c
#if HU_PTR_BYTES != 4
      HU_SIZE_T bnd(0:0)
      pointer ( bnd_ptr, bnd )
#endif
c
c     Called functions
      HU_PTR_TYPE getbptr
      integer stkchk, h2c, lstchr
      HU_PTR_TYPE mdgrowmem_st
      integer get_name, grow_name
c
c ----------------------------------------------------------------------
c
c
c
c ... Make sure bin is in legal range
c
      if ( bin.lt.0 .OR. bin.gt.NBINS ) then
        stkgrow_ST = -1
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkgrow:  bin out of range - ',bin
        endif
        return
      endif
c
c ... Has cookie been set indicating proper initialization?
c
      if ( cookie.ne.MAGIC ) then
        stkgrow_ST = -2
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkgrow:  stack has not been initialized'
        endif
        return
      endif
c
c ... Has memory for this been already been allocated??
c
      if ( biggest(bin).le.0 ) then
        stkgrow_ST = -3
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkgrow:  bin has not been allocated - ',
     &                   bin,biggest(bin)
        endif
        return
      endif
c
c ... if # of current allocations > 1, ERROR
c
      if ( count(bin).gt.1 ) then
        stkgrow_ST = -4
        if ( lustk.gt.MINLU ) then
          write(lustk,100)'stkgrow:  illegal request - count > 1 - ',
     &                   bin,count(bin)
        endif
        return
      endif
c
c ... if # of current allocations > 0 and we're trying to shrink, ERROR
c
      if ( count(bin).gt.0 .AND. (newbsize.lt.biggest(bin) .OR. 
     &                            newasize+2*bndwrds.gt.newbsize) ) then
        stkgrow_ST = -4
        if ( lustk.gt.MINLU ) then
          write(lustk,100)'stkgrow: Illegal resize request  ',
     &                   bin,count(bin)
          write(lustk,110) newasize,newbsize,biggest(bin)
        endif
        return
      endif
c
c ... check bin for integrity before attempting to grow it
c
      ierr = stkchk(bin)
      if ( ierr.ne.0 ) then
        stkgrow_ST = -6
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkgrow:  bin integrity violated'
        endif
        return
      endif
c
c ... get local pointer to required memory, process error in allocate
c
      a_ptr = getbptr(bin)
      b_ptr = mdgrowmem_st(a_ptr,newbsize)
      if ( b_ptr.eq.0 ) then
        stkgrow_ST = -5
        if ( lustk.gt.MINLU ) then
          write(lustk,200)'stkgrow:  unable to reallocate memory - ',
     &                     bin,newbsize,biggest(bin)
        endif
        return
      endif
c
c ... set pointer for this bin to local pointer to allocated space
c ... reset counter to next address for bin
c
      call setbptr(bin,b_ptr)
      biggest(bin) = newbsize
c
      if ( luall(bin).gt.MINLU ) then
        write(luall(bin),300) bin, biggest(bin)
      endif
      if ( count(bin).eq.1 ) then
        if ( newasize.le.0 ) newasize = newbsize - 2*bndwrds
        stkgrow_ST = loc(b(bndwrds))
#if HU_PTR_BYTES == 4
        b(b(0)+1) = 0      
        b(0) = newasize
        b(newasize+bndwrds) = newasize
#else
        bnd_ptr = loc(b(0))
        leng = bnd(0)
        bnd(0) = newasize
        bnd_ptr = loc(b(leng+bndwrds))
        bnd(0) = 0
        bnd_ptr = loc(b(newasize+bndwrds))
        bnd(0) = newasize
#endif
        next(bin) = newasize + 2*bndwrds
 
        if ( snames(bin) ) then
          ierr = grow_name(bin,newasize)
          if ( luall(bin).gt.MINLU ) then
            ierr = get_name( bin, 1, off, leng, type, hname )
            ierr = h2c(hname,HMAX,cname )
            clen = lstchr(cname)
            write(luall(bin),400) cname(1:clen), bin, off, newasize
          endif
        else if ( luall(bin).gt.MINLU ) then
          write(luall(bin),410) count(bin), bin, off, newasize
        endif
      else
        stkgrow_ST = 0
      endif
c
      return
c
  100 format(a,/,'  bin: ',i5,/,'  # of allocations: ',i10)
  110 format('  requested new size: ',i20,/,
     &       '  requested new size for 1st allocation: ',i20,/,
     &       '  current size of bin: ',i20)
  200 format(a,/,'  bin: ',i5,/,
     &       '  requested new size: ',i20,/,
     &       '  current size: ',i20)
  300 format('stkgrow:  bin ',i5,' reallocated with ',i20,' words')
  400 format('stkgrow:  variable ',a,' in bin ',i5,' with offset ',
     &        i10,/,
     &       '          resized to ',i20,' words')
  410 format('stkgrow:  variable ',i5,' in bin ',i5,' with offset ',
     &        i10,/,
     &       '          resized to ',i20,' words')
c
      end
c>  \} END of PublicInterface group
