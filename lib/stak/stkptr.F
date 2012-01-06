      function stkptr ( bin, size, nc, name )
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
c     This routine allocates pointer to an integer, real, or character 
c     array from a dynamic memory bin.  It works in 3 modes.
c        1)  If stkinit was called with size=0, it DOES NOT allocate a 
c            pointer, BUT instead updates a counter to the end of the
c            memory bin.  Then, after a subsequent call to stkinit, a
c            pointer is allocated.
c        2)  If stkinit was called with size>0 (or after a 2nd call to
c            stkinit with size=0), a pointer is allocated.
c        3)  If stkinit was called with size<0 (indicating "reset" 
c            mode), a pointer is allocated, BUT control link words are 
c            checked for consistency rather than being set.
c
c     Return Value:
c       >0  -  pointer to character, or length of requested space so far
c              if in mode where pointers are NOT allocated (see 1 above)
c        0  -  One of the following errors occurred:
c                  Bin out of range
c                  Bin has not been initialized
c                  Request exceeds allocated memory
c                  control word inconsistency in "reset" mode
c                  illegal value for "nc"
c                  size must be > 0 unless in allocation mode
c                  character allocation not implemented
c
c ----------------------------------------------------------------------
c
c     Passed variables:
c
c     Input:
c       bin       -  bin from which to allocate pointer
c       size      -  size (in words) of array to be allocated. (In 
c                    allocation mode ONLY, size may be non-positive, 
c                    indicating that all remaining space is to be used
c                    for this allocation.  In this case, size is also
c                    used as an output variable, see below).
c       nc        -  INTPTR (0) for integer arrays,
c                    FLTPTR (-1) for real arrays,
c                    I2PTR  (-2) for integer*2 arrays,
c                    I4PTR  (-3) for integer*4 arrays,
c                    LOGPTR (-4) for logical arrays,
c                    L1PTR  (-5) for logical*1 arrays,
c                    DBLPTR (-6) for double precision arrays,
c                    R4PTR  (-7) for real*4 arrays,
c                    R8PTR  (-8) for real*8 arrays,
c                    PTRPTR (-9) for pointer arrays,
c                    length (nc>0) of character variable to be allocated
c       name      -  character string identifier for allocation
c
c     Output:
c       size      -  In allocation mode only, if "size" was input as 
c                    non-positive, it returns the actual size that was
c                    allocated to exhaust the entire bin.
c
c     NOTE: "size" and "nc" are consistent with allocating space for:
c           character*(nc) cvar(1:size)
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
      HU_PTR_TYPE stkptr
c
c     Passed variables
      integer bin, size, nc
      character name*(*)
c
c     Local variables
      HU_SIZE_T size_st
c
c     Called functions
      HU_PTR_TYPE stkptr_ST

      size_st = size

      stkptr = stkptr_ST( bin, size_st, nc, name )

      if ( size.ne.size_s t) size = size_st
c
      return
      end

      function stkptr_ST ( bin, size, nc, name )
c
c ----------------------------------------------------------------------
c
      implicit none
c
c ----------------------------------------------------------------------
c
c     Return value
      HU_PTR_TYPE stkptr_ST
c
c     Passed variables
      integer bin, nc
      HU_SIZE_T size
      character name*(*)
c
c     Common blocks
#include "stak.inc"
#include "stkenum.inc"
c
c     Local variables
      integer ng, iused, ierr, moff, aoff, bndwrds
      HU_SIZE_T j, k, m, ll, lu, tsize, negloc
      integer*8 addr

      parameter ( bndwrds = HU_PTR_BYTES/4 )
c
      integer a(0:1)
      pointer ( a_ptr, a )
c
#if HU_PTR_BYTES != 4
      integer i
      HU_SIZE_T bnd(0:0)
      pointer ( bnd_ptr, bnd )
#endif
c
c     Called functions
      HU_PTR_TYPE getbptr
      integer c2h, lstchr
      integer add_name
c
c ----------------------------------------------------------------------
c
c ... Make sure bin is in legal range
c
      if ( bin.lt.1 .OR. bin.gt.NBINS ) then
        stkptr_ST = 0
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkptr:  bin out of range - ',bin
        endif
        return
      endif
c
c ... Has this bin been initialized?
c
      if ( cookie.ne.MAGIC .OR. biggest(bin).lt.0 ) then
c ..... software not initialized or bin in NOT_SET or RESRVED state
        stkptr_ST = 0
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkptr:  bin has not been initialized - ',bin
        endif
        return
      endif
c
c ... is NC value legal ?
c
      if (nc.lt.LASTPTR) then
        stkptr_ST = 0
        write(lustk,*)'stkptr:  illegal value for "nc" - ',nc
        return
      endif
c
c ... Is character allocation implemented on this platform?
c
      if ( nc.gt.0 .AND. NBPC.eq.0 ) then
        stkptr_ST = 0
        write(lustk,*)'stkptr:  character allocation not implemented'
        return
      endif
c
c ... no guard words before the 1st allocation
c
      if ( next(bin).gt.0 ) then
        ng = (ngard(bin) - 1)/bndwrds + 1
      else
        ng = 0
      endif
c
      moff = 0
      aoff = 0
      a_ptr = 0
c
c ... do we need to worry about byte alignment for this datatype
c
      if ( nc.le.0 .AND. nc.ge.LASTPTR ) then
        if ( alignbyts(nc).gt.0 ) then
c
c ....... if so, compute maximum # of pad words
          moff = alignbyts(nc)/NBPI - 1
c
c ....... if memory is already allocated, compute actual # of pad words
          if ( biggest(bin).gt.0 ) then
c
c ......... set local pointer to point to requested memory bin
c
            a_ptr = getbptr(bin)
            m = next(bin) + (ng+1)*bndwrds
            addr = loc(a(m))
            aoff = mod(moff+1 - mod(addr/NBPI, moff+1), moff+1)
          endif
        endif
      endif
c
      if ( size.gt.0 ) then
c
c ..... compute # of integer words required for requested # of items
c
        if ( nc.eq.INTPTR ) then
          tsize = size
        else if ( nc.gt.0 ) then
          tsize = (nc*size*NBPC - 1)/NBPI + 1
        else if ( nc.ge.LASTPTR ) then
          tsize = (size*typebyts(nc) - 1)/NBPI + 1
        endif
c
      else if ( biggest(bin).gt.0 ) then
c
c ..... compute new size for # of integer words left
c
        tsize = biggest(bin) - next(bin) - (ng+2)*bndwrds
c
        if ( nc.eq.INTPTR ) then
          size = tsize - aoff
        else if ( nc.gt.0 ) then
          size = (tsize - aoff)*NBPI/(nc*NBPC)
        else if ( nc.ge.LASTPTR ) then
          size = (tsize - aoff)*NBPI/typebyts(nc)
        endif
c
      else
        stkptr_ST = 0
        write(lustk,100)
     &         'stkptr:  size must be > 0 unless in allocation mode',
     &         bin, size, biggest(bin)
        return
      endif
c
c ... if memory has not been allocated, increment length counter and 
c ... return total length for this bin so far
c
      if ( biggest(bin).eq.0 ) then
        next(bin) = next(bin) + tsize + (ng+2)*bndwrds + moff
        stkptr_ST = next(bin)
c
c ... if memory has been allocated AND there's enough space left ...
c
      else if ( size.le.0 .OR. 
     &          next(bin)+tsize+(ng+2)*bndwrds+aoff .LE. biggest(bin)
     &        ) then
c
c ..... set local pointer to point to requested memory bin if needed
c
        if ( a_ptr.eq.0 ) a_ptr = getbptr(bin)
c
c ..... store (or check) length of allocation before 1st word and after 
c ..... last word
c
        m = next(bin) + ng*bndwrds
        tsize = tsize + aoff
        j = m + tsize + bndwrds
c
        if ( reset(bin) ) then
c
c ....... if in "reset" mode, just check control words for consistency
c
          ierr = 0
#if HU_PTR_BYTES == 4
          ll = a(m)
          lu = a(j)
#else
          bnd_ptr = loc(a(j))
          lu = bnd(0)
          bnd_ptr = loc(a(m-ng*bndwrds))
          ll = bnd(ng)
          i = 0
#endif
          if ( ll.ne.tsize .OR. lu.ne.tsize ) then
            ierr = 1
          else
            do k=next(bin),m-1,bndwrds
#if HU_PTR_BYTES == 4
              if ( a(k).ne.-k ) ierr = 1
#else
              if ( bnd(i).ne.-k ) ierr = 1
              i = i + 1
#endif
            enddo
          endif
          if ( ierr.eq.1 ) then
            stkptr_ST = 0
            if ( lustk.gt.MINLU ) then
              write(lustk,200) bin,next(bin),tsize,ll,lu
              write(lustk,201) (k,a(next(bin)+k-1),k=1,ng*bndwrds)
            endif
            return
          endif
c
        else
c
c ....... if not in "reset" mode, set control words
c
#if HU_PTR_BYTES == 4
          do k=next(bin),m-1
            a(k) = -k
          enddo
          a(m) = tsize
          a(j) = tsize
#else
          bnd_ptr = loc(a(m-ng*bndwrds))
          i = 0
          do k=next(bin),m-1,bndwrds
            bnd(i) = -k
            i = i + 1
          enddo
          bnd(ng) = tsize
          bnd_ptr = loc(a(j))
          bnd(0) = tsize
#endif
c
        endif
c
c ..... set pointer to location of 1st word of request; update counter
c
        stkptr_ST = loc(a(m+bndwrds+aoff))
        next(bin) = j + bndwrds
        count(bin) = count(bin) + 1
c
        if ( snames(bin) ) then
          ierr = c2h(name,hname,HMAX,iused)
          ierr = add_name(bin,m+bndwrds,tsize,nc,hname)
        endif
        if ( luall(bin).gt.MINLU ) then
          iused = lstchr(name)
          write(luall(bin),300)name(1:iused),bin,m+1,tsize
          call mdflsh(luall(bin))
        endif
c
c ... if there's NOT enough space left ...
c
      else
c
        stkptr_ST = 0
        if ( lustk.gt.MINLU ) then
          write(lustk,400)bin,size,biggest(bin),next(bin) + tsize + 1
        endif
        return
c
      endif
c
      return
c
  100 format(a,/,
     &       '         bin #:                  ',i5,/,
     &       '         requested size:         ',i20,/,
     &       '         current allocated size: ',i20)
  200 format('stkptr:  control word inconsistency - ',/,
     &       '         bin #:                    ',i5,/,
     &       '         addr. of 1st guard word:  ',i20,/,
     &       '         computed size:            ',i10,/,
     &       '         lower/upper control size: ',2i10,/,
     &       '         guard word values:')
  201 format('           ',i5,i20)
  300 format('stkptr: ',a,' allocated in bin ',i5,' offset=',i10,
     &       ' length=',i10)
  400 format('stkptr:  request exceeds allocated memory - ',/,
     &       '         bin #:                   ',i5,/,
     &       '         requested size:          ',i20,/,
     &       '         allocated size:          ',i20,/,
     &       '         size w/ current request: ',i20)
c
      end
