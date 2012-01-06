      integer function stkfptr ( bin, nptrs )
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
c     This routine releases the memory (if allocated) for the last "nptr"
c     pointers returned by the pointer allocation routine (stkptr).
c     NOTE:  Pointers in the same bin are released in the OPPOSITE 
c            order in which they were allocated.
c
c     Return Value:
c        0  -  successful completion
c        1  -  Bin out of range
c        2  -  stack has not been initialized
c        3  -  Pointer could not be freed
c
c ----------------------------------------------------------------------
c
c     Passed variables:
c
c       bin       -  bin for which pointers will be released
c       nptrs     -  # of pointers to be released from bin
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
c     Passed variables
      integer bin, nptrs
c
c     Common blocks
#include "stak.inc"
#include "stkenum.inc"
c
c     Local variables
c
      integer i, k, type, ierr, ng, bndwrds
      HU_SIZE_T j, m, jl, ll, lh, off, leng, negloc
c
      parameter ( bndwrds = HU_PTR_BYTES/4 )
c
      integer a(0:1)
      pointer ( a_ptr, a )
c
#if HU_PTR_BYTES != 4
      HU_SIZE_T bnd(0:0)
      pointer ( bnd_ptr, bnd )
#endif
c
c     Called functions
      HU_PTR_TYPE getbptr
      integer h2c, lstchr
      integer get_name, remove_name
c
c ----------------------------------------------------------------------
c
c ... Make sure bin is in legal range
c
      if ( bin.lt.0 .OR. bin.gt.NBINS ) then
        stkfptr = 1
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkfptr:  bin out of range - ',bin
        endif
        return
      endif
c
c ... Has cookie been set indicating proper initialization?
c
      if ( cookie.ne.MAGIC ) then
        stkfptr = 2
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkfptr:  stack has not been initialized'
        endif
        return
      endif
c
c ... do only for allocated bins
c
      stkfptr = 0
      if ( biggest(bin).gt.0 ) then
c
        if ( nptrs.gt.count(bin) ) then
          stkfptr = count(bin) - nptrs
          if ( lustk.gt.MINLU ) then
            write(lustk,100) bin, nptrs, count(bin)
          endif
        endif
c
c ..... set local pointer to point to requested memory bin
c
        a_ptr = getbptr(bin)
c
c ..... loop over pointers to be taken from stack

        ng = (ngard(bin) - 1)/bndwrds + 1
c
        do i=1,min(nptrs,count(bin))
c
c ....... go backwards to location of 1st control word for last 
c ....... allocation for this bin
c
          j = next(bin) - bndwrds
#if HU_PTR_BYTES == 4
          lh = a(j)
#else
          bnd_ptr = loc(a(j))
          lh = bnd(0)
#endif
          j = j - lh - bndwrds
          jl = max(j,0)
c
c ....... do control words match ?
c
c
c ....... if length or leading control word's address is negative, 
c ....... links are corrupted
c
          if ( lh.lt.0 .OR. j.lt.0 ) then
            stkfptr = 4
            ll = -1
c
c ....... if lengths in control words don't match, links are 
c ....... corrupted
c
          else
#if HU_PTR_BYTES == 4
            ll = a(j)
#else
            bnd_ptr = loc(a(j))
            ll = bnd(0)
#endif
            if ( ll.ne.lh ) then
              stkfptr = 4
c
c ......... if not the 1st allocation, check guard words
c
            else if ( j.gt.0 ) then
              do k=1,ng
                j = j - bndwrds
                if ( j.lt.0 ) then
                  stkfptr = 4
                else
#if HU_PTR_BYTES == 4
                  negloc = a(j)
#else
                  bnd_ptr = loc(a(j))
                  negloc = bnd(0)
#endif
                  if (negloc.ne.-j ) stkfptr = 4
                endif
              end do        
            else if ( j.ne.0 ) then
              stkfptr = 4
            endif
          endif
c
          if ( stkfptr.gt.0 ) then
            if ( lustk.gt.MINLU ) then
              write(lustk,200) bin,i,count(bin)
              write(lustk,220) ll,lh
              m = max(0,j)
              if (j.lt.jl) write(lustk,230) (k,a(m+k-1),k=1,ng*bndwrds)
             endif
            return
          endif
c
          if ( snames(bin) ) then
            ierr = get_name( bin, L_LAST, off, leng, type, hname )
            if ( ierr.ne.0 ) print*,'stkfptr: getname error ',ierr,bin
            ierr = remove_name(bin)
            if ( ierr.ne.0 ) print*,'stkfptr: removename error ',ierr
            if ( luall(bin).gt.MINLU ) then
              ierr = h2c(hname,HMAX,cname )
              leng = lstchr(cname)
              write(luall(bin),300) cname(1:leng), bin
            endif
          else if ( luall(bin).gt.MINLU ) then
            write(luall(bin),310) count(bin), bin
          endif
c
          next(bin)  = j
          count(bin) = count(bin) - 1
c
        end do
c
      else
        stkfptr = 3
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkfptr:  bin not allocated',bin
        endif
      endif
c
      return
c
  100 format('stkfptr:  request exceeds allocations',/,
     &       '          bin #:                     ',i5,/,
     &       '          # pointers to be released: ',i10,/,
     &       '          # pointers available:      ',i10)
  200 format('stkfptr:  stack corrupted for bin ',i5,/,
     &       '          release pointer #:      ',i5,/,
     &       '          variable # = ',i20)
  220 format('          control words:',/,
     &       '            low value:  ',i20,/,
     &       '            high value: ',i20)
  230 format('          guard cell values:',/,
     &      ('              ',i5,i20))
  300 format('stkfptr:  variable ',a,' released from bin ',i5)
  310 format('stkfptr:  variable ',i5,' released from bin ',i5)
c
      end
