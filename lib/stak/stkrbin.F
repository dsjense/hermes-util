      integer function stkrbin ( bin )
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
c     This routine releases the memory (if allocated) for an entire 
c     dynamic memory bin (or all bins).  After being released by this 
c     routine, a memory bin can be reinitialized with a call to stkinit.
c
c     Return Value:
c        0  -  successful completion
c       >0  -  indicates error in one or more of the releases.  If
c              releasing more than one bin, bits are set for bins with
c              errors, bit 1 corresponding to bin 1, ... (e.g., a return
c              value of 5 indicates an error in releasing bins 1 & 3)
c       -1  -  Bin out of range
c       -2  -  Bin/bins has/have not been initialized
c
c ----------------------------------------------------------------------
c
c     Passed variables:
c
c       bin       -  bin to be released (if bin=0, all allocated bins
c                    will be released)
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
      integer bin
c
c     Common blocks
#include "stak.inc"
#include "stkenum.inc"
c
c     Local variables
      integer i, errtmp, ierr, frst, last
c
      integer a(0:1)
      pointer ( a_ptr, a )
c
c     Called functions
      HU_PTR_TYPE getbptr
      integer mdrmem
      integer clear_name
c
c ----------------------------------------------------------------------
c
c ... Make sure bin is in legal range
c
      if ( bin.lt.0 .OR. bin.gt.NBINS ) then
        stkrbin = -1
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkrbin:  bin out of range - ',bin
        endif
        return
      endif
c
c ... Has cookie been set indicating proper initialization?
c
      if ( cookie.ne.MAGIC ) then
        stkrbin = -2
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkrbin:  stack has not been initialized'
        endif
        return
      endif
c
c ... bin=0 is special case indicating release of all bins
c
      if ( bin.eq.0 )then
        frst = 1
        last = NBINS
      else
        frst = bin
        last = bin
      endif
c
c ... initialize error counter and loop over all bins to be released
c ... loop backwards so that error bits are ordered sensibly
c
      stkrbin = 0
      do i=last,frst,-1
        errtmp = 0
c
c ..... only need to free memory if it was in fact allocated
c
        if ( biggest(i).gt.0 ) then
c
c ..... set local pointer to point to requested memory bin, and release
c
          a_ptr = getbptr(i)
          ierr = mdrmem(a_ptr)
          if ( ierr.ne.0 ) then
            errtmp = 1
            if ( lustk.gt.MINLU ) then
              write(lustk,100) i, ierr
            endif
          endif
          if ( snames(i) ) then
            ierr = clear_name(i)
          endif
        endif
c
        if ( biggest(i).ne.NOT_SET .AND. luall(i).gt.MINLU ) then
          write(luall(i),200) i
          call mdflsh(luall(i))
        endif
c
c ..... set counters for bin to initial "NOT_SET" values
c
        next(i) = 0
        count(i) = 0
        biggest(i) = NOT_SET
c
c ..... set error bit for this bin
c
        stkrbin = 2*stkrbin + errtmp
      end do
c
      return
c
  100 format('stkrbin:  error releasing memory - ',/,
     &       '  bin: ',i5,/,
     &       '  mdrmem error: ',i10)
  200 format('stkrbin:  bin ',i5,' released')
c
      end
