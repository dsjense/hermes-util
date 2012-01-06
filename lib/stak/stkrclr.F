      integer function stkrclr ( bin )
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
c     This routine changes the reset mode of a file to FALSE.  After 
c     this call, the pointer allocation function (stkptr) can be used 
c     to extend the memory used from a partially used bin.
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
c       bin      -  bin for which reset flag is to be cleared (if bin=0, 
c                   flags for all allocated bins will be cleared)
c
c ----------------------------------------------------------------------
c
      implicit none
c
c ----------------------------------------------------------------------
c
c     Passed variables
      integer bin
c
c     Common blocks
#include "stak.inc"
#include "stkenum.inc"
c
c     Local variables
      integer i, errtmp, frst, last
c
c ----------------------------------------------------------------------
c
c ... Make sure bin is in legal range
c
      if ( bin.lt.0 .OR. bin.gt.NBINS ) then
        stkrclr = -1
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkrclr:  bin out of range - ',bin
        endif
        return
      endif
c
c ... Has cookie been set indicating proper initialization?
c
      if ( cookie.ne.MAGIC ) then
        stkrclr = -2
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkrclr:  stack has not been initialized'
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
      stkrclr = 0
      do i=last,frst,-1
        errtmp = 0
c
c ..... only need reset flag if memory was in fact allocated
c
        if ( biggest(i).gt.0 ) reset(i) = .FALSE.
c
        stkrclr = 2*stkrclr + errtmp
      end do
c
      return
      end
