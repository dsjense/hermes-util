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
c> \file stkrclr.F
c> \brief File containing integer function \ref stkrclr of the STAK library.
c>
c> \addtogroup PublicInterface
c>   \{
c>
c ----------------------------------------------------------------------
c
c> \brief This function changes the reset mode of a memory bin (or all bins) to
c>        FALSE.
c>
c> After this call, the pointer allocation function (\ref stkptr) can be used 
c> to extend the memory used from a partially used bin.
c>
c> \param[in] bin  bin for which the reset flag is to be cleared (if \e bin=0,
c>                 all allocated bins will be cleared)
c>
c> \returns
c>  \li  0, Successful completion
c>  \li -1, Bin out of range
c>  \li -2, Bin/bins has/have not been initialized
c>
c> \note
c>   The reset flag is set by \ref stkinit if its supplied \e size argument is
c>   negative. This is designed to be used when restoring data to a memory bin
c>   that was previously written to a "restart" file by \ref stkdmp. The data
c>   is then restored to the bin using \ref stkload. The original pointers are
c>   then reallocated using repeated calls to \ref stkptr. A call to \ref
c>   stkrclr will clear the reset flag so that subsequent calls to \ref stkptr
c>   can extend the use of the bin if its allocated size has not been entirely
c>   consumed. 
c
      integer function stkrclr ( bin )
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
c NOTE errtmp is always zero, so stkrclr remains zero
        stkrclr = 2*stkrclr + errtmp
      end do
c
      return
      end
c>  \} END of PublicInterface group
