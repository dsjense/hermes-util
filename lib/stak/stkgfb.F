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
c> \file stkgfb.F
c> \brief File containing integer function \ref stkgfb of the STAK library.
c>
c> \addtogroup PublicInterface
c>   \{
c
c ----------------------------------------------------------------------
c
c> \brief This function reserves a memory bin that is not currently in use
c>        and returns its number.
c>
c> RESERVED means that a subsequent call to this function will not return the
c> same bin unless it has been subsequently released with a call to function
c> \ref stkrbin.
c>
c> \returns
c>  \li >0, Bin # of a currently unused memory bin that will be reserved
c>  \li  0, No free bins are available
c>  \li -2, Bin/bins has/have not been initialized
c ----------------------------------------------------------------------
c
      integer function stkgfb ( )
c
      implicit none
c
c ----------------------------------------------------------------------
c
c     Passed variables
c
c     Common blocks
#include "stak.inc"
#include "stkenum.inc"
c
c     Local variables
      integer i
c
c     Called functions
c
c ----------------------------------------------------------------------
c
c ... Has cookie been set indicating proper initialization?
c
      if ( cookie.ne.MAGIC ) then
        stkgfb = -2
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkgfb:  stack has not been initialized'
        endif
        return
      endif
c
c ... loop over all bins and return the first one found that is not
c ... in use
c
      do i=1,NBINS
        if ( biggest(i).eq.NOT_SET ) then
          biggest(i) = RESRVED
          stkgfb = i
          return
        endif
      end do
c
      stkgfb = 0
c
      return
      end
c>  \} END of PublicInterface group
