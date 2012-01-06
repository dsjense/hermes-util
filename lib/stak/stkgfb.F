      integer function stkgfb ( )
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
c     This function reserves and returns the number of a memory bin that 
c     is not currently in use.  RESERVED means that a subsequent call to
c     this function will not return this bin unless it has been released 
c     with a call to function stkrbin.
c
c     Return Value:
c       >0  -  bin number of a memory bin that is not currently in use
c        0  -  no free bins are available
c       -2  -  Bin/bins has/have not been initialized
c
c ----------------------------------------------------------------------
c
c     Passed variables: NONE
c
c ----------------------------------------------------------------------
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
