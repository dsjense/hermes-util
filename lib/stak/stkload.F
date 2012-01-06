      integer function stkload(ludmp, frmt, bin, lbin)
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
c     This routine loads the contents of a specified set of bins from
c     a file written by stkdmp.  This routine ONLY reads the data 
c     written by the second write statement in stkdmp, i.e., it only 
c     reads the data from the bin.  The calling program must have 
c     previously read the 1st record containing the bin # and length.
c
c     Return values:
c        0  -  Successful completion
c       -1  -  Bin out of range
c       -2  -  Bin/bins has/have not been initialized
c       -3  -  Insufficient storage in bin
c
c ----------------------------------------------------------------------
c
c     Passed variables:
c
c     Input:
c       ludmp     -  Logical unit of an open file to read info from
c       frmt      -  character variable containing format to read data
c                    from file (e.g., '(10z10)', OR, if frmt is blank (' ')
c                    data is written unformatted.
c       bin       -  number of bin to be loaded
c       lbin      -  # of words of data to be read into the bin
c
c     Output:  NONE
c
c ----------------------------------------------------------------------
c
      implicit none
c
c ----------------------------------------------------------------------
c
#include "mdptr.h"
c
c     Passed variables
      integer   ludmp, bin
      HU_SIZE_T lbin
      character frmt*(*)
c     Common blocks
#include "stak.inc"
#include "stkenum.inc"
c
c     Local variables
      HU_SIZE_T j
c
cc      integer a_ptr, a(0:1)
      integer a(0:1)
      pointer ( a_ptr, a )
c
c     Called functions
      HU_PTR_TYPE getbptr
c
c ----------------------------------------------------------------------
c
c ... Make sure bin is in legal range
c
      if ( bin.lt.1 .OR. bin.gt.NBINS ) then
        stkload = -1
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkload:  bin out of range - ',bin
        endif
        return
      endif
c
c ... Has cookie been set indicating proper initialization?
c
      if ( cookie.ne.MAGIC ) then
        stkload = -2
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkload:  stack has not been initialized'
        endif
        return
      endif
c
c ... is there enough allocated memory in the bin?
c
      if ( lbin.gt.biggest(bin) ) then
        stkload = -3
        if ( lustk.gt.MINLU ) then
          write(lustk,100) 'stkload:  insufficient storage in bin - ',
     &                   bin,lbin,biggest(bin)
        endif
        return
      endif
c
c ... set local pointer to point to requested memory bin
c
      a_ptr = getbptr(bin)
c
      if ( frmt.eq.' ' ) then
        if( lbin.gt.0 ) read(ludmp) (a(j),j=0,lbin-1)
      else
        if( lbin.gt.0 ) read(ludmp,frmt) (a(j),j=0,lbin-1)
      endif
c
      stkload = 0
c
      return
  100 format(a,/,'  bin: ',i5,/,
     &           '  # of data words supplied: ',i10,/,
     &           '  # of words available: ',i10)
      end
