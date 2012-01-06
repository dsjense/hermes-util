      integer function stkdmp(ludmp, frmt, n, bin)
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
c     This routine dumps the contents of a specified set of bins to
c     a file, in either formatted or unformatted form.
c
c     Return values:
c        0  -  Successful completion
c       -1  -  Bin out of range
c       -2  -  Stack has not been initialized
c
c ----------------------------------------------------------------------
c
c     Passed variables:
c
c     Input:
c       ludmp     -  Logical unit of an open file to write info to
c       frmt      -  character variable containing format to write data
c                    to file (e.g., '(10z10)', OR, if frmt is blank (' ')
c                    data is written unformatted.
c       n         -  number of bins to be dumped (if 0, all in-use bins
c                    are dumped)
c       bin       -  (*) array of bin #'s whose contents are to be dumped
c
c     Output:  NONE
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
      integer   ludmp, n, bin(*)
      character frmt*(*)
c
c     Common blocks
#include "stak.inc"
#include "stkenum.inc"
c
c     Local variables
      integer i, ibin, nmax
      HU_SIZE_T j
c
      integer a(0:1)
      pointer ( a_ptr, a )
c
c     Called functions
      HU_PTR_TYPE getbptr
c
c ----------------------------------------------------------------------
c
c ... Has cookie been set indicating proper initialization?
c
      if ( cookie.ne.MAGIC ) then
        stkdmp = -2
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkdmp:  stack has not been initialized'
        endif
        return
      endif
c
      if ( n.eq.0 ) then
        nmax = NBINS
      else
        nmax = n
      endif
c
c ... loop over bins
c
      do i=1,nmax
c
c ..... skip bins that are not in use (unless explicitly requested)
c
        if ( n.gt.0 .OR. biggest(i).ne.NOT_SET ) then
c
          if ( n.gt.0 ) then
c
c ......... Make sure bin is in legal range
c
            if ( bin(i).lt.1 .OR. bin(i).gt.NBINS ) then
              stkdmp = -1
              if ( lustk.gt.MINLU ) then
                write(lustk,*)'stkdmp:  bin out of range - ',i,bin(i)
              endif
              return
            endif
            ibin = bin(i)
          else
            ibin = i
          endif
c
c ....... set local pointer to point to requested memory bin
c
          a_ptr = getbptr(ibin)
c
c ....... for unformatted, write bin #, length, and used portion of bin 
c ....... in a pair of write statements
c
          if ( frmt.eq.' ' ) then
            write(ludmp) ibin,biggest(ibin),next(ibin),ngard(ibin)
            if( next(ibin).gt.0 )
     &          write(ludmp) (a(j),j=0,next(ibin)-1)
c
c ....... for formatted, write bin # and length separately and write 
c ....... used portion of bin using supplied format
c
          else
            write(ludmp,100) ibin,biggest(ibin),next(ibin),ngard(ibin)
            if( next(ibin).gt.0 )
     &          write(ludmp,frmt) (a(j),j=0,next(ibin)-1)
          endif
        endif
      end do
c
      stkdmp = 0
c
      return
c
  100 format('Bin # ',i5,/,
     &       '  Size         = ',i20,/,
     &       '  Used length  = ',i20,/,
     &       '  Guard length = ',i20)
c
      end
