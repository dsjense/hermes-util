      integer function stklist ( bin, lu )
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
c     This function lists allocation names and info for the requested 
c     bin.  If name-saving is not turned on (namesave option of stkinit),
c     only limited bin info is listed.
c
c ----------------------------------------------------------------------
c
c     Passed variables:
c
c     Input:
c       bin       -  bin to list (if bin=0, all allocated bins will be 
c                    listed)
c       lu        -  unit number to write the list to
c
c     Return Value:
c        0  -  successful completion
c        1  -  Bin out of range
c        2  -  Bin/bins has/have not been initialized
c
c ----------------------------------------------------------------------
c
      implicit none
c
c ----------------------------------------------------------------------
c
c     Parameters:
      integer MXC
      parameter ( MXC = 32 )
c
c     Passed variables
      integer bin, lu
c
c     Common blocks
#include "stak.inc"
#include "stkenum.inc"
c
c     Local variables
      integer i, frst, last, ierr, type
      HU_SIZE_T off, leng
      character ctyp(LASTPTR:1)*3, cs*5
c
c     Called functions
      integer h2c
      integer get_name
c
      data ctyp / 'PTR', 'R8', 'R4', 'D', 'L1', 'LOG', 'I4', 'I2', 'F',
     &            'I', 'C' /
c ----------------------------------------------------------------------
c
c
c ... Make sure bin is in legal range
c
      if ( bin.lt.0 .OR. bin.gt.NBINS ) then
        stklist = 1
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stklist:  bin out of range - ',bin
        endif
        return
      endif
c
c ... Has cookie been set indicating proper initialization?
c
      if ( cookie.ne.MAGIC ) then
        stklist = 2
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stklist:  stack has not been initialized'
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
c
      stklist = 0
      do i=frst,last
        if ( biggest(i).gt.0 ) then
          write(lu,200) i, biggest(i)
          if ( snames(i) ) then
            ierr = get_name(i,L_FIRST,off,leng,type,hname)
            if ( ierr.eq.0 ) write(lu,101)
c
   10       continue
c
              if ( ierr.eq.2 ) goto 20
              ierr = h2c(hname,HMAX,cname )
              if ( type.gt.0 ) then
                write(cs,'(i5)') type
              else
                cs = ' '
              endif
              write(lu,100)cname(1:MXC),ctyp(min(type,1)),cs,off,leng
              ierr = get_name(i,L_NXT,off,leng,type,hname)
            goto 10
c
   20       continue
          endif
        endif
      enddo
c
      return
  100 format('  ',a,' ',a,a,2i15)
 101  format('  Name',t36,'Type',t53,'Offset',t68,'Length')
  200 format('Bin ',i5,' allocated with ',i10,' words')
      end
