      subroutine ioshow (
c ... INPUT
     1 shoitm )
c
c***********************************************************************
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
c***********************************************************************
c
c     IOSHOW displays information about the current settings of TIOLIB,
c     based on requested SHOW ITEM
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments
c
c     INPUT
c       shoitm  -  Requested SHOW item:-
c                    ARCHIVE
c                    CHAR
c                    SYMBOL
c
      character*(*)  shoitm
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "iocmdf.inc"
#include "ioctrl.inc"
#include "iolun.inc"
#include "iosymbol.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer  lennb
      integer  NS_getname
      logical  strmch
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character*132 symnam, symval
      integer       i, len, nchnam, nchval
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      if (strmch(shoitm,'archive')) then
c
        if (arcfnm .EQ. ' ') then
          write(luttyo,100)
        else
          len = lennb(arcfnm)
          write(luttyo,110) arcfnm(1:len)
c
          if (luarcv .LT. 0) then
            write(luttyo,115) 'closed'
          else
            write(luttyo,115) 'open'
            write(luttyo,120) arcbas
          endif
        endif
c
  100   format(/' No archiving ')
  110   format(/' Archive file : ',a)
  115   format( ' File status  : ',a)
  120   format( ' Archive level: ',i2)
c
      elseif (strmch(shoitm,'char')) then
c
        write(luttyo,270) cmtchr, conchr
        if (ndelim .GT. 2) write(luttyo,275) delim(3:ndelim)
        write(luttyo,280)
c
        do 200 i=1,ncmdch
          len = max (1, lennb(cmdlst(i)))
          write(luttyo,290) i, cmdchr(i:i), cmdlst(i)(1:len)
  200   continue
c
  270   format(/' Comment      character: ''',a,''''/
     1          ' Continuation character: ''',a,'''')
  275   format(/' User delimiters (in addition to SPACE and TAB): ''',
     1          a,'''')
  280   format(/' Auxiliary command control characters'/)
  290   format(i5,t10,a,t15,a)
c
      elseif (strmch (shoitm,'symbol')) then
c
        if (ngsym .GT. 0) then
          write(luttyo,340)
          do i=1,ngsym
            nchnam = NS_getname (hgsymnam(i), symnam)
            if (nelgsym(i) .EQ. 1) then
              nchval = NS_getname (hgsymval(i), symval)
              write(luttyo,350) i, symnam(1:nchnam), symval(1:nchval)
            else
              write(luttyo,351) i, symnam(1:nchnam), nelgsym(i)
            endif
          enddo
        endif
c
        if (nlsym(cmflvl)-ncfpar(cmflvl) .GT. 0) then
          write(luttyo,345) cmflvl
          do i=loclsym(cmflvl)+ncfpar(cmflvl),
     &         loclsym(cmflvl)+nlsym(cmflvl)-1
            nchnam = NS_getname (hlsymnam(i), symnam)
            if (nellsym(i) .EQ. 1) then
              nchval = NS_getname (hlsymval(i), symval)
              write(luttyo,350) i, symnam(1:nchnam), symval(1:nchval)
            else
              write(luttyo,351) i, symnam(1:nchnam), nellsym(i)
            endif
          enddo
        endif
c
  340   format(/'GLOBAL symbols'/t16,'name', t36,'value'/
     1          t16,'----', t36,'-----')
  345   format(/'LOCAL symbols at level ',i1/t16,'name', t36,'value'/
     1          t16,'----', t36,'-----')
  350   format(i5, t16,a, t35,'''',a,'''')
  351   format(i5, t16,a, t35,'*ARRAY*: n = ',i3)
c
      endif
c
      write(luttyo,'()')
c
      return
      end
