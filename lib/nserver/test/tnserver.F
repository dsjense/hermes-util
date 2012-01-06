c **********************************************************************
c   Test program for Name Server software
c   D. Seidel
c   3/2/99
c   $Id$
c   
c   Copyright (2008) Sandia Corporation. Under the terms of
c   Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
c   Government retains certain rights in this software.
c   
c   Hermes is free software: you can redistribute it and/or modify
c   it under the terms of the GNU Lesser General Public License as
c   published by the Free Software Foundation, either version 3 of
c   the License, or (at your option) any later version.
c   
c   Hermes is distributed in the hope that it will be useful, but
c   WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU Lesser General Public License for more details.
c   
c   You should have received a copy of the GNU Lesser General
c   Public License along with Hermes.  If not, see
c   <http://www.gnu.org/licenses/>.
c   
c **********************************************************************
C_Groups main standalone
c
      program tnserver

      integer i, l, ierr, h, r, fl
      integer flist(7), mlist(5)
      integer alist(7)
      character ccc*30

      integer NSU_lastchr

      integer NS_freename, NS_locate, NS_putname, NS_getname, 
     &        NS_find, NS_find_match, NS_findorput, NS_locate_match
      integer NS_defarray, NS_freearray, NS_putarrval, NS_getarrval,
     &        NS_getarrlims    

      do i=1,7
        flist(i) = 0
        alist(i) = 0
      end do

      print*,'NS_find(EMPTY,...,0): ', NS_find('empty',flist,0)
      print*,'NS_find(EMPTY,...,5): ', NS_find('empty',flist,5)

      flist(1) = NS_putname('function_1')
      flist(2) = NS_putname('function_2')
      flist(3) = NS_putname('function_3')
      print*,'NS_find(PARTIAL,...,5): ', NS_find('PARTIAL',flist,5)
      call NS_debug
      mlist(1) = NS_findorput('function_2',flist,3)
      mlist(2) = NS_findorput('function_4',flist,3)

      alist(1) = NS_defarray(6,12,999)
      call NS_debug
      call NS_debug_array
      flist(4) = NS_putname('function_4')
      flist(5) = NS_putname('function_5')
      do i=5,13
        h = 8*i - 72
        ierr = NS_putarrval(alist(1),i,h)
        if ( ierr.ne.0 ) print*,'putarrval error, index,val: ',ierr,i,h
        r = 999
        ierr = NS_getarrval(alist(1),i,r)
        if ( ierr.ne.0 ) print*,'getarrval error, index,val: ',ierr,i,r
        if ( h .ne.r ) print*,'get-putarrval mismatch: ',h,r
      end do
      alist(2) = NS_defarray(-2,2,-5)
      ierr = NS_putarrval(alist(2),-2,50)
      if ( ierr.ne.0 ) print*,'putarrval error 2: ',ierr
      ierr = NS_putarrval(20,-2,50)
      if ( ierr.ne.0 ) print*,'putarrval error 3: ',ierr
      ierr = NS_putarrval(-4,-2,50)
      if ( ierr.ne.0 ) print*,'getarrval error 3: ',ierr

      call NS_debug
      call NS_debug_array
      print*,'alist: ',(alist(i),i=1,5)
      ierr = NS_getarrlims ( alist(1), h, r )
      if ( ierr.ne.0 ) print*,'getarrlims error 1: ',ierr
      print*,'List 1 range: ',h,' to ',r
      ierr = NS_getarrlims ( alist(2), h, r )
      if ( ierr.ne.0 ) print*,'getarrlims error 2: ',ierr
      print*,'List 2 range: ',h,' to ',r
      mlist(3) = NS_findorput('function_5',flist,5)
      ierr = NS_freearray(alist(1))
      if ( ierr.ne.0 ) print*,'freearray error: ',ierr
      
      call NS_debug
      call NS_debug_array
      mlist(4) = NS_findorput('function_6',flist,5)
c
      print*,'alist: ',(alist(i),i=1,5)
      print*,'flist: ',(flist(i),i=1,5)
      print*,'mlist: ',(mlist(i),i=1,5)
c
      print*,'NS_find(function_4,...): ', NS_find('function_4',flist,5)
      print*,'NS_find(function_6,...): ', NS_find('function_6',flist,5)
c
      call NS_debug
c
      do i=1,4
        if ( mlist(i).le.0 ) then
          h = -mlist(i)
          mlist(i) = NS_locate(h, flist, 5)
          if ( mlist(i).eq.0 ) then
            ierr = NS_getname(h,ccc(1:15))
            if ( ierr.gt.0 ) then
              print*,'locate error, h,string: ',
     &                i,mlist(i), h,'"',ccc(1:ierr),'"'
            else
              print*,'get/locate error, h: ',
     &                i, mlist(i),h,ierr
            endif
            write(ccc(16:),'(a)') '--- some more'
            print*,'CCC: ',ccc
          endif
          print*,'freeing mlist, h: ',i, h, NS_freename(h)
        endif
      end do
c
      mlist(5) = NS_findorput('function_22',flist,5)
c
c ... test match locate
c
      h = NS_putname('no_match')
      r = NS_find_match('no_match',flist,5)
      if ( r.gt.0 ) then
        ierr = NS_getname(flist(r),ccc)
        l = NSU_lastchr(ccc)
        print*,'found unique match w/ NS_find_match: ',r,' ',ccc(1:l)
      else
        print*,'failed unique match w/ NS_find_match: ',r
      endif
      r = NS_locate_match(h,flist,5)
      if ( r.gt.0 ) then
        ierr = NS_getname(flist(r),ccc)
        l = NSU_lastchr(ccc)
        print*,'found unique match w/ NS_locate_match: ',r,' ',ccc(1:l)
      else
        print*,'failed unique match w/ NS_locate_match: ',r
      endif

      h = NS_putname('function')
      fl = 5
      do i=1,3
        r = NS_find_match('function',flist,fl)
        if ( r.gt.0 ) then
          ierr = NS_getname(flist(r),ccc)
          l = NSU_lastchr(ccc)
          print*,'found unique match w/ NS_find_match: ',r,' ',ccc(1:l)
        else
          print*,'failed unique match w/ NS_find_match: ',r
        endif
        r = NS_locate_match(h,flist,fl)
        if ( r.gt.0 ) then
          ierr = NS_getname(flist(r),ccc)
          l = NSU_lastchr(ccc)
          print*,'found unique match w/ NS_locate_match: ',r,' ',
     &           ccc(1:l)
        else
          print*,'failed unique match w/ NS_locate_match: ',r
        endif
        if ( i.lt.3 ) then
          fl = fl + 1
          flist(fl) = NS_putname('function')
        endif
      enddo
      print*,'freeing handle: ', h, NS_freename(h)
c
      call NS_debug
c
c
      end
