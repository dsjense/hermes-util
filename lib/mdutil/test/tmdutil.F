      program tmdutil
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
c***********************************************************************
C_Groups main standalone
c
c     Program TMDUTIL tests the MDUTIL library by explicitly
c     exercising the following routines:
c
c       mdargc
c       mdargv
c       mddelf
c       mddtim
c       mdgpsz
c       mdprmt
c       mdrget
c       mdrset
c       mdslep
c       mdspwn
c       mdswid
c
c     Program TMDUTIL also tests the statement functions defined in the
c     following include files:
c       bitwise.inc
c       mdranf.h
c
c-----------------------------------------------------------------------
c
      implicit none
c
      character file*80, str*80, dum*1, path*500
      integer na, i, ic, nd, iwid, ipag, nseeds(2)

      integer mdargc, mdargv, mdrchk, lstchr, mdotxf, mdflsh, fgeten,
     &        fputen
      real x1
c
#include "mdranf.h"
#include "crseed.inc"
      integer jseed(1:NUM_SEEDS)
c
      integer stdin, stdout, stderr
c
#include "stdio.inc"
c
      call mddtim(str)
      print*,'--',str(1:lstchr(str)),'--'
      
      na = mdargc()
      if ( na.lt.0 ) then
        print*,'MDARGC:  ',na
      else
        str = ' '
        print *, 'There were ',na,' command line arguments:'
        do 1 i=0,na+1
          ic = mdargv(i,str)
          if ( ic.ge.0 ) then
            print'(2i5,'' "'',a,''"'')',i,ic,str(1:ic)
          else
            print'(2i5)',i,ic
          endif
    1   continue
        if(na.gt.0) then
          ic = mdargv(1,str)
          i = fgeten(str,path)
          print*,str(1:lstchr(str)),': ',i,' ',path(1:lstchr(path))
          if ( na.gt.1 ) then
            ic = mdargv(2,str)
             print*,'fputen Ret. val.: ',fputen(str,path)
             i = fgeten(str,path)
             print*,str(1:lstchr(str)),': ',i,' ',path(1:lstchr(path))
          endif
       endif
c
      endif
c
      print*, 'begin sleep test (5 sec)'
      call mdslep( 5.0 )
      print*, 'end sleep test'
c
      print*, 'begin mdotxf test'
    2 continue
      call mdprmt(stdout,'Temporary File Name ?')
      read(*,'(a)') file
      if ( file.ne.' ') then
        ic = mdotxf (.FALSE., 1, file, 'new', 'list', '.dat', stderr)
        if ( ic.eq.0 ) then
          write(1,*) 'This is the file: ',file(1:lstchr(file))
          call mdprmt(stdout,'One line written: <CR> to continue')
          read(*,'(a)') dum
          i = mdflsh(1)
          call mdprmt(stdout,'mdflsh called: <CR> to continue')
          read(*,'(a)') dum
          close(1)
          print*,'file ',file(1:lstchr(file)),' closed.'
        else
          print*,'error opening ',file(1:lstchr(file)),': ',ic
          goto 2
        endif
      endif
c
      print*, 'begin spawn test'
      call mdspwn( '"Spawned>> "')
      print*, 'end spawn test'
c
      print*, 'begin delete file test'
   10 continue
c
        call mdprmt(stdout,'File Name ?')
        read(*,'(a)') file
        if ( file.eq.' ') goto 20
        call mddelf(file,nd)
        print *, 'ND = ',nd
c
        goto 10
c
   20 continue
c
      print*, 'begin set width test'
      call mdswid(stdout,1,iwid)
      print '(1x,50(''*''),'' width = '',i5,1x,50(''*''))' , iwid 
c
      call mdprmt(stdout,'<CR> to continue:')
      read(*,'(a)') file
      call mdswid(stdout,0,iwid)
      print*, 'width = ',iwid
      print*, 'end set width test'
c
      call mdgpsz(ipag)
      print*, 'Page length = ',ipag
c
c     test random number tools
      print*,'begin random number tools test'
      i = mdrchk(NUM_SEEDS, -1, nseeds)
      print*,'Required size of random number seed array: ',nseeds(1)
      print*,'Current size of random number seed array:  ',NUM_SEEDS
      print*,'Library size of random number seed array:  ',nseeds(2)
      if ( i.ne.0 ) then
        print '(a,i3)','mdrchk error: ',i
      else
c
        call mdrget(jseed)
        print *,'1st five random #s'
        do i=1,5
          RANF(x1)
          print'(i2,1pe15.3,z10)',i,x1,x1
        end do
c
        call mdrset(jseed)
        print *,'1st five random #s (repeated)'
        do i=1,5
          RANF(x1)
          print'(i2,1pe15.3,z10)',i,x1,x1
        end do
      endif
c
      print*, 'begin bitwise operations test'
      call bittest()
c
      print*, 'begin stderr test - writing text message to stderr'
      write(stderr,*) 'This message should go to STDERR'
c      
      print*, 'calling cexit with value 2'
      call cexit(2)
      end

      subroutine bittest()
c
      integer ival, iadd, jval, i
#include "bitwise.inc"
c
      ival = 1
c
      print '(i5,z10)',ival,ival
      do i=1,5
        ival = ior(lshift(ival,2),1)
        print '(i5,z10)',ival,ival
      end do
c
      ival = -1
      iadd = rshift(ival,1)
      jval = rshift(ival,5)
      print'(2z10,a,z10)',iadd,jval,' XOR: ',ieor(iadd,jval)
      iadd = comp(iadd)
c
      print'(2z10,a,z10)', ival,jval,' AND: ',iand(ival,jval)
      do i=1,5
        ival = ior(rshift(ival,2),iadd)
        print'(2z10,a,z10)', ival,jval,' AND: ',iand(ival,jval)
      end do
c
      return
      end
