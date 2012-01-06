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
c       mdslep
c       mdspwn
c       mdswid
c
c-----------------------------------------------------------------------
c
c
      character file*80, str*80, dum*1, path*500
      integer na, i, ic, nd, iwid, ipag

      integer mdargc, mdargv, lstchr, mdotxf, mdflsh, fgeten, fputen
cdbs  integer mdhelp
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
cdbs  remove help test since vhelp has it's own test suite
ccc      print*, 'begin vhelp test'
ccc      call mdprmt(stdout,'Help File Name ?')
ccc      read(*,'(a)') file
ccc      if ( file.ne.' ') then
ccc        i = mdhelp(' ',file)
ccc	print*,'mdhelp returns: ',i
ccc      endif
c
      print*, 'begin stderr test - writing text message to stderr'
      write(stderr,*) 'This message should go to STDERR'
c      
      print*, 'calling cexit with value 2'
      call cexit(2)
      end
