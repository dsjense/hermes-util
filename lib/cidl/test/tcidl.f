      program tcidl
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
c     Program TCIDL tests the CIDL interface
c
c-----------------------------------------------------------------------
c
      IMPLICIT NONE
      integer nx, i, j
      character title*50
      real xa(12), ya(12), y
      integer ia(12), ja(12), dims(8)
      character   command*96, prompt*48
      integer dim, ier, npass, ndim
      logical lexist

      integer lennb

      data dims /8*0/

      y = 12345.6
      dim = 12
      do i = 1, dim
        xa(i) = i
        ia(i) = i
      enddo
      title = 'This is a test'
      nx = 12345
      print *, title
      call start_call_idl(0, ier)
      call write_err(ier, 'start_call_idl')
      call send_command('  DISPLAY = getenv(''DISPLAY'') ', ier)
      call write_err(ier, ' ')
      call send_command('IDL_DIR = getenv(''IDL_DIR'') ', ier)
      write (title, 22) ' Y =', y
      call idl_write(title, 1)
      call send_real_var('  ts  ', xa(5), ier)
      call write_err(ier, 'send_real_var')
      call send_int_var('  its  ', 5, ier)
      call write_err(ier, 'send_int_var')
      title = 'This is the string "STR_TS"'
      call send_str_var('  str_ts  ', title, ier)
      call write_err(ier, 'send_str_var')
      call send_command('help ', ier)
      i = 50
      call get_real_var('  tS  ', y, i, ier)
      call write_err(ier, 'get_real_var')
      call send_command('help, ts ', ier)
      call write_err(ier, ' ')
      write (title, 22) ' TS =', y
      call idl_write(title, 0)
      call get_integer_var('  itS  ', j, i, ier)
      call write_err(ier, 'get_integer_var')
      call send_command('help, its ', ier)
      write (title, *) ' ITS =', j
      call idl_write(title, 0)
      call send_command('hak, /mess', ier)
      write (title, 22) ' After Hak', y, 7
      call idl_write(title, 0)
      call get_str_var('DISPLAY', title,i, ier)
      call write_err(ier, 'get_str_var')
      title = 'DISPLAY = '//title
      call idl_write(title, 0)
      call get_str_var('IDL_DIR', title,i, ier)
      call write_err(ier, 'get_str_var')
      title = 'IDL_DIR = '//title
      call idl_write(title, 0)

      call send_real_array('  xa ',xa,1,12,ier)
      call write_err(ier, 'send_real_array')
      call send_command('help,xa & print,xa', ier)
      call get_real_array('  xa ',ya,ndim,dims,12,npass,ier)
      call write_err(ier, 'get_real_array')
      write(title,'(a,10i3)')'ndim,dims,npass: ',ndim,dims,npass
      call idl_write(title, 0)
      do i=1,12
        if ( xa(i).ne.ya(i) ) then
          write(title,'(a,i3,2f10.1)')'real_array error:',i,xa(i),ya(i)
          call idl_write(title, 0)
        endif
      end do

      call send_integer_array('  ia ',ia,1,12,ier)
      call write_err(ier, 'send_integer_array')
      call send_command('help,ia & print,ia', ier)
      call get_integer_array('  ia ',ja,ndim,dims,12,npass,ier)
      call write_err(ier, 'get_integer_array')
      write(title,'(a,10i3)')'ndim,dims,npass: ',ndim,dims,npass
      call idl_write(title, 0)
      do i=1,12
        if ( ia(i).ne.ja(i) ) then
          write(title,'(a,i3,2i10)')'int_array error:',i,ja(i),ja(i)
          call idl_write(title, 0)
        endif
      end do

      call send_byte_array('  ba ',ia,1,12,ier)
      call write_err(ier, 'send_byte_array')
      call send_command('help,ba & print,ba', ier)
      call get_byte_array('  ba ',ja,ndim,dims,12,npass,ier)
      call write_err(ier, 'get_byte_array')
      write(title,'(a,10i3)')'ndim,dims,npass: ',ndim,dims,npass
      call idl_write(title, 0)
      do i=1,12
        if ( ia(i).ne.ja(i) ) then
          write(title,'(a,i3,2i10)')'byte_array error:',i,ja(i),ja(i)
          call idl_write(title, 0)
        endif
      end do

      dims(1) = 3
      dims(2) = 4
      call send_real_array('  xa2 ',xa,2,dims,ier)
      call write_err(ier, 'send_real_array (3x4)')
      call send_command('help,xa2 & print,xa2', ier)
      call get_real_array('  xa2 ',ya,ndim,dims,12,npass,ier)
      call write_err(ier, 'get_real_array (3x4)')
      write(title,'(a,10i3)')'ndim,dims,npass: ',ndim,dims,npass
      call idl_write(title, 0)
      do i=1,12
        if ( xa(i).ne.ya(i) ) then
          write(title,'(a,i3,2f10.1)')'real_array error:',i,xa(i),ya(i)
          call idl_write(title, 0)
        endif
      end do

      call send_integer_array('  ia2 ',ia,2,dims,ier)
      call write_err(ier, 'send_integer_array (3x4)')
      call send_command('help,ia2 & print,ia2', ier)
      call get_integer_array('  ia2 ',ja,ndim,dims,12,npass,ier)
      call write_err(ier, 'get_integer_array (3x4)')
      write(title,'(a,10i3)')'ndim,dims,npass: ',ndim,dims,npass
      call idl_write(title, 0)
      do i=1,12
        if ( ia(i).ne.ja(i) ) then
          write(title,'(a,i3,2i10)')'int_array error:',i,ia(i),ja(i)
          call idl_write(title, 0)
        endif
      end do

      dims(2) = 2
      dims(3) = 2
      call send_real_array('  xa3 ',xa,3,dims,ier)
      call write_err(ier, 'send_real_array (3x2x2)')
      call send_command('help,xa3 & print,xa3', ier)
      call get_real_array('  xa3 ',ya,ndim,dims,12,npass,ier)
      call write_err(ier, 'get_real_array (3x2x2)')
      write(title,'(a,10i3)')'ndim,dims,npass: ',ndim,dims,npass
      call idl_write(title, 0)
      do i=1,12
        if ( xa(i).ne.ya(i) ) then
          write(title,'(a,i3,2f10.1)')'real_array error:',i,xa(i),ya(i)
          call idl_write(title, 0)
        endif
      end do

      call send_command('y=0.0 & read,y,prompt=" Enter number: "',ier)
      call get_real_var('Y', y, nx, ier)
      print * , 'y = ', y, nx

      call idl_main(1, 1, 'Enter a string:', 0, 0, 0, title, ier)
      i = lennb(title)
      call write_err(ier, 'idl_main 1 1 - '//title(1:i))
      call idl_main(0, 1, ' Enter a another string: ',0,0,0,title,ier)
      i = lennb(title)
      call write_err(ier, 'idl_main 0 1 - '//title(1:i))

      prompt = 'Enter command below'
      call idl_main(0, 0, prompt, 0, 0, 0, command, ier)
      call write_err(ier, 'idl_main 0 0')
      call idl_main(1, 0, ' ', 0, 0, 0, command, ier)
      call write_err(ier, 'idl_main 1 0')

      call send_command("help & ba = 0 & hak,mes ='Ending - HAK'",ier)

      inquire(file='IDL_CMD_INTERFACE.no',exist=lexist)
      if ( lexist ) then
        print *,'Testing IDL-based command interface: SKIPPED'
      else
        print *,'Testing IDL-based command interface:'
        print '(a,$)','Command ? ' 
        call send_command('str=" "&read,prompt="",str',i)
        call get_str_var('str',command,i,ier)
        call write_err(ier, ' Read "'//command(1:i)//'"')
      endif

      inquire(file='FTN_CMD_INTERFACE.no',exist=lexist)
      if ( lexist ) then
        print *,'Testing  Fortran I/O-based command interface: SKIPPED'
      else
        print *,'Testing Fortran I/O-based command interface:'
        print '(a,$)','Command ? ' 
        read(5,'(a)',end=800,err=9000, iostat=j) command
        i = lennb(command)
        call write_err(0, ' Read "'//command(1:i)//'"')
        goto 1000
 800    continue
        call write_err(1, ' EOF encountered')
        goto 1000
 9000   continue
        call write_err(j, ' ERROR encountered')
 1000   continue
      endif

      print *,'Calling end_call_idl'
cc      endif
      call  end_call_idl(ier)
      print *,'end_call_idl returned ',ier

22    format (A, 1pe10.3, i5)

      end

      subroutine write_err(ierr, prefix)
      integer ierr
      character prefix*(*)

      integer i,j
      character message*80

      if (prefix.eq.' ') then
        write (message, 23) 'IER  =', ierr
      else
        do i=len(prefix),1,-1
          if (prefix(i:i).ne.' ') then
            j = i
            goto 1
          endif
        end do
 1      continue
        write (message, 23) prefix // ': IER  =', ierr
      endif
          
      call idl_write(message, 0)

      return
23    format (a,i5)
      end

      integer function lennb(str)

      character str*(*)

      integer i

      do i=len(str),1,-1
        if ( str(i:i) .ne. ' ' ) then
          lennb = i
          return
        endif
      end do
      lennb = 1
      return
      end
