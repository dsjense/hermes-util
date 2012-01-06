#if defined(WIN32sys)
# define REMOVE_QUOTATION_MARKS
#endif
#if !defined(PGNsys)
# define TILDE_SUPPORTED
#endif
      integer function mdpath (file,fulnam)
c
c **********************************************************************
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
c **********************************************************************
c
c     Summary:
c
c     Machine-dependent routine to return full file path name of 
c     specified file
c
c     **********************  UNIX Version  ****************************
c
c     The UNIX version will successfully decipher file specifications of 
c     the following forms:
c
c         $ddd/file, ${ddd}/file,      where ddd is an environment 
c                    or  $(ddd)/file   variable pointing to a directory
c
c         $fff, ${fff}, or $(fff)      where fff is an environment 
c                                      variable pointing to a file
c
c         ${xxx}yyy or $(xxx)yyy       where xxx is an environment 
c                                      variable and the entire string 
c                                      points to a file
c
c         ~/file or ~user/file         where user is a valid username
c
c ----------------------------------------------------------------------
c
c     Input:  
c       file    -  file to be expanded to full path
c
c     Output:  
c       fulnam  -  full pathname of file
c
c     Return Value:
c        0  -  Normal return
c        1  -  Illegal parenthesis use for environment variable
c        2  -  Error translating an environment variable
c        3  -  Insufficient space in "fulnam" for full pathname
c        4  -  Illegal use of '~' directory specification 
c        5  -  Illegal username in '~' directory specification 
c
c     Internals:  
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c
c ... Included common decks:  NONE
c
c ----------------------------------------------------------------------
c
c     Declare variables:  
c
c     Passed:
      character file*(*),fulnam*(*)
c
c ... Parameters:  NONE
c
c ... Internal:
c
c ----------------------------------------------------------------------
c
c
c     UNIX specific internals:
c       flen    -  last nonblank character in "file"
c       i       -  loop index
c       ic      -  ASCII value of character
c       ierr    -  error status flag
c       ll      -  last nonblank character in "fulnam"
c       lr      -  1st character position of rest of file name
c       ltb     -  1st character position of environment variable
c       lte     -  last character position of environment variable
c       uname   -  storage space for username
c
      character uname*24
      integer   flen,ltb,lte,lr,i,ic,ll,ierr
      integer   fgeten,fgtdir,lstchr
c
      flen   = lstchr(file)
      fulnam = ' '
c
c ... Is there an environment variable to decode ?
c
      if ( file(1:1).eq.'$' ) then
c
c ..... Is environment variable enclosed in parens ?
c
        if (file(2:2).eq.'(' .OR. file(2:2).eq.'{') then
          if (file(2:2).eq.'(') then
            lte = index(file(3:),')') + 1
          else
            lte = index(file(3:),'}') + 1
          endif
          if (lte.lt.3) then
            mdpath = 1
            return
          endif
          ltb = 3
          lr = lte + 2
        else
          ltb = 2
          do 1 i=2,flen
            if ( file(i:i).eq.'/' ) then
              lte = i - 1
              goto 2
            endif
            ic = ichar(file(i:i))
    1     continue
            lte = flen
    2     continue
c
          lr = lte + 1
        endif
c
c ..... call system interface routine to find environment variable
c
        ierr = fgeten(file(ltb:lte),fulnam)
        if ( ierr.ne.0 ) then
          if (ierr .eq. 2) then
            mdpath = 3
          else
            mdpath = 2
          endif
          return
        endif
#if defined(REMOVE_QUOTATION_MARKS)
c ..... look for '"' characters in environment variable and remove them
c
    3   continue
c
          i = index(fulnam,'"')
          if ( i.eq.0 ) goto 4
          fulnam(i:) = fulnam(i+1:)
          goto 3
c
    4   continue
#endif
c
c ..... tack rest of file name to path
c
        if (lr.le.flen) then
          ll = lstchr(fulnam)
          if ( ll + flen - lr + 1 .gt. len(fulnam) ) then
            mdpath = 3
            return
          endif
          fulnam(ll+1:) = file(lr:flen)
        endif
#if defined(TILDE_SUPPORTED)
c
c ... Is there a '~' to decode ?
c
      else if ( file(1:1).eq.'~' ) then
c
c ..... find end of '~' spec.
c
        lr = index(file(1:flen),'/')
c
c ..... a file spec is also needed
c
        if (lr.eq.0) then
          mdpath = 4
          return
c
c ..... '~' points to current user, get username from LOGNAME env. var.
c
        else if ( lr.eq.2 ) then
          ierr = fgeten('LOGNAME',uname)
          if ( ierr.ne.0 ) then
            mdpath = 2
            return
          endif
c
c ..... '~' points to another user,get username from '~' spec.
c
        else
          uname = file(2:lr-1)
        endif
c
c ..... use system call to find user's default directory
c
        ierr = fgtdir(uname,fulnam)
        if ( ierr.ne.0 ) then
          mdpath = 5
          return
        endif
c
c ..... tack rest of file name to path
c
        if (lr.le.flen) then
          ll = lstchr(fulnam)
          if ( ll + flen - lr + 1 .gt. len(fulnam) ) then
            mdpath = 3
            return
          endif
          fulnam(ll+1:) = file(lr:flen)
        endif
#endif
c
c ... file is already full path name ?  Copy straight to "fulnam"
c        
      else
c
        if ( flen .gt. len(fulnam) ) then
          mdpath = 3
          return
        endif
        fulnam = file
c
      endif
c
      mdpath = 0
c
      return
      end
