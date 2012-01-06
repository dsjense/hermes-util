      integer function mdgetopt ( optstring, optopt, isarg, optarg )
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
c     This function mimics the behavior of the C function getopt.

c     mdgetopt finds the next option letter from the command line that 
c     matches a letter in optstring and returns it in optopt, if there 
c     is one that matches.  If the option takes an argument, mdgetopt 
c     sets the variable optarg to be the option-argument as follows:
c       +  If the option was the last character in the current command 
c          argument, then optarg is set to the next command argument.
c          If there is no supplied argument, or it begins with the 
c          character '-', then mdgetopt returns an error indication.
c       +  Otherwise, optarg is set to the remainder of the command 
c          argument following the option character.

c     If, when mdgetopt is called, there are no more arguments or the
c     current argument either does not begin with the character -
c     or consists only of the character -, mdgetopt returns the number 
c     of the current argument.  If the current argument is --, mdgetopt
c     returns the number of the next argument.
c
c     If mdgetopt encounters an option character that is not contained 
c     in optstring, it returns the a value of -1.  If it detects a 
c     missing option argument, it returns a value of -2.  In either 
c     case, mdgetopt sets the variable optopt to the option character 
c     that caused the error.  If mdgetopt encounters any errors parsing 
c     the command line, it returns a value of -3.  Finally, if an option 
c     has been successfully processed without error, it returns a value 
c     of 0.
c
c     If the the first character of optstring is not a colon, mdgetopt 
c     also prints a diagnostic message to standard error.
c
c ----------------------------------------------------------------------
c
c     Input:
c       optstring - a string of recognized option characters; if a 
c                   character is followed by a colon, the option takes 
c                   an argument which may or may not be separated from 
c                   it by white space.
c
c     Output:
c       optopt    - the next command option letter parsed from the 
c                   command line
c       isarg     - logical indicating that an argument is supplied for 
c                   this option letter
c       optarg    - the argument supplied with this option letter (only 
c                   if isarg = .TRUE.)
c
c     Return Value:
c        0      - command line option sucessfully processed
c        > 0    - all options processed, return value is next command
c                 line argument to be processed
c       -1      - Invalid option encountered
c       -2      - Missing option argument
c       -3      - Error parsing command arguments
c
c ----------------------------------------------------------------------
c
c     Passed variables
c
      character*(*)  optstring, optopt, optarg
      logical        isarg
c
c     Internal variables
c
      integer BUFLEN
      parameter ( BUFLEN = 256 )
c
      integer i, iloc, ip
      logical perror
c
      integer mdargc, mdargv
c
c     Internal STATIC variables
c
      integer optind, optpos, nargs, lbuf, lcmd
      character  argbuf*(BUFLEN), cmdnam*80
      save    optind, optpos, nargs, lbuf, argbuf
c
      common /mgetopt/ lcmd, cmdnam
c
      data nargs / -1 /     
c
c ... If 1st char not ':', print error messages
c
      perror = optstring(1:1) .ne. ':'
c
      isarg = .FALSE.
      optarg = ' '
c
c ... nargs is < 0 means 1st call; need to initialize
c
      if ( nargs.lt.0 ) then
        ip = mdargv(0,argbuf)
        if ( ip.gt.0 ) then
          iloc = 0
          do 1 i=1,ip
            if (argbuf(i:i).eq.'/') iloc = i
    1     continue
          lcmd = ip - iloc
        else
          lcmd = ip
        endif
        if ( lcmd.gt.0 ) then
          cmdnam = argbuf(iloc+1:ip)
        else
          cmdnam = 'mdgetopt'
          lcmd   = 8
        endif
        nargs = mdargc()
        if ( nargs.lt. 0 ) then
c ....... Error in mdargc call
          if (perror) call mdgetopt_msg('Error accessing command line',
     &                                  ' ')
          mdgetopt = -3
          return
        else if ( nargs.eq.0 ) then
c ....... No arguments, return 1
          mdgetopt = 1
          return
        endif
c ..... initialize argument pointer and character pointer
        optind = 0
        optpos = 0
      endif
c
c
c ... if optpos = 0, need to load next command argument into buffer
c
      if ( optpos.eq.0 ) then
c
c ..... increment argument pointer and get argument
        optind = optind + 1
        if ( optind.gt.nargs ) then
c ....... No arguments left, therefore no more options
          mdgetopt = optind
          return
        endif
        lbuf = mdargv(optind,argbuf)
        if ( lbuf.lt.0 ) then
c ....... Error in mdargv call
          if (perror) call mdgetopt_msg('Error accessing command line',
     &                                  ' ')
          mdgetopt = -3
          return
        endif
c ..... if argument is null or of length one; has to be end of options
        if ( lbuf.le.1 ) then
          mdgetopt = optind
          return
        endif
c ..... if 1st char of argument is not a '-', has to be end of options
        if ( argbuf(1:1).ne.'-' ) then
          mdgetopt = optind
          return
        endif
c ..... if argument is '--', has to be end of options (but return 
c .....    pointer to next option)
        if ( argbuf.eq.'--' ) then
          mdgetopt = optind + 1
          return
        endif
c ..... set char. pointer to next character
        optpos = 2
      endif
c
c ... grab option character; increment pointer, set to zero if exhausted
      optopt = argbuf(optpos:optpos)
      optpos = optpos + 1
      if ( optpos.gt.lbuf ) optpos = 0
c
c ... find option in supplied list
      iloc = index(optstring,optopt)
      if (iloc.le.0) then
c ..... option not found; therefore invalid
        if (perror) call mdgetopt_msg('invalid option',optopt)
        mdgetopt = -1
        return
      endif
c ... look for ':' in next char. -- indicates required argument
      ip = min(len(optstring),iloc+1)
      if ( optstring(ip:ip).eq.':') then
c
c ..... argument required
c
        if ( optpos.eq.0 ) then
c ....... Need to get option argument from next command argument
          optind = optind + 1
          if ( optind.gt.nargs ) then
c ......... No arguments left, therefore option argument not found
            if (perror)
     &        call mdgetopt_msg('option requires argument',optopt)
            mdgetopt = -2
            return
          endif
c ....... get next command argument
          lbuf = mdargv(optind,argbuf)
          if ( lbuf.lt.0 ) then
c ......... Error in mdargv call
            if (perror) 
     &        call mdgetopt_msg('Error accessing command line',' ')
            mdgetopt = -3
            return
          else if ( lbuf.ne.0 ) then
c ......... If not null, set optarg (remember, optarg initialized to ' ')
            optarg = argbuf(1:lbuf)
          endif
c ....... make sure optarg isn't another option
          if ( optarg(1:1).eq.'-' ) then
            if (perror)
     &        call mdgetopt_msg('option requires argument',optopt)
            mdgetopt = -2
            return
          endif
        else
c ....... Need to get option argument from rest of command argument
          optarg = argbuf(optpos:lbuf)
        endif
c ..... if we got to here, option argument successfully processed
        isarg = .TRUE.
        optpos = 0
      endif
c
c ... if we got to here, option successfully processed
      mdgetopt = 0
c
      return
      end

      subroutine mdgetopt_msg(msg,opt)
c
c ----------------------------------------------------------------------
c     This module writes error messages for function mdgetopt to stderr
c ----------------------------------------------------------------------
c
      character*(*)  msg,opt
c
      integer    lcmd
      character  cmdnam*80
      common /mgetopt/ lcmd, cmdnam
c
      integer lm, lo
      integer stdin, stdout, stderr
      integer lstchr
c
#include "stdio.inc"
c
      lm = lstchr(msg)
      if ( opt.eq.' ' ) then
        write(stderr,'(a,'': '',a)') cmdnam(1:lcmd),msg(1:lm)
      else
        lo = lstchr(opt)
        write(stderr,'(a,'': '',a,'': -'',a)') cmdnam(1:lcmd),
     &                                         msg(1:lm),opt(1:lo)
      endif
c
      return
      end
