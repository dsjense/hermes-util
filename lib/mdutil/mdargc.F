#if defined(ABSOFTsys)
# define PROMPT_FOR
#endif
      integer function mdargc ()
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
C_Groups @(#)
c **********************************************************************
c
c     Summary:
c
c     Machine-dependent routine to return # of command line arguments
c
c ----------------------------------------------------------------------
c
c     Input:  NONE
c
c     Output:  NONE
c
c     Return Value:
c        0   -  No arguments
c        >0  -  # of command line arguments
c        <0  -  Error flag
c               -2  -  Not Implemented
#if defined(PROMPT_FOR)
c               -3  -  Error obtaining unused file unit
c               -4  -  Error accessing command arguments
c               -5  -  Insufficient buffer space for command arguments
#endif
c
c     Internals:  
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c
c ... Included common decks:
c
c ----------------------------------------------------------------------
c
c     Declare variables:  
c
c     Passed:
c
c ... Parameters:  NONE
c
c ... Internal:
c
#ifdef PROMPT_FOR
c     ************************ prompting Version  **********************
c
c     For machines that don't have command line argument capability 
c     (e.g., MAC-OS), prompt the user for the command line arguments.
c     This is done in the routine "mdargs" so that both "mdargc" and
c     "mdargv" have access to the saved arguments.
c
      integer  mdargs
      character dum*1
c
c ----------------------------------------------------------------------
c
      mdargc = mdargs(-1, dum)
#else
c
c     Call system routine iargc()
c
      integer iargc
c
c ----------------------------------------------------------------------
c
      mdargc = iargc()
#endif
c
      return
      end
# ifdef PROMPT_FOR
      integer function mdargs(iarg, arg)
c
c     This routine parses a line of command arguments, and stores them
c     in a packed character string, with pointers to each argument. The
c     0th argument is the name of the calling routine (if available).
c
c     Special characters
c         whitespace (spaces and tabs)
c           Whitespace delimits command line arguments except when 
c           enclosed within apostrophes or quotes.  Also, the
c           backslash (\) character can be used to escape a single 
c           whitespace character.
c         backslash (\)
c           A backslash causes the next character to be interpreted 
c           literally UNLESS it is contained in an apostrophe- or 
c           quote-delimited string.
c         apostrophes(') and quotes (")
c           All characters enclosed within matching apostrophes or 
c           quotes are interpreted literally, including whitespace and 
c           backslashes. If a second matching apostrophe or quote is 
c           not encountered by the end of the command line, it is
c           implicitly assumed to be at the end of the line.
c
      implicit none
c
c ... passed arguments
c
      integer iarg
      character arg*(*)
c
      character bslash*1, tab*1, q1*1, q2*1, qchar*1
      logical   lopen, ws, quote, esc
      integer   lui, luo, ic, jc, last
c
c
      integer lstchr
c
# if defined(ABSOFTsys)
      character WINSIZE*(*)
      parameter ( WINSIZE = '80,500,100,200' )
c
      integer GETARG
# else
      integer stdin, stdout, stderr
# endif
c
      integer MAXARGS, MXARGCHRS
      parameter ( MAXARGS = 100, MXARGCHRS = 500 )
c
      integer nargs, pargs(-1:MAXARGS)
      character argbuf*(MXARGCHRS)
      save nargs, pargs, argbuf
      data nargs / -1 /
c
      if ( nargs.lt.0 ) then
c
# if defined(ABSOFTsys)
c       for ABSoft MAC, use unixlib GETARG to get the 0th argument,
c       open text window to prompt the user
c
        call GETARG(0,argbuf)
        do lui=11,99
          inquire(unit=lui,opened=lopen)
          if ( .NOT. lopen ) goto 1
        end do
          mdargs = -3
          return
    1   continue
        open(unit=lui,file='Command Line Arguments',err=98,
     &       access='window,'//WINSIZE)
        lopen = .TRUE.
        luo = lui
# else
c       if no prompt window available, just use stdin and stdout
        lopen = .FALSE.
c
c       if available, get the 0th argument
#  if defined(HPsys)
        ic = igetarg(0,argbuf,len(argbuf))
#  else
c       no way to get 0th argument, set it to be blank
        argbuf = ' '
#  endif
#  include "stdio.inc"
        lui = stdin
        luo = stdout
# endif
        pargs(-1) = 0
        pargs(0) = lstchr(argbuf)
        write(luo,'(a,/)',err=98) 'Enter any command line arguments'
        if ( pargs(0).gt.0 ) then
          write(luo,'(a,$)',err=98) argbuf(1:pargs(0)) // ' '
        endif
c
        bslash = char(92)
        tab    = char(9)
        q1     = char(39)
        q2     = char(34)
        jc = pargs(0)
        nargs = 0
        ws = .TRUE.
        quote = .FALSE.
        esc = .FALSE.
c
   10   continue
c
          read(lui,'(a)',err=98) argbuf(jc+1:)
          ic = jc
          last = lstchr(argbuf)
          if ( last.ge.MXARGCHRS ) then
            mdargs = -5
            if ( lopen ) close(lui)
          endif
c
   15     continue
c
            ic = ic + 1
c
            if ( ic.gt.last ) goto 20
c
            if ( quote .OR. esc ) then
              esc = .FALSE.
              if ( quote .AND. argbuf(ic:ic).eq.qchar ) then
                quote = .FALSE.
              else
                jc = jc + 1
                argbuf(jc:jc) = argbuf(ic:ic)
              endif
            else
              if ( argbuf(ic:ic).eq.bslash ) then
                esc = .TRUE.
                if ( ws ) then
                  ws = .FALSE.
                  nargs = nargs + 1
                endif
              else
                esc = .FALSE.
                if ( argbuf(ic:ic).eq.' ' .OR.
     &               argbuf(ic:ic).eq.tab     ) then
                  if ( .NOT. ws ) then
                    pargs(nargs) = jc
                    ws = .TRUE.
                  endif
                else if ( argbuf(ic:ic).eq.q1 .OR.
     &                    argbuf(ic:ic).eq.q2     ) then
                  if ( ws ) then
                    ws = .FALSE.
                    nargs = nargs + 1
                  endif
                  quote = .TRUE.
                  qchar = argbuf(ic:ic)
                else
                  if ( ws ) then
                    ws = .FALSE.
                    nargs = nargs + 1
                  endif
                  jc = jc + 1
                  argbuf(jc:jc) = argbuf(ic:ic)
                endif
              endif
            endif
c
            goto 15
c
   20     continue
c
        if ( esc ) then
          esc = .FALSE.
          goto 10
        endif
c
        if ( nargs.gt.0 ) pargs(nargs) = jc
        argbuf(jc+1:) = ' '
        if ( lopen ) close(lui)
c
# ifdef DEBUG
        print*,'nargs: ',nargs
        print*,q2 // argbuf(1:jc) // q2
        print'(2i10,a)', (ic,pargs(ic),
     &           ' "'//argbuf(pargs(ic-1)+1:pargs(ic))//'"',ic=0,nargs)
c
# endif
      endif
c
      if ( iarg.lt.0 ) then
        mdargs = nargs
      else if ( iarg.le.nargs ) then
        mdargs = pargs(iarg) - pargs(iarg-1)
        if ( mdargs.gt.0 ) then
          arg = argbuf(pargs(iarg-1)+1:pargs(iarg))
        else
          arg = ' '
        endif
      else
        mdargs = -1
      endif
c
      return
c
   98 continue
c
      mdargs = -4
      return
c
      end
#endif
