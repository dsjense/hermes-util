      program tiopp
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
C_Groups tiopp main
c***********************************************************************
c

c     TIOPP is a processor for files with TIO syntax. It reads an input
c     file with TIOLIB symbol definitions, conditionals, loops, etc. and
c     by using TIOLIB's ECHOING feature, writes and output file
c     substituting all symbols, loops, and conditionals.
c
c     Usage: tiopp [args] inputfile outputfile
c     where the supported arguments are:-
c       -c{ch}         Comment character (must be single character)
c       -C{ch}         Continuation character: ' ' is legal
c       -D{sym[=val]}  Define TIO symbol: default val = 1
c       -e{num}        Maximum error count: abort after num errors
c       -f             Force overwrite of outputfile
c       -h             Print help
c       -w{num}        Output line width: default = 76
c     Spaces between flags and values are legal, except for -C, to
c     support the no-continuation character option (-C by itself)
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and common block decks
c
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer  mdargc, mdargv, mdotxf, mdpath
      integer  chrtoi, lennb , tioini
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS and variables
c
      integer    MAXARG,  MTIOSY
      parameter (MAXARG = 30)
      parameter (MTIOSY = 20)
c
      character*80   comand, infile, outfil, newouf, udelim
      character*80   tiosyn(1:MTIOSY), tiosyv(1:MTIOSY)
      character*200  fulouf
      character*80   argx, arg(1:MAXARG)
      character*1    cmtch, conch
      integer        stdin, stdout, stderr
      integer        exitv, iw, larg, nw, maxerr, status, outwidth
      integer        i, ioerr, lnfnam, luin, luout, ndel, ntioer
      integer        lenarg, loceq, ntiosy
      logical        lexist, moreopts, ovrwrt, wusage
c
      data           maxerr, ntiosy / 20, 0 /
      data           ovrwrt / .FALSE. /
      data           cmtch / ' ' /
c
c
c=======================================================================
c     BEGIN: Initialize
c=======================================================================
c
#include "stdio.inc"
c
      luin  = 21
      luout = 22
      exitv = 1
      outwidth = 76
      udelim = ',='
      conch = char(0)
c
c-----------------------------------------------------------------------
c ... Process command line
c-----------------------------------------------------------------------
c
      nw = mdargc()
      do 1 iw=1,nw
        larg = mdargv (iw, arg(iw))
    1 continue
c
      iw = 1
      wusage = .TRUE.
      moreopts = .TRUE.
c
c ... Process command line options
c
    2 continue
      if (moreopts .AND. iw.LE.nw .AND. arg(iw)(1:1).EQ.'-') then
c
c ..... -: Start of arguments (input==stdin)
c
        if (lennb(arg(iw)) .EQ. 1) then
          moreopts = .FALSE.
c         need to decrement to counteract increment at end of outer if-test
          iw = iw - 1
c
c ..... --: option/argument separator
c
        else if (arg(iw)(1:2) .EQ. '--') then
          moreopts = .FALSE.
c
c ..... -c: Comment character
c
        else if (arg(iw)(1:2) .EQ. '-c') then
c
          if (lennb(arg(iw)) .GT. 2) then
            cmtch = arg(iw)(3:3)
          else
            iw = iw + 1
            if (iw .GT. nw) then
              write(stderr,9840) '-c'
              go to 900
            endif
            cmtch = arg(iw)(1:1)
          endif
c
c ..... -C: Continuation character
c
        elseif (arg(iw)(1:2) .EQ. '-C') then
c
          if (lennb(arg(iw)) .GT. 2) then
            conch = arg(iw)(3:3)
          else
            conch = ' '
          endif
c
c ..... -D: TIO symbol definition option
c
        elseif (arg(iw)(1:2) .EQ. '-D') then
c
          if (lennb(arg(iw)) .GT. 2) then
            argx = arg(iw)(3:)
          else
            iw = iw + 1
            if (iw .GT. nw) then
              write(stderr,9840) '-D'
              go to 900
            endif
            argx = arg(iw)
          endif
c
          lenarg = lennb(argx)
          loceq  = index (argx,'=')
c
          if (loceq.EQ.1 .OR. loceq.GE.lenarg) then
            write(stderr,9840) '-D'
            go to 900
          endif
c
          ntiosy = ntiosy + 1
c
          if (loceq .GT. 0) then
            tiosyn(ntiosy) = argx(:loceq-1)
            tiosyv(ntiosy) = argx(loceq+1:)
          else
            tiosyn(ntiosy) = argx
            tiosyv(ntiosy) = '1'
          endif
c
c ..... -e: Maximum error count option
c
        elseif (arg(iw)(1:2) .EQ. '-e') then
c
          if (lennb(arg(iw)) .GT. 2) then
            argx = arg(iw)(3:)
          else
            iw = iw + 1
            if (iw .GT. nw) then
              write(stderr,9840) '-e'
              go to 900
            endif
            argx = arg(iw)
          endif
c
          status = 0
          maxerr = chrtoi (argx, status)
          if (status .NE. 0) then
            write(stderr,9850) '-e', argx(1:lennb(argx))
            go to 900
          endif
c
c ..... -f: Output file overwrite option
c
        elseif (arg(iw) .EQ. '-f') then
          ovrwrt = .TRUE.
c
c ..... -h: help
c
        elseif (arg(iw) .EQ. '-h') then
          call tiopp_usage(stdout,0)
          call cexit(0)
c
c ..... Output width
c
        elseif (arg(iw)(1:2) .EQ. '-w') then
c
          if (lennb(arg(iw)) .GT. 2) then
            argx = arg(iw)(3:)
          else
            iw = iw + 1
            if (iw .GT. nw) then
              write(stderr,9840) '-w'
              go to 900
            endif
            argx = arg(iw)
          endif
c
          status = lennb(argx)
          outwidth = 0
          do i=1,status
            larg = ichar(argx(i:i)) - ichar('0')
            if ( larg.ge.0 .AND. larg.le.9 ) then
              outwidth = 10*outwidth + larg
            else
              write(stderr,9850) '-w', argx(1:status)
              goto 900
            endif
          end do
          status = 0
c
        else
          write(stderr,9830) arg(iw)(1:lennb(arg(iw)))
          go to 900
        endif        
c
        iw = iw + 1
        go to 2
      endif
      wusage = .FALSE.

c
c-----------------------------------------------------------------------
c ... Process file names
c-----------------------------------------------------------------------
c
c ... Get input file; From command line if supplied, or use stdin
c
      if (iw .LE. nw) then
        infile = arg(iw)
        if (infile .EQ. '-') then
          infile = ' '
          luin = stdin
        endif
        iw     = iw + 1
      else
        luin = stdin
        infile = ' '
      endif
c
c ... Get output file; From command line if supplied, or use stdout
c     from input
c
      if (iw .LE. nw) then
        outfil = arg(iw)
        if (outfil .EQ. '-') then
          outfil = ' '
          luout = stdout
        endif
        iw     = iw + 1
      else
        luout = stdout
        outfil = ' '
      endif
c
      if (luout .NE. stdout) then
c
c ..... Get full-path output file name (must do this explicitly, because
c       we are going to check if the file exists)
c
    5   continue
        ioerr = mdpath (outfil, fulouf)
c
        if (ioerr .NE. 0) then
          lnfnam = lennb(outfil)
          write(stderr,9810) outfil(1:lnfnam)
          go to 900
        endif
c
c ..... Check to see if output file exists, and if it does, give the
c       user the option to stop overwriting it
c
        inquire (file=fulouf, exist=lexist)
        if (lexist) then
          if (.NOT.ovrwrt) then
            write (stderr,10)
 10         format(/,'tiopp: OUTPUT FILE ALREADY EXISTS -- '
     &              ,'Use -f option to force overwrite')
            go to 900
          endif
          call mddelf (fulouf, ndel)
        endif
c
c ..... Now open output file
c
        ioerr = mdotxf (.FALSE., luout, fulouf, 'new', 'list', ' ',
     1                  stderr)
      endif
c
c ... Initialize TIO, which also opens the input file
c
      ioerr = tioini (udelim, ' ', ' ', 0, ' ',
     1                0, stdout, luin, infile, luout, stderr,
     2                ' ', ' ', ' ', 'PREMERK>')
      if (ioerr .LT. 0) go to 900
c
c-----------------------------------------------------------------------
c ... Set up TIOLIB to send echo output to requested output file
c-----------------------------------------------------------------------
c
      call iostfe ('CONTINUE', ioerr)
      call iostec ('ON', 0, .FALSE., .TRUE., outwidth, ioerr)
      call iostca ('NONE', ioerr)
      if (cmtch .NE. ' ') call iostch ('COMMENT', cmtch, ioerr)
      if (conch .NE. char(0)) call iostch ('CONTINUATION', conch, ioerr)
c
c ... Set any TIO symbols defined on command line -- note they are
c     defined as global symbols
c
      do 20 i=1,ntiosy
        call iodefsca (GLOBALSYM, tiosyn(i), tiosyv(i), status)
   20 continue
c
      if (ioerr .LT. 0) go to 900
c
c-----------------------------------------------------------------------
c ... Process lines
c-----------------------------------------------------------------------
c
      ntioer = 0
c
      do 100 i=1,99999
        call cmdini (0, ' ', comand, ioerr)
c
        if (ioerr .EQ. IOINTERR) then
          ntioer = ntioer + 1
          if (ntioer .EQ. maxerr) go to 200
        elseif (ioerr .NE. 0) then
          if (ioerr .EQ. IOFTLERR) ntioer = ntioer + 1
          go to 200
        endif
  100 continue
c
c-----------------------------------------------------------------------
c ... Finish Up
c-----------------------------------------------------------------------
c
  200 continue
      close (unit=luin)
      close (unit=luout)
c
c ... NORMAL Completion
c
      if (ntioer .EQ. 0) then
        exitv = 0
c
c ... ERROR exit
c
      else
        if (ntioer .LT. maxerr) then
          write(stderr,9800) ntioer
        else
          write(stderr,9801) ntioer
        endif
      endif
c
  900 continue
      if ( wusage ) call tiopp_usage(stderr,1)
      call cexit (exitv)
      stop
c
c=======================================================================
c
 9800 format(/' Program TIOPP: ERROR completion'/
     1        '                 ',i4,' errors detected'/)
 9801 format(/' Program TIOPP: ABORT - maximum error count exceeded'/
     1        '                 ',i4,' errors detected'/)
c
 9810 format(' %TIOPP-ERROR: Unable to expand full output file name'/
     1       '                File name: ',a)
 9830 format(' %TIOPP-ERROR: Unknown command line option: ',a)
 9840 format(' %TIOPP-ERROR: Error processing ',a,' option')
 9850 format(' %TIOPP-ERROR: ',a,' option must provide an integer: ',a)
c
      end
