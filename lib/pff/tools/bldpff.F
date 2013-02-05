#include "mdptr.h"
      program bldpff
c
c------------------------------------------------------------------------------
c                    >>>   Restricted Rights Notice   <<<
c------------------------------------------------------------------------------
c The licensing agreement for the QUICKSILVER suite prohibits its unauthorized
c dissemination, or the dissemination of any part thereof, without the express
c permission of Sandia National Laboratories.  This restriction applies to ALL
c code, source and binary, and includes ALL utilities, libraries, and scripts.
c------------------------------------------------------------------------------
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
C_Groups bldpff main
c***********************************************************************
c
c
c ----------------------------------------------------------------------
c
c     Summary:
c
c       -
c
c ----------------------------------------------------------------------
c
c     Internals:
c       exact   -  if .TRUE., string searchs will be case-sensitive;
c                  otherwise string searches will be case-insensitive
c       ids     -  loop index over datasets on input file
c       ib      -  entry # of first dataset to be processed
c       ie      -  entry # of last dataset to be processed
c       isk     -  skip factor for dataset processing
c
c ----------------------------------------------------------------------
c     Call common blocks and parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz    -  MCHTTL
c     pfstfg    -  RE, WR, RW
c     hlpfil    -  HLPFIL
c     settings  -  NO_FILE_OK
c     iopar     -  OPT, RQD
c     typpar    -  TYINTG
c
c ... Included common decks:
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "incfil.inc"
#include "settings.inc"
#include "iopar.inc"
c
#include "mdfpos.h"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c ... Parameters:
c
      integer   NUXCMD
      parameter ( NUXCMD = 2 )
      integer   IN, OUT
      parameter ( IN = 1, OUT = 2 )
      integer      NCMD
      parameter    (NCMD=22)
c
      integer MAXSYMB
      parameter(MAXSYMB = 20)
c
c ... Internal
      integer   count, dspntr, fid, fpnt
      integer   i, ib, icols, icols0, ids, ie, ierr, ifid
      integer   irec, isk, kb
      integer   lcolon, lines, luin, lupr, lurrn, ndir, nmap
      integer   ofid, rwmode
      integer   slen, srchl, subb, subbdf, subbsv, sube, subedf, subesv
      integer   istat, iw, lcmd, loceq, nsymb, n
      integer   nargs, iarg, nlarg, errcnt, inout, pomode
      integer   fname_req, nch, imatch, ptype
      integer   nchfsuf
      real      max_bytes, rmax_words, rtest
      HU_FPOS_TYPE max_words, word_lim, sav_mxw
      integer   dpnt(IN:OUT), poifid(IN:OUT)
      logical   logans
      logical   conflg, logflg, quit, cmdflg, arcflg
      logical   contmp, logtmp
      logical   exact, isarg
      logical   batch_mode
      character srchst*80, str80*80, fulnam*132, infile*132
      character cptr(IN:OUT)*2
      character uxcmd(NUXCMD)*8
      character onoff(3)*4
      character cmd*32, blank*32, opmode, tioexit*4
      character cmdnam(1:NCMD)*32
      character symnam(MAXSYMB)*80, symval(MAXSYMB)*80, uline*132
      character hlppath(2)*28
c
      integer   MWORK
#ifdef STATIC
      parameter ( MWORK = 4096 )
      integer   iwork(MWORK)
#else
      integer iwork(1)
      pointer (iwork_ptr, iwork)
      integer tsize
      HU_PTR_TYPE mdgrowmem
#endif
c
      integer   stdin, stdout, stderr
c
c ... Functions:
      integer   mdotxf, mdargc, mdargv, mdpath, mdgetopt,lennb
      integer   pfg1lu, pfgfid, pfgnds, pfgnof, pfuopn
      integer   mstrng
      integer   tioini, intpar, lstmch, partyp, chrtoi, gtfrlu
      real      reapar
      character chrpar*132
      logical   strmch, yesno
      integer   iogtlu
      HU_FPOS_TYPE pfgmxsek
c
c ----------------------------------------------------------------------
c
c ... Initialize data
c
      data cptr(IN)  / 'I>' /
      data cptr(OUT) / 'O>' /
      data blank     / ' ' /
      data poifid / 2*0 /
      data uxcmd  / 'end' , 'quit' /
      data onoff  / 'on'  , 'off'  , '?' /
      data nsymb / 0 /
c
c
      data conflg / .FALSE. /
      data logflg / .FALSE. /
      data exact  / .TRUE.  /
      data batch_mode / .FALSE. /
      data nchfsuf / 1 /
c
      data subbdf,subb / 2*1  /
      data subedf,sube / 2*32 /

      data  (cmdnam(i),i=1,NCMD)   /
     &    'append'           , 'build'            , 'close'            ,
     &    'confirm'          , 'cp'               , 'exact'            ,
     &    'file'             , 'idir'             , 'input'            ,
     &    'log'              , 'multi_output'     , 'odir'             ,
     &    'open'             ,
     &    'output'           , 'pack'             , 'pidir'            ,
     &    'podir'            , 'repair'           , 'rewind'           ,
     &    'scp'              , 'sort'             , 'substring'   /

      data hlppath / HLPFIL, '$HERMES_ROOT/doc/Bldpff.pdf' /
c
#ifndef STATIC
      data mwork /0/
      data iwork_ptr /0/
#endif
c
c ----------------------------------------------------------------------
c     Startup
c ----------------------------------------------------------------------
c
c
#include "stdio.inc"
c
      if ( NO_FILE_OK ) then
        fname_req = OPT
      else
        fname_req = RQD
      endif
c
      ierr = 0
c
      call pfsvrb ( stderr, ierr )
      if (ierr.ne.0)then
        write(stderr,*) 'PFSVRB:  ErrorCode = ',ierr
        call cexit(2)
      endif
c
      luin     = stdin
      infile   = ' '
c
      word_lim = pfgmxsek()
c
c ... start with 75% of maximum
      max_words = (word_lim/4) * 3
c
      nargs = mdargc()
c
      nlarg = mdargv(0,str80)
      lcmd = 0
      do 3 i=1,nlarg
        if (str80(i:i).eq.'/') lcmd = i
    3 continue
      lcmd = min(nlarg,lcmd+1)
      cmd = str80(lcmd:nlarg)
      lcmd = nlarg - lcmd + 1
c
      iw = mdgetopt('hBo:a:b:c:D:',opmode,isarg,str80)
c
c ... Is usage info requested; if so, write it and quit  (-h)
c
      if (iw.eq.0 .AND. opmode .EQ. 'h') then
        call bldpff_usage(stdout,0)
        call cexit(0)
      endif
c
c ----------------------------------------------------------------------
c     Process any file names on command line:
c ----------------------------------------------------------------------
c
      errcnt = 0
      inout  = IN
c
c ... Process command line options
c
      if ( iw.le.0 ) then
c
    2   continue
c
          if ( iw.lt.0 ) then
            errcnt = errcnt + 1
c
          else
c
            if ( opmode .EQ. 'B') then
              batch_mode = .TRUE.
            else if ( opmode.eq.'c' ) then
              infile = str80
            else if ( opmode.eq.'D' ) then
              nsymb = nsymb + 1
              loceq = index(str80, '=')
              if(loceq .eq. 0) then
                symnam(nsymb) = str80
                symval(nsymb) = '1'
              else if(loceq .gt. 1 .and. loceq .lt. lennb(str80)) then
                symnam(nsymb) = str80(1:loceq-1)
                symval(nsymb) = str80(loceq+1:)
              else
                write(stderr,'(3a)') cmd(1:lcmd),
     &               'Error in -D option: ', str80
              endif
            else
c
c ........... Open a file in append mode  (-a)
c
              if ( opmode .EQ. 'a') then
                pomode = RW
                inout = OUT
c
c ........... Open a file in write mode  (-b)
c
              else if ( opmode .EQ. 'b') then
                pomode = WR
                inout = OUT
c
c ........... Open a file in read mode  (-o)
c
              else if ( opmode .EQ. 'o') then
                pomode = RE
                inout = IN
c
              endif        
c
              if ( inout.eq.OUT .AND. poifid(inout).ne.0 ) then
                write(stderr,'(2a)') cmd(1:lcmd),
     &         ': only one output file can be specified on command line'
                errcnt = errcnt + 1
              else
                nlarg = lennb(str80)
                ierr = mdpath(str80(1:nlarg),fulnam)
                if ( ierr.ne.0 ) then
                  write(stderr,'(3a)')cmd(1:lcmd),
     &                 ': Error parsing file name',
     &                            str80(1:nlarg)
                  ierr = 0
                  errcnt = errcnt + 1
                else
                  nlarg = lennb(fulnam)
                  fid = pfuopn(fulnam(1:nlarg),pomode,ierr,ndir)
                  if(ierr.ne.0) then
                    write(stderr,'(3a)')cmd(1:lcmd),': Error opening ',
     &                                fulnam(1:nlarg)
                    ierr = 0
                    errcnt = errcnt + 1
                  else
                    poifid(inout) = fid
                    if ( opmode.eq.'a' ) then
                      dpnt(inout) = ndir + 1
                      call pfsdsp(fid,dpnt(inout),ierr)
                    else
                      dpnt(inout) = 1
                    endif
                  endif
                endif
              endif
            endif
          endif
c
          iw = mdgetopt('hBo:a:b:c:D:',opmode,isarg,str80)
c
        if ( iw.le.0 ) goto 2
      endif
c
      do 4 iarg=iw,nargs  
c
        nlarg = mdargv(iarg,str80)
        ierr = mdpath(str80(1:nlarg),fulnam)
        if ( ierr.ne.0 ) then
          write(stderr,'(3a)')cmd(1:lcmd),
     &              ': Error parsing file name',
     &                          str80(1:nlarg)
          ierr = 0
          errcnt = errcnt + 1
        else
          nlarg = lennb(fulnam)
          fid = pfuopn(fulnam(1:nlarg),RE,ierr,ndir)
          if(ierr.ne.0) then
            write(stderr,'(3a)')cmd(1:lcmd),': Error opening ',
     &                                fulnam(1:nlarg)
            ierr = 0
            errcnt = errcnt + 1
          else
            poifid(IN) = fid
            dpnt(inout) = 1
          endif
        endif
c
    4 continue
c
      fpnt = pfgnof(ierr)

c
      if ( batch_mode .AND. infile.eq.' ' ) then
        write(stderr,'(3a)')cmd(1:lcmd),': -c option (command file) ',
     &                      'required with -B option (batch mode)'
        errcnt = errcnt + 1
      endif
c
      if (errcnt.gt.0) then
        call bldpff_usage(stdout,1)
        if (fpnt.gt.0) call pfucls(0,ierr)
        call cexit(1)
      endif
c
      if ( infile .ne. ' ' ) luin = gtfrlu(20, 50, stderr)
c
      call sthlpf (hlppath, 2, fulnam)
c
      istat = 0
      istat = tioini (
c ... INPUT
     &    ',', ' ', ' ', NUXCMD, uxcmd,
     &    stdin, stdout, luin, infile, stdout, stderr,
     &    ARCFIL, fulnam, INIFIL, 'BLDPFF $ ' )
c
      if ( istat.lt.0 ) then
        write(stderr,*)'TIOINI:  Error # ',istat
        stop
      endif
c
      call iostpr( '?' )
      call iostca('lower',istat)
c
c ... If reading command file but not in batch mode, don't exit on
c ... return from processing command file
c
      if ( infile .ne. ' ' .AND. .NOT.batch_mode ) then
        tioexit = 'OFF'
      else
        tioexit = 'ON'
      endif
      call iostex(tioexit,istat)

      call iostfe('CONTINUE', istat)
ccc   call iostch('COMMENT', '#', istat)
c
c ... Set any TIO symbols defined on the command line, archive lines
c
      do n=1, nsymb
        call iodefsca(GLOBALSYM, symnam(n), symval(n), istat)
        uline = '^gdefine ' // symnam(n)(1:lennb(symnam(n))) //
     &          ' ' // symval(n)(1:lennb(symval(n)))
        call ioarcl(2,uline,istat)
        print*,'IOARCL: ',istat,' ',uline(1:lennb(uline))
      enddo

      call gettsz ( lines, icols0 )
      icols = icols0
c
      call pfstty ( stdin, stdout, lines, icols, ierr )
      if (ierr.ne.0) then
        write(stderr,*) 'PFSTTY:  ErrorCode = ',ierr
        call cexit(2)
      endif
c
      lurrn = stdout
      write(stdout,'(a)') "                  Hermes Utilities -- BLDPFF"
      write(stdout,'(2a)') " *************************************",
     &                     "****************************"
      call dump_copyright(stdout)
      write(stdout,'(2a)') " *************************************",
     &                     "****************************"
c
      print*, ' '
      print*, '  Enter "help" for a list of supported commands.'
c
      if (fpnt.gt.0) call pffscr (0,2,fpnt,OUT,poifid,cptr,ierr)
c
c ----------------------------------------------------------------------
c     Command Interface LOOP
c ----------------------------------------------------------------------
c
   50 continue
c
        ierr = 0
c
        istat = 0
        call cmdini (NCMD, cmdnam, cmd, istat)
        lcmd = lennb(cmd)
c
        cmdflg = iogtlu('input') .ne. luin
        arcflg = .TRUE.
c
c ..... check for possible stop conditions  ( END, QUIT, ^Z)
c
        if ( istat.eq.IOEXIT ) goto 1000
c
        if (istat.EQ.IOEOF) then
          if(luin .ne. stdin) then
            close(luin)
            luin = stdin
            call iostlu('INPUT', luin, istat)
            call iostex('ON',istat)
          endif
          go to 50
        endif
c
        if (istat .EQ. IOBADCMD) then
          write(stderr,*) cmd(1:lcmd),':  unknown command'
          istat = 0
        else if (istat .EQ. IOINTERR ) then
          write(stderr,*) cmd(1:lcmd),': TIO library error'
        endif
        if ( istat.lt.0 ) then
          call cexit(2)
        endif
c
c ..... Append option
c
        if ( strmch(cmd,'append') )then
c
          str80 = chrpar(fname_req,'File Name',' ',' ',istat,nch)
c
          if ( istat.ne.0 ) then
            write(stderr,*) 'Usage:  a[ppend] file_name'
            goto 99
          else if ( str80 .eq. ' ' ) then
            fulnam = ' '
          else
            istat = mdpath(str80,fulnam)
            if ( istat.ne.0 ) then
              write(stderr,*)'Error parsing file name !!!'
              goto 99
            endif
          endif
c
          fid = pfuopn(fulnam,RW,ierr,ndir)
          if(ierr.ne.0) goto 99
c
          poifid(OUT) = fid
          dpnt(OUT) = ndir + 1
          call pfsdsp(fid,dpnt(OUT),ierr)
c
          fpnt = pfgnof(ierr)
          if ( .NOT.cmdflg ) then
            call gettsz ( lines, icols )
            call pfstty ( stdin, stdout, lines, icols, ierr )
            call pffscr (0,2,fpnt,OUT,poifid,cptr,ierr)
          endif
c
c ..... B*UILD option
c
        else if ( strmch(cmd,'build') )then
c
          str80 = chrpar(fname_req,'File Name',' ',' ',istat,nch)
c
          if ( istat.ne.0 ) then
            write(stderr,*) 'Usage:  b[uild] file_name'
            goto 99
          else if ( str80 .eq. ' ' ) then
            fulnam = ' '
          else
            istat = mdpath(str80,fulnam)
            if ( istat.ne.0 ) then
              write(stderr,*)'Error parsing file name !!!'
              goto 99
            endif
          endif
c
          fid = pfuopn(fulnam,WR,ierr,ndir)
          if(ierr.ne.0) goto 99
c
          poifid(OUT) = fid
          dpnt(OUT) = 1
c
          fpnt = pfgnof(ierr)
          if ( .NOT.cmdflg ) then
            call gettsz ( lines, icols )
            call pfstty ( stdin, stdout, lines, icols, ierr )
            call pffscr (0,2,fpnt,OUT,poifid,cptr,ierr)
          endif
c
c ..... CL*OSE option
c
        else if ( strmch(cmd,'close') )then
c
          str80 = chrpar(RQD,'File ID',' ',' ',istat,nch)
          if ( istat.ne.0 ) then
            write(stderr,*) 'Usage:  cl[ose] file_num/I[NPUT]/O[UTPUT]'
            goto 99
          endif
c
          if (partyp(str80) .EQ. INT) then
            fpnt = chrtoi(str80,istat)
            fid  = pfgfid(fpnt,ierr)
          elseif (strmch(str80,'input')) then
            fid = poifid(IN)
            if (fid .EQ. 0) then
              write(stderr,*) 'close INPUT error: no INPUT file open'
              ierr = 1
            endif
          elseif (strmch(str80,'output')) then
            fid = poifid(OUT)
            if (fid .EQ. 0) then
              write(stderr,*) 'close OUTPUT error: no OUTPUT file open'
              ierr = 1
            endif
          else
            write(stderr,*) 'Illegal CLOSE file ID'
            ierr = 1
          endif
c
          call pfucls(fid,ierr)
c
          if(ierr.ne.0)goto 99
c
          if (fid.eq.poifid(IN) ) poifid(IN)  = 0
          if (fid.eq.poifid(OUT)) poifid(OUT) = 0
c
          fpnt = min(fpnt,pfgnof(ierr))
          if ( .NOT.cmdflg ) then
            call gettsz ( lines, icols )
            call pfstty ( stdin, stdout, lines, icols, ierr )
            call pffscr (0,2,fpnt,OUT,poifid,cptr,ierr)
          endif
c
c ..... CON*FIRM option
c
        else if ( strmch(cmd,'confirm') )then
c
          arcflg = .FALSE.
c
          str80 = chrpar(OPT,' ','on',' ',istat,nch)
          imatch = lstmch(str80,onoff,3)
          if ( istat.ne.0 .OR. imatch.lt.0 ) then
            write(stderr,*) 'Usage:  co[nfirm] [ on | of[f] | ? ]'
            goto 99
          endif

          if ( imatch.eq.1 )then
            conflg = .TRUE.
          else if ( imatch.eq.2 )then
            conflg = .FALSE.
          else if ( imatch.eq.3 )then
            if ( conflg ) then
              write(stdout,*) 'CONFIRM mode is ON'
            else
              write(stdout,*) 'CONFIRM mode is OFF'
            endif
          endif
c
c ..... CP option   (copy datasets w/ do-list)
c
        else if ( strmch(cmd,'cp') )then
c
          quit = .FALSE.
          if (poifid(OUT).eq.0)then
            write(stderr,*) 'Output file is not selected !!'
            quit = .TRUE.
          endif
          if (poifid(IN).eq.0)then
            write(stderr,*) 'Input file is not selected !!'
            quit = .TRUE.
          endif
c
          if ( quit ) goto 99
c
          ifid = poifid(IN)
          ofid = poifid(OUT)
          ndir = pfgnds(ifid,ierr)
c
          ib = intpar(RQD,'do-list:  ib [ie [iskip] ]',0,' ',istat)
          ie = intpar(OPT,'ie',ib,' ',istat)
          isk = intpar(OPT,'iskip',1,' ',istat)

          if ( istat.ne.0 ) then
            write(stderr,*) 'Usage:  cp ib [ ie [iskip] ]'
            goto 99
          endif
c
          call pfgfin( ofid, rwmode, str80(1:1), count, dspntr, ierr )
          if (dspntr.ne.count+1) call pfsdsp( ofid, count+1, ierr)
c
          if( logflg .OR. conflg )then
c
            ib = max(1,ib)
            if (ie.le.0)then
              ie = ndir
            else
              ie = min(ndir,ie)
            endif
            isk = max(1,isk)
c
            do 10 i=ib,ie,isk
c
              slen = mstrng(ifid,i,10,str80,ierr)
              if(ierr.gt.0)then
                write(stderr,*) 'MSTRNG:  Error = ',ierr
                goto 99
              endif
              ierr = 0
c
              if ( conflg .AND. luin.eq.stdin ) then
                write(stdout,'(1x,a)') str80(1:slen)
                logans = yesno(stdin,stdout,'Copy ?')
              else
                logans = .TRUE.
              endif
c
              if(logans)then
                call chk_outfile(nchfsuf,max_words,stdout,ofid,ierr)
                call pfucpy(ifid,ofid,i,i,1,ierr)
                if ( ierr.ne.0 ) goto 99
                if (logflg)then
                  write(stdout,'(1x,a,$)') str80(1:slen)
                  write(stdout,*) ' Copied !'
                endif
              endif
c
   10         continue
c
          else
c
            call chk_outfile(nchfsuf,max_words,stdout,ofid,ierr)
            call pfucpy(ifid,ofid,ib,ie,isk,ierr)
c
          endif
c
          call pfgfin( ofid, rwmode, str80(1:1), ndir, dpnt(OUT), ierr )
c
c ..... EXACT option
c
        else if ( strmch(cmd,'exact') )then
c
          str80 = chrpar(OPT,' ','on',' ',istat,nch)
          imatch = lstmch(str80,onoff,3)
          if ( istat.ne.0 .OR. imatch.lt.0 ) then
            write(stderr,*) 'Usage:  e[xact] [ on | of[f] | ? ]'
            goto 99
          endif

          if ( imatch.eq.1 )then
            exact = .TRUE.
          else if ( imatch.eq.2 )then
            exact = .FALSE.
          else if ( imatch.eq.3 )then
            arcflg = .FALSE.
            if ( exact ) then
              write(stdout,*) 'String searches are CASE-SENSITIVE'
            else
              write(stdout,*) 'String searches are CASE-INSENSITIVE'
            endif
          endif
c
c .....  F*ILE option  (list files)
c
        else if ( strmch(cmd,'file') )then
c
          arcflg = .FALSE.
c
          call scropt(stdout,'f[ile]',irec,ierr)
c
          if (ierr.ne.0) goto 99
          if (irec.eq.-3) irec = pfgnof(ierr)
c
          call gettsz ( lines, icols )
          call pfstty ( stdin, stdout, lines, icols, ierr )
c
          call pffscr (irec,2,fpnt,OUT,poifid,cptr,ierr)
c
c ..... ID*IR option  (Input Directory)
c
        else if ( strmch(cmd,'idir') )then
c
          arcflg = .FALSE.
c
          if(poifid(IN).eq.0)then
            write(stderr,*) 'No input file selected !!'
            goto 99
          endif
c
          call scropt(stdout,'id[ir]',irec,ierr)
c
          if (ierr.ne.0) goto 99
          if (irec.eq.-3) irec = pfgnds(poifid(IN),ierr)
c
          dpnt(IN) = max(1,dpnt(IN))
          call gettsz ( lines, icols )
          call pfstty ( stdin, stdout, lines, icols, ierr )
          call pfdscr (poifid(IN),irec,-1,2,dpnt(IN),1,' ',ierr)
c
c ..... IN*PUT option
c
        else if ( strmch(cmd,'input') )then
c
          fpnt = intpar(RQD,'Input file #',0,' ',istat)
c
          if ( istat.ne.0 ) then
            write(stderr,*) 'Usage:  in[put] file_number'
            goto 99
          endif
c
          fid = pfgfid(fpnt,ierr)
          if ( ierr.ne.0 ) goto 99
c
          if ( fid.ne.poifid(IN) ) then
c
            call pfgfin ( fid, rwmode, str80(1:1), ndir, dspntr, ierr )
            if(ierr.ne.0) goto 99
c
            if ( rwmode.ne.RE .AND. rwmode.ne.RW ) then
              write(stderr,*) 'WARNING -- File ',fpnt,
     &                        ' is NOT in READ mode !!!'
              write(stderr,*) '           (Use REWIND first)'
              goto 99
            else
              poifid(IN) = fid
              if ( poifid(OUT).eq.fid ) poifid(OUT) = 0
            endif
c
          endif
c
          if ( .NOT.cmdflg ) then
            call gettsz ( lines, icols )
            call pfstty ( stdin, stdout, lines, icols, ierr )
            call pffscr (0,2,fpnt,OUT,poifid,cptr,ierr)
          endif
c
c ..... LOG option
c
        else if ( strmch(cmd,'log') )then
c
          arcflg = .FALSE.
c
          str80 = chrpar(OPT,' ','on',' ',istat,nch)
          imatch = lstmch(str80,onoff,3)
          if ( istat.ne.0 .OR. imatch.lt.0 ) then
            write(stderr,*) 'Usage:  l[og] [ on | of[f] | ? ]'
            goto 99
          endif

          if ( imatch.eq.1 )then
            logflg = .TRUE.
          else if ( imatch.eq.2 )then
            logflg = .FALSE.
          else if ( imatch.eq.3 )then
            if ( logflg ) then
              write(stdout,*) 'LOG mode is ON'
            else
              write(stdout,*) 'LOG mode is OFF'
            endif
          endif
c
c ..... MULTI_OUTPUT option
c
        else if ( strmch(cmd,'multi_output') )then
c
          sav_mxw   = max_words
          max_bytes = reapar(RQD,'max. # bytes', 0.0, ' ',istat)
          rmax_words = 0.5*max_bytes
c
          str80 = chrpar(OPT,'MB|GB',' ',' ',istat,nch)
          if (str80.eq.' ' .or. partyp(str80) .EQ. INT) then
            if (str80.ne.' ') nchfsuf = chrtoi(str80,istat)
            rtest = float(word_lim)
            if ( rmax_words.lt.rtest ) then
              max_words = rmax_words
            else
              istat = 2
              str80 = ' '
            endif
          else
            if ( strmch(str80,'GB') ) then
              rtest = word_lim / 1073741824.0
              if ( rmax_words.lt.rtest ) then
                max_words = rmax_words * 1073741824
              else
                istat = 2
              endif
            else if ( strmch(str80,'MB') ) then
              rtest = word_lim / 1048576.0
              if ( max_words.lt.rtest ) then
                max_words = rmax_words * 1048576
              else
                istat = 2
              endif
            else if (str80.ne.' ') then
              istat = 1
            endif
            if (istat.eq.0) then
              nchfsuf   = intpar(OPT,'# chars. for multi-file suffixes',
     &                           nchfsuf,' ',istat)
            endif
          endif
c
          if ( istat.ne.0 ) then
            write(stderr,*)
     &        'Usage:  m[ulti_output] max_bytes [MB|GB] [ nchfsuf ]'
            if ( istat.eq.2 )  write(stderr,*)
     &        '        MAX_BYTES must be <= ',rtest,' ',str80(1:2)
            max_words = sav_mxw
            goto 99
          endif
c
          if (nchfsuf.LT.1 .OR. nchfsuf.GT.9) then
            write(stderr,*) 'nchfsuf must be in the range 1 - 9'
            nchfsuf = 1
          endif
c
c ..... OD*IR option  (Output Directory)
c
        else if ( strmch(cmd,'odir') )then
c
          arcflg = .FALSE.
c
          if(poifid(OUT).eq.0)then
            write(stderr,*) 'No output file selected !!'
            goto 99
          endif
c
          call scropt(stdout,'od[ir]',irec,ierr)
c
          if (ierr.ne.0) goto 99
          if (irec.eq.-3) irec = pfgnds(poifid(OUT),ierr)
c
          dpnt(OUT) = max(1,dpnt(OUT))
          call gettsz ( lines, icols )
          call pfstty ( stdin, stdout, lines, icols, ierr )
          call pfdscr (poifid(OUT),irec,-1,2,dpnt(OUT),1,' ',ierr)
c
c ..... OP*EN option
c
        else if ( strmch(cmd,'open') ) then
c
          str80 = chrpar(fname_req,'File Name',' ',' ',istat,nch)
c
          if ( istat.ne.0 ) then
            write(stderr,*) 'Usage:  op[en] file_name'
            goto 99
          else if ( str80 .eq. ' ' ) then
            fulnam = ' '
          else
            istat = mdpath(str80,fulnam)
            if ( istat.ne.0 ) then
              write(stderr,*)'Error parsing file name !!!'
              goto 99
            endif
          endif
c
          fid = pfuopn(fulnam,RE,ierr,ndir)
          if(ierr.ne.0) goto 99
c
          poifid(IN) = fid
          dpnt(IN) = 1
c
          fpnt = pfgnof(ierr)
          if ( .NOT.cmdflg ) then
            call gettsz ( lines, icols )
            call pfstty ( stdin, stdout, lines, icols, ierr )
            call pffscr (0,2,fpnt,OUT,poifid,cptr,ierr)
          endif
c
c ..... OUT*PUT option
c
        else if ( strmch(cmd,'output') )then
c
          fpnt = intpar(RQD,'Output file #',0,' ',istat)
c
          if ( istat.ne.0 ) then
            write(stderr,*) 'Usage:  0u[tput] file_number'
            goto 99
          endif
c
          fid = pfgfid(fpnt,ierr)
          if ( ierr.ne.0 ) goto 99
c
          if ( fid.ne.poifid(OUT) ) then
c
            call pfgfin ( fid, rwmode, str80(1:1), ndir, dspntr, ierr )
            if(ierr.ne.0) goto 99
c
            if ( rwmode.ne.WR .AND. rwmode.ne.RW ) then
              write(stderr,*) 'WARNING -- File ',fpnt,
     &                        ' is NOT in WRITE mode !!!'
              goto 99
            else
              poifid(OUT) = fid
              if ( poifid(IN).eq.fid ) poifid(IN) = 0
              dpnt(OUT) = pfgnds(fid,ierr) + 1
              if (rwmode.eq.RW) call pfsdsp( fid, dpnt(OUT), ierr)
            endif
c
          endif
c
          if ( .NOT.cmdflg ) then
            call gettsz ( lines, icols )
            call pfstty ( stdin, stdout, lines, icols, ierr )
            call pffscr (0,2,fpnt,OUT,poifid,cptr,ierr)
          endif
c
c ..... PACK option
c
        else if ( strmch(cmd,'pack') ) then
c
          str80 = chrpar(fname_req,'File Name',' ',' ',istat,nch)
c
          if ( istat.ne.0 ) then
            write(stderr,*) 'Usage:  pa[ck] file_name'
            goto 99
          else if ( str80 .eq. ' ' ) then
            fulnam = ' '
          else
            istat = mdpath(str80,fulnam)
            if ( istat.ne.0 ) then
              write(stderr,*)'Error parsing file name !!!'
              goto 99
            endif
          endif
c
          fid = pfuopn(fulnam,RW,ierr,ndir)
          if(ierr.ne.0) goto 99
c
          call pfupck ( fid, irec, ierr )
          call pfucls ( fid, ierr )
c
          if(ierr.ne.0) goto 99
c
          if ( irec.eq.0 ) then
c
            write(stderr,*) 'File already was packed !!'
c
          else if ( irec.eq.1 ) then
c
            write(stdout,*) 'File pack complete'
c
          else if ( irec.eq.-1 ) then
c
            write(stdout,*)
     &     'File datasets are NOT in order -- File CANNOT be packed !!!'
            write(stdout,*)
     &          'Use the following commands to build a new packed file:'
            write(stdout,*) '      open ',str80(1:nch)
            write(stdout,*) '      build new_file_name'
            write(stdout,*) '      cp 1 0'
c
          endif
c
c ..... PID*IR options  (Print Input Directory)
c
        else if ( strmch(cmd,'pidir') )then
c
          arcflg = .FALSE.
c
          if(poifid(IN).eq.0)then
            write(stderr,*) 'No input file selected !!'
            goto 99
          endif
c
          str80 = chrpar(fname_req,'List File Name',' ',' ',istat,nch)
c
          if ( istat.ne.0 ) then
            write(stderr,*) 'Usage:  pi[dir] file_name'
            goto 99
          else if ( str80 .eq. ' ' ) then
            fulnam = ' '
          else
            istat = mdpath(str80,fulnam)
            if ( istat.ne.0 ) then
              write(stderr,*)'Error parsing file name !!!'
              goto 99
            endif
          endif
c
          lupr = pfg1lu(1,99,ierr)
          ierr = mdotxf(.FALSE., lupr, fulnam, 'new', 'list', '.lis',
     &                  stderr)
          if ( ierr.ne.0 ) goto 99
c
          call pfdprt( lupr, poifid(IN), 132, 1, 0, 0, 0, ' ', ierr )
c
          close(unit=lupr)
c
c ..... POD*IR options  (Print Output Directory)
c
        else if ( strmch(cmd,'podir') )then
c
          arcflg = .FALSE.
c
          if(poifid(OUT).eq.0)then
            write(stderr,*) 'No output file selected !!'
            goto 99
          endif
c
          str80 = chrpar(fname_req,'List File Name',' ',' ',istat,nch)
c
          if ( istat.ne.0 ) then
            write(stderr,*) 'Usage:  pi[dir] file_name'
            goto 99
          else if ( str80 .eq. ' ' ) then
            fulnam = ' '
          else
            istat = mdpath(str80,fulnam)
            if ( istat.ne.0 ) then
              write(stderr,*)'Error parsing file name !!!'
              goto 99
            endif
          endif
c
          lupr = pfg1lu(1,99,ierr)
          ierr = mdotxf(.FALSE., lupr, fulnam, 'new', 'list','.lis',
     &                  stderr)
          if ( ierr.ne.0 ) goto 99
c
          call pfdprt( lupr, poifid(OUT), 132, 1, 0, 0, 0, ' ', ierr )
c
          close(unit=lupr)
c
c ..... REP*AIR option   (repair pff file)
c
        else if ( strmch(cmd,'repair') )then
c
          str80 = chrpar(fname_req,'File Name',' ',' ',istat,nch)
c
          if ( istat.ne.0 ) then
            write(stderr,*) 'Usage:  rep[air] file_name'
            goto 99
          else if ( str80 .eq. ' ' ) then
            fulnam = ' '
          else
            istat = mdpath(str80,fulnam)
            if ( istat.ne.0 ) then
              write(stderr,*)'Error parsing file name !!!'
              goto 99
            endif
          endif
c
          call pfurep(fulnam,ierr,ndir)
c
          if ( ierr.eq.0 )then
            write(stdout,*)'File repair not required.'
          else if ( ierr.lt.0 )then
            if ( ierr.eq.-1 )then
              write(stdout,*)'File repaired with no loss of data.'
            else if ( ierr.eq.-2 ) then
              write(stdout,*)
     &               'File repaired with potential loss of data.'
            else
              write(stderr,*)'Error encountered while repairing file.'
              goto 99
            endif
            write(stdout,*)'Repaired file contains ',ndir,' datasets.'
          else
            write(stderr,*)'Error encountered while repairing file.'
            goto 99
          endif
c
c ..... REW*IND option   (rewind dataset)
c
        else if ( strmch(cmd,'rewind') )then
c
          fpnt = intpar(RQD,'File #',0,' ',istat)
c
          if ( istat.ne.0 ) then
            write(stderr,*) 'Usage:  rew[ind] file_number'
            goto 99
          endif
c
          fid = pfgfid(fpnt,ierr)
          if ( ierr.ne.0 ) goto 99
c
          call pfurwd ( fid, ierr )
c
c ..... SCP option   (copy datasets w/ search string & optional do-list)
c
        else if ( strmch(cmd,'scp') )then
c
          quit = .FALSE.
          if (poifid(IN).eq.0)then
            write(stderr,*) 'Input file is not selected !!'
            quit = .TRUE.
          endif
          if (poifid(OUT).eq.0)then
            write(stderr,*) 'Output file is not selected !!'
            quit = .TRUE.
          endif
c
          if ( quit ) goto 99
c
          ifid = poifid(IN)
          ofid = poifid(OUT)
          ndir = pfgnds(ifid,ierr)
c
          srchst = chrpar(RQD,'search string',' ',' ',istat,srchl)
c
          ib = intpar(OPT,'ib',0,' ',istat)
          if ( ib.eq.0 ) then
            ib = 1
            ie = ndir
            isk = 1
          else
            ie = intpar(OPT,'ie',ib,' ',istat)
            isk = intpar(OPT,'iskip',1,' ',istat)
          endif

          if ( istat.ne.0 ) then
            write(stderr,*)
     &           'Usage:  scp ''search_string'' [ ib [ ie [iskip] ] ]'
            goto 99
          endif
c
          call pfgfin( ofid, rwmode, str80(1:1), count, dspntr, ierr )
          if (dspntr.ne.count+1) call pfsdsp( ofid, count+1, ierr)
c
          kb = ib
#ifndef STATIC
          tsize = (ie - kb)/isk + 1
          if ( mwork.lt.tsize) then
            iwork_ptr = mdgrowmem(iwork_ptr, tsize)
            mwork = tsize
          endif
#endif
   20     continue
            call pfuscn(ifid,kb,ie,isk,subb,sube,srchst,srchl,
     &                  MWORK,.TRUE.,exact,iwork,nmap,ierr)
            if (ierr.gt.0) goto 99
c
            kb = -ierr
            ierr = 0
c
            do 30 i=1,nmap
              ids = iwork(i)
              if ( logflg .OR. conflg ) then
                slen = mstrng(ifid,ids,10,str80,ierr)
                if(ierr.gt.0)then
                  write(stderr,*) 'MSTRNG:  Error = ',ierr
                  goto 99
                endif
                ierr = 0
              endif
c
              if ( conflg .AND. luin.eq.stdin ) then
                write(stdout,'(1x,a)') str80(1:slen)
                logans = yesno(stdin,stdout,'Copy ?')
              else
                logans = .TRUE.
              endif
c
              if(logans)then
                call chk_outfile(nchfsuf,max_words,stdout,ofid,ierr)
                call pfucpy(ifid,ofid,ids,ids,1,ierr)
                if (logflg)then
                  write(stdout,'(1x,a,$)') str80(1:slen)
                  write(stdout,*)'Copied !'
                endif
              endif
c
   30       continue
c
            if ( kb.gt.0 ) goto 20
c
          continue
c
          call pfgfin( ofid, rwmode, str80(1:1), ndir, dpnt(OUT), ierr )
c
c ..... SORT option
c
        else if ( strmch(cmd,'sort') )then
c
          ifid = poifid(IN)
          ofid = poifid(OUT)
c
          quit = .FALSE.
          if (ifid.eq.0)then
            write(stderr,*) 'Input file is not selected !!'
            quit = .TRUE.
          endif
          if (ofid.eq.0)then
            write(stderr,*) 'Output file is not selected !!'
            quit = .TRUE.
          endif
c
          if ( quit ) goto 99
c
          ndir = pfgnds(ifid,ierr)
c
          ib = intpar(OPT,'ib',0,' ',istat)
          if ( ib.eq.0 ) then
            ib = 1
            ie = ndir
            isk = 1
          else
            ie = intpar(RQD,'ie',0,' ',istat)
            isk = intpar(OPT,'iskip',1,' ',istat)
          endif
c
          if ( istat.ne.0 ) then
            write(stderr,*) 'Usage:  so[rt] [ ib ie [iskip] ]'
            goto 99
          endif
c
          contmp = conflg
          if ( contmp .AND. luin.eq.stdin ) then
            write(stdout,*) 'Confirm mode is ON !!'
            if( yesno(stdin,stdout,
     &            'Do you want to temporarily turn it off during sort ?'
     &                 )                                      )then
              contmp = .FALSE.
            endif
          else
            contmp = .FALSE.
          endif
c
          logtmp = logflg
          if ( logtmp .AND. luin.eq.stdin )then
            write(stdout,*) 'Logging mode is ON !!'
            if( yesno(stdin,stdout,
     &            'Do you want to temporarily turn it off during sort ?'
     &                 )                                      )then
              logtmp = .FALSE.
            endif
          else
            logtmp = .FALSE.
          endif
c
          call pfgfin( ofid, rwmode, str80(1:1), count, dspntr, ierr )
          if (dspntr.ne.count+1) call pfsdsp( ofid, count+1, ierr)
c
#ifndef STATIC
          tsize = 2*((ie - ib)/isk + 1)
          if ( mwork.lt.tsize) then
            iwork_ptr = mdgrowmem(iwork_ptr, tsize)
            mwork = tsize
          endif
#endif
          call dssort ( ifid,ofid,nchfsuf,max_words,
     &                  ib,ie,isk, subb,sube,exact,contmp,
     &                  logtmp,stdin,stdout,MWORK,iwork,ierr)
c
          if(ierr.ne.0)then
            write(stderr, *) 'DSSORT:  error code ',ierr
            goto 99
          endif
c
          call pfgfin( ofid, rwmode, str80(1:1), ndir, dpnt(OUT), ierr )
c
c ..... SUB*STRING option
c
        else if ( strmch(cmd,'substring') )then
c
          subbsv = subb
          subesv = sube
c
          str80 = chrpar(OPT,' ',' ',' ',istat,nch)
          if ( str80.eq.' ' .OR. str80.eq.'?' ) then
            arcflg = .FALSE.
            write(stdout,*) 'Current search range = ',subb,' to ',
     &                        sube
            write(stdout,*) 'Default search range = ',subbdf,' to ',
     &                        subedf
          else
c
            lcolon = index(str80,':')
c
            if (lcolon.eq.1) then
              subb = subbdf
            else if (lcolon.gt.1) then
              ptype = partyp(str80(1:lcolon-1))
              if( ptype.ne.INT ) goto 60
              subb = chrtoi(str80(1:lcolon-1),istat)
            else
              goto 60
            endif
c
            if (lcolon.eq.nch) then
              sube = subedf
            else if (lcolon.lt.nch) then
              ptype = partyp(str80(lcolon+1:nch))
              if( ptype.ne.INT ) goto 60
              sube = chrtoi(str80(lcolon+1:nch),istat)
            else
              goto 60
            endif
c
          endif
c
          subb = max( 1, min(MCHTTL,subb) )
          sube = max( 1, min(MCHTTL,sube) )
c
          goto 61
c
   60     continue
c
            write(stderr,*) 'Usage:  sub [ [ib]:[ie] ]'
            subb = subbsv
            sube = subesv
c
            goto 99
c
   61     continue
c
        endif
c
        call cmdxit(arcflg,0,istat)
c
        goto 50
c
   99 continue
c
        write(stderr,*) 'Command Aborted !!'
        call cmdxit(arcflg,-1,istat)
c
        goto 50
c
 1000 continue
c
      call pfucls(0,ierr)
c
      call gettsz ( lines, icols )
      if (icols.ne.icols0) then
        call mdswid (stdout,0,icols)
      endif
c
      call cexit(0)
c
      end
