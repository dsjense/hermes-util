      integer function tioini (
c ... INPUT
     1  udelim, uauxc , uauxl, nuxcmd, uxcmd,
     2  ulutyi, ulutyo, uluin0, fnmin0, ulueco, uluerr,
     3  uarcf, uhelpf, inicmf, udsppr)
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
c     Initialize the TIOLIB package.  This consists of the following
c     tasks:-
c
c       1.  Initialize internal control variables
c       2.  Set up on-line help
c       3.  Open default input unit if it is requested (user requests
c           this by having LUIN0.NE.LUTTYI, and FNMIN0.NE.' ')
c       4.  Set up application to initially take input from a command
c           file (for custom initialization)
c
c     It returns the following values
c
c        0  -  Normal return: initialization complete
c       -1  -  Auxiliary command character is a reserved character
c       -2  -  Default input file is already open
c       -3  -  Could not open default input file
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments
c
c     INPUT:
c*      udelim  -  List of user delimiters (in addition to SP and TAB)
c       uauxc   -  User auxiliary command control characters
c       uauxl   -  (*) List of user auxiliary command sets
c       nuxcmd  -  # exit commands
c       uxcmd   -  (*) Array of exit commands
c*      ulutyi  -  LU for terminal input.  Passing ULUTYI < 0 sets TIO to
c                  operate in non-interactive mode (no help, show, etc.)
c       ulutyo  -  LU for terminal output
c       uluin0  -  default LU for input
c       fnmin0  -  File name for default input
c*      uluerr  -  LU for error reporting
c       ulueco  -  LU for command line echoing
c*      uarcf   -  Archive file name
c       uhelpf  -  Help file name for user commands
c       inicmf  -  Command file for TIOLIB to INITIALLY use for input
c       udsppr  -  User default spawn prompt
c
      character*(*) fnmin0, uauxc, udelim, uarcf, uhelpf, inicmf, udsppr
      character*(*) uauxl(1:*), uxcmd(1:*)
      integer       nuxcmd, ulutyi, ulutyo, uluin0, ulueco, uluerr
c
c-----------------------------------------------------------------------
c
#include "iocalcop.inc"
#include "ioctrl.inc"
#include "iocmdf.inc"
#include "ioif.inc"
#include "ioloop.inc"
#include "iohelp.inc"
#include "iolun.inc"
#include "iopar.inc"
#include "iosymbol.inc"
#ifdef USE_MPI
# include "mpi_tio.inc"
# include "mpif.h"
#endif
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer   fgeten, gtfrlu, lennb, mdotxf, tioerh
      integer   NS_putname
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS and variables
c
      character*132 errmsg
      character*28  hlppath(2)
      integer       i, ierr, luini, nch, nudlim, status
      logical       lopen
c
      data hlppath / IOHFIL, '$HERMES_ROOT/doc/Tiolib.pdf' /
c
c
c=======================================================================
c    BEGIN: Set TIOLIB control variables
c=======================================================================
c
      ierr = 0
c
#ifdef USE_MPI
      call mpi_initialized ( lopen, ierr )
      if ( lopen ) then
        call mpi_comm_rank ( mpi_comm_world, node,   ierr )
        call mpi_comm_size ( mpi_comm_world, nprocs, ierr )
      else
        node = 0
        nprocs = 1
      endif
c
#endif
c ... Logical unit #'s
c
#ifdef USE_MPI
      if ( nprocs.gt.1 ) then
        luttyi = -1
        luttyo = -1
      else
#endif
        luttyi = ulutyi
        luttyo = ulutyo
#ifdef USE_MPI
      endif
#endif
      luin0  = uluin0
      luecho = ulueco
      luerr  = uluerr
c
c ... Control characters
c
c ... User delimiters
c
      delim(1:1) = SPACE
      delim(2:2) = TAB
c
      nudlim = lennb(udelim)
      do 10 i=1,nudlim
        delim(2+i:2+i) = udelim(i:i)
   10 continue
c 
      ndelim = 2 + nudlim
c
c ... Put a double quote as the NDELIM+1 character to handle variable
c     substitution within a double quoted string (e.g. "... $var")
c
      delim(ndelim+1:ndelim+1) = DUBLQT
c
c ... Reserved characters
c
      nrsvdc = 11 + ndelim
c
      rsrvdc(1:11) = LITCHR // SNGLQT // DUBLQT // SUBCHR //
     &     LBRACE // RBRACE // LPAREN // RPAREN // LBRAKT // RBRAKT //
     &     ADRCHR
c
      do 20 i=1,ndelim
        rsrvdc(11+i:11+i) = delim(i:i)
   20 continue
c
c ... Control characters
c
      cmtchr = '!'
      conchr = '&'
      termch = '.'
c
c ... Auxiliary command characters.  NOTE that the I/O command character
c     is defined first, then any user aux. command chars. are added
c
      cmdchr     = '^'
      cmdlst(1)  = 'I/O'
#ifdef USE_MPI
      if ( nprocs.gt.1 ) then
        ncmdch   = 1
      else
#endif
        ncmdch     = lennb(uauxc) + 1
        cmdchr(2:) = uauxc
c
        do 25 i=2,ncmdch
          cmdlst(i) = uauxl(i-1)
   25   continue
#ifdef USE_MPI
      endif
#endif
c
      do 30 i=1,ncmdch
        if (index(rsrvdc(1:nrsvdc),cmdchr(i:i)) .GT. 0) then
          write(errmsg,9800) cmdchr(i:i)
          ierr = tioerh ('TIO', 'TIOINI', 2, errmsg, -1)
        endif
   30 continue
c
c ... Command archiving
c
#ifdef USE_MPI
      if ( nprocs.gt.1 ) then
        arcfnm = ' '
      else
#endif
        arcfnm = uarcf
#ifdef USE_MPI
      endif
#endif
      arcbas = -1
      arclvl = -1
      luarcv = -1
      arcunc = .FALSE.
c
c ... Command line echoing
c
      ecolvl = 1000
      echoio = .TRUE.
      ecocmt = .FALSE.
      echowd = 80
c
c ... Pause control
c
      pause = .TRUE.
c
c ... Command file processing
c
      cmflvl = 0
      ncfpar(cmflvl)  = 0
      lnumcf(cmflvl)  = 0
      hcmfnam(cmflvl) = -1
      fatalcfer = .FALSE.
      lstcfp    = 0
c
      cferlv = 1
      prepro = .FALSE.
      ppmode = .FALSE.
c
c ... Symbols and arrays
c
      ngsym      = 0
      nlsym(0)   = 0
      loclsym(0) = 1
c
c ... Calculator operations
c
      unop(1) = 'int'
      unop(2) = 'nint'
      unop(3) = 'rea'
      unop(4) = 'chs'
      unop(5) = 'sqrt'
      unop(6) = 'exp'
      unop(7) = 'ln'
      unop(8) = 'sin'
      unop(9) = 'cos'
      unop(10) = 'tan'
      unop(11) = 'asin'
      unop(12) = 'acos'
      unop(13) = 'atan'
c
      binop(1) = '+'
      binop(2) = '-'
      binop(3) = '*'
      binop(4) = '/'
      binop(5) = '^'
      binop(6) = 'mod'
      binop(7) = 'min'
      binop(8) = 'max'
      binop(9) = 'token'
      nschbop  = 5
      schbinop = '+-*/^'
c
      precbop(1) = 1
      precbop(2) = 1
      precbop(3) = 2
      precbop(4) = 2
      precbop(5) = 3
      precbop(6) = 0
      precbop(7) = 0
      precbop(8) = 0
      precbop(9) = 2
c
c ... FOR loop processing
c
      nlpln     = 0
      lstlch    = 0
      lplche(0) = 0
c
      do 52 i=0,MXCMFL
        looplv(i) = 0
   52 continue
c
c ... IF statement processing
c
      do 55 i=0,MXCMFL
        iflvl(i) = 0
   55 continue
c
c ... Terminal input
c
      prompt = '*'
      case   = 'lower'
      uprcas = .FALSE.
      lwrcas = .TRUE.
c
c ... Carriage return mode
c
      crmode = .FALSE.
c
c ... Case-sensitive checking for equality of strings
c
      csmode = .FALSE.
c
      wstate = 0
c
      if (luin0 .EQ. luttyi) then
c***  Disable call to set screen width (default to 80 columns)
c***        call mdswid (luttyo, wstate, ntcols)
        ntcols = 80
        call mdgpsz (ntlins)
      else
        ntcols = 80
        ntlins = 48
      endif
c
c ... Exit commands
c
      do 60 i=1,nuxcmd
        xitcmd(i) = uxcmd(i)
  60  continue
c
      nexitc = nuxcmd
      eofxit = .TRUE.
c
c ... Default spawn prompt
c
      dfsppr = udsppr
c
c-----------------------------------------------------------------------
c ... INITIALIZE user on-line help
c-----------------------------------------------------------------------
c
      if (luttyi .GE. 0) then
c
c ..... First set up TIOLIB help file
c
        call sthlpf (hlppath, 2, iohlpf)
c
c ..... And now user command help file
c
        call sthlpf (uhelpf, 1, uhlpfl)
c
        status = fgeten('TIO_help_reader',reader)
        if(status .ne. 0) reader = 'acroread'
      endif
c
c-----------------------------------------------------------------------
c ... Initialize default input unit
c-----------------------------------------------------------------------
c
c ... Set up default input as "command file level 0".
c
      lucmd(0) = luin0
c
c ... If the default input is from a file--file name is supplied, and
c     unit # is not the terminal input unit, try to open file
c
      if (luin0.NE.luttyi .AND. fnmin0.NE.' ') then
c
#ifdef USE_MPI
        if ( node.eq.0 ) then
c
#endif
c ....... Check that unit is not open
c
          inquire (unit=luin0, opened=lopen)
c
          if (lopen) then
            write(errmsg,9810) fnmin0
            ierr = tioerh ('TIO', 'TIOINI', 2, errmsg, -2)
          endif
c
c .  .... Try to open file
c
          if (ierr .EQ. 0) then
            status = mdotxf (.TRUE., luin0, fnmin0, 'old', ' ', ' ',
     1                       luerr )
c
            if (status .EQ. 0) then
              nch = lennb(fnmin0)
              hcmfnam(0) = NS_putname(fnmin0(1:nch))
            else
              ierr = -3
            endif
          endif
#ifdef USE_MPI
        endif
#endif
      endif
c
c-----------------------------------------------------------------------
c
      luin   = luin0
#ifdef USE_MPI
      if ( nprocs.gt.1 ) then
        call mpi_bcast(ierr, 1, mpi_integer, 0, mpi_comm_world, ierr)
      endif
#endif
      tioini = ierr
      if (tioini .LT. 0) return
c
c-----------------------------------------------------------------------
c     Set up initialization command file
c-----------------------------------------------------------------------
c
c ... NOTE that we cannot use CMFINI, since it gets its input from
c     the input line and writes error messages (if any errors occur
c     here, no messages are written, since this is assumed to be an
c     optional command file).
c
#ifdef USE_MPI
      if ( node.eq.0 .AND. inicmf .NE. ' ') then
#else
      if (inicmf .NE. ' ') then
#endif
c
c ..... Get logical unit # for file
c
        luini = gtfrlu (20, 90, 0)
c
        if (luini .GT. 0) then
c
c ....... Open file
c
          status = mdotxf (.TRUE., luini, inicmf, 'old', ' ', ' ',
     1                     -100 )
c
c ....... And set up input from the command file
c
          if (status .EQ. 0) then
            cmflvl    = 1
            ncfpar(1) = 0
            lucmd(1)  = luini
            luin      = luini
c
            nch = lennb(inicmf)
            hcmfnam(1) = NS_putname(inicmf(1:nch))
c
            if (luttyi .GE. 0) write(luttyo,100) inicmf(1:nch)
  100       format(/' *** Message from input processor'/
     1              '     Opened initialization command file: ',a)
ccc          else
ccc            write(luttyo,101) inicmf, status, luini
ccc  101       format(/' *** Message from input processor'/
ccc     1              '     Unable to open init. file: ',a,2i5)
          endif
c
        endif
      endif
c
      return
c
c
c=======================================================================
c     ERROR messages
c=======================================================================
c
 9800 format('Illegal auxiliary command char. ',a,' (reserved)')
 9810 format('Default input file already open: ',a)
c
      end
