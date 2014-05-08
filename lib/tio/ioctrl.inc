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
c ... Internal control variables for TIOLIB
c
c-----------------------------------------------------------------------
c
c ... Hard-wired characters used
c
c ... QUOTING characters
c
      character*1  LITCHR       , SNGLQT        , DUBLQT
      parameter   (LITCHR = '\' , SNGLQT = '''' , DUBLQT = '"' )
c
c ... Expression processing
c
      character*1  SUBCHR       , LBRACE       , RBRACE
      parameter   (SUBCHR = '$' , LBRACE = '{' , RBRACE = '}')
c
      character*1  LPAREN       , RPAREN
      parameter   (LPAREN = '(' , RPAREN = ')')
c
      character*1  LBRAKT       , RBRAKT
      parameter   (LBRAKT = '[' , RBRAKT = ']')
c
c ... Address character
c
      character*1  ADRCHR
      parameter   (ADRCHR = '@')
c
c ... Delimiters
c
      character*1  SPACE       , TAB
      parameter   (SPACE  = ' ', TAB    = '	')
c
c ... TIOLIB's reserved characters (Cannot define chars. in this list)
c
c     nrsvdc  -  # reserved characters
c     rsrvdc  -  Character string with all reserved characters
c
      character*32    rsrvdc
      common /ioctla/ rsrvdc
c
      integer         nrsvdc
      common /ioctln/ nrsvdc
c
c-----------------------------------------------------------------------
c
c ... User-definable characters
c
c     cmtchr  -  Comment character
c     conchr  -  Continuation character
c     termch  -  List termination character
c     delim   -  Character string with delimiter list
c     ndelim  -  # delimiters (SP, TAB, + user-supplied list)
c
      character*16    delim
      character*1     cmtchr, conchr, termch
      common /ioctla/ delim, cmtchr, conchr, termch
c
      integer         ndelim
      common /ioctln/ ndelim
c
c-----------------------------------------------------------------------
c
c ... Auxiliary command characters and command description list (note
c     that first auxiliary command set is always I/O)
c
      integer    MXCMDC
      parameter (MXCMDC = 10)
c
c     cmdchr  -  Character string with COMMAND CONTROL CHAR. list
c     cmdlst  -  (*) List of auxiliary command sets
c     ncmdch  -  # command control characters
c
      character*(MXCMDC) cmdchr
      character*80       cmdlst(1:MXCMDC)
      common /ioctla/ cmdchr, cmdlst
c
      integer         ncmdch
      common /ioctln/ ncmdch
c
c-----------------------------------------------------------------------
c
c ... Control Variables
c
c     arcbas  -  Base value for highest command file level archived
c     arcfnm  -  Archive file name
c     arclvl  -  Current value for highest command file level archived
c     arcunc  -  Flag that archiving set unconditionally by application
c     arcvln  -  Flag controlling archiving of current input line
c     case    -  Case for unquoted input characters to be converted to
c     cferlv  -  Command file error handle status
c     crmode  -  Flag that <CR> in interactive mode handled like EOF
c     csmode  -  Flag to check strings for equality case-sensitively
c     dfsppr  -  Default prompt for spawned sub-processes
c     ecolvl  -  Minimum level at which expanded input lines are echoed
c     ecocmt  -  Flag controlling echoing of stand-alone comment lines
c     echoio  -  Flag controlling echoing of I/O command lines
c     echowd  -  Width (in columns.) for echoing of expanded input lines
c     eofxit  -  Flag controlling handling of EOF as EXIT command
c     ioerr   -  TIOLIB I/O error flag for processing I/O commands
c     iohlpf  -  I/O help file name (full name)
c     lwrcas  -  Flag indicating lower case conversion for input
c     ntcols  -  # columns on terminal screen
c     ntlins  -  # lines on terminal screen
c     nexitc  -  # user exit commands
c     pause   -  Flag controlling whether PAUSE command is enabled
c     ppmode  -  Flag that we are currently preprocessing an input file
c     prepro  -  Flag that command files should be preprocessed
c     prompt  -  Input prompt in GETLIN for input from terminal
c     reader  -  Path and name of pdf reader to start for help
c     uhlpfl  -  User help file name (full name)
c     uprcas  -  Flag indicating upper case conversion for input
c     useconch - Flag that file syntax uses a continuation character
c     wstate  -  Current width state for terminal (80 or 132 column)
c     xitcmd  -  (*) array of commands interpreted as END PROGRAM
c
      character*200   iohlpf, uhlpfl, reader
      character*80    arcfnm
      character*32    case  , dfsppr, prompt
      character*16    xitcmd(1:10)
      common /ioctla/ arcfnm, iohlpf, uhlpfl, case, dfsppr, prompt,
     1                reader, xitcmd
c
      integer         arcbas, arclvl, cferlv, ecolvl, echowd, ioerr, 
     1                ntcols, ntlins, nexitc, wstate
      logical         arcunc, arcvln, echoio, ecocmt, eofxit, lwrcas,
     1                pause , prepro, ppmode, uprcas, crmode, csmode,
     2                useconch
      common /ioctln/ arcbas, arclvl, cferlv, ecolvl, echowd, ioerr , 
     1                ntcols, ntlins, nexitc, wstate, arcvln, arcunc, 
     2                echoio, ecocmt, eofxit, lwrcas, pause , prepro, 
     3                ppmode, uprcas, crmode, csmode, useconch
c
