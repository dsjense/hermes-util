      subroutine xpndln (
c ... INPUT
     1 frstln,
c ... INPUT/OUTPUT
     2 ierr )
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
c     XPNDLN expands out the user input lines in UINPTL into the single
c     large input character buffer UINPTC.  The following processing
c     of the input is also performed:-
c
c       1.  Any sequence of delimiters (SPACE's, TAB's and COMMA's) is
c           replaced with a single SPACE.
c
c       2.  Anything after a CONTINUATION or COMMENT character is
c           ignored (only the ACTIVE part of the input line is processed).
c
c       3.  UNIX-like processing of literal and substitution characters
c
c            3a. The literal prefix character ('\') is identified, and
c                the next character is put into UINPTC, disabling any
c                special function it may have, EXCEPT that \' is put
c                into a QUOTED string as \' processed later
c
c            3b. The single quote overrides variable substitution (but
c                not '\'.)
c
c            3c. The double quote permits variable substitution
c
c            3d. Substitution of simple variables is enabled with the
c                syntax $varnam if character following the variable is
c                a delimiter, or ${varnam} if not.
c
c            3e. Substitution of complex expressions is enable with
c                the syntax ${expression}.
c
c           Substitution of both simple variables and expressions is
c           performed in subroutine SUBSTEXPR.
c
c           NOTE that both single and double quoted strings are put
c           into UINPTC as SINGLE QUOTED STRINGS (i.e. the quotes are
c           not actually removed until later)
c
c       4.  All non-quoted alpanumeric strings are converted to the
c           the current case.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT
c       frstln  -  First line of UINPTL to use in expanding out the
c                  input line buffer
c
c     INPUT/OUTPUT:
c       ierr    -  INPUT:  Do nothing if ierr.NE.0
c                  OUTPUT: Error flag:-
c                             0 = normal return
c                            -1 = Input/Output character buffer overflow
c                            -2 = syntax error
c                            -3 = Error substituting variable
c
      integer  frstln, ierr
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and Common block files
c
#include "iocmdf.inc"
#include "ioctrl.inc"
#include "ioinln.inc"
#include "iolun.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls:
c
      integer  tioerh
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS (ASCII codes for 'a', 'z', 'A', and 'Z')
c
      integer    ASCILA   , ASCILZ    , ASCIUA   , ASCIUZ
      parameter (ASCILA=97, ASCILZ=122, ASCIUA=65, ASCIUZ=90)
c
c ... Local variables:
c
c      ic     - Current character in current input user line
c      oc     - Current character in output character buffer
c      oc0    - Value of OC before current token being processed
c      quochr - Current QUOTE character (SNGLQT, DUBLQT or SPACE for none)
c
      character line*(MXICHR)
      character errmsg*132, quochr*1
      integer   echolw, ic, ic0, l0, lc, lc0, lchar,
     1          lenlin, lentkn, ln, nxsp, oc, oc0
      logical   litmod
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      uinptc = ' '
      oc     = 0
c
      if (ierr .NE. 0) then
        lenxln = 0
        cuichr = 0
        return
      endif
c
c-----------------------------------------------------------------------
c ... Concatenate the active part of all input lines into a single
c     character variable, inserting a SPACE between each line to ensure
c     that we have a valid delimiter after the last token on each line
c-----------------------------------------------------------------------
c
      line   = uinptl(locuil(frstln):locuil(frstln)+lnauil(frstln)-1)
      lenlin = lnauil(frstln)
c
      do ln=frstln+1,nuiln
        if (uiltyp(ln) .NE. CMTLIN) then
          if (lenlin+lnauil(ln)+1 .LE. MXICHR) then
            line = line(1:lenlin+1) //
     &             uinptl(locuil(ln):locuil(ln)+lnauil(ln)-1)
            lenlin = lenlin + 1 + lnauil(ln)
          else
            ierr = tioerh ('TIO', 'XPNDLN', 2,
     1                     'Input character buffer overflow', -1)
            return
          endif
        endif
      enddo
c
      ic = 0
c
c-----------------------------------------------------------------------
c ... Process next token in user input line
c-----------------------------------------------------------------------
c
  100 continue
      quochr = SPACE
      oc0    = oc
c
c ... Loop over remaining characters in input line: NOTE this not a
c     DO loop because the loop index (ic) is modified inside the loop.
c
c *** BEGIN LOOP over input characters
  200 continue
c
        if (oc .GE. MXICHR) then
          ierr = tioerh ('TIO', 'XPNDLN', 2,
     1                   'Output character buffer overflow', -1)
          return
        endif
c
        ic = ic + 1
c
c ..... Process literal prefix character
c
        if (line(ic:ic) .EQ. LITCHR) then
c
          if (ic .EQ. lenlin) then
            write(errmsg,9800) 'Literal char. at end-of-line'
            ierr = tioerh ('TIO', 'XPNDLN', 2, errmsg, -2)
            return
          else
            ic = ic + 1
c
c ......... Put a \' into a quoted string without removing the \
c
            if (line(ic:ic).EQ.SNGLQT .AND. quochr.NE.SPACE) then
              uinptc(oc+1:oc+2) = LITCHR // SNGLQT
              oc = min (oc+2, MXICHR)
c
c ......... Otherwise just put the next character in
c
            else
              oc = min (oc+1, MXICHR)
              uinptc(oc:oc) = line(ic:ic)
            endif
          endif
c
c ..... Process quote characters:
c
        elseif (line(ic:ic).EQ.SNGLQT .OR. line(ic:ic).EQ.DUBLQT) then
c
c ....... Write out quote character.  This is the character itself
c         EXCEPT for the following cases:-
c               1. Write out \' for SNGLQT in double quote mode
c               2. Write out SNGLQT for DUBLQT if not in single quote mode
c
          if (quochr.EQ.DUBLQT .AND. line(ic:ic).EQ.SNGLQT) then
            uinptc(oc+1:oc+2) = LITCHR // SNGLQT
            oc = min (oc+2, MXICHR)
          elseif (quochr .NE. SNGLQT) then
            oc = min (oc+1, MXICHR)
            uinptc(oc:oc) = SNGLQT
          else
            oc = min (oc+1, MXICHR)
            uinptc(oc:oc) = line(ic:ic)
          endif
c
c ....... And toggle mode
c
          if (quochr .EQ. SPACE) then
            quochr = line(ic:ic)
          elseif (quochr .EQ. line(ic:ic)) then
            quochr = SPACE
          endif
c
c ..... Process substitution character (except if in single quotes)
c
        elseif (line(ic:ic).EQ.SUBCHR .AND. quochr.NE.SNGLQT) then
c
          if (ic .EQ. lenlin) then
            write(errmsg,9800) 'Substitution char. at end-of-line'
            ierr = tioerh ('TIO', 'XPNDLN', 2, errmsg, -2)
            return
          endif
c
c ....... Substitute expression into output string
c
          call substexpr (line, lenlin, ic, oc, ierr)
          if (ierr .NE. 0) return
c
c ..... Process anything else
c
        else
c
c ....... If in literal mode or the character is not a delimiter,
c         put character in output line
c
          if (quochr .NE. SPACE  .OR.
     1       index (delim(1:ndelim), line(ic:ic)).EQ.0) then
c
            oc = min (oc+1, MXICHR)
            uinptc(oc:oc) = line(ic:ic)
c
c ......... If not in literal mode, convert char. case as required
c
            if (quochr .EQ. SPACE) then
              if (uprcas) then
                lchar = ichar(uinptc(oc:oc))
                if (lchar.GE.ASCILA .AND. lchar.LE.ASCILZ)
     1          uinptc(oc:oc) = char(lchar-32)
              elseif (lwrcas) then
                lchar = ichar(uinptc(oc:oc))
                if (lchar.GE.ASCIUA .AND. lchar.LE.ASCIUZ)
     1          uinptc(oc:oc) = char(lchar+32)
              endif
            endif
c
c ....... Otherwise, we have hit a delimiter outside quotes.
c
          else
c
c ......... If we are currently building a token, it is now complete.
c           add a trailing blank in the output buffer, and reload for
c           the next token
c
            if (oc .GT. oc0) then
              oc = min (oc+1, MXICHR)
              go to 100
            endif
c
          endif
c
        endif
c
      if (ic .LE. lenlin) go to 200
c *** END LOOP over input characters
c
c ... If we are still in quote mode after processing all characters
c     in the line, must have unbalanced quotes
c
      if (quochr .NE. SPACE) then
        write(errmsg,9800) 'Unbalanced quotes'
        ierr = tioerh ('TIO', 'XPNDLN', 2, errmsg, -2)
        return
      endif
c
c ... Set expanded line length, input character pointer, and 'last
c     parameter read
c
      lenxln = oc - 1
      cuichr = 0
      lstpar = ' '
      nchlsp = 1
c
c-----------------------------------------------------------------------
c ... ECHO expanded line
c-----------------------------------------------------------------------
c
      if (cmflvl .GE. ecolvl) then
c
c ..... If echoing of I/O commands is disabled, see if this is an
c       I/O command, and if so, do not echo it
c
        if (.NOT.echoio) then
          do 310 ic=1,lenxln
            if (uinptc(ic:ic) .NE. ' ') then
              if (uinptc(ic:ic) .EQ. cmdchr(1:1)) then
                go to 500
              else
                go to 320
              endif
            endif
  310     continue
        endif
c
c ..... For echoing to terminal, use current terminal width, and always
c       leave 1st char. in line blank (FORTRAN char. control).  For
c       output to file, use user-defined echo line width, and do not
c       have leading blank in line
c
  320   continue
          echolw = echowd
          l0     = 0
c
        litmod = .FALSE.
        ic     = 0
        line   = ' '
        lc0    = l0
        lc     = lc0
        ic0    = ic
        nxsp   = 1
c
c ..... Loop over characters in buffer to find token (note again that
c       this is not a DO-loop since the loop var. (ic) is modified inside
c
c************************************************** BEGIN LOOP *********
  400   continue
          ic = ic + 1
c
c ....... Process literal and quote characters
c
          if (uinptc(ic:ic) .EQ. LITCHR) then
            ic = ic + 1
            if (uinptc(ic:ic) .EQ. SNGLQT) ic = ic + 1
          endif
c
          if (uinptc(ic:ic) .EQ. SNGLQT) then
            litmod = .NOT.litmod
          endif
c
c ....... If we are not in literal mode, and hit a space, we have
c         found the end of the next token
c
          if (.NOT.litmod .AND. uinptc(ic:ic).EQ.SPACE) then
c
c ......... Compute token length, and if there is not enough room
c           to add token to current output line, write out line first,
c           together with a continuation character
c
            lentkn = ic - ic0 - 1
c
            if (lc+lentkn .GT. echolw) then
              if (useconch) then
                write(luecho,'(2a)',err=9000) line(1:lc), conchr
              else
                write(luecho,'(a)',err=9000) line(1:max(1,lc-1))
              endif
              line = ' '
              lc0  = 2 + l0
              lc   = lc0
            endif
c
c ......... Put token in line, and nxsp extra spaces, except for last one
c
            line(lc+1:lc+lentkn) = uinptc(ic0+1:ic-1)
            if (ic .GT. lenxln) nxsp = 0
            lc  = lc + lentkn + nxsp
            ic0 = ic
c
          endif
c
        if (ic .LE. lenxln) go to 400
c************************************************** END LOOP ***********
c
c ..... Write out remaining part of line (if any)
c
        if (lc .GT. lc0) write(luecho,'(a)',err=9000) line(1:lc)
c
      endif
c
  500 continue
      return
c
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9000 continue
      ierr = tioerh ('TIO', 'XPNDLN',1, 'Write error echoing output', 0)
      ecolvl = 1000
      return
c
 9800 format('Syntax error: ',a)
c
      end
