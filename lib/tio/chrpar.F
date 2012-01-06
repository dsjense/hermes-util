      character*(*) function chrpar (
c ... INPUT
     1 rqsta, pdscrp, dfault, uspchr,
c ... INPUT/OUTPUT
     2 ierr,
c ... OUTPUT
     3 nchpar )
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
c ... CHRPAR returns a CHARACTER parameter to the calling routine.
c
c     It first tries to one from the expanded input line.  It assumes
c     that parameters are delimited by single spaces, or any of the
c     list of user-defined 'special' characters.  These special
c     characters are themselves returned as parameters.  Note that
c     CHRPAR is the routine that finally removes single quotes from
c     quoted strings (and concatenates with any other non-delimiter
c     strings adjoining these)
c
c     If CHRPAR fails to find a parameter in the expanded input line,
c     and it is optional, the default value is returned. For a required
c     parameter, there are three cases.  First if we are not in
c     interactive mode, we return a blank string and an error condition.
c     Second, in interactive mode with PDSCRP non-blank, CHRPAR will
c     interactively request a parameter.  Finally, in interactive mode
c     with PDSCRP a blank string, CHRPAR also returns an error.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       rqsta   -  Required/Optional switch for parameter
c       pdscrp  -  Brief description of parameter (for interactive
c                  parameter request)
c       dfault  -  Default value
c       uspchr  -  List of user special characters
c
c     OUTPUT:
c       nchpar  -  Non-blank length of CHRPAR
c
c     INPUT/OUTPUT:
c       ierr    -  INPUT:   Do nothing if ierr.NE.0
c                  OUTPUT:  ERROR flag:-
c                     0 - normal return
c                    -1 - Unable to return required parameter
c                    -2 - Syntax error in line (unbalanced quotes)
c
        character*(*) pdscrp, dfault, uspchr
        integer       rqsta, nchpar, ierr
c
c-----------------------------------------------------------------------
c
c ... Include parameter and common block decks
c
#include "ioctrl.inc"
#include "iolun.inc"
#include "ioinln.inc"
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer  lennb, tioerh
c
c-----------------------------------------------------------------------
c
      character errmsg*132, tmppar*132, lprmpt*132
      integer   i, ispchr, ldscrp, lenpar, nspchr, pc
      logical   litmod
c
c
c=======================================================================
c ... BEGIN:
c=======================================================================
c
      lenpar = len(chrpar)
      nspchr = lennb(uspchr)
c
      if (ierr   .NE. 0     ) go to 500
      if (cuichr .GE. lenxln) go to 200
c
c ... Process characters in the current input line, looking for a
c     parameter
c
    1 continue
      i      = cuichr
      tmppar = ' '
      pc     = 0
      litmod = .FALSE.
c
c-----------------------------------------------------------------------
c ... Find next parameter in expanded input line
c-----------------------------------------------------------------------
c
c ... Loop over characters in line (note that this is not a DO-loop
c     since we alter loop index (i) within the loop)
c
c************************************************** BEGIN LOOP *********
  100 continue
        i = i + 1
c
c ..... If character is a single quote, toggle literal mode but do
c       not put into output parameter
c
        if (uinptc(i:i) .EQ. SNGLQT) then
c
          litmod = .NOT.litmod
c
c ..... If the character is the literal character, put directly into
c       the parameter, unless it is ESCAPING a single quote
c
        elseif (uinptc(i:i) .EQ. LITCHR) then
c
          pc = pc + 1
          if(uinptc(i+1:i+1) .EQ. SNGLQT) then
            tmppar(pc:pc) = SNGLQT
            i = i + 1
          else
            tmppar(pc:pc) = LITCHR
          endif
c
c ..... In literal mode put character into parameter
c
        elseif (litmod) then
c
          pc = pc + 1
          tmppar(pc:pc) = uinptc(i:i)
c
c ..... Not in literal mode, and not a quote or literal character
c
        else
c
c ....... Check if this is a user special character
c
          if (nspchr .GT. 0) then
            ispchr = index (uspchr(1:nspchr), uinptc(i:i) )
            if (ispchr .GT. 0) then
c
c ........... If already building a parameter, this special character
c             is a delimiter.  Parameter is complete, but we must
c             back up the input line pointer to pick up the special
c             character on the next call
c
              if (pc .GT. 0) then
                i = i - 1
c
c ........... Otherwise, the special character is the parameter
c
              else
                pc     = 1
                tmppar = uspchr(ispchr:ispchr)
              endif
              go to 400
            endif
          endif
c
c ....... Character is a space
c
          if (uinptc(i:i) .EQ. SPACE) then
c
c ......... If we are already building a parameter, parameter is
c           complete, otherwise ignore the space
c
            if (pc .GT. 0) go to 400
c
c ....... Character is nothing special: put it into parameter
c
          else
c
            pc = pc + 1
            tmppar(pc:pc) = uinptc(i:i)
c
          endif
c
        endif
c
      if (i .lt. lenxln) go to 100
c************************************************** END LOOP ***********
c
      cuichr = lenxln
c
      if (litmod) then
        ierr = tioerh ('TIO', 'CHRPAR', 2, 'Unbalanced quotes', -2)
        write(errmsg,9800) lstpar(1:nchlsp)
        ierr = tioerh (' ', ' ', 2, errmsg, -2)
        go to 500
      endif
c
      if (pc .GT. 0) go to 400
c
c-----------------------------------------------------------------------
c ... No parameter in current input line
c-----------------------------------------------------------------------
c
  200 continue
c
c ... For an optional parameter, simply use the default value
c
      if (rqsta .NE. RQD) go to 500
c
c ... Required parameter.  Fatal error if we are not interactive, or
c     user does not want to interactively request parameter
c
      if (luin.NE.luttyi .OR. pdscrp.EQ.' ') then
        ierr = tioerh ('TIO', 'CHRPAR', 2,
     1                 'Unable to return parameter', -1)
        write(errmsg,9800) lstpar(1:nchlsp)
        ierr = tioerh (' ', ' ', 2, errmsg, -1)
        go to 500
      endif
c
c ... Get continuation of command line
c
      ldscrp = lennb(pdscrp)
      lprmpt = 'Enter '''//pdscrp(1:ldscrp)//''' &>'
c
      call cmdcon (lprmpt, ioerr)
c
      if (ioerr .LT. 0) then
        ierr = -1
        go to 500
      endif
c
c ... And go back to get the parameter
c
      go to 1
c
c-----------------------------------------------------------------------
c ... Parameter found in current input line
c-----------------------------------------------------------------------
c
  400 continue
      cuichr = i
      chrpar = tmppar
      nchpar = min (lenpar, pc)
c
      go to 600
c
c-----------------------------------------------------------------------
c ... Set parameter to default value
c-----------------------------------------------------------------------
c
  500 continue
      chrpar = dfault
      nchpar = min (lenpar, lennb(dfault))
      nchpar = max (1, nchpar)
c
c-----------------------------------------------------------------------
c ... Save parameter as the 'last read parameter', and return
c-----------------------------------------------------------------------
c
  600 continue
      lstpar = chrpar
      nchlsp = nchpar
c
      return
c
c=======================================================================
c     ERROR conditions and messages
c=======================================================================
c
 9800 format('             Last parameter read: ',a)
c
      end
