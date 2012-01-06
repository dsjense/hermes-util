      integer function iogifv (
c ... OUTPUT
     1 status )
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
c ... IOGIFV is a TIO internal utility routine for parsing the rest of
c     the command line as a TIO conditional construct.
c     It returns either 0 (false) or 1 (true).
c
c     Binary conditional tests use the following operators:-
c
c        EQ   Equal
c        NE   Not Equal
c        GT   Greater than
c        GE   Greater than or equal to
c        LT   Less than
c        LE   Less than or equal to
c
c     Two tests can also be combined with the logical operators 
c     AND and OR.  However, because there is no parenthesis parsing
c     more complex tests are not allowed.
c
c     Comparisons are made with the highest type of either operand.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments
c
c     status - Status flag:
c                0  -  Normal completion
c               -1  -  Syntax error in statement
c
      integer status
c
c-----------------------------------------------------------------------
c
c ... Include parameter and common block decks
c
#include "ioctrl.inc"
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      character         chrpar*80
      double precision  chrtod
      integer           chrtoi, partyp, lookup, tioerh
      logical           endofl
      real              chrtor
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character*80     par, parl
      character*80     aval(2)
      character*4      cmpop(6), logop(2)
      logical          ifval, logval(2)
      integer          cmpndx, cmptyp, ier, logtyp, maxtyp, opndx, nch
      integer          optyp(2)
      integer          ival1, ival2
      real             rval1, rval2
      double precision dval1, dval2
c
c-----------------------------------------------------------------------
c
      data   cmpop / 'eq', 'ne', 'gt', 'lt', 'ge', 'le' /
      data   logop / 'and', 'or' /
c
c
c=======================================================================
c ... BEGIN:
c=======================================================================
c
      cmpndx = 0
      opndx  = 0
c
c ... Get next input parameter
c
    1 continue
      if (endofl(ioerr)) go to 100
c
      par = chrpar (OPT, ' ', ' ', ' ', ioerr, nch)
      if (ioerr .NE. 0) go to 100
c
      parl = par
      call fxcase (parl, 'lower')
c
c=======================================================================
c ... Process next par
c=======================================================================
c
c ... End-of-IF statement
c
      if (parl .EQ. 'then') then
c
        if (cmpndx .EQ. 1) then
          ifval = logval(1)
        elseif (cmpndx .EQ. 2) then
          if (logtyp .EQ. 1) then
            ifval = logval(1) .AND. logval(2)
          elseif (logtyp .EQ. 2) then
            ifval = logval(1) .OR. logval(2)
          endif
        else
          go to 100
        endif
c
        if (ifval) then
          iogifv = 1
        else
          iogifv = 0
        endif
        status = 0
        return
c
c ... Comparison operator
c
      elseif (lookup(parl,cmpop,6) .GT. 0) then
c
        if (opndx .EQ. 1) then
          cmptyp = lookup(parl,cmpop,6)
        else
          go to 100
        endif                
c
c ... Logical comparison operator
c
      elseif (lookup(parl,logop,2) .GT. 0) then
c
        if (cmpndx .EQ. 1) then
          logtyp = lookup(parl,logop,2)
        else
          go to 100
        endif
c
c ... Operand of comparison operator
c
      else
c
        opndx = opndx + 1
        if (opndx .EQ. 1) then
          cmpndx = cmpndx + 1
        elseif (opndx .GT. 2) then
          go to 100
        endif
c
        aval(opndx)  = par
        optyp(opndx) = partyp(aval(opndx))
c
c ..... If this is the second operand, evaluate logical expression
c
        if (opndx .EQ. 2) then
          maxtyp = max (optyp(1), optyp(2))
          ier    = 0
c
          if (maxtyp .EQ. INT) then
c
            ival1 = chrtoi(aval(1), ier)
            ival2 = chrtoi(aval(2), ier)
            if (cmptyp .EQ. 1) then
              logval(cmpndx) = ival1 .EQ. ival2
            elseif (cmptyp .EQ. 2) then
              logval(cmpndx) = ival1 .NE. ival2
            elseif (cmptyp .EQ. 3) then
              logval(cmpndx) = ival1 .GT. ival2
            elseif (cmptyp .EQ. 4) then
              logval(cmpndx) = ival1 .LT. ival2
            elseif (cmptyp .EQ. 5) then
              logval(cmpndx) = ival1 .GE. ival2
            elseif (cmptyp .EQ. 6) then
              logval(cmpndx) = ival1 .LE. ival2
            endif
c
          elseif (maxtyp .EQ. REA) then
c
            rval1 = chrtor(aval(1), ier)
            rval2 = chrtor(aval(2), ier)
            if (cmptyp .EQ. 1) then
              logval(cmpndx) = rval1 .EQ. rval2
            elseif (cmptyp .EQ. 2) then
              logval(cmpndx) = rval1 .NE. rval2
            elseif (cmptyp .EQ. 3) then
              logval(cmpndx) = rval1 .GT. rval2
            elseif (cmptyp .EQ. 4) then
              logval(cmpndx) = rval1 .LT. rval2
            elseif (cmptyp .EQ. 5) then
              logval(cmpndx) = rval1 .GE. rval2
            elseif (cmptyp .EQ. 6) then
              logval(cmpndx) = rval1 .LE. rval2
            endif
c
          elseif (maxtyp .EQ. DUBL) then
c
            dval1 = chrtod(aval(1), ier)
            dval2 = chrtod(aval(2), ier)
            if (cmptyp .EQ. 1) then
              logval(cmpndx) = dval1 .EQ. dval2
            elseif (cmptyp .EQ. 2) then
              logval(cmpndx) = dval1 .NE. dval2
            elseif (cmptyp .EQ. 3) then
              logval(cmpndx) = dval1 .GT. dval2
            elseif (cmptyp .EQ. 4) then
              logval(cmpndx) = dval1 .LT. dval2
            elseif (cmptyp .EQ. 5) then
              logval(cmpndx) = dval1 .GE. dval2
            elseif (cmptyp .EQ. 6) then
              logval(cmpndx) = dval1 .LE. dval2
            endif
c
          else
c
            if (cmptyp .EQ. 1) then
              logval(cmpndx) = aval(1) .EQ. aval(2)
            elseif (cmptyp .EQ. 2) then
              logval(cmpndx) = aval(1) .NE. aval(2)
            else
              go to 100
            endif
c
          endif
c
          opndx = 0
        endif
c
      endif
c
      go to 1
c
c
c=======================================================================
c     ERROR conditions and messages
c=======================================================================
c
  100 continue
      status = tioerh ('TIO', 'IOGIFV', 2,
     1                 'Syntax error in IF statement', -1)
      iogifv = 0
      return
c
      end
