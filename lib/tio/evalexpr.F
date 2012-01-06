      subroutine evalexpr (
c ... INPUT
     &  ninput,
c ... INPUT/OUTPUT/WORKSPACE
     &  stktyp, stkstr, stkival, stkdval, stknch,
c ... OUTPUT
     1  status )
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
c ... EVALEXPR takes an input list of operands and operators for an 
c     RPN calculator, and returns a final value.  The ninput input
c     values are passed in the four arrays:-
c        stkstr  -  String value
c        stktyp  -  Value type (see below)
c        stkival -  Value's integer value
c        stkdval -  Value's double precision value
c     If stktyp(1) = NUL (include file iopar.inc), this routine assumes
c     that only the strings are passed down, and first computes the
c     type and numeric values for all input tokens.  If stktyp(1) is
c     a legal TIO variable type index, it assumes that the calling
c     has already computed all variable types and/or numeric value(s).
c     This second option allows the inline expression evaluation routine
c     to evaluate complex expressions at machine precision.
c
c ... The result of binary expressions are computed with the highest
c     TIO type of the two arguments, where the levels are:-
c         1. INTEGER
c         2. REAL
c         3. DOUBLE
c         4. STRING
c
c ... The recognized stack operators are defined in include file 
c     iocalcop.inc.  The only legal operation with strings are
c     concatenation, using the + operator, and the "token" operator.
c
c     The "token" operator requires that its 2nd operand (the token
c     number) be an integer. If there are fewer tokens in the 1st
c     operand than this token number, a NUL variable is put on the stack.
c     Otherwise, the appropriate blank-delimited token is put on the 
c     stack after being promoted to the appropriate type with a call to 
c     partyp.  Also, if the 1st operator is of lower type than character 
c     (i.e.., an integer, real, or double), it is treated for the
c     purpose of this operation as a one-token character string.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       ninput  - # tokens in the input list
c
c     INPUT/OUTPUT/WORKSPACE:
c       stktyp  - (*) Type of each stkstr string
c       stkstr  - (*) input list of tokens
c       stkival - (*) Integer value of each STKSTR(*)
c       stkdval - (*) Double precisiion value of each STKSTR(*)
c       stknch  - (*) # characters in STKSTR(*) (WORKSPACE only)
c
c     OUTPUT:
c       status  -  Status flag
c                     0 - normal return
c                    -4 - Too few operands for requested stack operation
c                    -5 - Illegal operator for given operands
c                    -6 - Attempted divison by zero
c                    -8 - No operand on stack on return
c                    -9 - More than one operand on stack on return
c                         (legal, but almost certainly an error!)
c
      integer          ninput, status
      integer          stkival(1:*), stknch(1:*), stktyp(1:*)
      double precision stkdval(1:*)
      character*(*)    stkstr(1:*)
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and common block files
c
#include "ioctrl.inc"
#include "iocalcop.inc"
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer           partyp, lennb, lsteql, tioerh, gttokv
      logical           streql
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character*132     errmsg
      character*80      par
      double precision  dpar1, dpar2
      integer           i, iinput, ipar1, ipar2, istk, maxtyp, nstk
      logical           valoprnd
c
c
c=======================================================================
c ... BEGIN:
c=======================================================================
c
      status  = 0
      nstk    = 0
c
c ... Get the type and numeric values of each input token in the list
c     if they are not supplied on input
c
      if (stktyp(1) .EQ. NUL) then
        do iinput=1,ninput
          call bldscaval (  stkstr(iinput) , 
     &      stktyp(iinput), stkival(iinput), stkdval(iinput) )
c
          if (stktyp(iinput) .EQ. ALFA) then
            if ( lsteql(stkstr(iinput),unop,MUNOP) .GT. 0) then
              stktyp(iinput) =  UNARYOP
            elseif ( lsteql(stkstr(iinput),binop,MBINOP) .GT. 0) then
              stktyp(iinput) =  BINARYOP
            endif
          endif
c
        enddo
      endif
c
c-----------------------------------------------------------------------
c ... Process the input list
c-----------------------------------------------------------------------
c
      do iinput=1,ninput
c
c ..... Load next input parameter into local string
c
        par = stkstr(iinput)
c
c-----------------------------------------------------------------------
c ..... Unary operations
c-----------------------------------------------------------------------
c
        if (stktyp(iinput) .EQ. UNARYOP) then
          if (nstk .LT. 1) then
            write(errmsg,9800) 'No', par(1:16)
            status = tioerh ('TIO', 'EVALEXPR', 2, errmsg, -4)
            go to 600
          endif
c
          if (stktyp(nstk) .LE. DUBL) then
            valoprnd = .TRUE.
          elseif (stktyp(nstk) .EQ. ALFA) then
            valoprnd = .FALSE.
            go to 100
          endif
c
c ....... INT/NINT (REAL or DOUBLE to INT type conversion)
c
          if (streql(par,'int') .OR. streql(par,'nint')) then
c
            if (stktyp(nstk).EQ.REA .OR. stktyp(nstk).EQ.DUBL) then
              if (streql(par,'int')) then
                stkival(nstk) = stkdval(nstk)
              else
                stkival(nstk) = nint(stkdval(nstk))
              endif
              stkdval(nstk) = stkival(nstk)
              stktyp(nstk)  = INT
            endif
c
c ....... REAL (INT/DOUBLE to REAL type conversion)
c
          elseif (streql(par,'rea')) then
c
c ......... DOUBLE to REAL conversion.  Replace the double-precision string
c           with the equivalent single-precision string
c
            if (stktyp(nstk) .EQ. INT) then
              stkdval(nstk) = stkival(nstk)
              stktyp(nstk)  = REA
            elseif (stktyp(nstk) .EQ. DUBL) then
              stktyp(nstk)  = REA
            endif
c
c ....... CHS (change sign)
c
          elseif (streql(par,'chs')) then
            if (stktyp(nstk) .LE. INT) then
              stkival(nstk) = -stkival(nstk)
              stkdval(nstk) = -stkdval(nstk)
            elseif (stktyp(nstk).EQ.REA .OR. stktyp(nstk).EQ.DUBL) then
              stkdval(nstk) = -stkdval(nstk)
            endif
c
c ....... Mathematical functions
c
          else
c
c ......... First convert INTs to REALs
c
            if (stktyp(nstk) .EQ. INT) then
              stkdval(nstk) = stkival(nstk)
              stktyp(nstk)  = REA
            endif
c
c ......... Square root
c
            if (streql(par, 'sqrt')) then
              if (stkdval(nstk) .GE. 0.0) then
                stkdval(nstk) = sqrt(stkdval(nstk))
              else
                valoprnd = .FALSE.
              endif
c
c ......... Exponential
c
            elseif (streql(par, 'exp')) then
              stkdval(nstk) = exp(stkdval(nstk))
c
c ......... Logarithm
c
            elseif (streql(par, 'ln')) then
              if (stkdval(nstk) .GT. 0.0) then
                stkdval(nstk) = log(stkdval(nstk))
              else
                valoprnd = .FALSE.
              endif
c
c ......... Trigonometric functions
c
            elseif (streql(par, 'sin')) then
              stkdval(nstk) = sin(stkdval(nstk))
            elseif (streql(par, 'cos')) then
              stkdval(nstk) = cos(stkdval(nstk))
            elseif (streql(par, 'tan')) then
              stkdval(nstk) = tan(stkdval(nstk))
c
c ......... Inverse trigonometric functions
c
            elseif (streql(par, 'asin')) then
              if (abs(stkdval(nstk)) .LE. 1.0) then
                stkdval(nstk) = asin(stkdval(nstk))
              else
                valoprnd = .FALSE.
              endif
            elseif (streql(par, 'acos')) then
              if (abs(stkdval(nstk)) .LE. 1.0) then
                stkdval(nstk) = acos(stkdval(nstk))
              else
                valoprnd = .FALSE.
              endif
            elseif (streql(par, 'atan')) then
              stkdval(nstk) = atan(stkdval(nstk))
c
            endif
c
          endif
c
  100     continue
          if (.NOT.valoprnd) then
            write(errmsg,9800) 'Illegal', par(1:16)
            status = tioerh ('TIO', 'EVALEXPR', 2, errmsg, -5)
            go to 600
          endif
c
c ....... Update character string for new value on top of stack
c
          if (stktyp(nstk) .EQ. INT) then
            call ljusti (stkival(nstk), 0, stkstr(nstk),
     &                   stknch(nstk), status)
          elseif (stktyp(nstk).EQ.REA .or. stktyp(nstk).EQ.DUBL) then
            call ljustf (stktyp(nstk), stkdval(nstk), 
     &                   stkstr(nstk), stknch(nstk) )
        endif
c
c-----------------------------------------------------------------------
c ..... Binary operations
c-----------------------------------------------------------------------
c
        elseif (stktyp(iinput) .EQ. BINARYOP) then
c
          if (nstk .LT. 2) then
            write(errmsg,9800) 'Too few', par(1:16)
            status = tioerh ('TIO', 'EVALEXPR', 2, errmsg, -4)
            go to 600
c
          else
c
c ......... Get maximum type of two stack operands
c
            maxtyp = max (stktyp(nstk), stktyp(nstk-1))
            if (streql(par,'token')) maxtyp = ALFA
c
c ......... INTEGER arithmetic
c
            if (maxtyp .EQ. INT) then
              ipar1 = stkival(nstk-1)
              ipar2 = stkival(nstk)
c
              if (par .EQ. '+') then
                ipar1 = ipar1 + ipar2
              elseif (par .EQ. '-') then
                ipar1 = ipar1 - ipar2
              elseif (par .EQ. '*') then
                ipar1 = ipar1 * ipar2
              elseif (par .EQ. '/') then
                if (ipar2 .EQ. 0) then
                  status = tioerh ('TIO', 'EVALEXPR', 2,
     &                             'Zero divisor', -6)
                  go to 600
                else
                  ipar1 = ipar1 / ipar2
                endif
              elseif (par .EQ. '^') then
                ipar1 = ipar1 ** ipar2
              elseif (streql(par,'mod')) then
                ipar1 = mod (ipar1,ipar2)
              elseif (streql(par,'min')) then
                ipar1 = min (ipar1,ipar2)
              elseif (streql(par,'max')) then
                ipar1 = max (ipar1,ipar2)
              else
                write(errmsg,9810) 'Illegal', par(1:16)
                status = tioerh ('TIO', 'EVALEXPR', 2, errmsg, -5)
                go to 600
              endif
c
c ........... Update values on the stack (note that type is already INT)
c
              nstk = nstk - 1
              stkival(nstk) = ipar1
              stkdval(nstk) = ipar1
              call ljusti (stkival(nstk), 0, stkstr(nstk),
     &                     stknch(nstk) , status)
c
c ......... REAL/DOUBLE_PRECISION arithmetic
c
            elseif (maxtyp.EQ.REA .or. maxtyp.EQ.DUBL) then
              dpar1 = stkdval(nstk-1)
              dpar2 = stkdval(nstk)
c
              if (par .EQ. '+') then
                dpar1 = dpar1 + dpar2
              elseif (par .EQ. '-') then
                dpar1 = dpar1 - dpar2
              elseif (par .EQ. '*') then
                dpar1 = dpar1 * dpar2
              elseif (par .EQ. '/') then
                if (dpar2 .EQ. 0.0D0) then
                  status = tioerh ('TIO', 'EVALEXPR', 2,
     &                             'Zero divisor', -6)
                  go to 600
                else
                  dpar1 = dpar1 / dpar2
                endif
              elseif (par .EQ. '^') then
                if (stktyp(nstk) .EQ. INT) then
                  ipar2 = nint(dpar2)
                  dpar1 = dpar1 ** ipar2
                else
                  dpar1 = dpar1 ** dpar2
                endif
              elseif (streql(par,'mod')) then
                dpar1 = mod (dpar1,dpar2)
              elseif (streql(par,'min')) then
                dpar1 = min (dpar1,dpar2)
              elseif (streql(par,'max')) then
                dpar1 = max (dpar1,dpar2)
              else
                write(errmsg,9810) 'Illegal', par(1:16)
                status = tioerh ('TIO', 'EVALEXPR', 2, errmsg, -5)
                go to 600
              endif
c
c ........... Update values on the stack
c
              nstk = nstk - 1
              stktyp(nstk)  = maxtyp
              stkdval(nstk) = dpar1
              call ljustf (stktyp(nstk), stkdval(nstk), 
     &                     stkstr(nstk), stknch(nstk) )
c
c ......... STRING operations
c
            else
              if (par .EQ. '+') then
                nstk = nstk - 1
                stkstr(nstk)(stknch(nstk)+1:) = stkstr(nstk+1)
                stknch(nstk) = stknch(nstk) + stknch(nstk+1)
c
              elseif (streql(par,'token')) then
                if ( stktyp(nstk).ne.INT ) then
                  write(errmsg,9810) 'Illegal', par(1:16)
                  status = tioerh ('TIO', 'EVALEXPR', 2, errmsg, -5)
                endif
                ipar2 = stkival(nstk)
                nstk = nstk - 1
                stknch(nstk) = gttokv(stkstr(nstk)(1:stknch(nstk)),
     &                                ipar2, par)
                stkstr(nstk)  = par
                call bldscaval (  stkstr(nstk) , 
     &            stktyp(nstk), stkival(nstk), stkdval(nstk) )
              else
                status = tioerh ('TIO', 'EVALEXPR', 2,
     &                    'Illegal binary operation on strings', -5)
                go to 600
              endif
c
              stktyp(nstk) = partyp(stkstr(nstk))
            endif
c
          endif
c
c-----------------------------------------------------------------------
c ..... Put operand on stack
c-----------------------------------------------------------------------
c
        else
          nstk = nstk + 1
          stkstr(nstk)  = stkstr(iinput)
          stktyp(nstk)  = stktyp(iinput)
          stkival(nstk) = stkival(iinput)
          stkdval(nstk) = stkdval(iinput)
          stknch(nstk)  = lennb(stkstr(nstk))
        endif
c
      enddo
c
c-----------------------------------------------------------------------
c ... Finish up description
c-----------------------------------------------------------------------
c
      if (nstk.EQ.0 .OR. stknch(nstk).EQ.0 .OR. stktyp(nstk).EQ.NUL)then
        status = tioerh ('TIO', 'EVALEXPR', 2, 'No output on stack', -8)
      elseif (nstk .GT. 1) then
        status = tioerh ('TIO', 'EVALEXPR', 2,
     &                   'More than one operand on stack on output', -9)
      endif
c
  600 continue
      return
c
c
c=======================================================================
c     ERROR conditions and messages
c=======================================================================
c
 9800 format(a,' operand  for unary op: ',a)
 9810 format(a,' operands for binary op: ',a)
c
      end
