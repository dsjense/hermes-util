      subroutine substexpr (
c ... INPUT
     &  line, lenlin,
c ... INPUT/OUTPUT
     &  ic, oc, ierr)
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
c     SUBSTEXPR is used by subroutine XPNDLN to evaluate an expression
c     in the user input lines (concatenated into the single character
c     variable LINE) and substitute its value into the input character
c     buffer (UINPTC in common block IOINLN).
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       line   -  Concatenated user input line
c       lenlin -  Non-blank length of input line
c
c     INPUT/OUTPUT:
c       ic     -  XPNDLN's pointer to last character processed in the
c                 input line
c       oc     -  XPNDLN's pointer to last character loaded in the
c                 input character buffer
c
c     OUTPUT:
c       ierr  -  Error status on return:-
c                   0 = normal return
c                  -3 = Error evaluating expression
c
      character*(*) line
      integer    ic, ierr, lenlin, oc
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and Common block decks:
c
#include "iocalcop.inc"
#include "ioctrl.inc"
#include "ioinln.inc"
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls:
c
      integer           lennb, lsteql, partyp, tioerh
      logical           streql
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERs and variables:
c
      character*4  EXPONCHR
      integer      MXTOKEN
      parameter   (EXPONCHR = 'eEdD')
      parameter   (MXTOKEN  = 50)
c
c ... Expression token types: Note that this is a local list augmenting
c     internal TIO variable types
c
      integer  BEGIDXEXPR, BEGSUBEXPR, OPERAND
      integer  UNKNOWN
      parameter (BEGSUBEXPR = 21)
      parameter (BEGIDXEXPR = 22)
      parameter (OPERAND    = 18)
      parameter (UNKNOWN    = -1)
c
      character*132     errmsg
      character*132     stkstr(1:MXTOKEN), tokenstr(1:MXTOKEN)
      integer           pendbop(1:10), penduop(1:10)
      integer           nextoken(1:MXTOKEN), tokenidx(1:MXTOKEN),
     &                  tokenlev(1:MXTOKEN), tokentyp(1:MXTOKEN)
      integer           stkival(1:MXTOKEN), tokival(1:MXTOKEN)
      integer           stknch(1:MXTOKEN) , stktyp(1:MXTOKEN)
      integer           braklev, brcelev, parlev, expsn, litstrg,
     &                  mxtoklev, nchval, nchtok, ntoken, tc,
     &                  tok, toklev, varidx, vartyp, xc
      integer           ibop, iuop, nbop, nopr, nstk, nuop, toki
      integer           arridx, nelvar, status, typexpr
      double precision  stkdval(1:MXTOKEN), tokdval(1:MXTOKEN)
c
c
c=======================================================================
c *** BEGIN: Process the expression substring in the input line, loading
c            expression tokens into local arrays
c=======================================================================
c
      if (ierr .NE. 0) return
c
      mxtoklev = 0
      toklev  = 0
      parlev  = 0
      braklev = 0
      litstrg = 0
c
      ntoken  = 0
      nchtok  = 0
c
c ... Loop over characters in expression string. XPNDNLN's input char.
c     pointer is currently sitting on the expression eval. character.
c     If the next character is a '{', move pointer forward, and set
c     the BRACE level counter to 1.
c
      if (line(ic+1:ic+1) .EQ. LBRACE) then
        ic = ic + 1
        brcelev = 1
      else
        brcelev = 0
      endif
c
c ... Loop one character past the end of the line, to ensure that
c     we hit a trailing delimiter
c
      do 10 tc=ic+1,lenlin+1
        xc = tc
c
c ..... QUOTE character
c
        if (line(xc:xc) .EQ. SNGLQT) then
          if (litstrg .EQ. 1) then
            litstrg = 0
          else
            litstrg = 1
            nchtok  = 0
            ntoken  = ntoken + 1
            tokenlev(ntoken) = toklev
            tokenstr(ntoken) = ' '
            tokentyp(ntoken) = ALFA
          endif
          go to 10
        endif
c
        if (litstrg .EQ. 1) then
          nchtok = nchtok + 1
          tokenstr(ntoken)(nchtok:nchtok) = line(xc:xc)
        else
c
c ....... Left parenthesis
c
          if (line(xc:xc) .EQ. LPAREN) then
            parlev = parlev + 1
            toklev = toklev + 1
            nchtok = 0
            ntoken = ntoken + 1
            tokentyp(ntoken) = BEGSUBEXPR
            tokenlev(ntoken) = toklev
c
c ....... Left bracket
c
          elseif (line(xc:xc) .EQ. LBRAKT) then
            braklev = braklev + 1
            toklev  = toklev + 1
            nchtok  = 0
            ntoken  = ntoken + 1
            tokentyp(ntoken) = BEGIDXEXPR
            tokenlev(ntoken) = toklev
c
c ....... Right parenthesis: If this matches a leading left-paren,
c         then it terminates the expression
c
          elseif (line(xc:xc) .EQ. RPAREN) then
            if (parlev .GT. 0) then
              parlev = parlev - 1
              toklev = toklev - 1
              nchtok = 0
              if (toklev.EQ.0 .AND. brcelev.EQ.0) go to 100
            else
              write(errmsg,9800) 'unmatched parentheses'
              ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
              return
            endif
c
c ....... Right bracket. If it is has a matching left bracket, treat it
c         as part of the expression.  If not, we treat it as a terminating
c         delimiter to handle expressions used as array indices, but 
c         without the array element value being substituted, e.g. a[$i].
c         In this case we must back up the input character pointer so that
c         XPNDLN will reprocess the right bracket and add it to the token.
c
          elseif (line(xc:xc) .EQ. RBRAKT) then
            if (braklev .GT. 0) then
              braklev = braklev - 1
              toklev  = toklev - 1
              nchtok  = 0
            else
              xc = xc - 1
              go to 100
            endif
c
c ....... Right brace (only valid if expression begins with left brace)
c
          elseif (line(xc:xc) .EQ. RBRACE) then
            if (brcelev .EQ. 1) then
              brcelev = 0
              go to 100
            else
              write(errmsg,9800) 'unmatched braces'
              ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
              return
            endif
c
c ....... Single-character binary operator
c
          elseif (index (schbinop(1:nschbop),line(xc:xc)) .NE. 0) then
c
c ......... If the character is a '+' or '-' sign, we must handle the case
c           that the sign is part of the exponent of a floating constant
c
            expsn = 0
            if ( (line(xc:xc).EQ.'+' .OR. line(xc:xc).EQ.'-') .AND.
     &           nchtok.GE.2 ) then
              if (index (EXPONCHR,line(xc-1:xc-1)) .NE. 0) then
                if ( partyp(tokenstr(ntoken)(1:nchtok-1)) .LT. ALFA)
     &            expsn = 1
              endif
            endif
c
            if (expsn .EQ. 0) then
              ntoken = ntoken + 1
              tokentyp(ntoken) = BINARYOP
              tokenidx(ntoken) = lsteql (line(xc:xc), binop, MBINOP)
              tokenlev(ntoken) = toklev
              tokenstr(ntoken) = line(xc:xc)
              nchtok = 0
            else
              nchtok = nchtok + 1
              tokenstr(ntoken)(nchtok:nchtok) = line(xc:xc)
            endif
c
c ....... Delimiter.  If this terminates the expression, back up the
c         input character pointer so that XPNDLN will reprocess it
c
          elseif (index (delim(1:ndelim+1),line(xc:xc)) .NE. 0) then
            nchtok = 0
            if (parlev.EQ.0 .AND. braklev.EQ.0 .AND.
     &          line(ic:ic).NE.LBRACE) then
              xc = xc - 1
              go to 100
            endif
c
c ....... Character in an unquoted token: unknown type as yet
c
          elseif (line(xc:xc) .NE. LITCHR) then
            if (nchtok .EQ. 0) then
              ntoken = ntoken + 1
              tokenlev(ntoken) = toklev
              tokenstr(ntoken) = ' '
              tokentyp(ntoken) = UNKNOWN
            endif
c
            nchtok = nchtok + 1
            tokenstr(ntoken)(nchtok:nchtok) = line(xc:xc)
          endif
c
        endif
c
        mxtoklev = max(mxtoklev, toklev)
   10 continue
c
c ... Legal expression strings must jump to statement 100.  The only
c     way to reach this point must be unbalanced quotes
c
      if (litstrg .EQ. 1) then
        write(errmsg,9800) 'unmatched quotes in expression'
        ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
        return
      endif
c
c ... Complete processing of expression substring
c
  100 continue
      if (parlev.NE.0 .OR. braklev.NE.0 .OR. brcelev.NE.0) then
        write(errmsg,9800)
     &    'unmatched parentheses, braces, and/or brackets'
        ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
        return
      endif
c
      if (ntoken .EQ. 0) then
        write(errmsg,9800) 'No tokens in expression string'
        ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
        return
      endif
c
c ... Set input character pointer to last value processing expression:
c     required for XPNDLN to correctly process the rest of the input line
c
      ic  = xc
c
c-----------------------------------------------------------------------
c ... Handle a single-token expression as a special case
c-----------------------------------------------------------------------
c
c ... The single token must be a simple variable name: substitute it's
c     value directly, and return. This not only speeds up the code, but
c     supports the legacy case of command file parameters being integer
c     values, which cannot be distinguished actual integers in expressions 
c
      if (ntoken .EQ. 1) then
        call fxcase(tokenstr(1), case)
        call iovartyp (tokenstr(1), 0, vartyp, varidx, nelvar)
c
        if (vartyp .GT. 0) then
          call iovarval( vartyp, varidx, -1,
     &                   uinptc(oc+1:), nchval, ierr)
          if (ierr .NE. 0) return
c
          oc = oc + nchval
        else
          write(errmsg,9810) 'Undefined variable: ',
     &      tokenstr(1)(1:lennb(tokenstr(1)))
          ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
        endif
c
        return
      endif
c
c-----------------------------------------------------------------------
c ... Handle the n_elements(var) construct as a 2nd special case
c-----------------------------------------------------------------------
c
c ... The single token must be a simple variable name: substitute it's
c     value directly, and return. This not only speeds up the code, but
c     supports the legacy case of command file parameters being integer
c     values, which cannot be distinguished actual integers in expressions 
c
      if (ntoken .EQ. 3 .AND. streql(tokenstr(1),'n_elements')) then
        call fxcase(tokenstr(3), case)
        call iovartyp (tokenstr(3), 0, vartyp, varidx, nelvar)
        call ljusti (nelvar, 0, uinptc(oc+1:), nchval, ierr)
        oc = oc + nchval
        return
      endif
c
c-----------------------------------------------------------------------
c ... Complete input token list
c-----------------------------------------------------------------------
c
c ... Process 'unknown-type' tokens, identifying their type. For scalar
c     variables, also substitute their equivalence strings now
c
      do tok=1,ntoken
        if (tokentyp(tok) .EQ. UNKNOWN) then
          call fxcase(tokenstr(tok), case)
c
c ....... Unary operator?
c
          if (lsteql(tokenstr(tok),unop,MUNOP) .GT. 0) then
            tokentyp(tok) = UNARYOP
c
          else
c
c ....... Binary operator? Save index of operator to get it's
c         precedence when building the RPN list
c
            tokenidx(tok) = lsteql (tokenstr(tok), binop, MBINOP)
c
            if (tokenidx(tok) .GT. 0) then
              tokentyp(tok) = BINARYOP
c
c ......... User variable or numeric constant
c
            else
c
c ........... If token is of numeric type, it is an operand
c
              if (partyp(tokenstr(tok)) .LT. ALFA) then
                tokentyp(tok) = OPERAND
c
c ........... If it is a string, must be user variable
c
              else
                call iovartyp (tokenstr(tok), 0, vartyp, varidx, nelvar)
c
                if (vartyp .GT. 0) then
c
c ............... If the next token is the start of an index expression,
c                 save the variable type and index, to be evaluated as an
c                 array element when the index has been obtained.
c
                  if (tok.LT.ntoken .AND.
     &                tokentyp(tok+1).EQ.BEGIDXEXPR) then
                    tokentyp(tok) = vartyp
                    tokenidx(tok) = varidx
c
c ................. Variable without an index expression
c                     * For a scalar variable, immediately load tokenstr
c                       with the equivalence string of the variable
c                     * Error condition for an array
c
                  else
                    if (nelvar .EQ. 1) then
                      call iovarval( vartyp, varidx, 0,
     &                               tokenstr(tok), nchval, ierr)
                      if (ierr .NE. 0) return
c
                      tokentyp(tok) = OPERAND
                    else
                      write(errmsg,9800) 'array reference without index'
                      ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
                      return
                    endif
                  endif
                endif
              endif
            endif
c
          endif
c
c ....... For a scalar operand, get operand's type, and associated
c         numeric value(s)
c
          if (tokentyp(tok) .EQ. OPERAND) then
            call bldscaval ( tokenstr(tok),
     &        tokentyp(tok), tokival(tok), tokdval(tok) )
c
          elseif (tokentyp(tok) .EQ. UNKNOWN) then
            write(errmsg,9810) 'Undefined expression token: ',
     &        tokenstr(tok)(1:lennb(tokenstr(tok)))
            ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
          endif
c
        endif
      enddo
c
      if (ierr .NE. 0) return
c
c ... Initialize linked list for the tokens
c
      do tok=1,ntoken
        nextoken(tok) = tok + 1
      enddo
      tokenlev(ntoken+1) = -1
c
c-----------------------------------------------------------------------
c ... Evaluate the expression
c-----------------------------------------------------------------------
c
c ... Recursively evaluate sub-expressions from the highest level
c     down to the lowest
c
      do 299 toklev=mxtoklev,0,-1
        tok = 1
c
c ..... Skip over tokens until we find one at the current
c       subexpression level
c
  200   continue
        if (tokenlev(tok) .LT. 0) then
          go to 299
        elseif (tokenlev(tok) .LT. toklev) then
          tok = nextoken(tok)
          go to 200
        endif
c
c ..... Save the location of the first token: this is where the
c       result of the expression will be put
c
        toki = tok
c
c ..... If first token is BEG*EXPR, skip to the next one.
c
        if (tokentyp(tok).EQ.BEGSUBEXPR .OR.
     &      tokentyp(tok).EQ.BEGIDXEXPR ) then
          tok = nextoken(tok)
          if (tokenlev(tok) .LT. toklev) then
            write(errmsg,9800) 'empty subexpression'
            ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
            return
          endif
        endif
c
c ..... Handle leading signs of subexpressions: replace a leading '-'
c       sign with the unary CHS operator; skip over a leading '+'
c
        if (tokenstr(tok) .EQ. '-') then
          tokenstr(tok) = 'chs'
          tokentyp(tok) = UNARYOP
        elseif (tokenstr(tok) .EQ. '+') then
          tok = nextoken(tok)
          if (tokenlev(tok) .LT. toklev) then
            write(errmsg,9800) 'empty subexpression'
            ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
            return
          endif
        endif
c
c ..... Load subexpression tokens onto the stack
c
        nstk = 0
        nuop = 0
        nbop = 0
        nopr = 0
c
  210   continue
c
c ....... UNARY OPERATOR: Add it to the pending unary operator list
c         to be unrolled when the operand is processed
c
          if (tokentyp(tok) .EQ. UNARYOP) then
            nuop = nuop + 1
            penduop(nuop) = tok
c
c ....... BINARY OPERATOR
c
          elseif (tokentyp(tok) .EQ. BINARYOP) then
c
c ......... Syntax error if we have any pending unary operators
c
            if (nuop .GT. 0) then
              write(errmsg,9800) 'unary operator without operand'
              ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
              return
            endif
c
c ......... If we have pending bin-ops, and the last one on the list
c           has at least the same precedence the one currently being
c           processed, put last pending bin-op on the stack now.
c
  220       continue
            if (nbop .GT. 0) then
              if (precbop(tokenidx(pendbop(nbop))) .GE. 
     &            precbop(tokenidx(tok)) ) then
                if (nopr .GE. 2) then
                  nstk = nstk + 1
                  stkstr(nstk) = tokenstr(pendbop(nbop))
                  stktyp(nstk) = BINARYOP
                  nbop = nbop - 1
                  nopr = nopr - 1
                else
                  write(errmsg,9800) 'binary operator has < 2 operands'
                  ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
                  return
                endif
                goto 220
              endif
            endif
c
c ......... Add this bin-op to the pending list
c
            nbop = nbop + 1
            pendbop(nbop) = tok
c
c ....... OPERAND: unconditionally put it on the stack, followed
c         by any unary operators acting on it (in reverse order)
c
          elseif (tokentyp(tok) .LE. ALFA) then
            nopr = nopr + 1
            nstk = nstk + 1
            stktyp(nstk)  = tokentyp(tok)
            stkstr(nstk)  = tokenstr(tok)
            stkival(nstk) = tokival(tok)
            stkdval(nstk) = tokdval(tok)
c
c ......... Unroll pending unary operators onto the RPN list
c
            if (nuop .GT. 0) then
              do iuop=nuop,1,-1
                nstk = nstk + 1
                stkstr(nstk) = tokenstr(penduop(iuop))
                stktyp(nstk) = UNARYOP
              enddo
              nuop = 0
            endif
c
          endif
c
c ....... Process next token if it is part of the current subexpression
c 
          tok = nextoken(tok)
        if (tokenlev(tok) .EQ. toklev) go to 210
c
c ..... Unroll any pending binary operators onto the stack
c
        do ibop=nbop,1,-1
          if (nopr .GE. 2) then
            nstk = nstk + 1
            stkstr(nstk) = tokenstr(pendbop(ibop))
            stktyp(nstk) = BINARYOP
            nopr = nopr - 1
          else
            write(errmsg,9800) 'binary operator has < 2 operands'
            ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
            return
          endif
        enddo
c
c ..... Final syntax check: the subexpression should result in exactly
c       one operand when evaluated
c
        if (nopr .NE. 1) then 
          write(errmsg,9800) 
     &      'subexpression does not result in 1 operand'
          ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
          return
        endif
c
c ..... Evaluate RPN subexpression
c
        if (nstk .EQ. 1) then
          if (stktyp(1) .GT. ALFA) then
            write(errmsg,9800) 'single non-operand-token expression'
            ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
            return
          endif
c
        else
          call evalexpr (nstk, stktyp , stkstr , 
     &                         stkival, stkdval, stknch, status )
          if (status .NE. 0) then
            ierr = -3
            return
          endif
        endif
c
c-----------------------------------------------------------------------
c ..... Complete handling of subexpression
c-----------------------------------------------------------------------
c
c ..... The end-result of a subexpression must be a simple operand.
c       For an algebraic expression, we already have the value.  For 
c       an array index, the final result is the array element.  In either
c       case, set the resulting OPERAND type, and adjust the linked
c       list of tokens to skip over the tokens just processed for
c       subsequent passes at lower expression levels.
c
  250   continue
c
c ..... Algebraic subexpression: put result in tokenstr(toki), and
c       set token type to stack result, and level to toklev-1
c
        if (tokentyp(toki).EQ.BEGSUBEXPR .OR. toklev.EQ.0) then
          tokentyp(toki) = stktyp(1)
          tokenstr(toki) = stkstr(1)
          tokival(toki)  = stkival(1)
          tokdval(toki)  = stkdval(1)
          tokenlev(toki) = toklev - 1
          nextoken(toki) = tok
c
c ..... Array index subexpression: the array type and number is saved
c       in token toki-1, and is already at expression level toklev-1.
c       Ensure that the subexpression is an integer, and then get
c       the array element. The OPERAND will be in token toki-1
c
        elseif (tokentyp(toki) .EQ. BEGIDXEXPR) then
          if (stktyp(1) .EQ. INT) then
            arridx = stkival(1)
            call iovarval( tokentyp(toki-1), tokenidx(toki-1),
     &        stkival(1), tokenstr(toki-1), nchval, ierr)
            if (ierr .NE. 0) return
c
            call bldscaval ( tokenstr(toki-1),
     &        tokentyp(toki-1), tokival(toki-1), tokdval(toki-1) )
            nextoken(toki-1) = tok
          else
            write(errmsg,9800) 'array index expression not an integer'
            ierr = tioerh ('TIO', 'SUBSTEXPR', 2, errmsg, -3)
            return
          endif
        endif
c
c ..... If we have not reached the final token, return to process any
c       remaining subexpressions at this expression level
c
        if (tokenlev(tok) .GE. 0) go to 200
  299 continue
c
c ... Final result is in token #1, put it into output character buffer
c
      uinptc(oc+1:) = tokenstr(1)
      oc = oc + lennb(tokenstr(1))
c
      return
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9800 format('Syntax error: ',a)
 9810 format(a,1x,a)
c
      end
