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
c ... Common block for defining operations supported by the TIO
c     calculator for evaluating expressions. The operators currently
c     supported are:-
c
c        Unary
c        -----
c        INT    Convert current stack element to INTEGER (truncation)
c        NINT   Convert current stack element to NEAREST INTEGER
c        REA    Convert current stack element to REAL
c        CHS    Change sign
c        SQRT   Square root
c        EXP    Exponential
c        LN     Natural logarithm
c        SIN    sine
c        COS    cosine
c        TAN    tangent
c        ASIN   arc sine
c        ACOS   arc cosine
c        ATAN   arc tangent
c
c        Binary:
c        ------
c        + - * /  Basic binary arithmetic
c          ^      Raise to power
c          MOD    Return modulus
c          MIN    minimum
c          MAX    maximum
c         TOKEN   Return nth token of blank-delimited string
c
c-----------------------------------------------------------------------
c
c     MBINOP   - # binary operators
c     MUNOP    - # unary operators
c
      integer    MBINOP, MUNOP
      parameter (MBINOP = 9)
      parameter (MUNOP  = 13)
c
c     BINARYOP - TIO type index for BINARY operator
c     UNARYOP  - TIO type index for UNARY  operator
c
      integer      BINARYOP, UNARYOP
      parameter  ( BINARYOP = 11 )
      parameter  ( UNARYOP  = 12 )
c
c     binop    - (*) Binary operator names: Note there is an order
c                    dependence -- put all single-char operators first
c     unop     - (*) Array of unary operator names
c     schbinop - (*) character variable containing a concatenated
c                    string of all single-char binary operators
c
      character*8 binop(1:MBINOP)
      character*8 unop(1:MUNOP)
      character*8 schbinop
c
      common /calcopa/ binop, unop, schbinop
c
c     nschbop  - # single character binary operators
c     precbop  - (*) Precedence level of each binary operator
c
      integer nschbop
      integer precbop(1:MBINOP)
c
      common /calcopn/ nschbop, precbop
c
c-----------------------------------------------------------------------
