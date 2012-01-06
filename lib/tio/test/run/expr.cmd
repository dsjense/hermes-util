! EXPR.CMD: Illustrate new TIO expression features
!
! $Id$
! 
! Copyright (2008) Sandia Corporation. Under the terms of
! Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
! Government retains certain rights in this software.
! 
! Hermes is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License as
! published by the Free Software Foundation, either version 3 of
! the License, or (at your option) any later version.
! 
! Hermes is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
! 
! You should have received a copy of the GNU Lesser General
! Public License along with Hermes.  If not, see
! <http://www.gnu.org/licenses/>.
! 
!
! The new version of TIO supports inline expressions, with the syntax:
!    $expr_string ,  $(expr_string) , OR    ${expr_string}
! In the first form, the parser will keep loading expression tokens
! until it runs into a delimiter (space, tab, user-delimiter, end-of-line).
! In the second and third forms, the parser loads expression tokens
! until it runs into the matching ) or }, skipping over white space.
! the string (spaces, tabs, and additional user delimiters).
!
! An expression string can be a simple variable name, or an INLINE
! expression. In the latter case, DO NOT put a $ in front of a variable
! name; any unquoted name is already assumed to be a variable name.
!
! INLINE expressions can be used anywhere. The expression parser converts
! the string to RPN format, and uses the original RPN calculator

! Define symbols a1 and a2 the old and new ways. Note that in the
! definition of a2, k0 & k1 DO NOT have $ in front.

^def k0 2
^def k1 3
^def a1 $k0 1.0 + $k1 1 - *    ! a1 & a2 have IDENTICAL values, because
^def a2 $( (k0+1.0) * (k1-1) ) ! they are evaluated with the same code

! The inline form can all be used directly in a command. The first inline
! token illustrates the use of BRACEs. The second illustrates that no
! parentheses or braces are needed if there is no white space

cmd1 $a1 $a2 ${(k0+1.0) * (k1-1)} $k0+4.0

! In the expression parser, unquoted strings are assumed to be variable
! names. To insert string constants, they have to be quoted. Note that
! TIO already has easier ways to concatenate strings, especially if
! they have spaces in them.

^def str0 ' World'
^def str1 "$('Hello' + str0)" ! Using the expression parser
^def str2 "Hello$str0"        ! The OLD way is cleaner
cmd1 "$str1" "$str2"

! A few new functions have been added:-
!   UNARY             BINARY
!   -----             ------
!    CHS               MIN
!    SQRT              MAX
!    EXP
!    LN
!
! The CHS operator was not really needed in the old version TIO, but 
! was added to handle leading signs in expressions

^def i    $(( ((1)+((2))) * (-(-3+1)) )) ! legal, but inefficient
^def x    $(2e-7*ln(2.5/2)) 
^def y    $sqrt(4)   ! Note that sqrt(INT) results in a REAL
^def z    2.0 exp    ! These functions also work the old way
^def logz $ln(z)

cmd1 $i $x $y $z $logz

^def min $(5 min 3) ! The syntax for ALL binary operators is
^def max $(5 max 3) ! (operand1 OPERATOR operand2)

cmd1 $min $max

! Feb. 2005: TIO now handles trig functions -- sin, cos, tan.
!            and also their inverses -- asin, acos, atan

^def pi 3.141592654
^def s1 $sin(pi/3)
^def c1 $cos(pi/3)
^def t1 $tan(pi/4)

cmd1 $s1 $c1 $t1

^def as1 $(asin(s1)/pi)
^def ac1 $(acos(c1)/pi)
^def at1 $(atan(t1)/pi)

cmd1 $as1 $ac1 $at1
