! TEST_ALL.CMD: Run through basic TIO tests
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
! ... Calling sequence: ^r TEST_ALL.CMD

!  Set up TIO for processing:-
!    1. Continue command file execution even with TIO errors
!    2. Only echo APPLICATION (TESTIO) commands

^cmferr continue
^run    cmdonly.cmd

! Basic command file execution

cmd1 '----------------------------------------'
cmd2 "*** Running SAMPLE.CMD ***"
^run sample.cmd

! Symbol definition

cmd1 '----------------------------------------'
cmd2 "*** Running SYMBOL.CMD ***"
^run symbol.cmd

cmd1 '----------------------------------------'
cmd2 "*** Running SYMBOL2.CMD ***"
^echo on 1 1   ! Turn on echoing of TIO command lines for SYMBOL2.CMD,
               ! since it deliberately has TIO errors
^run symbol2.cmd
^echo on 1 0   ! Turn off TIO echoing again

! Loops

cmd1 '----------------------------------------'
cmd2 "*** Running LOOP.CMD ***"
^run loop.cmd   3  0.1  "loop iteration #"

cmd1 '----------------------------------------'
cmd2 "*** Running LOOPX.CMD ***"
^run loopx.cmd   3  0.1  "loop iteration #"

cmd1 '----------------------------------------'
cmd2 "*** Running LOOP2.CMD ***"
^run loop2.cmd 2 3  0.1  "string"

cmd1 '----------------------------------------'
cmd2 "*** Running LOOP3.CMD ***"
^run loop3.cmd   4  "new_string"

cmd1 '----------------------------------------'
cmd2 "*** Running LOOP4B.CMD ***"
^run loop4b.cmd  2 2 2 iter

cmd1 '----------------------------------------'
cmd2 "*** Running LOOP4B.CMD (zero trip 1) ***"
^run loop4b.cmd  0 2 2 iter

cmd1 '----------------------------------------'
cmd2 "*** Running LOOP4B.CMD (zero trip 2) ***"
^run loop4b.cmd  2 2 1 iter

cmd1 '----------------------------------------'
cmd2 "*** Running LOOP4B.CMD (zero trip 3) ***"
^run loop4b.cmd  2 0 2 iter

! IF-statements

cmd1 '----------------------------------------'
cmd2 "*** Running IF1.CMD ***"
^run if1.cmd 1 eq 1

cmd1 '----------------------------------------'
cmd2 "*** Running IF1.CMD ***"
^run if1.cmd 1 eq 2

^gdef xxx 1

cmd1 '----------------------------------------'
cmd2 "*** Running IF1A.CMD ***"
^run if1a.cmd xxx
cmd1
cmd2 "*** Running IF1B.CMD ***"
^run if1b.cmd xxx

^undef xxx
cmd1
cmd2 "*** Running IF1A.CMD ***"
^run if1a.cmd xxx
cmd1
cmd2 "*** Running IF1B.CMD ***"
^run if1b.cmd xxx

cmd1 '----------------------------------------'
cmd2 "*** Running IF2.CMD ***"
^run if2.cmd 2 gt 1 and 3.1 lt 3.2
cmd1
cmd2 "*** Running IF2.CMD ***"
^run if2.cmd 2 gt 1 or 3.1 lt 3.0
cmd1
cmd2 "*** Running IF2.CMD ***"
^run if2.cmd 1 gt 2 or 3.1 lt 3.0

cmd1 '----------------------------------------'
cmd2 "*** Running IF3.CMD ***"
^run if3.cmd 2 gt 1

cmd1 '----------------------------------------'
cmd2 "*** Running IF4.CMD ***"
^run if4.cmd 1 gt 2

cmd1 '----------------------------------------'
cmd2 "*** Running IF5.CMD ***"
^run if5.cmd 2 gt 1 3 lt 7

cmd1 '----------------------------------------'
cmd2 "*** Running IF6.CMD ***"
^run if6.cmd 2 gt 1 3 lt 7

! Tests for NEW TIO features (Feb. 03)

! Symbol scope

cmd1 '----------------------------------------'
cmd2 "*** Running SCOPE1.CMD ***"
^run scope1.cmd
^show sym
^undef gb gc
^show sym

! Expressions

cmd1 '----------------------------------------'
cmd2 "*** Running EXPR.CMD ***"
^run expr.cmd

! Precision

cmd1 '----------------------------------------'
cmd2 "*** Running PRECISION.CMD ***"
^run precision.cmd

! Arrays

cmd1 '----------------------------------------'
cmd2 "*** Running ARRAY.CMD ***"
^run array.cmd

! New command file parameter linkage

! Pass by value with named parameters

cmd1 '----------------------------------------'
cmd2 "*** Running PARAM1.CMD ***"
^run param1.cmd a b c

! Pass by reference:

cmd1 '----------------------------------------'
cmd2 "*** Running PARAM2.CMD ***"
^def a 3
^run param2.cmd @a @b  ! This defines local sym 'b'
cmd1 $a $b

^def c [1 10 100]
^run param2.cmd @c[0] @c[2] ! Redefine array elements
cmd1 $c

^run param2.cmd @a @c ! This redefines c as a scalar
cmd1 $a $c

^undef a b c

! Pass by reference over multiple levels

cmd1 '----------------------------------------'
cmd2 "*** Running PARAM3.CMD ***"
^def a 2
^run param3.cmd @a @b ! calls PARAM2.CMD
cmd1 $a $b

! Build local arrays in pass-by-reference command files

cmd1 '----------------------------------------'
cmd2 "*** Running BLDARR.CMD ***"
^def n 5
^run bldarr.cmd $n @arr
^def nlab "n = ${n}:"
cmd1 $nlab $arr

! Check command file error handling

^echo on 1 1   ! Turn on echoing of TIO command lines for these tests,
               ! since we deliberately have errors

! Make sure that if there is a problem setting up named parameters
! in a command file, we abort the file, EVEN if cmferr = 'continue'

cmd1 '----------------------------------------'
cmd2 "*** Running PARAM2.CMD with errors***"
^run param2.cmd    ! This is an error: param2 needs 2 parameters

! Now set error handling to abort any file if there is an error
^cmferr close_cur  ! Close only current command file on error

cmd1 '----------------------------------------'
cmd2 "*** Running ERR.CMD ***"
^r err.cmd
cmd1 'Still running after errors in ERR.CMD'

! Finally, set error handling to abort ALL command file levels
! DO THIS TEST LAST: because it will shut down THIS file!
^cmferr close_all  ! Close all command file levels on error

cmd1 '----------------------------------------'
cmd2 "*** Running ERR2.CMD ***"
^r err.cmd

! This next line should not be executed !!!!
cmd1 'It is an error to see this line!!!'
