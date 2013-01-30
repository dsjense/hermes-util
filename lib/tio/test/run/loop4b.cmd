! LOOP4B.CMD: Check 2 level-2 FOR loops inside a single level-1 loop
!             with multiple file levels
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
! ... Calling sequence: ^r LOOP4B.CMD max-outer max-inner1 max-inner2 string0

!  Define local symbols from the passed arguments

^def max_o    $1
^def max_i1   $2
^def max_i2   $3
^def string0 "$4"

^for i 1 $max_o
  ^def string1 ${string0}_lev1
  ^def string2 ${string0}_lev2
  ^def max_ob  $((max_o)-1  MAX 0)
  ^def max_i1b $((max_i1)-1 MAX 0)
  ^def max_i2b $((max_i2)-1 MAX 0)

! first inner loop
  cmd1 $(10\*i) 0.0 ${string1}_iloop1
  ^for j 1 $max_i1
    ^run loop4.cmd $max_ob $max_i1b $max_i2b $string2
  ^endfor

! second inner loop
  cmd1 $(10\*i) 0.0 ${string1}_iloop1
  ^for j 1 $max_i2
    ^run loop4.cmd $max_ob $max_i1b $max_i2b $string2
  ^endfor
^endfor
