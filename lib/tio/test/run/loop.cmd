! LOOP.CMD: Check command file parameter passing and FOR loops
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
! ... Calling sequence: ^r LOOP.CMD nloop fnum string

!  Define local symbols from the passed arguments

^def nloop  $1
^def fnum   $2
^def string "$3"      ! NOTE: This ensures that symbol STRING is 
                      ! correctly defined if $3 includes spaces

! Execute the loop

^for i 1 $nloop
  ^def fnum2 $fnum $i +
  ^def string2 "$string" '_' + $i +
  cmd1 $i $fnum2 "$string2"
  ^pause
^endfor
