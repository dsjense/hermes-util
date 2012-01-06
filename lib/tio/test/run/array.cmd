! ARRAY.CMD: Illustrate new TIO array features
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
! You build arrays in one of two ways. The most direct way is to
! explicitly define the elements on the ^define or ^gdefine line
! The second example shows that the elements can be expressions,
! and that there is no global type of array -- it is an array of
! strings that only has a type for certain contexts

^def  arr1  [ 1 2 3 ]
^gdef arr2  [ $(10+2) $(20-3) 'ENDarr2' ]

! The second way defines an array of n undefined elements. To use
! it, you must then explicitly define an element before using it.

^def arr3  [3]   ! define an array of three elements

! The following loop defines the elements of ARR3. NOTE the following:-
!   * Array indices go from 0 to max-1
!   * The LHS expression cannot have any white space in the 'NAME[INDEX]'
!     construct (it must parse as a single token).

^for i 0 2
  ^def arr3[$i] $(10*arr1[i]+1)
^endfor

! Subscripted array elements are treated as simple scalar symbols. If
! you use the special construct $ARRAY_NAME, all elements will be output
! to the command, delimited by spaces

cmd1 $(arr1[2]+arr1[0]) $arr2 $arr3[1]

! Arrays names are treated just like symbols. If you redefine a name,
! the old value is removed and replaced with the new one: a scalar
! can replace an array, and vice versa

^gdef arr2 'new-value'
^gdef sym  1
^gdef sym  [10 20 30]

cmd1 $arr2 $sym

! To handle arrays of unknown length, there is a special function
! to put the length of an array into a symbol. You cannot use the
! n_elements() function in a complex INLINE expression or RPN construct.
! It can only be used exactly as follows...

^def nelarr1 $n_elements(arr1)
cmd1 '# of elements in array ARR1 = ', $nelarr1

! Delete global arrays (all local symbols deleted on exit)

^undef arr2 sym
