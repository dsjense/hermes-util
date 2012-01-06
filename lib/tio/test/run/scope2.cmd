! SCOPE2.CMD: Command file to demonstrate LOCAL versus GLOBAL
!             symbols. It works in concert with SCOPE1
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
cmd1
cmd1 '-------------------- level-2 -----------------------'

! Modify global symbol gb, and add gc

^gdef  gb  'gb-mod'
^gdef  gc  'gc'

! Add some local symbols

^def   a   [a1 a2]
^def   c   'a string'

! Display defined symbols known this command level

^show sym

cmd1 $a[0] c $ga $gb $gc
cmd1 $b   ! should be an error: b local to level 1

! delete global symbol ga

^undef ga
