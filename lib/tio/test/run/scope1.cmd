! SCOPE1.CMD: Command file to demonstrate LOCAL versus GLOBAL
!             symbols. It works in concert with SCOPE2
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
! Define a few local and global symbols here

cmd1 '-------------------- level-1 -----------------------'

^gdef  ga  'ga-orig'
^gdef  gb  [g1 g2 g3]

^def   a   'a-0'
^def   b   [b1 b2 b3]

^show sym

cmd1 $a $b[1] $ga $gb[2]

! undefine ga, redefine gb to scalar, define gc

^run scope2.cmd

cmd1
cmd1 '-------------------- level-1 -----------------------'

! Show symbols known to this command file level

^show sym

cmd1 $a $b[2] $gb $gc
cmd1 $ga  ! should be an error: ga was undefined by scope2.cmd
