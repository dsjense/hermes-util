! SYMBOL2.CMD: Second Command file for TIOLIB symbol processing:
!              Test token extraction (based on a DBS test file)
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

^define x      'a b 1.0 1.d0 44'
^define d_bad  $x 4 token
^define y      "$x" 1 token
^define z      "$x" 3 token
^define d      "$x" 4 token
^define i      "$x" 5 token

cmd1 $x  "$x"
cmd1 $y $z $d $i

^define p1 $i 1 +

cmd1 $p1

^define qq $y 1 token
^define pp $y 2 token

cmd1 qq: -${qq}-
cmd1 pp: -${pp}-

^define ff $i 1 token
^define gg $i 2 token
^define hh $ff 10 \*

cmd1 ff: -${ff}-
cmd1 gg: -${gg}-
cmd1 hh: -${hh}-

!!!^show sym
