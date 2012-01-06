! SYMBOL.CMD: Command file for TIOLIB symbol processing
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

!-----------------------------------------------------------------------
! ... Arithmetic symbols
!-----------------------------------------------------------------------

^def  ia 2
^def  ib 3
^def  ra 4.0
^def  rb 6.0

cmd1  "$ia $ib $ra $rb" -$ib -$ra

^def  sum $ia $ib +
^def  dif $ia $ib -
^def  mul $ia $ib *
^def  div $ia $ib /

cmd1  int_ops  $sum $dif $mul $div

^def  sum $ra $rb +
^def  dif $ra $rb -
^def  mul $ra $rb *
^def  div $ra $rb /

cmd1  real_ops  $sum $dif $mul $div

^def  sum $ia $rb +
^def  dif $ra $ib -
^def  mul $ia $rb *
^def  div $ra $ib /

cmd1  mixed_ops  $sum $dif $mul $div

^def  c $ia $rb + $ra $ib - *
^def  d $ia -$rb 2 ^ +

cmd1  multi_ops  $c $d

^pause

!-----------------------------------------------------------------------
! ... String symbols
!-----------------------------------------------------------------------

^def str1 hello
^def str2 world
^gdef str3 "$str1 $str2"
cmd2  str1 $str1 \$str1 "$str1" '$str1' $str3 "$str3"

^pause

!-----------------------------------------------------------------------
! ... Show defined symbols. Note that the only symbol defined on return 
!     is STR3, since it is defined as a global symbol

!!!^show sym
