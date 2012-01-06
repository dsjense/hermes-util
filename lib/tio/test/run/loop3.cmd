! LOOP3.CMD: Check FOR loops running simultaneously at different
!            command file levels.  This routine loops from 1 to MAX,
!            calling LOOP.CMD with loop argument as MAX loop argument
!            for LOOP.CMD.
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
! ... Calling sequence: ^r LOOP3.CMD max string0

!  Define local symbols from the passed arguments

^def max    $1
^def string0 "$2"

^for i 1 $max
  ^def string "$string0" $i +
  ^run loop.cmd $i 0.0 "$string"
^endfor
