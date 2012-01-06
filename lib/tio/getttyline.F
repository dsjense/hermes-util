      subroutine getttyline (
c ... OUTPUT
     2 line,
c ... INPUT/OUTPUT
     3 ierr)
c
c***********************************************************************
c     $Id$
c     
c     Copyright (2008) Sandia Corporation. Under the terms of
c     Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
c     Government retains certain rights in this software.
c     
c     Hermes is free software: you can redistribute it and/or modify
c     it under the terms of the GNU Lesser General Public License as
c     published by the Free Software Foundation, either version 3 of
c     the License, or (at your option) any later version.
c     
c     Hermes is distributed in the hope that it will be useful, but
c     WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU Lesser General Public License for more details.
c     
c     You should have received a copy of the GNU Lesser General
c     Public License along with Hermes.  If not, see
c     <http://www.gnu.org/licenses/>.
c     
C_Groups @(#)
c***********************************************************************
c
c     Returns A SINGLE input line from the terminal input unit (luttyi).
c
c-----------------------------------------------------------------------
c
c ... Function arguments:
c
c     INPUT: none
c
c     OUTPUT:
c       line    -  Input line
c
c     INPUT/OUTPUT:
c       ierr    -  INPUT:  Do nothing if ierr.NE.0
c                  OUTPUT: Error flag:-
c                            0 = normal return
c                           -1 = End-of-File on input
c                           -2 = Error on read
c                           -3 = terminal input not available
c
      character*(*) line
      integer       ierr
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "iolun.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer       lennb, tioerh
c
c-----------------------------------------------------------------------
c
c ... Local variables:
c
      integer       nchars
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      nchars = 0
      line = ' '

      if (ierr .NE. 0) return
c
c ... Terminal input unit must be available
c
      if (luttyi .LT. 0) then
        ierr = -3
        return
      endif
c
c-----------------------------------------------------------------------
c ... Get line
c-----------------------------------------------------------------------
c
#ifdef USE_IDL_INPUT_INTERFACE
c ... Use callable IDL to read input line
      call send_command('__TIOstr=" "&read,prompt="",__TIOstr',ierr)
      if ( ierr.eq.0 ) call get_str_var('__TIOstr',line,nchars,ierr)
      if ( ierr.ne.0 ) ierr = -1
#else
c ... Use Fortran runtime library to read input line
      read(luttyi,'(a)',end=800,err=9000) line
      return
c
c ... EOF on input
  800 continue
      ierr = -1
      return
c
c ... ERROR conditions and messages
 9000 continue
      ierr = tioerh ('TIO', 'GETSLTTY', 2, 'Error on read', -2)
#endif

      return
c
      end
