      subroutine iostca (
c ... INPUT
     1 ucase,
c ... OUTPUT
     2 status )
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
c ... Set TIOLIB case conversion control based on values of UCASE:
c
c       L*OWER - Convert all non-quoted characters to LOWER case
c       U*PPER - Convert all non-quoted characters to UPPER case
c       N*ONE  - No case conversion
c
c     Any other value of case results in an error
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       ucase  -  User case
c
c     OUTPUT:
c       status  -  Status flag:-
c                     0  -  Normal return
c                    -1  -  Illegal value for user case
c
      character*(*) ucase
      integer       status
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and common block decks:
c
#include "ioctrl.inc"
#include "iolun.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      logical  strmch
      integer  tioerh
c
c-----------------------------------------------------------------------
c
      character errmsg*132
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
c
      status = 0
c
      if (strmch(ucase,'lower')) then
        case   = 'lower'
        lwrcas = .TRUE.
        uprcas = .FALSE.
      elseif (strmch(ucase,'upper')) then
        case   = 'upper'
        lwrcas = .FALSE.
        uprcas = .TRUE.
      elseif (strmch(ucase,'none')) then
        case   = 'none'
        lwrcas = .FALSE.
        uprcas = .FALSE.
      else
        write(errmsg,9800) ucase
        status = tioerh ('TIO', 'IOSTCA', 2, errmsg, -1)
      endif
c
      return
c
c
c=======================================================================
c     ERROR messages
c=======================================================================
c
 9800 format('Illegal value for CASE: ',a)
c
      end
