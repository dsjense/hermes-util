      subroutine iostch (
c ... INPUT
     1 chrtyp, uchar,
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
c ... Set TIOLIB control characters.  CHRTYP is a switch selecting
c     which character to change:-
c
c       COMMENT       -  Comment character
c       CONTINUATION  -  Line continuation character
c       IO_COMMAND    -  I/O command control character
c       AUXILIARY     -  User auxiliary command control characters
c
c     Note that for the auxiliary characters, AUXILIARY by itself
c     selects command # 1.  For command sets 1 - MXAUXC, the character
c     can be set using the syntax CHRTYP='AUX(i)'
c
c     The value for the new character is in CHRVAL.  It is checked
c     against the list of TIOLIB reserved characters
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
c                    -1  -  Illegal character value (reserved)
c                    -2  -  Illegal character type
c                    -3  -  Bad syntax or illegal index encountered
c                           processing AUX character type
c
      character*(*) chrtyp, uchar
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
      integer   chrtoi, tioerh
      logical   strmch
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character errmsg*132
      integer   iaux
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
c
      status = 0
c
c ... Check that character is not a reserved character
c
      if ( index (rsrvdc(1:nrsvdc), uchar(1:1)) .GT. 0) then
        write(errmsg,9800) uchar
        status = tioerh ('TIO', 'IOSTCH', 2, errmsg, -2)
        return
      endif
c
c ... And set character
c
      if (strmch(chrtyp,'comment')) then
        cmtchr = uchar
      elseif (strmch(chrtyp,'continuation')) then
        conchr = uchar
      elseif (strmch(chrtyp,'io_command')) then
        cmdchr(1:1) = uchar
      elseif (strmch(chrtyp,'aux')) then
        if (chrtyp(4:4).NE.'(') then
          iaux = 1
        else
          iaux = chrtoi(chrtyp(5:5),status)
          if (status.LT.0 .OR. iaux.GT.ncmdch-1) then
            write(errmsg,9820) chrtyp
            status = tioerh ('TIO', 'IOSTCH', 2, errmsg, -3)
            return
          endif
        endif
        cmdchr(1+iaux:1+iaux) = uchar
      else
        write(errmsg,9810) chrtyp
        status = tioerh ('TIO', 'IOSTCH', 2, errmsg, -1)
      endif
      return
c
c
c=======================================================================
c     ERROR messages
c=======================================================================
c
 9800 format('Reserved TIOLIB character: ',a)
 9810 format('Unknown character type: ',a)
 9820 format('Setting auxiliary command char.: ',a)
c
      end
