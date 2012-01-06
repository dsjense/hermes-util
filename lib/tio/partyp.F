      integer function partyp (
c ... INPUT
     1 param )
c
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
c ... Determine the type of the supplied parameter, either INT, REA,
c     DUBL, ALFA or NUL
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       param   -  Input parameter
c
      character*(*) param
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and common block decks:
c
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer  lennb
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS and variables
c
c       LDNGSN  -  Current parameter status is 'LEADING SIGN'
c       LDNGPR  -  Current parameter status is 'LEADING PERIOD'
c       LDSNPR  -  Current status is 'LEADING SIGN AND PERIOD'
c
      integer    LDNGSN,   LDNGPR,   LDSNPR
      parameter (LDNGSN=6, LDNGPR=7, LDSNPR=8)
c
c       noxpch  -  .TRUE. if we have not already hit exponent character
c                         ('e' or 'd') when processing a REAL or DOUBLE
c       noxpsn  -  .TRUE. if we have not already hit exponent sign when
c                         processing a REAL or DOUBLE
c       noxpi   -  .TRUE. if we have not already hit at least one
c                         exponent digit when processing a REAL or DOUBLE
      integer     i, lnpar
      logical     noxpch, noxpsn, noxpi
c
c=======================================================================
c=======================================================================
c
      lnpar  = lennb(param)
      partyp = NUL
c
c ... Process characters in parameter
c
      noxpch = .TRUE.
      noxpsn = .TRUE.
      noxpi  = .TRUE.
c
      do 100 i=1,lnpar
c
c ..... PLUS or MINUS sign
c
        if (param(i:i).EQ.'+' .OR. param(i:i).EQ.'-') then
c
          if (partyp.EQ.NUL .OR. partyp.EQ.LDNGSN) then
            partyp = LDNGSN
          elseif ( noxpsn .AND. .NOT. noxpch ) then
            noxpsn = .FALSE.
          else
            go to 200
          endif
c
c ..... PERIOD
c
        elseif (param(i:i) .EQ. '.') then
c
          if (partyp .EQ. NUL) then
            partyp = LDNGPR
          elseif (partyp .EQ. INT) then
            partyp = REA
          elseif (partyp .EQ. LDNGSN) then
            partyp = LDSNPR
          else
            go to 200
          endif
c
c ..... DIGIT
c
        elseif (lge(param(i:i),'0') .AND. lle(param(i:i),'9')) then
c
          if (partyp.EQ.NUL .OR. partyp.EQ.LDNGSN) then
            partyp = INT
          elseif (partyp.EQ.LDNGPR .OR. partyp.EQ.LDSNPR) then
            partyp = REA
          else if ( .NOT. noxpch ) then
c ......... exponent sign is implicitly '+'
            noxpsn = .FALSE.
            noxpi  = .FALSE.
          endif
c
c ..... EXPONENT CHARACTER
c
        elseif (partyp.LE.REA .AND. noxpch) then
c
          if (param(i:i).EQ.'e' .OR. param(i:i).EQ.'E') then
            partyp = REA
            noxpch = .FALSE.
          elseif (param(i:i).EQ.'d' .OR. param(i:i).EQ.'D') then
            partyp = DUBL
            noxpch = .FALSE.
          else
            go to 200
          endif
c
c ..... Must be ALFA
c
        else
          go to 200
        endif
c
  100 continue
c
      if ( partyp.GE.LDNGSN .OR.
     &    (partyp.LE.DUBL .AND. .NOT.noxpch .AND. noxpi) ) partyp = ALFA
      return
c
c ... Parameter type is ALFA
c
  200 continue
      partyp = ALFA
      return
c
      end
