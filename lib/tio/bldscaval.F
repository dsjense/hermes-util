      subroutine bldscaval (
c ... INPUT
     1  scastr,
c ... OUTPUT
     2  scatyp, scaival, scadval)
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
c ... Take a TIO scalar variable in character string 'scastr', and 
c     get it's type. If it is a numeric type, also get the integer
c     and/or double numeric value:
c       *  An integer string returns only an INT value
c       *  A floating string returns both an INT and a DUBL value
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       scastr  -  Input character string
c
c     OUTPUT:
c       scatyp  -  TIO variable type (see include file iopar.inc)
c       scaival -  INT numeric value
c       scadval -  DUBL numeric value
c
      character*(*)     scastr
      integer           scatyp, scaival
      double precision  scadval
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer           chrtoi, partyp
      double precision  chrtod
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS and variables:
c
      integer status
c
c
c=======================================================================
c ***  BEGIN:
c=======================================================================
c
      status = 0
c
c ... Get scalar variable type
c
      scatyp = partyp(scastr)
c
c ... INT: construct INT and DUBL values
c
      if (scatyp .EQ. INT) then
        scaival = chrtoi(scastr,status)
        scadval = scaival
c
c ... REA or DUBL: just construct DUBL value
c
      elseif (scatyp.EQ.REA .OR.  scatyp.EQ.DUBL ) then
        scadval = chrtod(scastr,status)
c
      endif
c
      return
      end
