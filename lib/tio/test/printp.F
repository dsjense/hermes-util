      subroutine printp (
c ... INPUT
     1 stdout,
c ... INPUT/OUTPUT
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
c ... Print out all parameters returned by TIOLIB from the current
c     command line.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments
c
c     INPUT:
c       stdout - Logical unit for standard output
c
c     INPUT/OUTPUT:
c       status - TIOLIB status flag
c
      integer stdout, status
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and common block decks
c
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      character        chrpar*80
      double precision chrtod
      integer          chrtoi, partyp
      logical          endofl
      real             chrtor
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character*80      cpar
      double precision  dpar
      integer           ipar, nch, par, typ
      real              rpar
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
c ... Loop over input parameters
c
      do 100 par=1,999
c
        if (endofl(status)) then
          write(stdout,10) par - 1, status
  10      format(/' END-OF-LINE: ',i2,' parameters read'/
     1            ' Status = ',i3/)
          return
        else
c
c ....... Get next character parameter, and convert according to
c         it's type.  NOTE: most TIOLIB applications will know
c         what type of parameter is next, and use calls to INTPAR,
c         REAPAR and DBLPAR directly, rather than calling CHRPAR
c         and then CHRTOI, CHRTOR and CHRTOD
c
          cpar = chrpar (RQD, ' ', ' ', '[]', status, nch)
c
          if (status .EQ. 0) then
            if (par .EQ. 1) write (stdout,20)
   20       format(3x,'par.',4x,'type',t20,'parameter string',
     1             t45,'numeric value'/
     2             3x,'----',4x,'----',t20,'----------------',
     3             t45,'-------------')
c
c ......... Get the type of the next parameter
c
            typ  = partyp(cpar)
            if (typ .LE. DUBL) nch = min (nch, 20)
c
c ......... And print it out accordingly
c
            if (typ .EQ. INT) then
              ipar = chrtoi(cpar,status)
              write (stdout,30) par, typ, cpar(1:nch), ipar
   30         format(1x,i5,3x,i5,t20,'''',a,'''',t45,i10)
            elseif (typ .EQ. REA) then
              rpar = chrtor(cpar,status)
              write (stdout,40) par, typ, cpar(1:nch), rpar
   40         format(1x,i5,3x,i5,t20,'''',a,'''',t45,1pe14.6)
            elseif (typ .EQ. DUBL) then
              dpar = chrtod(cpar,status)
              write (stdout,50) par, typ, cpar(1:nch), dpar
   50         format(1x,i5,3x,i5,t20,'''',a,'''',t45,1pd20.12)
            else
              write (stdout,60) par, typ, cpar(1:nch)
   60         format(1x,i5,3x,i5,t20,'''',a,'''')
            endif
          endif
        endif
  100 continue
c
      end
