      subroutine rstofl (
c ... OUTPUT
     1 uchbuf, nchbuf,
c ... INPUT/OUTPUT:
     2 ierr )
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
c     Return the rest of the expanding input line, and the number of
c     non-blank characters in line.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     OUTPUT:
c       uchbuf  -  User character buffer for expanded line
c       nchbuf  -  Non-blank length of returned line
c
c     INPUT/OUTPUT:
c       ierr    -  INPUT:  Do nothing if ierr.NE.0
c                  OUTPUT: Error flag:-
c                             0 = normal return
c                            +1 = Output character buffer overflow
c                                 (input line truncated)
c
      character*(*) uchbuf
      integer  ierr, nchbuf
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "ioinln.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls:
c
      integer      tioerh
c
c-----------------------------------------------------------------------
c
c ... Local variables:
c
      character*132 errmsg
      integer       lnuchb
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      uchbuf = ' '
c
      if (ierr .EQ. 0) then
        nchbuf = lenxln - cuichr
        lnuchb = len(uchbuf)
c
        if (nchbuf .LT. lnuchb) then
          if (nchbuf.GT.0) uchbuf(1:nchbuf) = uinptc(cuichr+1:lenxln)
        else
          write(errmsg,9800) nchbuf
          ierr = tioerh ('TIO', 'RSTOFL', 2, errmsg, 1)
          uchbuf = uinptc(cuichr+1:lenxln)
        endif
      else
        nchbuf = 0
      endif
c
      return
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9800 format('Buffer overflow for line (',i4,' chars.)')
c
      end
