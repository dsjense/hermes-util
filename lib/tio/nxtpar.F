      character*(*) function nxtpar (
c ... INPUT
     1 uspchr,
c ... INPUT/OUTPUT
     2 ierr,
c ... OUTPUT
     3 nchpar )
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
c ... NXTPAR returns the next parameter in the expanded input line.  
c     It leaves the character pointer in the expanded line at the 
c     same position.
c
c     The list of user special characters is used in exactly the same
c     way as CHRPAR
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       uspchr  -  List of user special characters
c
c     INPUT/OUTPUT
c       ierr    -  INPUT:   Do nothing if ierr.NE.0
c                  OUTPUT:  ERROR flag:-
c                     0 - normal return
c                    -1 - Unable to return required parameter
c                    -2 - Syntax error in line (unbalanced quotes)
c
c     OUTPUT:
c       nchpar  -  Non-blank length of NXTPAR
c
        character*(*) uspchr
        integer       ierr, nchpar
c
c-----------------------------------------------------------------------
c
c ... Include parameter and common block decks
c
#include "ioinln.inc"
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      character*80 chrpar
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      integer       oldchr
c
c
c=======================================================================
c ... BEGIN:
c=======================================================================
c
c
c ... Save old character pointer
c
      oldchr = cuichr
c
c ... Get next parameter
c
      nxtpar = chrpar (OPT, ' ', ' ', uspchr, ierr, nchpar)
c
c ... And reset character pointer
c
      cuichr = oldchr
c
      return
      end
