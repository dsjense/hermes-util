      subroutine ioloadpar (
c ... INPUT
     &  mpar, spchr,
c ... OUTPUT
     &  npar, apar,
c ... INPUT/OUTPUT
     &  ierr )
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
c ... IOLOADPAR is a utility routine that loads all the remaining
c     parameters on the input line into character array apar[1:mpar].
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       mpar    -  Dimensioned size of array APAR
c       spchr   -  "Special characters": delimiters returned as
c                  parameters by CHRPAR
c
c     OUTPUT:
c       npar    -  # parameters loaded into APAR
c       apar    -  (*) Character array to load
c
c     INPUT/OUTPUT:
c       ierr    -  INPUT:   Do nothing if ierr.NE.0
c                  OUTPUT:  ERROR flag:-
c                     0 - normal return
c                    -1 - # parameters exceeds array dimension
c                    -2 - Parameter overflows character size of APAR
c
      integer       ierr, mpar, npar
      character*(*) spchr, apar(1:mpar)
c
c-----------------------------------------------------------------------
c
c ... PARAMETER and common include files
c
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      character*132  chrpar
      logical        endofl
      integer        tioerh
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      integer       lpar, mchpar, nch
c
c
c=======================================================================
c ... BEGIN:
c=======================================================================
c
      npar = 0
      if (ierr .NE. 0) return
c
      if (endofl(ierr)) return
c
      mchpar = len(apar(1))
c
c ... Load parameters into the array
c
  100 continue
        npar = npar + 1
        lpar = min (npar, mpar)
c
        apar(lpar) = chrpar(RQD, ' ', ' ', spchr, ierr, nch) 
        if (ierr .NE. 0) return
c
        if (nch .GT. mchpar) then
          ierr = tioerh ('TIO', 'IOLOADPAR', 2,
     &                   'Parameter overflows character size', -2)
          return
        endif
      if (.NOT.endofl(ierr)) go to 100
c
      if (npar .GT. mpar) ierr = tioerh ('TIO', 'IOLOADPAR', 2,
     &                    '# parameters exceeds array size', -1)
c
      return
      end
