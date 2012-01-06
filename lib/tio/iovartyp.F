      subroutine iovartyp (
c ... INPUT
     &  varnam, errflgval,
c ... OUTPUT
     &  vartyp, varidx, nelvar)
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
c ... Subroutine IOVARTYP takes a variable name, and checks whether it
c     it matches a defined TIO variable.  It searches in the following
c     order of precedence:-
c
c       1. FOR loop variable
c       2. Local symbol (or command file parameter)
c       3. Global symbol
c
c     NOTE that local symbols are searched on at the current command
c     file level.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       varnam     -  Variable name
c       errflgval  -  Error handling flag if variable cannot be found
c                       0 - don't call tioerh: calling routine will
c                           handle the error
c                      !0 - call tioerh with error status errflgval  
c
c     OUTPUT:
c       vartyp  -  Variable type index
c       varidx  -  Index of the variable in appropriate data structure
c       nelval  -  # elements in variable
c
      character*(*) varnam
      integer       errflgval, nelvar, vartyp, varidx
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and Common block decks:
c
#include "iocmdf.inc"
#include "ioloop.inc"
#include "iosymbol.inc"
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls:
c
      integer  lennb, tioerh
      integer  NS_find
c
c-----------------------------------------------------------------------
c
c ... Local variables:
c
c
      character*80 errmsg
      integer      ierr, lplvl, nchvnam
c
c=======================================================================
c
c ... FOR loop variable: only process active loop variables at the
c     current command file level
c
      do lplvl=looplv(cmflvl), 1, -1
        if (varnam .EQ. loopvn(lplvl,cmflvl)) then
          vartyp = FORLOOPVAR
          varidx = lplvl
          nelvar = 1
          return
        endif
      enddo
c
c ... LOCAL symbols, defined at current command file level
c     NOTE: the list of 'local' symbols includes the command file
c     parameters passed from the next lower level
c
      nchvnam = max (1, lennb(varnam))
c
      varidx = NS_find ( varnam(1:nchvnam), hlsymnam(loclsym(cmflvl)),
     &                   nlsym(cmflvl) )
c
      if (varidx .GT. 0) then
        vartyp = LOCALSYM
        varidx = varidx + loclsym(cmflvl) - 1
        nelvar = max (nellsym(varidx), 1)
        return
      endif
c
c ... GLOBAL symbols
c
      varidx = NS_find ( varnam(1:nchvnam), hgsymnam, ngsym)
c
      if (varidx .GT. 0) then
        vartyp = GLOBALSYM
        nelvar = nelgsym(varidx)
        return
      endif
c
c-----------------------------------------------------------------------
c
c ... Did not find variable name: handle error
c
      vartyp = -1
c
      if (errflgval .NE. 0) then
        nchvnam = max (1, lennb(varnam))
        write(errmsg,9810) varnam(1:nchvnam)
        ierr = tioerh ('TIO', 'IOVARTYP', 2, errmsg, errflgval)
      endif
c
      return
c
 9810 format('Undefined variable: ',a)
c
      end
