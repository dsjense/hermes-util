      integer function ioifdf (
c ... INPUT
     1 mode,
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
c ... IOIFDF is a TIO internal utility routine for parsing the rest of
c     the command line for a TIO IFDEF/IFNDEF construct.  It takes
c     next parameter from the command line (required), and matches
c     it against the local and/or global tables.
c
c     It returns either 0 (false) or 1 (true).
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments
c
c     INPUT
c       mode   -  Mode switch for test:-
c                   1  -  IFDEF
c                   0  -  IFNDEF
c     OUTPUT
c       status - Status flag:
c                  0  -  Normal completion
c                 -1  -  Syntax error in statement
c
      integer mode, status
c
c-----------------------------------------------------------------------
c
c ... Include parameter and common block decks
c
#include "ioctrl.inc"
#include "iocmdf.inc"
#include "iopar.inc"
#include "iosymbol.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      character  chrpar*80
      integer    tioerh
      integer    NS_find
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character*80  symnam
      integer       isym, nchnam
c
c=======================================================================
c ... BEGIN:
c=======================================================================
c
c ... Get symbol name from command line
c
      symnam = chrpar (RQD, ' ', ' ', ' ', ioerr, nchnam)
      if (ioerr .NE. 0) then
        status = tioerh ('TIO', 'IOIFDF', 2,
     1                   'Syntax error in IFDEF/IFNDEF statement', -1)
        ioifdf = 0
        return
      endif
c
c ... Check first against the local, and then the global, lists
c
      isym = NS_find ( symnam(1:nchnam), hlsymnam(loclsym(cmflvl)),
     &                 nlsym(cmflvl) )
c
      if (isym .LE. 0) then
        isym = NS_find ( symnam(1:nchnam), hgsymnam, ngsym)
      endif
c
c ... IFDEF
c
      if (mode .EQ. 1) then
        if (isym .GT. 0) then
          ioifdf = 1
        else
          ioifdf = 0
        endif
c
c ... IFNDEF
c
      else
        if (isym .GT. 0) then
          ioifdf = 0
        else
          ioifdf = 1
        endif
      endif
c
      return
      end
