      subroutine ioswpinfl (
c ... INPUT
     1 newfnam, newlu, newlnum,
c ... OUTPUT
     2 oldfnam, oldlu, oldlnum, status )
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
c ... Swap the zero-level input file for TIO. To support error handling,
c     the old file name, logical unit # and line number are returned
c     to the calling routine, just in case we need to swap back to
c     this file later.
c
c ... This routine will return with an error condition if TIO is 
c     currently running a command file.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       newfnam -  New file name
c       newlu   -  New logical unit #
c       newlnum -  Initial value of line # counter for file
C
c     OUTPUT:
c       oldfnam -  Old file name
c       oldlu   -  New logical unit #
c       oldlnum -  Initial value of line # counter for file
c       status  -  Status flag:-
c                     0  -  Normal return
c                    -1  -  Cannot change input unit while running
c                           command file!!
c
c ... NOTE: the old/new variables can share the same address
c
      character*(*) newfnam, oldfnam
      integer       newlu, newlnum, oldlu, oldlnum, status
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and common block decks:
c
#include "iocmdf.inc"
#include "iolun.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer  lennb, tioerh
      integer  NS_freename, NS_getname, NS_putname
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character*132 toldfnam
      integer       ierr, nch, toldlu, toldlnum
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      if (cmflvl .EQ. 0) then
        nch      = NS_getname (hcmfnam(0), toldfnam)
        toldlu   = luin
        toldlnum = lnumcf(0)
c
        ierr = NS_freename (hcmfnam(0))
c
        nch = max(lennb(newfnam), 1)
        hcmfnam(0) = NS_putname (newfnam(1:nch))
        luin       = newlu
        luin0      = newlu
        lucmd(0)   = newlu
        lnumcf(0)  = newlnum
c
        oldlu   = toldlu
        oldlnum = toldlnum
        oldfnam = toldfnam
c
        status = 0
      else
        status = tioerh ('TIO', 'IOSWPINFL', 2, 
     &    'Cannot change input unit while running command file', -1)
      endif
c
      return
      end
