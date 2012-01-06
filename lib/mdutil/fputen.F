      integer function fputen(name,value)
c
c **********************************************************************
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
c **********************************************************************
c
c     Define an environment variable.
c
c **************************** UNIX Version ****************************
c
c     - Converts FORTRAN character strings to packed-byte integers.
c     - Calls an intermediate C interface program that allocates static 
c       memory, builds the input string, and calls UNIX system call 
c       PUTENV
c
c ----------------------------------------------------------------------
c
c     Input:
c       name    -  character variable containing the name of the 
c                  environment variable to be defined
c       value   -  character variable containing the value of the 
c                  environment variable to be defined
c
c     Output:  NONE
c
c     Return Value:
c        0  -  Normal return
c        1  -  Error encoding "name" to integer
c        2  -  Error encoding "value" to integer
c        3  -  Error status returned from CPUTEN C interface routine
c
c     Internal:
c       ierr    -  error status flag
c       iused   -  # of integer words used by C2H call
c       ioff    -  offset of 1st free integer word in buffer
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     nbpi    -  NBPI
c
c ... Included common decks:
c ... ibuf
c       r:    IBLEN
c       w:    ibuf(1:IBLEN)
c
#include "nbpi.inc"
#include "ibuf.inc"
c
c ----------------------------------------------------------------------
c
      character name*(*),value*(*)
c
      integer   ierr
      integer   iused,ioff
      integer   c2h
      integer   cputen
c
c ----------------------------------------------------------------------
c
c
c ... pack 1st parameter into a null-terminated integer byte stream
c
      ierr = c2h(name,ibuf,IBLEN,iused)
      if ( ierr.ne.0 ) then
        fputen = 1
        return
      endif
c
c ... pack 2nd parameter into a null-terminated integer byte stream
c
      ioff = iused + 1
      ierr = c2h(value,ibuf(ioff),IBLEN-iused,iused)
      if ( ierr.ne.0 ) then
        fputen = 2
        return
      endif
c
c ... call C interface routine
c
      ierr = cputen(ibuf,ibuf(ioff))
c
      if ( ierr.ne.0 ) then
        fputen = 3
        return
      endif
c
      fputen = 0
      return
      end
