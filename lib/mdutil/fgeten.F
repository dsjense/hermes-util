      integer function fgeten(name,value)
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
c     Obtain the value of an environment variable.
c
c **************************** UNIX Version ****************************
c
c     - Converts FORTRAN character strings to packed-byte integers.
c     - Calls an intermediate C interface program to make the GETENV 
c       system call
c
c ----------------------------------------------------------------------
c
c     Input:
c       name    -  character variable containing the name of the 
c                  environment variable whose value is to be returned
c
c     Output:
c       value   -  character variable returning the value of the 
c                  environment variable
c
c     Return Value:
c        0  -  Normal return
c        1  -  Error encoding "name" to integer
c        2  -  Error decoding "value" from integer
c        3  -  Error status returned from CGETEN C interface routine
c
c     Internal:
c       ierr    -  error status flag
c       iused   -  # of integer words used by C2H call
c       ileft   -  # of integer words remaining in buffer
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     nbpi
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
      integer   iused,ileft
      integer   c2h,h2c
      integer   cgeten
c
c ----------------------------------------------------------------------
c
c ... pack 1st parameter into a null-terminated integer byte stream
c
      ierr = c2h(name,ibuf,IBLEN,iused)
      if ( ierr.ne.0 ) then
        fgeten = 1
        return
      endif
c
c ... how many characters are left in integer buffer ?
c
      ileft = (IBLEN - iused)
c
c ... call C interface routine
c
      ierr = cgeten(ibuf,ibuf(iused+1),ileft*NBPI)
      if ( ierr.ne.0 ) then
        fgeten = 3
        return
      endif
c
c ... unpack 2nd parameter from a null-terminated integer byte stream
c      
      ierr = h2c(ibuf(iused+1),ileft,value)
      if ( ierr.ne.0 ) then
        fgeten = 2
        return
      endif
c
      fgeten = 0
c
      return
      end
