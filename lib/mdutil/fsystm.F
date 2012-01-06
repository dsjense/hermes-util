      integer function fsystm(comand)
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
c     Execute a shell command.
c
c **************************** UNIX Version ****************************
c
c     - Converts FORTRAN character string to packed-byte integers.
c     - Calls an intermediate C interface program that calls SYSTEM
c
c ----------------------------------------------------------------------
c
c     Input:
c       comand  -  character variable containing the shell command
c
c     Output:  NONE
c
c     Return Value:
c        0  -  Normal return
c        1  -  Error encoding "comand" to integer
c        2  -  Error status returned from CSYSTM C interface routine
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
      character comand*(*)
c
      integer   ierr
      integer   iused
      integer   c2h
      integer   csystm
c
c ----------------------------------------------------------------------
c
c
c ... pack parameter into a null-terminated integer byte stream
c
      ierr = c2h(comand,ibuf,IBLEN,iused)
      if ( ierr.ne.0 ) then
        fsystm = 1
        return
      endif
c      
c ... call C interface routine
c
      ierr = csystm(ibuf)
      if ( ierr.ne.0 ) then
        fsystm = 2
        return
      endif
c
      fsystm = 0

      return
      end
