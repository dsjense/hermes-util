      integer function facces(path,amode)
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
c     Return calling program's access rights to a file.
c
c **************************** UNIX Version ****************************
c
c     - Converts FORTRAN character strings to packed-byte integers.
c     - Calls an intermediate C interface program that converts 
c       character access mode (amode) to integer mask for C call to 
c       ACCESS system call.
c
c ----------------------------------------------------------------------
c
c     Input:
c       path    -  pathname of file to be checked for access rights
c       amode   -  character string containing requested access.  
c                  Characters and their meanings are:
c                     r    -  read access
c                     w    -  write/delete access
c                     x    -  execute access
c                  If the string "amode" is blank, the file's existance 
c                  is checked.
c
c     Output:  NONE
c
c     Return Value:
c        0  -  Access allowed
c       -1  -  Access NOT allowed
c        1  -  Error encoding "path" to integer
c        2  -  Error encoding "amode" to integer
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
      character path*(*),amode*(*)
c
      integer   ierr
      integer   iused,ioff
      integer   c2h
      integer   cacces
c
c ----------------------------------------------------------------------
c
c
c ... pack 1st parameter into a null-terminated integer byte stream
c
      ierr = c2h(path,ibuf,IBLEN,iused)
      if ( ierr.ne.0 ) then
        facces = 1
        return
      endif
c
c ... pack 2nd parameter into a null-terminated integer byte stream
c
      ioff = iused + 1
      ierr = c2h(amode,ibuf(ioff),IBLEN-iused,iused)
      if ( ierr.ne.0 ) then
        facces = 2
        return
      endif
c
c ... call C interface routine
c
      facces = cacces(ibuf,ibuf(ioff))
c
      return
      end



