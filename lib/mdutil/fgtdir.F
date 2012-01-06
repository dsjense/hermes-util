      integer function fgtdir(name,dir)
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
c     This routine returns the home directory of a specified user.
c
c **************************** UNIX Version ****************************
c
c     - Converts FORTRAN character strings to packed-byte integers.
c     - Calls an intermediate C interface program to make the GETPWNAM 
c       UNIX system call to extract the pw_dir field of PASSWD structure
c
c ----------------------------------------------------------------------
c
c     Input:
c       name    -  character variable containing the name of the 
c                  environment variable to be defined
c       dir     -  character variable in which the value of the specified 
c                  user's home directory is returned
c
c     Output:  NONE
c
c     Return Value:
c        0  -  Normal return
c        1  -  Error encoding "name" to integer
c        2  -  Error decoding "dir" from integer
c        3  -  Error status returned from CGPWNM C interface routine
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
      character name*(*),dir*(*)
c
      integer   ierr
      integer   iused,ileft
      integer   c2h,h2c
      integer   cgpwnm
c
c ----------------------------------------------------------------------
c
c ... pack 1st parameter into a null-terminated integer byte stream
c
      ierr = c2h(name,ibuf,IBLEN,iused)
      if ( ierr.ne.0 ) then
        fgtdir = 1
        return
      endif
c
c ... how many characters are left in integer buffer ?
c
      ileft = (IBLEN - iused)
c
c ... call C interface routine
c
      ierr = cgpwnm(ibuf,ibuf(iused + 1),ileft*NBPI)
      if ( ierr.ne.0 ) then
        fgtdir = 2
        return
      endif
c
c ... unpack 2nd parameter from a null-terminated integer byte stream
c
      ierr = h2c(ibuf(iused + 1),ileft,dir)
      if ( ierr.ne.0 ) then
        fgtdir = 3
        return
      endif
c
      fgtdir = 0
c
      return
      end
