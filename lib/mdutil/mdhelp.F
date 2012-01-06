      integer function mdhelp(string,hlplib)
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
c     FORTRAN interface to Help utility.
c
c     Input:
c       string     - string of help requests
c       hlplib     - HELP library name
c
c     Output:  NONE
c
c     Return Value:
c        0  -  Normal return
c        1  -  Error encoding "hlplib" to integer
c        2  -  Error encoding "string" to integer
c        3  -  Error status returned from CVHELP C interface routine
c
c     Internal:
c       status  -  error status flag
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
c
      character string*(*), hlplib*(*)
c
      integer status
c
c
c ******************************   UNIX   ****************************** 
c
c     Function MDHELP calls a VAX HELP emulator written in C.
c
c     - It does this via an intermediate C call.
c     - Converts FORTRAN character strings to packed-byte integers.
c
c     UNIX specific Internals:
c       iused   -  # of integer words used by C2H call
c       ioff    -  offset of 1st free integer word in buffer
c       lines   _  # of lines on terminal screen display
c
      integer   iused, ioff, lines
      integer   c2h
      integer   cvhelp
c
c ... get the screen size
c
      call mdgpsz(lines)
c
c ... pack 1st parameter into a null-terminated integer byte stream
c
      status = c2h(hlplib,ibuf,IBLEN,iused)
      if ( status.ne.0 ) then
        mdhelp = 1
        return
      endif
c
c ... pack 2nd parameter into a null-terminated integer byte stream
c
      ioff = iused + 1
      status = c2h(string,ibuf(ioff),IBLEN-iused,iused)
      if ( status.ne.0 ) then
        mdhelp = 2
        return
      endif
c
c ... call C interface routine to run HELP utility
c
      status = cvhelp(ibuf,ibuf(ioff),lines)      
      if ( status.ne.0 ) then
        mdhelp = 3
        return
      endif
c
      mdhelp = 0
c
      return
      end

