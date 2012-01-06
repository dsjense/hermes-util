      subroutine mddtim(cdate) 
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
c     returns cdate as a date-time string of the following form:
c           mm/dd/yy  hh:mm:ss
c     The length of cdate must be .ge. 18
c
c ----------------------------------------------------------------------
c
c     Input:  None
c
c     Output:
c       cdate   -  character variable returning the date-time string
c
c     Internal:
c       ierr    -  error status flag
c       slen    -  length of variable "cdate"
c
      character cdate*(*)
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     nbpi
c
c ... Included common decks:
c ... ibuf  (UNIX systems only)
c       r:    IBLEN
c       w:    ibuf(1:IBLEN)
c
#include "nbpi.inc"
#include "ibuf.inc"
c
      integer   i, ierr, slen
c
c *** UNIX Specific *******************
c
c     Unix Version uses STANDARD standard C system calls
c
      integer   h2c
      integer   cdtim
c
      slen = min(IBLEN-1,len(cdate))
c
      ierr = cdtim ( ibuf, slen )
      if ( ierr.eq.0 ) then
c
c ..... unpack return value from a null-terminated integer byte stream
c      
        ierr = h2c(ibuf,IBLEN,cdate)
      endif
c
c *************************************
c
      if ( ierr.ne.0 ) then
        do 1 i=1,slen
          cdate(i:i) = '*'
    1   continue
        return
      endif
c
      return
      end
