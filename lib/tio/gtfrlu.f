      integer function gtfrlu (
c ... INPUT
     1 first, last, luerr )
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
c ... GTFRLU searchs a supplied range of logical unit #'s, and returns
c     the first available one (i.e. first one not currently open).  If
c     no unit # is available, GTFRLU returns the value -1
c
c-----------------------------------------------------------------------
c
c ... Function arguments:
c
c     INPUT:
c       first   -  first unit # to be tested
c       last    -  last unit # to be tested
c       luerr   -  Logical unit # for error message (only written out
c                  if LUERR > 0)
c
      integer  first, last, luerr
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer  tioerh
c
c-----------------------------------------------------------------------
c
c ... Local variables:
c
      character errmsg*132
      integer   i
      logical   lopen
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
c ... search for 1st available logical unit within specified range
c
      do 1 i=first,last
        inquire ( unit=i, opened=lopen )
        if ( .NOT. lopen ) then
          gtfrlu = i
          return
        endif
    1 continue
c
c ... Did not find free unit, write error message and flag error
c
      gtfrlu = -1
      if ( luerr.gt.0 ) then
        write(errmsg,9800) first, last
        gtfrlu = tioerh ('TIO', 'GTFRLU', 2, errmsg, -1)
      endif
c
c
c=======================================================================
c ... ERROR messages
c=======================================================================
c
 9800 format('Unable to find free logical unit # between ',
     1       i2,' and ',i2)
c
      end
