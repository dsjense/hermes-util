      integer function lsteql (
c ... INPUT
     1 name, namlst, lstlen)
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
c     LSTEQL compares CHARACTER variable NAME against the elements of
c     the array NAMLST, and returns the array index of the first matching
c     element if a match is found, or -1 if no match is found.
c
c     LSTEQL does an exact comparison of the entire CHARACTER variables 
c     (unlike LSTMCH, which looks for matching sub-strings in case-
c     insensitive fashion).  The case-sensitivity of the comparison
c     depends on the case-sensitive mode switch CSMODE, which is set
c     to .FALSE. or case-insensitive by default.
c
c-----------------------------------------------------------------------
c
c ... Function arguments
c
      integer       lstlen
      character*(*) name, namlst(1)
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      logical streql
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      integer i
c
c
c=======================================================================
c     BEGIN
c=======================================================================
c
      do 100 i=1,lstlen
        if (streql(name, namlst(i))) then
          lsteql = i
          return
        endif
  100 continue
c
      lsteql = -1
      return
      end
