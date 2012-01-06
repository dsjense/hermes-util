c **********************************************************************
c   Fortran interface to C Name Server software
c   D. Seidel
c   3/2/99
c   $Id$
c   
c   Copyright (2008) Sandia Corporation. Under the terms of
c   Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
c   Government retains certain rights in this software.
c   
c   Hermes is free software: you can redistribute it and/or modify
c   it under the terms of the GNU Lesser General Public License as
c   published by the Free Software Foundation, either version 3 of
c   the License, or (at your option) any later version.
c   
c   Hermes is distributed in the hope that it will be useful, but
c   WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU Lesser General Public License for more details.
c   
c   You should have received a copy of the GNU Lesser General
c   Public License along with Hermes.  If not, see
c   <http://www.gnu.org/licenses/>.
c   
c **********************************************************************
c
      integer function NS_putname ( name )
c
c ----------------------------------------------------------------------

c      NS_putname stores the supplied string and returns a handle to that 
c      string
c
c      Input:
c        name  -  supplied string (read only)
c
c      Return value:
c               >  0    - string's handle for later retrieval
c                  0    - memory allocation error
c                 -1    - string is too long for internal storage
c
c ----------------------------------------------------------------------
c
      character name*(*)
c
      integer last
c
      integer NSU_lastchr
      integer NSU_putname
c
      last = NSU_lastchr(name)
      NS_putname = NSU_putname ( last, name )
c
      return
      end

      integer function NS_getname ( handle, name )
c
c ----------------------------------------------------------------------
c
c      NS_getname accesses the string referenced by handle and returns the 
c      length of the string or a status code on error
c
c      Input:
c        handle  -  supplied string handle (read only)
c        maxlen  -  maximum length of returned string (read only)
c
c      Output:
c        name    -  returned string (write only)
c
c      Return value:
c             >= 0      - length of string returned
c               -1      - illegal handle value
c               -2      - handle is not active
c
c ----------------------------------------------------------------------
c
      integer   handle
      character name*(*)
c
      integer NSU_getname
c
      NS_getname = NSU_getname ( handle, len(name), name )
c
      return
      end

      integer function NS_find ( name, hlist, nlist )
c
c ----------------------------------------------------------------------
c
c      NS_find attempts to find a supplied string among the strings referenced 
c      by handles in the supplied handle list.  If a matching string is 
c      found, the index of the associated handle in the list is returned.
c      (Note that this index starts at 1).  If the string is not found, zero
c      is returned.
c
c      Input:
c        name    -  supplied string (read only)
c        hlist   -  supplied list of string handle (read only)
c        nlist   -  length of handle list (read only)
c
c      Return value:
c             > 0         - list index of handle matching string
c             0           - no match found
c             -1          - an invalid handle supplied
c             -2          - supplied string exceeds maximum string length
c
c ----------------------------------------------------------------------
c
      character name*(*)
      integer   nlist, hlist(1:nlist)
c
      integer last
c
      integer NSU_lastchr
      integer NSU_find
c
      last = NSU_lastchr(name)
      NS_find = NSU_find ( last, name, hlist, nlist )

      return
      end

      integer function NS_findorput ( name, hlist, nlist )
c
c ----------------------------------------------------------------------
c
c      NS_findorput attempts to find a supplied string among the strings
c      referenced by handles in the supplied handle list.  If a matching string
c      is found, the index of the associated handle in the list is returned.
c      (Note that this index starts at 1).  If the string is not found, the
c      string is stored and the negative of the assigned handle is returned.
c
c      Input:
c        name    -  supplied string (read only)
c        hlist   -  supplied list of string handle (read only)
c        nlist   -  length of handle list (read only)
c
c      Return value:
c             > nlist     - no match found and error encountered while
c                           attempting to add the string to the list
c             > 0         - list index of handle matching string
c             < 0         - negative of handle assigned to the string if
c                           no match is found
c
c ----------------------------------------------------------------------
c
      character name*(*)
      integer   nlist, hlist(1:nlist)
c
      integer last
c
      integer NSU_lastchr
      integer NSU_findorput
c
      last = NSU_lastchr(name)
      NS_findorput = NSU_findorput ( last, name, hlist, nlist )
c
      return
      end

      integer function NS_find_match ( name, hlist, nlist )
c
c ----------------------------------------------------------------------
c
c     NS_find_match attempts to uniquely match a supplied string to the
c     leading characters of the strings referenced by handles in the
c     supplied handle list. If a unique match is not found, but one
c     of the strings in the list exactly matches the string, it is
c     considered the unique match. If a unique match is found, the index
c     of the associated handle in the list is returned. (Note that this
c     index starts at 1). If the string is not matched, zero is returned.
c     If the match is not unique, -1 is returned. If one of the supplied
c     handles is not valid, -2 is returned.
c
c     name   --  supplied string (read only)
c     hlist  --  supplied list of string handle (read only)
c     nlist  --  length of handle list (read only)
c
c     Return value:  0     -- no match found
c                    > 0   -- list index of handle matching string
c                    -1    -- the match is not unique (multiple matches)
c                    -2    -- one of the supplied handles is invalid
c                    -3    -- supplied string exceeds maximum string length
c
c ----------------------------------------------------------------------
c
      character name*(*)
      integer   nlist, hlist(1:nlist)
c
      integer last
c
      integer NSU_lastchr
      integer NSU_find_match
c
      last = NSU_lastchr(name)
      NS_find_match = NSU_find_match ( last, name, hlist, nlist )

      return
      end

      integer function NSU_lastchr( string )
c
c ----------------------------------------------------------------------
c
c      NSU_lastchr is a utility routine that returns the non-blank 
c      length of a Fortran character string.
c
c      Input:
c        name    -  supplied string (read only)
c
c      Return value:  non-blank length of string
c
c ----------------------------------------------------------------------
c
      character string*(*)
c
      integer i
c
      NSU_lastchr = 0
      do i=len(string),1,-1
        if ( string(i:i).ne.' ' ) then
          NSU_lastchr = i
          return
        endif
      end do
c
      return
      end
