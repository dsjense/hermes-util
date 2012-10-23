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
c> \file nserver_f.F
c> \brief File containing Fortran wrapper routines used by the Name Server.
c>

c> \addtogroup PublicInterface
c>   \{
c>

c ----------------------------------------------------------------------
c
c> \brief ns_putname stores the supplied string and returns a handle to that
c>        string.
c>
c> \param[in]  name  supplied string
c>
c> \return  \li > 0, string's handle for later retrieval
c>          \li   0, memory allocation error
c>          \li  -1, string is too long for internal storage
c> \note ns_putname is a member of NSERVER's application (public) interface
c
      integer function ns_putname ( name )
c
c ----------------------------------------------------------------------
c
      character name*(*)
c
      integer last
c
      integer ns_lastchr
      integer nsu_putname
c
      last = ns_lastchr(name)
      ns_putname = nsu_putname ( last, name )
c
      return
      end

c ----------------------------------------------------------------------
c
c> \brief ns_getname accesses the string referenced by handle and returns the 
c>        length of the string or a status code on error.
c>
c> \param[in]    handle  supplied string handle
c> \param[out]   name    returned string
c>
c> \return  \li >= 0, length of string returned
c>          \li   -1, illegal handle value
c>          \li   -2, handle is not active
c> \note ns_getname is a member of NSERVER's application (public) interface
c
      integer function ns_getname ( handle, name )
c
c ----------------------------------------------------------------------
c
      integer   handle
      character name*(*)
c
      integer nsu_getname
c
      ns_getname = nsu_getname ( handle, len(name), name )
c
      return
      end

c ----------------------------------------------------------------------
c
c> \brief ns_find attempts to find a supplied string among the strings
c>        referenced by handles in the supplied handle list.
c>
c>   If a matching string is found, the index of the associated handle
c>   in the list is returned. (Note that this index starts at 1).
c>   If the string is not found, zero is returned.
c>
c> \param[in]   name   supplied string
c> \param[in]   hlist  supplied list of string handles
c> \param[in]   nlist  length of handle list
c>
c> \return  \li > 0, list index of handle matching string
c>          \li   0,   no match found
c>          \li  -1,  an invalid handle supplied
c>          \li  -2,  supplied string exceeds maximum string length
c> \note ns_find is a member of NSERVER's application (public) interface
c
      integer function ns_find ( name, hlist, nlist )
c
c ----------------------------------------------------------------------
c
      character name*(*)
      integer   nlist, hlist(1:nlist)
c
      integer last
c
      integer ns_lastchr
      integer nsu_find
c
      last = ns_lastchr(name)
      ns_find = nsu_find ( last, name, hlist, nlist )

      return
      end

c ----------------------------------------------------------------------
c
c> \brief ns_findorput attempts to find a supplied string among the strings
c>        referenced by handles in the supplied handle list.
c>
c>  If a matching string is found, the index of the associated handle in the
c>  list is returned. (Note that this index starts at 1). If the string is
c>  not found, the string is stored and the negative of the assigned handle
c>  is returned.
c>
c> \param[in]  name    supplied string
c> \param[in]  hlist   supplied list of string handle
c> \param[in]  nlist   length of handle list
c>
c> \return  \li > nlist, no match found and error encountered while attempting
c>                       to add the string to the list
c>          \li > 0,     list index of handle matching string
c>          \li < 0,     negative of handle assigned to the string if
c>                       no match is found
c> \note ns_findorput is a member of NSERVER's application (public) interface
c
      integer function ns_findorput ( name, hlist, nlist )
c
c ----------------------------------------------------------------------
c
      character name*(*)
      integer   nlist, hlist(1:nlist)
c
      integer last
c
      integer ns_lastchr
      integer nsu_findorput
c
      last = ns_lastchr(name)
      ns_findorput = nsu_findorput ( last, name, hlist, nlist )
c
      return
      end

c ----------------------------------------------------------------------
c
c> \brief ns_find_match attempts to uniquely match a supplied string to the
c>        leading characters of the strings referenced by handles in the
c>        supplied handle list.
c>
c>   If a unique match is not found, but one of the strings in the list
c>   exactly matches the string, it is considered the unique match. If a
c>   unique match is found, the index of the associated handle in the list
c>   is returned. (Note that this index starts at 1). If the string is not
c>   matched, zero is returned. If the match is not unique, -1 is returned.
c>   If one of the supplied handles is not valid, -2 is returned.
c>
c> \param[in] name   supplied string
c> \param[in] hlist  supplied list of string handle
c> \param[in] nlist  length of handle list
c>
c> \return \li    0, no match found
c>         \li  > 0, list index of handle matching string
c>         \li   -1, the match is not unique (multiple matches)
c>         \li   -2, one of the supplied handles is invalid
c>         \li   -3, supplied string exceeds maximum string length
c> \note ns_find_match is a member of NSERVER's application (public) interface
c
      integer function ns_find_match ( name, hlist, nlist )
c
c ----------------------------------------------------------------------
c
      character name*(*)
      integer   nlist, hlist(1:nlist)
c
      integer last
c
      integer ns_lastchr
      integer nsu_find_match
c
      last = ns_lastchr(name)
      ns_find_match = nsu_find_match ( last, name, hlist, nlist )

      return
      end

c ----------------------------------------------------------------------
c
c> ns_debug dumps a list of currently allocated strings to standard output.
c> \note ns_debug is a member of NSERVER's application (public) interface

      subroutine ns_debug ()
c
c ----------------------------------------------------------------------
c
      integer i, cnt, d1, d2, err, free, hmax

      character name*1024, frmt*16

      integer nsu_getname

      call nsu_debug (cnt, free, hmax)
      d1 = int(log10(max(1.0,float(cnt)))) + 1
      d2 = int(log10(max(1.0,float(free)))) + 1
      write(frmt,200) d1, d2
      print frmt,'Strings allocated: ',cnt,'   free: ',free

      d1 = int(log10(max(1.0,float(hmax)))) + 1
      write(frmt,100) d1
      do i=1,cnt
        err = nsu_getname( i, len(name), name )
        if (err.ge.0) then
          err = max(err,1)
          print frmt, '  ', i, '  "', name(1:err), '"'
        endif
      end do

 100  format('(a,i',i2,',a,a,a)')
 200  format('(a,i',i2,',a,i',i2,')')

      return
      end

c ----------------------------------------------------------------------
c
c> \brief  ns_debug_array dumps a list of currently allocated integer arrays,
c>         and their contents, to standard output.
c> \note ns_debug_array is a member of NSERVER's application (public) interface
c
      subroutine ns_debug_array ()
c
c ----------------------------------------------------------------------
c
      integer i, j, j1, j2, cnt, d0, d1, d2, err, free, hi, low, val

      character name*1024, frmt*24

      integer ns_getarrlims, ns_getarrval

      call nsu_debug_array (cnt, free)
      d1 = int(log10(max(1.0,float(cnt)))) + 1
      d2 = int(log10(max(1.0,float(free)))) + 1
      write(frmt,200) d1, d2
      print frmt,'Arrays allocated: ',cnt,'   free: ',free

      do i=1,cnt
        err = ns_getarrlims( i, low, hi)
        if (err.eq.0) then
          d0 = int(log10(max(1.0,float(i)))) + 1
          d1 = int(log10(max(1.0,float(abs(low))))) + 1
          if ( low .lt. 0 ) d1 = d1 + 1
          d2 = int(log10(max(1.0,float(abs(hi))))) + 1
          if ( hi .lt. 0 ) d2 = d2 + 1
          write(frmt,300) d0, d1, d2
          print frmt,'  handle: ',i,'  low: ',low,' high: ',hi
          d1 = max(d1,d2)
          d2 = 0
          j2 = low - 1
          j1 = j2
          do j=low,hi
            err = ns_getarrval( i, j, val )
            if (err.eq.0) then
              d0 = int(log10(max(1.0,float(abs(val))))) + 1
              if ( val .lt. 0 ) d0 = d0 + 1
              d2 = max(d2, d0)
              j2 = j
              if ( j1.lt.low ) j1 = j
            endif
          end do
          write(frmt,200) d1, d2
          do j=j1,j2
            err = ns_getarrval( i, j, val )
            if (err.eq.0) print frmt, '    ',j, '   ', val
          end do
        endif
      end do

 200  format('(a,i',i2,',a,i',i2,')')
 300  format('(a,i',i2,',a,i',i2,',a,i',i2,')')

      return
      end

c ----------------------------------------------------------------------
c
c> \brief ns_lastchr is a utility routine that returns the non-blank 
c>        length of a Fortran character string.
c>
c> \param[in] string  supplied string
c>
c> \return  non-blank length of string
c> \note ns_lastchr is a member of NSERVER's application (public) interface
c
      integer function ns_lastchr( string )
c
      entry nsu_lastchr( string )
      integer nsu_lastchr
c
c ----------------------------------------------------------------------
c
      character string*(*)
c
      integer i
c
      ns_lastchr = 0
      do i=len(string),1,-1
        if ( string(i:i).ne.' ' ) then
          ns_lastchr = i
          return
        endif
      end do
c
      return
      end

c>  \} END of PublicInterface group

