      subroutine iodefsym (
c ... INPUT
     & mxlist, symtyp, symnam, arridx, nsymval, symval,
c ... INPUT/OUTPUT
     & nlist, nellist, hlistnam, hlistval, status)
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
c ... IODEFSYM adds or redefines a symbol with one or more values in the
c     supplied list.
c
c     It supports three different modes of operation:-
c      1. Add a new symbol to the list 
c      2. Redefine an existing symbol on the list: delete all previous
c         value(s), and create new one(s).
c      3. Redefine a single element of an existing array.
c
c     For modes 1 and 2, if the symbol is an array and symval(1) = ' '
c     an array with nsymval elements is created, but with undefined
c     values.
c     For mode 2, if nellist = 0, the value cannot be redefined
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       mxlist   -  Max. # symbols allowed in list
c       symtyp   -  Symbol type: either LOCALSYM or GLOBALSYM
c       symnam   -  Name of symbol to be add or redefine
c       arridx   -  Array index for scalar redefine: only for mode 3
c       nsymval  -  # values to be associated with SYMNAM: 1 for a
c                   scalar or array element, > 1 for an entire array
c       symval   -  (*) List of array element values for new variable:
c
c     INPUT/OUTPUT:
c       nlist    -  # variables in the list:
c       nellist  -  (*) # values for each variable in the list
c       hlistnam -  (*) handle to variable names in list
c       status   -  Status flag:-
c                      0 = normal return
c                    !=0 = Error
c
      character*(*) symnam, symval(1:*)
      integer       arridx, mxlist, nlist, nsymval, symtyp, status
      integer       nellist(1:mxlist)
      integer       hlistnam(1:mxlist), hlistval(1:mxlist)
c
c-----------------------------------------------------------------------
c
c ... Include files
c
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Local variables:
c
      character  asymtyp*6, errmsg*132
      integer    ierr, ilist, ival, nchnam, nchval, nerr, strhandle
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer       lennb, tioerh
      integer       NS_find, NS_freename, NS_putname
      integer       NS_defarray, NS_getarrval, NS_putarrval
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      if (symtyp .EQ. LOCALSYM) then
        asymtyp = 'LOCAL'
      else
        asymtyp = 'GLOBAL'
      endif
c
c ... Look to see if the name is already defined in the list
c
      nchnam = lennb(symnam)
      ilist  = NS_find (symnam(1:nchnam), hlistnam, nlist)
c
c-----------------------------------------------------------------------
c ... Name is not on the list: add new list element
c-----------------------------------------------------------------------
c
      if (ilist .EQ. 0) then
c
c ..... Error if ARRIDX >= 0: we are trying to redefine an array
c       element of a non-existent array
c
        if (arridx .GE. 0) then
          write(errmsg,9800) asymtyp, symnam(1:nchnam)
          status = tioerh ('TIO', 'IODEFSYM', 2, errmsg, -1)
          return
        endif
c
c ..... Add variable name to the list if there is room
c
        if (nlist .GE. mxlist) then
          write(errmsg,9810) asymtyp, symnam(1:nchnam)
          status = tioerh ('TIO', 'IODEFSYM', 2, errmsg, -1)
          return
        endif
c
        nlist = nlist + 1
        ilist = nlist
        hlistnam(ilist) = NS_putname (symnam(1:nchnam))
c
c-----------------------------------------------------------------------
c ... Name is already on the list
c-----------------------------------------------------------------------
c
      else
c
c ..... Illegal to redefine an RVALUE command file parameter
c
        if (nellist(ilist) .EQ. 0) then
          write(errmsg,9820) symnam(1:nchnam)
          status = tioerh ('TIO', 'IODEFSYM', 2, errmsg, -1)
          return
c
c ..... Redefining a scalar or an entire array: delete all old values
c
        elseif ( arridx.EQ.-1 .OR.
     &          (arridx.EQ.0.AND.nellist(ilist).EQ.1) ) then
          call iosymdel (symnam, symtyp, 1, ierr)
c
c ..... Redefining a single array element: delete the old value, add
c       the new one right here and then return
c
        else
          if (arridx .LT. nellist(ilist)) then
            ierr =  NS_getarrval (hlistval(ilist), arridx, strhandle)
            if (strhandle .GT. 0) ierr = NS_freename (strhandle)
c
            nchval    = lennb(symval(1))
            strhandle = NS_putname (symval(1)(1:nchval))
            ierr      = NS_putarrval (hlistval(ilist), arridx,
     &                                strhandle)
          else
            write(errmsg,9830) symnam(1:nchnam), arridx,
     &                         nellist(ilist)
            status = tioerh ('TIO', 'IODEFSYM', 2, errmsg, -1)
          endif
          return
        endif
      endif
c
c-----------------------------------------------------------------------
c ... Create values for the symbol
c-----------------------------------------------------------------------
c
      nellist(ilist) = nsymval
      nerr = 0
c
c ... For a scalar symbol, hlistval is handle directly to the value
c
      if (nsymval .EQ. 1) then
        nchval = lennb(symval(1))
        hlistval(ilist) = NS_putname (symval(1)(1:nchval))
c
c ... For an array symbol, hlistval is a handle to array of handles
c
      else
        hlistval(ilist) = NS_defarray (0, nsymval-1, -1)
c
        if (hlistval(ilist) .GT. 0) then
c
c ....... Load array with handles to array element values
c
          if (symval(1) .NE. ' ') then
            do ival=1,nsymval
              nchval    = lennb(symval(ival))
              strhandle = NS_putname (symval(ival)(1:nchval))
              ierr      = NS_putarrval (hlistval(ilist), ival-1,
     &                                  strhandle)
              if (ierr .NE. 0) nerr = nerr + 1
            enddo
c
c ....... Load array with handles to undefined values
c
          else
            do ival=1,nsymval
              ierr = NS_putarrval (hlistval(ilist), ival-1, -1)
              if (ierr .NE. 0) nerr = nerr + 1
            enddo
          endif
        endif
c
      endif
c
      if (hlistval(ilist).LE.0 .OR. nerr.GT.0) then
        write(errmsg,9840) symnam(1:nchnam)
        call iosymdel (symnam, symtyp, 0, status)
        status = tioerh ('TIO', 'IODEFSYM', 2, errmsg, -1)
      endif
c
      return
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9800 format('Array reference to undefined ',a,' symbol: ',a)
 9810 format(a,' symbol list is full: Cannot create symbol ',a)
 9820 format('Cannot redefine command file parameter ',a)
 9830 format('Out-of-bounds index for array ',a,': iel,nel = ',2i4)
 9840 format('Error creating value(s) for variable: ',a)
c
      end
