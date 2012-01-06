      subroutine iovarval (
c ... INPUT
     &  vartyp, varidx, arridx,
c ... OUTPUT
     &  varval, nchval, status)
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
c ... Subroutine IOVARVAL returns the value of a defined TIO variable,
c     based on its type and index. The different types are:-
c
c       1. Local symbol, including command file parameters passed
c          from the next lower level
c       2. Global symbol
c       3. FOR loop variable
c
c ... No error checking is done for VARIDX: it assumed that the VARTYP
c     and VARIDX have been computed by subroutine IOVARTYP. However
c     for arrays, ARRIDX is checked to ensure it is in bounds, and
c     that the array element string has actually been defined.
c
c-----------------------------------------------------------------------
c
c ... Function arguments:
c
c     INPUT:
c       vartyp  -  Variable type index
c       varidx  -  Index of variable in appropriate data structure
c       arridx  -  Array index:
c                   -1 - Return scalar value, or ALL array elements
c                    0 - Return scalar value, or first array element
c                   >0 - Return arridx'th array element
c
c     OUTPUT:
c       varval  -  Variable equivalence string
c       nchval  -  # characters in VARVAL
c       status  -  Error status: 0 = OK, -3 = ERROR
c
      character*(*) varval
      integer       arridx, nchval, status, vartyp, varidx
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and Common block decks:
c
#include "iocmdf.inc"
#include "ioloop.inc"
#include "iopar.inc"
#include "iosymbol.inc"
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character*132 errmsg, symnam
      integer       i, nchnam, nelsym, nerr, strhandle, voc
c
c-----------------------------------------------------------------------
c
c ... Function names
c
      integer tioerh
      integer NS_getarrval, NS_getname
c
c=======================================================================
c
      status = 0
c
c ... FOR loop variable: ignore ARRIDX
c
      if (vartyp .EQ. FORLOOPVAR) then
c
        varval = loopvc(varidx,cmflvl)
        nchval = nchlpv(varidx,cmflvl)
c
c ... Local symbol or command file parameter
c
      elseif (vartyp .EQ. LOCALSYM) then
c
c ..... Handle read-only symbols (nellsym = 0) as scalars for evaluation
c
        nelsym = max(nellsym(varidx), 1)
c
c ..... Scalar
c
        if (nelsym .EQ. 1) then
          if (arridx .LE. 0) then
            if (hlsymval(varidx) .GT. 0) then
              nchval = NS_getname (hlsymval(varidx), varval)
            else
              nchnam = NS_getname (hlsymnam(varidx), symnam)
              write(errmsg,9800) symnam(1:nchnam)
              status = tioerh ('TIO', 'IOVARVAL', 2, errmsg, -3)
            endif
          else
            nchnam = NS_getname (hlsymnam(varidx), symnam)
            write(errmsg,9820) symnam(1:nchnam), arridx, nelsym
            status = tioerh ('TIO', 'IOVARVAL', 2, errmsg, -3)
          endif
c
c ..... Array
c
        else
          if (arridx .GE. 0 .AND. arridx. LE. nellsym(varidx)-1) then
            status = NS_getarrval (hlsymval(varidx), arridx, strhandle)
c
            if (strhandle .GT. 0) then
              nchval = NS_getname (strhandle, varval)
            else
              nchnam = NS_getname (hlsymnam(varidx), symnam)
              write(errmsg,9810) arridx, symnam(1:nchnam)
              status = tioerh ('TIO', 'IOVARVAL', 2, errmsg, -3)
            endif
          elseif (arridx .EQ. -1) then
            voc  = 0
            nerr = 0
c
            do i=0,nellsym(varidx)-1
              status = NS_getarrval (hlsymval(varidx),i,strhandle)
c
              if (strhandle .GT. 0) then
                nchval = NS_getname (strhandle, varval(voc+1:))
                voc    = voc + nchval + 1
                varval(voc:voc) = ' '
              else
                nchnam = NS_getname (hlsymnam(varidx), symnam)
                write(errmsg,9810) i, symnam(1:nchnam)
                status = tioerh ('TIO', 'IOVARVAL', 2, errmsg, -3)
                nerr = nerr + 1
              endif
            enddo
c
            nchval = voc - 1
            if (nerr .GT. 0) status = -3
          else
            nchnam = NS_getname (hlsymnam(varidx), symnam)
            write(errmsg,9820) symnam(1:nchnam), arridx, nellsym(varidx)
            status = tioerh ('TIO', 'IOVARVAL', 2, errmsg, -3)
          endif
        endif
c
c ... Global symbol
c
      elseif (vartyp .EQ. GLOBALSYM) then
c
c ..... Scalar
c
        if (nelgsym(varidx) .EQ. 1) then
          if (arridx .LE. 0) then
            nchval = NS_getname (hgsymval(varidx), varval)
          else
            nchnam = NS_getname (hgsymnam(varidx), symnam)
            write(errmsg,9820) symnam(1:nchnam), arridx, nelgsym(varidx)
            status = tioerh ('TIO', 'IOVARVAL', 2, errmsg, -3)
          endif
c
c ...... Array
c
        else
c
          if (arridx .GE. 0 .AND. arridx. LE. nelgsym(varidx)-1) then
            status = NS_getarrval (hgsymval(varidx), arridx, strhandle)
c
            if (strhandle .GT. 0) then
              nchval = NS_getname (strhandle, varval)
            else
              nchnam = NS_getname (hgsymnam(varidx), symnam)
              write(errmsg,9810) arridx, symnam(1:nchnam)
              status = tioerh ('TIO', 'IOVARVAL', 2, errmsg, -3)
            endif
          elseif (arridx .EQ. -1) then
            voc  = 0
            nerr = 0
c
            do i=0,nelgsym(varidx)-1
              status = NS_getarrval (hgsymval(varidx),i,strhandle)
c
              if (strhandle .GT. 0) then
                nchval = NS_getname (strhandle, varval(voc+1:))
                voc    = voc + nchval + 1
                varval(voc:voc) = ' '
              else
                nchnam = NS_getname (hgsymnam(varidx), symnam)
                write(errmsg,9810) i, symnam(1:nchnam)
                status = tioerh ('TIO', 'IOVARVAL', 2, errmsg, -3)
                nerr = nerr + 1
              endif
            enddo
c
            nchval = voc - 1
            if (nerr .GT. 0) status = -3
          else
            nchnam = NS_getname (hgsymnam(varidx), symnam)
            write(errmsg,9820) symnam(1:nchnam), arridx, nelgsym(varidx)
            status = tioerh ('TIO', 'IOVARVAL', 2, errmsg, -3)
          endif
        endif
c
      endif
c
      return
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9800 format('value for symbol ',a,' has not been defined')
 9810 format('element ',i3,' of array ',a,' has not been defined')
 9820 format('Out-of-bounds index for array ',a,': iel,nel = ',2i4)
c
      end
