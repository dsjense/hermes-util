      subroutine iodefsca (
c ... INPUT
     1 symtyp, usymnam, usymval,
c ... OUTPUT
     2 status)
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
c ... Add or redefine the value of a scalar symbol: either an lvalue
c     command file parameter, a local symbol, or a global symbol.
c
c     The symbol's value can either be explicitly passed in USYMVAL, or
c     or extracted from the command line if USYMVAL is blank. In the
c     the latter case, multiple parameters on the command line are
c     interpreted using the RPN stack calculator.
c
c ... If usymnam is of the form name[n], IODEFSYM will attempt to
c     put the string USYMVAL into the n'th element of array 'name'
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       symtyp  -  Symbol type: either LOCALSYM or GLOBALSYM. Note that
c                  the command file parameters are in the LOCAL list
c       usymnam -  Symbol or command file parameter name
c       usymval -  User-supplied symbol value
c
c     OUTPUT:
c       status  -  Status flag:-
c                      0 = normal return
c                    !=0 = Error
c
      character*(*) usymnam, usymval
      integer       symtyp, status
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "ioctrl.inc"
#include "iocmdf.inc"
#include "iopar.inc"
#include "iosymbol.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer  tioerh
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS and variables:
c
      integer    MXSTACK, NVAL
      parameter (MXSTACK = 20)
      parameter (NVAL    =  1)
c
      character*132    errmsg, symval
      character*80     symnam, stkstr(1:MXSTACK)
      integer          stkival(1:MXSTACK), stknch(1:MXSTACK)
      integer          stktyp(1:MXSTACK)
      integer          arridx, nchnam, nchval, nstack, typval
      double precision stkdval(1:MXSTACK)
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      status = 0
c
c ... Create a local copy of the symbol name, getting integer value of
c     index and stripping off substring if it is an array element
c
      call parsepar (usymnam, symnam, nchnam, arridx)
c
c ... Get symbol value: either user-supplied, or from the command line
c
      if (usymval .NE. ' ') then
        symval  = usymval
      else
        call ioloadpar (MXSTACK, ' ', nstack, stkstr, status)
        if (status .NE. 0) return
c
        if (nstack .EQ. 0) then
          write(errmsg,9800) symnam(1:nchnam)
          status = tioerh ('TIO', 'IOSYMDEF', 2, errmsg, -1)
          return
        elseif (nstack .GT. 1) then
          stktyp(1) = NUL
          call evalexpr (nstack, stktyp , stkstr ,
     &                           stkival, stkdval, stknch, status )
          if (status .NE. 0) return
        endif
        symval = stkstr(1)
      endif
c
c-----------------------------------------------------------------------
c
c ... Process LOCAL symbol (including command file parameters at this 
c     level)
c
      if (symtyp .EQ. LOCALSYM) then
        call iodefsym (MXLSYM-loclsym(cmflvl)+1, LOCALSYM, symnam,
     &    arridx, NVAL, symval,
     &    nlsym(cmflvl), nellsym(loclsym(cmflvl)),
     &    hlsymnam(loclsym(cmflvl)), hlsymval(loclsym(cmflvl)), status)
c
c ... Add symbol to the GLOBAL list
c
      elseif (symtyp .EQ. GLOBALSYM) then
        call iodefsym (MXGSYM, GLOBALSYM, symnam, arridx, NVAL, symval,
     &    ngsym, nelgsym, hgsymnam, hgsymval, status)
      endif
c
      return
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9800 format('Syntax error defining symbol ',a)
c
      end
