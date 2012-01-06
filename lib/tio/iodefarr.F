      subroutine iodefarr (
c ... INPUT
     1 arrtyp, arrnam, unelarr, uarrval,
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
c ... Add an array to either the local or global list, depending on input
c     parameter ARRTYP.  The array name is explicitly passed down by
c     the calling program in variable ARRNAM.
c
c     The array element values can be initialized as the array is 
c     created, or the array can simply be created as a place-holder
c     for array elements to be created later. If UNELARR = 0, the
c     array value information is extracted from the command line. Two
c     formats are used
c
c       1. ^def arrnam [num]
c       2. ^def arrnam [val_1 val_2 ... val_num]
c        
c     The first form simply creates an empty array of num elements,
c     the second explicitly defines their values.
c
c     For explicit input of values, if uarrval(1) = ' ', an empty array
c     of UNELARR elements is created. Otherwise, UARRVAL is assumed
c     to contain the array element values.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       arrtyp  -  Array type: either LOCALSYM or GLOBALSYM
c       arrnam  -  Array name
c       unelarr -  # elements in the array (0 => get array data from
c                  TIO command line).
c       uarrval -  User-supplied arrbol value (only used if unelarr > 0)
c
c     OUTPUT:
c       status  -  Status flag:-
c                      0 = normal return
c                    !=0 = Error
c
      character*(*) arrnam, uarrval(1:*)
      integer       arrtyp, status, unelarr
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
      character*132 chrpar
      integer       chrtoi, lennb, partyp, tioerh
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS and variables:
c
      integer    MXARREL, ARRIDX
      parameter (MXARREL = 200)
      parameter (ARRIDX  =  -1)
c
      character*132 errmsg, firstp, arrval(1:MXARREL)
      integer       nch, nchnam, nelarr, npar
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      status = 0
      nchnam = lennb(arrnam)
c
c ... Process input from the command line
c
      if (unelarr .EQ. 0) then
        firstp = chrpar (RQD, 'array values', ' ', '[', status, nch)
        if (firstp .NE. LBRAKT) then
          write(errmsg,9800) arrnam(1:nchnam)
          status = tioerh ('TIO', 'IODEFARR', 2, errmsg, -1)
          return
        endif
c         
c ..... Load parameters after the left bracket
c
        call ioloadpar (MXARREL, '[]', npar, arrval, status)
        if (status .NE. 0) return
c
        if (npar .LT. 2) then
          write(errmsg,9800) arrnam(1:nchnam)
          status = tioerh ('TIO', 'IODEFARR', 2, errmsg, -1)
          return
        endif
c
        if (arrval(npar) .NE. RBRAKT) then
          write(errmsg,9800) arrnam(1:nchnam)
          status = tioerh ('TIO', 'IODEFARR', 2, errmsg, -1)
          return
        endif
c
        if (npar.EQ.2 .AND. partyp(arrval(1)).EQ.INT) then
          nelarr = chrtoi(arrval(1), status)
          arrval(1) = ' '
        else
          nelarr = npar - 1
        endif
c
      endif
c
c-----------------------------------------------------------------------
c ... Add array to the LOCAL list
c-----------------------------------------------------------------------
c
      if (arrtyp .EQ. LOCALSYM) then
        if (unelarr .GT. 0) then
          call iodefsym (MXLSYM-loclsym(cmflvl)+1, LOCALSYM, arrnam,
     &      ARRIDX, unelarr, uarrval,
     &      nlsym(cmflvl), nellsym(loclsym(cmflvl)),
     &      hlsymnam(loclsym(cmflvl)), hlsymval(loclsym(cmflvl)),status)
c
        else
          call iodefsym (MXLSYM-loclsym(cmflvl)+1, LOCALSYM, arrnam,
     &      ARRIDX, nelarr, arrval,
     &      nlsym(cmflvl), nellsym(loclsym(cmflvl)),
     &      hlsymnam(loclsym(cmflvl)), hlsymval(loclsym(cmflvl)),status)
c
        endif
c
c-----------------------------------------------------------------------
c ... Add array to the GLOBAL list
c-----------------------------------------------------------------------
c
      elseif (arrtyp .EQ. GLOBALSYM) then
        if (unelarr .GT. 0) then
          call iodefsym (MXGSYM, GLOBALSYM, arrnam,
     &      ARRIDX, unelarr, uarrval,
     &      ngsym, nelgsym, hgsymnam, hgsymval, status)
c
        else
          call iodefsym (MXGSYM, GLOBALSYM, arrnam,
     &      ARRIDX, nelarr, arrval,
     &      ngsym, nelgsym, hgsymnam, hgsymval, status)
c
        endif
      endif
c
      return
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9800 format('syntax error defining array: ',a)
c
      end
