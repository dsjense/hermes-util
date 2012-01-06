      subroutine iosymdel (
c ... INPUT
     1  usymnam, symtyp, delmode,
c ... OUTPUT
     2  status)
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
c     Delete one or more symbols and/or arrays from either the local or
c     global list. If USYMNAM is not blank, it is used as the name of
c     the symbol to be deleted.  If it is blank, the name of one or
c     more symbols and/or are obtained from the command line.
c
c     By default, the local symbol list at the current command file
c     level is searched first.  If a match is found, the symbol or 
c     array is deleted. If not the global symbol list is tried.  Note that
c     if the name is present in both lists, only the local one will
c     be deleted.
c
c     Finally, if the first symbol name is '*', all local symbols and
c     arrays at the current command file level are deleted.  Note that
c     the wild-card option has no effect on global symbols -- they must
c     be explicitly deleted by name.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       usymnam   - User-supplied symbol name (if blank get from input)
c       symtyp    - Symbol type: either LOCALSYM, GLOBALSYM, or 0 for
c                   either type. 
c       delmode   - Delete mode:-
c                      0 - Remove symbol entirely from the list
c                      1 - Only delete values, keeping name in list. This
c                          is used in preparation to redefining the
c                          symbol's value(s)
c     OUTPUT:
c       status  -  Status flag:-
c                      0 = normal return
c                    !=0 = Symbol not found in specified list
c
      character*(*) usymnam
      integer       delmode, status, symtyp
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
      integer       lennb, tioerh
      integer       NS_find, NS_freename
      integer       NS_freearray, NS_getarrval
      logical       endofl
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS and variables:
c
      character*132 errmsg, symnam
      integer       iel, ierr, ioff, isym, nchnam, nerr, nsym, strhandle
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      status = 0
c
c ... Get first symbol name: either user-supplied, or from command line
c
      if (usymnam .NE. ' ') then
        symnam  = usymnam
        nchnam = lennb(symnam)
      else
        symnam = chrpar (RQD, 'symbol name', ' ', ' ', status, nchnam)
        if (status .NE. 0) return
      endif
c
c ... Wild-card option: Delete all local symbols and arrays, except for
c     this level's command file parameters, occupying first ncfpar slots
c     (These are only deleted in CMFXIT when the command file exits)
c
      nerr = 0
      if (symnam .EQ. '*') then
        do isym = loclsym(cmflvl)+nlsym(cmflvl) -1,
     &            loclsym(cmflvl)+ncfpar(cmflvl), -1
          ierr = NS_freename (hlsymnam(isym))
c
          if (nellsym(isym) .EQ. 1) then
            ierr = NS_freename (hlsymval(isym))
          else
            do iel=nellsym(isym)-1,0,-1
              ierr =  NS_getarrval (hlsymval(isym), iel, strhandle)
              if (strhandle .GT. 0) ierr = NS_freename (strhandle)
            enddo
            ierr = NS_freearray (hlsymval(isym))
          endif
        enddo
c
        nlsym(cmflvl) = ncfpar(cmflvl)
c
c ... Delete one or more symbols explicitly by name
c
      else
   10   continue
c
c ....... First try to match name in local symbol list
c
          if (symtyp .NE. GLOBALSYM) then
c
c ......... If removing the name from the list, skip over the command
c           file parameters (they are explicitly removed only when the
c           file exits).
c
            if (delmode .EQ. 0) then
              ioff = loclsym(cmflvl) + ncfpar(cmflvl) - 1
              isym = NS_find (symnam(1:nchnam),
     &               hlsymnam(loclsym(cmflvl)+ncfpar(cmflvl)),
     &               nlsym(cmflvl)-ncfpar(cmflvl) )
c
c ......... If just deleting values, include command file parameters
c
            else
              ioff = loclsym(cmflvl) - 1
              isym = NS_find (symnam(1:nchnam),
     &               hlsymnam(loclsym(cmflvl)), nlsym(cmflvl) )
            endif
c
            if (isym .GT. 0) then
              isym = isym + ioff
c
c ........... Delete symbol values
c
              if (nellsym(isym) .EQ. 1) then
                ierr = NS_freename (hlsymval(isym))
              else
                do iel=nellsym(isym)-1,0,-1
                  ierr =  NS_getarrval (hlsymval(isym), iel, strhandle)
                  if (strhandle .GT. 0) ierr = NS_freename (strhandle)
                enddo
                ierr = NS_freearray (hlsymval(isym))
              endif
c
c ........... Remove symbol from the list
c
              if (delmode .EQ. 0) then
                ierr = NS_freename (hlsymnam(isym))
                nsym = nlsym(cmflvl) + loclsym(cmflvl) - 1
c
                if (isym .LT. nsym) then
                  hlsymnam(isym) = hlsymnam(nsym)
                  hlsymval(isym) = hlsymval(nsym)
                  nellsym(isym)  = nellsym(nsym)
                endif
                nlsym(cmflvl) = nlsym(cmflvl) - 1
              endif
c
              go to 20
            endif
          endif
c
c ....... If no match on the local list, try to match a global symbol
c
          if (symtyp .NE. LOCALSYM) then
            isym = NS_find ( symnam(1:nchnam), hgsymnam, ngsym)
c
            if (isym .GT. 0) then
c
c ........... Delete symbol values
c
              if (nelgsym(isym) .EQ. 1) then
                ierr = NS_freename (hgsymval(isym))
              else
                do iel=nelgsym(isym)-1,0,-1
                  ierr =  NS_getarrval (hgsymval(isym), iel, strhandle)
                  if (strhandle .GT. 0) ierr = NS_freename (strhandle)
                enddo
                ierr = NS_freearray (hgsymval(isym))
              endif
c
c ........... Remove symbol from the list
c
              if (delmode .EQ. 0) then
                ierr = NS_freename (hgsymnam(isym))
c
                if (isym .LT. ngsym) then
                  hgsymnam(isym) = hgsymnam(ngsym)
                  hgsymval(isym) = hgsymval(ngsym)
                  nelgsym(isym)  = nelgsym(ngsym)
                endif
                ngsym = ngsym - 1
              endif
c
              go to 20
            endif
          endif
c
c ....... Undefined symbol
c
          write(errmsg,'(2a)') 'Undefined symbol: ',symnam(1:nchnam)
          ierr = tioerh ('TIO', 'IOSYMDEL', 2, errmsg, -1)
          nerr = nerr + 1
c
c ....... If using the command line for input, process additional names
c
   20     continue
          if (usymnam.EQ.' ' .AND. .NOT.endofl(status)) then
            symnam = chrpar (RQD, 'symbol name', ' ', ' ',
     &                        status, nchnam)
            if (status .EQ. 0) go to 10
          endif
      endif
c
      if (nerr .GT. 0) status = -1
      return
      end
