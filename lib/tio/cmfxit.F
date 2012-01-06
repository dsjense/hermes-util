      subroutine cmfxit (
c ... INPUT
     1 mode )
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
c ... Command file exit routine.  Four types of call are supported,
c     based on the value of MODE. Modes 0 and 1 are non-error exits, and
c     simply shut down the current level; modes 2 and 3 are error exits,
c     controlled by the user-selectable variable CFERLV:-
c        0 - Continue (Do not close command file)
c        1 - Close current command file
c        2 - Close open command files at all levels, returning
c            control to the main level
c
c     The four modes of CMFXIT are:-
c        0 - Normal exit call: close current level of command file
c        1 - Do all processing to shut down current level of command
c            file, but do not close the file (used by SAVARC)
c        2 - Error occurred processing file: follow CFERLV, except that
c            if FATALCFER = .TRUE., the current command file level is
c            unconditionally closed (override CFERLV=0 option)
c        3 - Same as  MODE=2, except that the current command file is
c            unconditionally closed (override CFERLV=0 option).
c     Mode 3 is used to force a command file to close BEFORE processing
c     an expanded command line in subroutine CMDINI (error in getlin).
c     Mode 2 is used when processing of internal TIO commands detects
c     an error (flpini, iocmfpdef) that needs the command file to close.
c     In this case, the FATALCFER flag is set, but the file is not
c     closed until the call to CMDXIT at the bottom of CMDINI.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments
c
      integer mode
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "ioctrl.inc"
#include "iocmdf.inc"
#include "ioif.inc"
#include "ioloop.inc"
#include "iolun.inc"
#include "iopar.inc"
#include "iosymbol.inc"
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character*32 symnam
      integer      i, icfp1, ierr, isym0, isym1, flvl, lvl,
     &             nchnam, status, tlvl
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer  NS_find, NS_freename, NS_getname, NS_putname
      integer  NS_putarrval
c
c
c=======================================================================
c ... BEGIN:
c=======================================================================
c
      if (cmflvl .EQ. 0) return
c
c ... Set up the levels of command file to close, from the current
c     level down to FLVL. We shut down only the current level, unless
c     there are errors (modes 2 and 3)
c
      flvl = cmflvl
c
      if (mode.EQ.2 .OR. mode.EQ.3) then
        if (cferlv.EQ.0) then
          if (mode.EQ.2 .AND. .NOT.fatalcfer) return
        elseif (cferlv .EQ. 2) then
          flvl = 1
        endif
      endif
c
      fatalcfer = .FALSE.
c
c ... Loop over command file levels
c
      do 200 lvl=cmflvl,flvl,-1
c
c ..... Close file (except in MODE 1), and free up the file name string
c
        if (mode .NE. 1) then
          close (unit=lucmd(lvl))
          ierr = NS_freename(hcmfnam(lvl))
        endif
c
c ..... Delete all local symbols and arrays defined at this level,
c       except for the command file parameters, which occupy the
c       first NCFPAR slots of this level's local symbol space.
c
        call iosymdel ('*', LOCALSYM, 0, status)
c
c ..... Complete handling of command file parameters
c
        tlvl  = lvl - 1
c
        do i=1,ncfpar(lvl)
          isym1 = loclsym(lvl) + i - 1
c
c ....... Free up it's name at this level
c
          ierr  = NS_freename(hlsymnam(isym1))
c
c ....... LVALUE parameter
c
          if (nellsym(isym1) .GT. 0) then
            icfp1  = loccfp(lvl) + i - 1
c
c ......... Load SYMNAM with the name of the local symbol used as the
c           the LVALUE parameter, and then free up the name in storage
c
            nchnam = NS_getname (hcfpsnm(icfp1), symnam)
            ierr   = NS_freename(hcfpsnm(icfp1))
c
c ......... Get this symbol's index in the local list at the call level
c
            isym0 = NS_find (symnam(1:nchnam), hlsymnam(loclsym(tlvl)),
     &                       nlsym(tlvl) )
c
c ......... If the symbol already exists ...
c
            if (isym0 .GT. 0) then
              isym0 = isym0 + loclsym(tlvl) - 1
c
c ........... For an unsubscripted symbol, simply copy the parameter's
c             handle and element count into the local symbol. Note that
c             if it is an array, the handle will point to the values
c
              if (cfpaidx(icfp1) .EQ. -1) then
                hlsymval(isym0) = hlsymval(isym1)
                nellsym(isym0)  = nellsym(isym1)
c
c ........... For a subscripted symbol, copy the parameter's handle
c             into the appropriate slot of the symbol's array of handles
c
              else
                ierr = NS_putarrval (hlsymval(isym0), cfpaidx(icfp1),
     &                               hlsymval(isym1) )
              endif
c
c ......... If symbol does not already exist, create a new symbol at
c           the calling level.
c
            else
              nlsym(tlvl) = nlsym(tlvl) + 1
              isym0 = nlsym(tlvl) + loclsym(tlvl) - 1
c
              hlsymnam(isym0) = NS_putname (symnam(1:nchnam))
              hlsymval(isym0) = hlsymval(isym1)
              nellsym(isym0)  = nellsym(isym1)
            endif
c
c ....... Free up RVALUE parameter: simply delete parameter value string
c
          else
            ierr = NS_freename(hlsymval(isym1))
          endif
        enddo
c
c ..... Destroy command file parameters
c
        nlsym(lvl) = 0
        lstcfp     = lstcfp - ncfpar(lvl)
c
c ..... Gracefully shut down any loops running at this level
c
        if (looplv(lvl) .GT. 0) then
          nlpln  = beglpl(1,lvl) - 1
          lstlch = lplche(nlpln)
          looplv(lvl) = 0
        endif
c
c ..... And shut down any conditional statement processing
c
        iflvl(cmflvl) = 0
  200 continue
c
c ... Reset archiving to base level
c
      arclvl = arcbas
c
c ... Switch logical unit #'s if this was the current input
c
      if (luin .EQ. lucmd(cmflvl)) then
        if (flvl .GT. 1) then
          luin = lucmd(flvl-1)
        else
          luin = luin0
        endif
      endif
c
c ... And finally reset command file level
c
      cmflvl = flvl - 1
c
      return
      end
