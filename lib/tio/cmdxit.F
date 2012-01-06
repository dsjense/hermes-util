      subroutine cmdxit (
c ... INPUT
     1  arccmd, status,
c ... OUTPUT
     2  arcerr)
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
c ... CMDXIT is called after the calling program has finished processing
c     a command returned by CMDINI.  It performs error handling and
c     archiving of the user input line.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       arccmd - Logical flag indicating this command is to be archived
c       status - Status flag for user command:
c                  .GE. 0  -  Command successfully executed
c                  .LT. 0  -  Error processing user command
c
c     OUTPUT:
c       arcerr - Archive error status flag:
c                   0  -  Normal return
c                  <0  -  Error in archiving: IOARCL will disable
c                         further archiving until user resets it
c
      logical  arccmd
      integer  arcerr, status
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and common block decks:
c
#include "ioctrl.inc"
#include "iocmdf.inc"
#include "ioloop.inc"
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
c ... Archive command
c
      if (arccmd .AND. arcvln .AND. status.GE.0)
     1    call ioarcl (1, ' ', arcerr)
c
c ... Handle command files or FOR loops if any errors occurred
c
      if (status .LT. 0) then
        if (cmflvl .GT. 0) then
          call cmfxit(2)
        elseif (looplv(cmflvl) .GT. 0) then
          nlpln  = beglpl(1,cmflvl) - 1
          lstlch = lplche(nlpln)
          looplv(cmflvl) = 0
        endif
      endif
c
      return
      end
