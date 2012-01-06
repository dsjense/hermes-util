      subroutine mdprmt( lutty,prompt )
c
c **********************************************************************
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
c **********************************************************************
c
c     Summary:
c
c     Routine to send a prompt to the output terminal w/o a trailing 
c     newline
c
c     **********************  UNIX Version  ****************************
c
c     THIS routine uses a HPUX extension $ format descriptor
c     UNIX does NOT typically use the 1st character for carriagecontrol, 
c     so NO leading blank is used.
c
c ----------------------------------------------------------------------
c
c     Input:  
c       lutty   -  Output unit for terminal
c       prompt  -  prompt string
c
c     Output:  NONE
c
c     Internals:  
c       i       -  loop index
c       ic      -  last non-blank character in prompt string
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:  NONE
c
c ... Included common decks:  NONE
c
c ----------------------------------------------------------------------
c
c     Declare variables:  
c
c     Passed:
      integer   lutty
      character prompt*(*)
c
c ... Parameters:  NONE
c
c ... Internal:
      integer   i, ic
c
c ----------------------------------------------------------------------
c
c
      do 1 i=len(prompt),1,-1
        if (prompt(i:i).ne.' ')then
          ic = i
          goto 2
        endif
    1 continue
      ic = 0
    2 continue
c
      if (ic.gt.0)then
        write(lutty,'(a,a,$)') prompt(1:ic) , ' '
      else
        write(lutty,'(''? '',$)')
      endif
c
      return
c
      end
