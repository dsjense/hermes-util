#if !defined(ABSOFTsys) && !defined(WIN32sys)
c     Use ANSI VT100 escape sequences
# define VT100_SEQUENCES_AVAILABLE
#endif
      subroutine mdswid(lutty,flag,width)
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
c     Routine to change the width of the terminal to and from 80/132
c
c     If VT100 escape sequences are not available, MDSWID assumes that
c     the window can be resized manually to the requested size -- so it
c     returns a width of 80 or 132 depending upon the value of flag.
c
c ----------------------------------------------------------------------
c
c     Input:  
c       lutty   -  Output unit for terminal
c       flag    -  set/reset flag for terminal width:
c                  = 0,       reset to 80
c                  otherwise, set to 132
c
c     Output:  
c       width   -  Width the terminal was actually set to by this call
c
c     Internals:  
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
      integer   flag, lutty, width
c
c ... Parameters:  NONE
c
c ... Internal:
c
c ----------------------------------------------------------------------
c
      if (flag.eq.0) then
        width = 80
#if defined(VT100_SEQUENCES_AVAILABLE)
        write(lutty,'(a)') char(27) // '[?3l'
#endif
      else
        width = 132
#if defined(VT100_SEQUENCES_AVAILABLE)
        write(lutty,'(a)') char(27) // '[?3h'
#endif
      endif
c
      return
c
      end
