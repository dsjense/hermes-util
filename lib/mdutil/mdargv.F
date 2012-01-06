#if defined(ABSOFTsys)
# define PROMPT_FOR
#endif
      integer function mdargv(n,str)
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
c **********************************************************************
c
c     Summary:
c
c     Machine-dependent routine to return a command line argument.
c
c ----------------------------------------------------------------------
c
c     Input:  
c       n      -  # of argument to be returned
c
c     Output:  
c       str    -  string containing the requested argument
c
c     Return Value:
c        >= 0  -  # of characters in argument
c        <  0  -  Error flag
c                 -1  -  nth argument was not supplied
c                 -2  -  Not Implemented
#if defined(PROMPT_FOR)
c                 -3  -  Error obtaining unused file unit
c                 -4  -  Error accessing command arguments
c                 -5  -  Insufficient buffer space for command arguments
#endif
c
c     Internals:  
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c
c ... Included common decks:
c
c ----------------------------------------------------------------------
c
c     Declare variables:  
c
c     Passed:
      integer   n
      character str*(*)
c
c ... Parameters:  NONE
c
#if defined(HPsys)
c     **********************  HPUX Version  ****************************
c
c     For HP Fortran, use the igetarg(n,str,max) call
c
c ... Internal:
      integer   igetarg
c
c ----------------------------------------------------------------------
c
      mdargv = igetarg(n,str,len(str))
#elif defined(CRAsys)
c     **********************  UNICOS Version  **************************
c
c     For UNICOS Fortran, use the getarg(n,str) call
c
c ... Internal:
      integer   getarg, iargc
c
c ----------------------------------------------------------------------
c
      if ( iargc().lt.n ) then
        mdargv = -1
        str = ' '
        return
      endif
c
      mdargv = getarg(n,str)
#elif defined(PROMPT_FOR)
c     ************************ prompting Version  **********************
c
c     For machines that don't have command line argument capability 
c     (e.g., MAC-OS), prompt the user for the command line arguments.
c     This is done in the routine "mdargs" so that both "mdargc" and
c     "mdargv" have access to the saved arguments.
c
c ... Internal:
      integer  mdargs
c
c ----------------------------------------------------------------------
c
      mdargv = mdargs(n, str)
#else
c
c     For all other systems, use the getarg(n,str) call
c
c ... Internal:
      integer   i
      integer   iargc
c
      integer   lstchr
c
c ----------------------------------------------------------------------
c
      if ( iargc().lt.n ) then
        mdargv = -1
        str = ' '
        return
      endif
c
      call getarg(n,str)
      mdargv = lstchr(str)
#endif
c
      return
      end
