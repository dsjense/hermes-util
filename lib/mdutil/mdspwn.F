      subroutine mdspwn( prompt )
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
c ... Machine-dependent routine to spawn an interactive subprocess
c
c     **********************  UNIX Version  ****************************
c
c     The UNIX version uses the GETENV system call to find the user's 
c     shell.  Unless it is KSH, SH will be used by the spawned subprocess.  
c     The PUTENV system call is used to set PS1 to the prompt specified.
c     Finally, the SYSTEM system call is used to start the shell.
c
c ----------------------------------------------------------------------
c
c     Input:
c       prompt  -  terminal prompt for the interactive subprocess
c
c     Output:  NONE
c
c     Internals:
c       istat   -  error status indicator
c
c
      character prompt*(*)
c
      integer   istat
c
c     UNIX specific internals:
c       shell   -  string to hold value of SHELL environment variable
c
      character shell*24
      integer   fgeten, fputen, fsystm
c
c ----------------------------------------------------------------------
c
      istat = fgeten('SHELL',shell)
      if ( istat.eq.0 ) then
        if (index(shell,'/ksh').eq.0) then
          shell = '/bin/sh'
        endif
      else
        shell = '/bin/sh'
      endif
c
      istat = fputen('PS1',prompt)
c
      istat = fsystm(shell)
c
      return
      end
