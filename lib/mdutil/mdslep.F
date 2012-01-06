#if defined(ABSOFTsys) || defined(WIN32sys)
# define USE_FTN_SLEEP
#endif
      subroutine mdslep( scnds )
#if defined(WIN32sys)
      use IFPORT
#endif
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
c ... Machine-dependent routine to pause execution for a specified time
c
c ----------------------------------------------------------------------
c
c     Input:
c       scnds   -  time to pause (in seconds)
c
c     Output:  NONE
c
c     Internals:
c
c ----------------------------------------------------------------------
c
      real       scnds
c
c ----------------------------------------------------------------------
c
#if defined(USE_FTN_SLEEP)
c
c     This version uses the unixlib routine SLEEP
c
c
      call SLEEP (nint(scnds))
#else
c     ************************ UNIX Version ****************************
c
c     The UNIX version spawns a "sleep" SYSTEM call
c
c     UNIX specific internals:
c       istat   -  returned status of UNIX SYSTEM call
c       iwait   -  string containing pause time rounded to nearest sec.
c
      integer   istat
      character iwait*10
      integer   fsystm
c
      write (iwait,'(i10)') nint(scnds)
c
      istat = fsystm('sleep ' // iwait)
#endif
c      
      return
      end
