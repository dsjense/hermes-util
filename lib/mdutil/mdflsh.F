      integer function mdflsh ( lun )
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
c     Routine to flush the output buffer associated with a Fortran 
c     logical unit
c
#if defined(MSFsys)
      use portlib
#elif defined(WIN32sys)
      use ifcore
#endif
c
c ----------------------------------------------------------------------
c
c     Input:  
c       lun     -  Output unit to be flushed
c
c     Output:  NONE
c
c     Return Value
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
      integer lun
c
c ... Parameters:  NONE
c
c ----------------------------------------------------------------------
c
#if defined(HPsys)
c     **********************  HPUX Version  ****************************
c
c    HPUX version first uses the fstream intrinsic to return the stdio
c    stream associated with a Fortran logical unit.  It then calls the 
c    C stdio function fflush to flush the stream.
c    Note:  a Fortran flush intrinsic will be standard in HP-UX 10.0
c
c       The return value of the C FFLUSH function is returned --
c       0 indicates successful completion
c
c ... Internals:
c       stream  -  pointer to stdio stream associated with lun
c
      integer*4 stream
c ... functions
      integer fflush
c
      stream = fstream(lun)
      mdflsh = fflush(%VAL(stream))
#elif defined(CRAsys)
c     **********************  UNICOS Version  **************************
c
c    CRAY version uses the Fortran intrinsic function FLUSH.
c
c       The status returned by the Fortran FLUSH function is returned --
c       0 indicates successful completion
c
c ... Internals:
c       istat   -  error status from CRAY Fortran flush call

      integer istat
c
c ... call Fortran intrinsic flush; return status
      call flush(lun,istat)
      mdflsh = istat
#elif defined(WIN32sys)
c     ***********************  WIN32 Version  **************************
c
c     Use Intel compilers COMMITQQ function in module IFCORE
c
      if ( commitqq(lun) ) then
        mdflsh = 0
      else
        mdflsh = 1
      endif
#else
c     **********************  All other systems  ***********************
c
c    Use the Fortran intrinsic function FLUSH.
c    This routine DOES NOT return an error status, so on these systems,
c    mdflsh ALWAYS returns normal completion (0).
c
# if defined(AIXsys)
#  define flush flush_
# elif defined(ABSOFTsys)
#  define flush FLUSH
# endif
      call flush(lun)
      mdflsh = 0
#endif
c
      return
      end
