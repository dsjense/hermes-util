      subroutine pfmflu ( lun, ierr )
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
c ----------------------------------------------------------------------
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
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       ierr    -  error flag;
c                      0,          No error
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pferrs.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:  
c
c     Passed:
      integer lun, ierr
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFMFLU' )
c
#if defined(HPsys)
c     **********************  HPUX Version  ****************************
c
c     HPUX version first uses the fstream intrinsic to return the stdio
c     stream associated with a Fortran logical unit.  It then calls the 
c     C stdio function fflush to flush the stream.
c     Note:  a Fortran flush intrinsic will be standard in HP-UX 10.0
c
c          (ierr is the return value of the C FFLUSH function --
c           0 indicates successful completion)

c ... Internals:
c       stream  -  pointer to stdio stream associated with lun
c
      integer*4 stream
c ... functions
      integer fflush
#elif defined(CRAsys)
c     **********************  UNICOS Version  **************************
c
c    CRAY version uses the Fortran intrinsic function FLUSH.

c          (ierr is the status returned by the Fortran FLUSH function --
c           0 indicates successful completion)
c
c ... Internals:
c       istat   -  error status from CRAY Fortran flush call
      integer istat
#endif
c
c ----------------------------------------------------------------------
c
c ... write status message if DEBUG is on
c
      if (ludbg.ne.OFFLUN)then
        write (ludbg,1000) PFMODU, 'Entry', ierr
      endif
c
c ... return if error flag is not zero on entry
c
      if(ierr.ne.0)then
        call pfserr ( PFMODU, 0, 0, 'Error Flag Non-zero on Entry' )
        goto 999
      endif
c
#if defined(HPsys)
c ... HP-UX version
c
c ... call Fortran intrinsic fstream to get stream associated w/ lun
      stream = fstream(lun)
c ... call C stdio function fflush, need to pass stream pointer by value
      ierr = fflush(%VAL(stream))
#elif defined(CRAsys)
c ... CRAY version
c
c ... call Fortran intrinsic flush; return status
      call flush(lun,istat)
      ierr = istat
#elif defined(WIN32sys)
c     ***********************  WIN32 Version  **************************
c
c     Use Intel compilers COMMITQQ function in module IFCORE
c
      if ( commitqq(lun) ) then
        ierr = 0
      else
        ierr = 1
      endif
#else
c ... All other implementations
c
c ... call Fortran intrinsic flush; return 0 since status not returned
c     (It has a slightly different name for some machines)
# if defined(AIXsys)
#  define flush flush_
# elif defined(ABSOFTsys)
#  define flush FLUSH
# endif
c
      call flush(lun)
      ierr = 0
#endif
c
      if(ierr.ne.0)then
        call pfserr ( PFMODU, ierr, lun,
     &                'Machine-Dependent Flush Error' )
      endif
c
  999 continue
c
c ... write status message if DEBUG is on and return
c
      if (ludbg.ne.OFFLUN)then
        write (ludbg,1000) PFMODU, 'Exit', ierr
      endif
c
 1000 format(' %PFF-',a,'-DBG:  ',a,' Error Status = ',i5)
c
      return
      end
