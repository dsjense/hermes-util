      subroutine pfuskiparr (fid, floc, lenfa, ierr)
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
c
c     Summary:
c
c       - This is a utility routine that skips over a floating-point
c         array, positioning the file pointer to the first word past
c         past the end of the array. It assumes that the file pointer
c         is currently located at the first word of the array.
c       - It is primarily designed for PFF-dataset read routines operating
c         in "just-return-file-pointers-to-floating-point-array" mode. In
c         addition to moving the internal file pointer, it also returns:-
c           * Pointer to first word of the array
c           * Length of the array
c       - Format for data primitive <FARRAY> (see pfrfa.F for more details):
c           IF (Float_precision = FULL)
c             <INT>          FP_FULL
c             <INT>          foff10
c             <LONG>         LENGTH
c             <F4ARRAY>      farray
c           ELSE
c             <FLOAT>        OFFSET
c             <FLOAT>        SCALE
c             <LONG>     LENGTH
c             LOOP 1,LENGTH   \
c               <INT>         -  converted integer array
c             ENDLOOP         /
c           ENDIF
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       floc    -  Pointer to first word of the array
c       lenfa   -  Length of floating point array
c       ierr    -  error status flag:
c                      0,  No error
c                    !=0,  Errors detected by utility routines, pfu*
c
c ----------------------------------------------------------------------
c
c     Internals:
c       floc    -  file pointer for file
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfstfg  -  RE
c
c ... Included common decks:
c ... pfbufp
c       r:    LPRBUF
c       w:    prmbuf(*)
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pfbufp.inc"
#include "pferrs.inc"
c
#include "mdfpos.h"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ierr, lenfa
      HU_FPOS_TYPE floc
c
c     Internal
      integer   fprec
      HU_FPOS_TYPE pfutel
c
c ... Parameters:
      character PFMODU*10
      parameter ( PFMODU = 'PFUSKIPARR' )
c
c ... External Block Data
      external  pffbd
c
c ----------------------------------------------------------------------
c
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
c ----------------------------------------------------------------------
c
      floc = pfutel(fid, ierr)
c
c ... Read in precision flag (if present) and base-10 offset
c ... For reduced precision, skip over 4 extra words in array header
c
      call pfusio (fid, RE, 2, prmbuf, ierr)
      if (prmbuf(1).lt.0) then
        fprec = FP_FULL
      else
        fprec = FP_REDU
        call pfusio (fid, RE, 4, prmbuf, ierr)
      endif
c
c ... read length from file as a <LONG>
c
      call pfusio (fid, RE, 3, prmbuf, ierr)
      if ( ierr.ne.0 ) goto 999
c
      call pfui2l ( prmbuf, lenfa, ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... Advance file pointer to first word after the FP array
c
      if (fprec.eq.FP_REDU) then
        call pfusek(fid,floc+9+lenfa,ierr)
      else
        call pfusek(fid,floc+5+2*lenfa,ierr)
      endif
c
c ----------------------------------------------------------------------
c
  999 continue
c
c ... write status message if DEBUG is on and return
c
      if (ludbg.ne.OFFLUN)then
        write (ludbg,1000) PFMODU, 'Exit', ierr
      endif
c
      return
c
 1000 format(' %PFF-',a,'-DBG:  ',a,' Error Status = ',i5)
c
      end
