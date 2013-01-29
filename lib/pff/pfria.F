      subroutine pfria (fid, mlenia, lenia, iarr, ierr)
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
c ----------------------------------------------------------------------
c
c     Summary:
c
c       - This routine is a READ routine that reads a variable length,
c         signed integer array (data primitive <IARRAY>) from a PFF file.
c       - All integers in the array will be in the range -32768 to 32767.
c       - Maximum array length is (2**45)-1, approximately 35.2 x 10**12
c       - Note that the length of the array is integrally associated
c         with this data primitive.
c       - This operation is ONLY ALLOWED in READ mode !!!
c       - Format for data primitive <IARRAY>:
c           IF (integer_precision = 4-byte)
c             <INT>          INTP_4
c             <LONG>         LENGTH
c             LOOP 1,LENGTH   \
c               <INT>,<INT>   -  4-byte integer array
c             ENDLOOP         /
c           ELSE
c             <LONG>         LENGTH
c             LOOP 1,LENGTH   \
c               <INT>         -  2-byte integer array
c             ENDLOOP         /
c           ENDIF
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file
c       mlenia  -  dimensioned length of destination integer array
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       lenia   -  length of integer array to be read from PFF file
c       iarr    -  integer array to be read from PFF file
c       ierr    -  error status flag;
c                      0,  No error
c                      1,  Array dimension exceeded -- array truncated
c
c ----------------------------------------------------------------------
c
c     Internals:
c       ilast   -  index of last word in buffer
c       iloc    -  file pointer to end of array
c       ioff    -  offset between buffer location and full string location
c       ip_mode -  integer precision mode of <IARRAY> being read
c       iw      -  loop index over words in array
c       mread   -  # of array elements that are actually read from the
c                  file
c       pfutel  -  PFF function for finding current file position
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfstfg  -  RE, INTP_2
c
c ... Included common decks:
c ... pfbufp
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
      integer   fid, ierr, lenia, mlenia
      integer   iarr(mlenia)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFRIA' )
c
c ... External Block Data
      external  pffbd
c
c     Internal
      integer   ilast, ioff, ip_mode, iw, mread
      HU_FPOS_TYPE iloc
      HU_FPOS_TYPE pfutel
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
c
c ... Read in precision flag (if present)
      call pfusio (fid, RE, 1, prmbuf, ierr)
c
      if ( prmbuf(1).lt.0 ) then
c
        ip_mode = prmbuf(1)
c ..... read length from file as a <LONG>
        call pfusio (fid, RE, 3, prmbuf, ierr)
c
      else
c
        ip_mode = INTP_2
c ..... read remainder of length from file as a <LONG>
        call pfusio (fid, RE, 2, prmbuf(2), ierr)

      endif
c
      call pfui2l ( prmbuf, lenia, ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... Don't read more than the the destination array can hold
c
      mread = min( lenia, mlenia )
c
c ... now read array from file
c
      if ( ip_mode.eq.INTP_2 ) then
        call pfusio (fid, RE, mread, iarr, ierr)
        if ( ierr.ne.0 ) goto 999
      else
        do iw=1,mread,LPRBUF/2
          ioff = iw - 1
          ilast = min(mread,ioff+LPRBUF/2)
          call pfusio (fid, RE, 2*(ilast-ioff), prmbuf, ierr)
          call pfmi4 (RE, ilast-ioff, prmbuf, iarr(iw), ierr)
          if ( ierr.ne.0 ) goto 999
        end do
      endif
c
c ... process "insufficient storage" error; skip forward to end of array
c
      if ( mread.lt.lenia ) then
c
        iloc = pfutel( fid, ierr ) + lenia - mread
        if ( ierr.ne.0 ) goto 999
        call pfusek( fid, iloc, ierr )
        if ( ierr.ne.0 ) goto 999
c
        ierr = 1
        call pfserr ( PFMODU, ierr, fid,
     &                'Array dimension exceeded -- array truncated' )
c
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
