      subroutine pfwia (fid, lenia, iarr, ierr)
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
c       - This routine is a WRITE routine that writes a variable length,
c         signed integer array (data primitive <IARRAY>) to a PFF file.
c       - All integers in the array must be in the range -32768 to 32767.
c       - Maximum array length is (2**45)-1, approximately 35.2 x 10**12
c       - Note that the length of the array is integrally associated
c         with this data primitive.
c       - This operation is ONLY ALLOWED in WRITE mode !!!
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
c       fid       -  File ID of PFF file
c       lenia     -  length of integer array to be written to PFF file
c       iarr      -  integer array to be written to PFF file
c       ierr      -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                      0,          No error
c                      otherwise,  Error from called PFF utility routine
c
c ----------------------------------------------------------------------
c
c     Internals:
c       ib      -  loop index over buffers
c       ilast   -  index of last word in buffer
c       ioff    -  offset between buffer location and full string location
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfstfg  -  WR, INTP_4
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
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ierr, lenia
      integer   iarr(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFWIA' )
      integer MAXSI2, MINSI2
      parameter ( MAXSI2 = 32767 , MINSI2 = -32768 )
c
c     Internal
      logical use_i4
      integer ib, ilast, ioff
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
      use_i4 = .FALSE.
      do ib=1,lenia
        if ( iarr(ib).gt.MAXSI2 .OR.iarr(ib).lt.MINSI2 ) then
          use_i4 = .TRUE.
          goto 1
        endif
      end do
 1    continue
c
      if ( use_i4 ) then
c ..... write out "full-precision" flag
        call pfusio (fid, WR, 1, INTP_4, ierr)
      endif
c
c ... write length to file as a <LONG>
c
      call pful2i ( lenia, prmbuf, ierr )
      if ( ierr.ne.0 ) goto 999
c
      call pfusio (fid, WR, 3, prmbuf, ierr)
      if ( ierr.ne.0 ) goto 999
c
c ... now write array to file
c
      if ( use_i4 ) then
        do ib=1,lenia,LPRBUF/2
          ioff = ib - 1
          ilast = min(lenia,ioff+LPRBUF/2)
          call pfmi4 (WR, ilast-ioff, prmbuf, iarr(ib), ierr)
          call pfusio (fid, WR, 2*(ilast-ioff), prmbuf, ierr)
        end do
      else
        call pfusio (fid, WR, lenia, iarr, ierr)
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
