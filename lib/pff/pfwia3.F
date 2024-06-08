      subroutine pfwia3 (fid, low, imax, jmax, kmax, idlist, jdlist,
     &                   kdlist, mskbits, shbits, iarr, ierr)
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
c       - The array is assumed to be 3D, and is written via the
c         prescription of a do-list in each coordinate direction.
c       - If mskbits is zero, the supplied input array is simply 
c         truncated to 16-bits and all integers in the array must be in 
c         the range -32768 to 32767.
c       - If mskbits is nonzero, the value written to the file is shifted 
c         and masked from the supplied input iarray, i.e.,
c               ival = mod( iarray/(2**shbits),2**mskbits)
c         Note that in this mode, the supplied values in iarray must be
c         non-negative.
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
c       fid     -  File ID of PFF file
c       idlist  -  (*) array do-list in "x" dimension, i.e.,
c                  do i=idlist(1),idlist(2),idlist(3), of "iarray"
c       imax    -  upper "x" dimension of "iarray"
c       jdlist  -  (*) array do-list in "y" dimension, i.e.,
c                  do j=jdlist(1),jdlist(2),jdlist(3), of "iarray"
c       jmax    -  upper "y" dimension of "iarray"
c       kdlist  -  (*) array do-list in "z" dimension, i.e.,
c                  do k=kdlist(1),kdlist(2),kdlist(3), of "iarray"
c       kmax    -  upper "z" dimension of "iarray"
c       low     -  lower dimension for all "i,j,k" directions of "iarray"
c       iarr    -  (*,*,*) integer array to be written to PFF file
c       ierr    -  if not zero, return with no operation
c       mskbits -  # of bits from input word to be written to dataset
c       shbits  -  # of bits to be shifted in input word before applying 
c                  bit mask (mskbits)
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                      0,  No error
c                      1,  Illegal DO-List
c
c ----------------------------------------------------------------------
c
c     Internals:
c       dshft      2^|shbits|
c       i       -  loop index "x" direction of "iarr"
c       i1st    -  starting index for inner do loop
c       ib      -  buffer counter
c       ib_incr -  buffer increment for each integer word processed
c       ilast   -  final index for inner do loop
c       ival    -  integer value after mask applied
c       j       -  loop index "y" direction of "iarr"
c       k       -  loop index "z" direction of "iarr"
c       lenia   -  # of words written to file from "iarr"
c       mskmod  -  2^(mskbits)
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfstfg  -  WR, DFAULT, INTP_4
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
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ierr, low, imax, jmax, kmax, mskbits, shbits
      integer   idlist(3), jdlist(3), kdlist(3)
      integer   iarr(low:imax,low:jmax,low:kmax)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFWIA3' )
      integer MAXSI2, MINSI2
      parameter ( MAXSI2 = 32767 , MINSI2 = -32768 )
c
c     Internal
      logical use_i4
      integer   i, j, k, ib, ib_incr, i1st, ilast, lenia, dshft, mskmod
      integer   ival
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
c
c ... check for legal do-list (don't allow zero-trip)
c
      if( (idlist(2) - idlist(1))*idlist(3).lt.0 .OR.
     &    (jdlist(2) - jdlist(1))*jdlist(3).lt.0 .OR.
     &    (kdlist(2) - kdlist(1))*kdlist(3).lt.0 .OR.
     &    idlist(3).eq.0  .OR. jdlist(3).eq.0  .OR.
     &    kdlist(3).eq.0 ) then
        ierr = 1
        call pfserr ( PFMODU, 0, 0, 'Illegal DO-List' )
        goto 999
      endif
c
c ... compute array length
c
      lenia = ( (idlist(2) - idlist(1))/idlist(3) + 1) *
     &        ( (jdlist(2) - jdlist(1))/jdlist(3) + 1) *
     &        ( (kdlist(2) - kdlist(1))/kdlist(3) + 1)
c
      use_i4 = .FALSE.
      do k=kdlist(1),kdlist(2),kdlist(3)
        do j=jdlist(1),jdlist(2),jdlist(3)
          do i=idlist(1),idlist(2),idlist(3)
            if( iarr(i,j,k).gt.MAXSI2 .OR.iarr(i,j,k).lt.MINSI2 )then
              use_i4 = .TRUE.
              goto 1
            endif
          end do
        end do
      end do
 1    continue
c
      if ( use_i4 ) then
c ..... write out "full-precision" flag
        call pfusio (fid, WR, 1, (/INTP_4/), ierr)
        ib_incr = 2
      else
        ib_incr = 1
      endif
c
c ... write length to file as a <LONG>
c
      call pful2i ( lenia, prmbuf, ierr )
      call pfusio (fid, WR, 3, prmbuf, ierr)
      if ( ierr.ne.0 ) goto 999
c
c ... now write array to file (1 buffer at a time)
c
      ib = 0
c
c ... if mask and shift supplied, compute working temporaries
c
      if (mskbits.gt.0) then
        mskmod = 2**(mskbits)
        dshft  = 2**(abs(shbits))
      endif
c
      do k=kdlist(1),kdlist(2),kdlist(3)
c
        do j=jdlist(1),jdlist(2),jdlist(3)
c
          do i=idlist(1),idlist(2),idlist(3)
c
            if (mskbits.gt.0) then
              ival = mod( iarr(i,j,k)/dshft, mskmod )
            else
              ival = iarr(i,j,k)
            endif

            if ( use_i4 ) then
c
              call pfmi4 (WR, 1, prmbuf(ib+1), ival, ierr)
c
            else
c
              prmbuf(ib+1) = ival
c
            endif
c
            ib = ib + ib_incr
            if (ib+ib_incr.gt.LPRBUF) then
              call pfusio (fid, WR, ib, prmbuf, ierr)
              if ( ierr.ne.0 ) goto 999
              ib = 0
            endif
c
          end do
c
        end do
c
      end do
c
      if (ib.gt.0) then
        call pfusio (fid, WR, ib, prmbuf, ierr)
        if ( ierr.ne.0 ) goto 999
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
