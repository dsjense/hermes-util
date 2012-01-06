      subroutine pfrfa3 (fid, keep, low, imax, jmax, kmax, idlist, 
     &                   jdlist, kdlist, farray, foff10, ierr)
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
c       - This routine is a READ routine that reads a variable length,
c         floating array (data primitives <FARRAY>) from a PFF file.
c       - The array is assumed to be 3D, and is read via the
c         prescription of a do-list in each coordinate direction.
c       - All floating point values will be converted from integers in
c         the range -32768 to 32767.  A floating point offset and
c         scaling factor are provided in the file for this conversion.
c       - If ANY of the floating point array values will overflow the
c         machine's floating point limit, the entire array is scaled by
c         a power-of-ten, which is returned as "foff10".  If ALL of the
c         floating point array values will underflow the machine's
c         floating point limit, AND if "keep" = .TRUE., then the entire
c         array is scaled by a power-of-ten, which is returned as
c         "foff10".
c       - Maximum array length is (2**45)-1, approximately 35.2 x 10**12
c       - Note that the length of the array is integrally associated
c         with this data primitive.
c       - This operation is ONLY ALLOWED in READ mode !!!
c       - Format for data primitive <FARRAY>:
c           IF (Float_precision = FULL)
c             <INT>          FP_FULL
c             <INT>          foff10
c             <LONG>         LENGTH
c             <F4ARRAY>      farray
c           ELSE
c             <FLOAT>        OFFSET
c             <FLOAT>        SCALE
c             <IARRAY> --->  <LONG>     LENGTH
c                            LOOP 1,LENGTH   \
c                              <INT>         -  converted integer array
c                            ENDLOOP         /
c           ENDIF
c
c ----------------------------------------------------------------------
c
c     Input:
c       keep    -  logical flag indicating whether or not to keep a
c                  non-zero value in the case of underflow
c       fid     -  File ID of PFF file
c       idlist  -  (*) array do-list in "x" dimension, i.e.,
c                  do i=idlist(1),idlist(2),idlist(3), of "farray"
c       imax    -  upper "x" dimension of "farray"
c       jdlist  -  (*) array do-list in "y" dimension, i.e.,
c                  do j=jdlist(1),jdlist(2),jdlist(3), of "farray"
c       jmax    -  upper "y" dimension of "farray"
c       kdlist  -  (*) array do-list in "z" dimension, i.e.,
c                  do k=kdlist(1),kdlist(2),kdlist(3), of "farray"
c       kmax    -  upper "z" dimension of "farray"
c       low     -  lower dimension for all "i,j,k" directions of "farray"
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       farray  -  (*,*,*) floating array to be read from PFF file
c       foff10  -  power-of-ten multiplier of farray
c       ierr    -  error status flag;
c                      0,  No error
c                      1,  Illegal DO-List
c                      2,  Array dimension exceeded -- array truncated
c                      3,  Insufficient data for do-list specification
c
c ----------------------------------------------------------------------
c
c
c     Internals:
c       biglog  -  log-base-10 of data upper bound
c       fp_mode -  floating point precision mode of <FARRAY> being read
c       i       -  loop index "x" direction of "farray"
c       ib      -  loop index over words in array
c       ib_incr -  buffer increment for each float word processed
c       iloc    -  file pointer to end of array
c       io1     -  power-of-ten multiplier of "offset"
c       io2     -  power-of-ten multiplier of "scale"
c       j       -  loop index "y" direction of "farray"
c       k       -  loop index "z" direction of "farray"
c       lenfa   -  # of floating point values in specified do-lists
c       lenia   -  # of floating point values in PFF <FARRAY> 
c       lgmax   -  maximum of lgoff and lgsca
c       lgoff   -  log-base-10 of data offset
c       lgrang  -  log-base-10 of scale factor*(2**15)
c       lgsca   -  log-base-10 of scale factor
c       mxlbuf  -  maximum length of buffer that will hold an integral 
c                  number of floats in their integer representation
c       nleft   -  # of array elements left to be read from the file
c       nread   -  # of array elements that are actually read from the
c                  file at a single time
c       off10   -  maximum power-of-ten offset for data in array
c       offset  -  offset for converting integers to floats
c       pfutel  -  PFF function for finding current file position
c       pf2max  -  Maximum power-of-2 exponent that is in range for this
c                  machine's default REAL data type
c       pf2min  -  Minimum power-of-2 exponent that is in range for this
c                  machine's default REAL data type
c       pfrmin  -  Minimum floating point number that can be inverted 
c                  without floating point overflow
c       roff10  -  real power-of-ten offset
c       scale   -  scale factor for converting integers to floats
c
c ----------------------------------------------------------------------
c
c     Parameters:
c       PFMODU  -  module name
c       LOG2    -  base-10 logarithm of 2
c       LOG215  -  base-10 logarithm of 2**15
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
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
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ierr, low, imax, jmax, kmax, foff10
      logical   keep
      integer   idlist(3), jdlist(3), kdlist(3)
      real      farray(low:imax,low:jmax,low:kmax)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFRFA3' )
      real      LOG2, LOG215
      parameter ( LOG2   = 0.30102999566398119521 )
      parameter ( LOG215 = 15.*LOG2 )
c
c ... External Block Data
      external  pffbd
c
c     Internal
      integer   fp_mode, i, j, k, iloc, io1, io2, ib, ib_incr
      integer   lenia, lenfa, mxlbuf, nleft, nread, off10
      integer   pf2max, pf2min
      real      offset, pfrmin, scale
      real      lgsca, lgoff, lgrang, lgmax, biglog, roff10
      integer   pfutel
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
c ... Read in precision flag (if present) and base-10 offset
      call pfusio (fid, RE, 2, prmbuf, ierr)
c
      if ( prmbuf(1).lt.0 ) then
c
        fp_mode = prmbuf(1)
        foff10  = prmbuf(2)
        ib_incr = 2
c
      else
c
        fp_mode = FP_REDU
        ib_incr = 1
c
c ..... Read in remainder of array offset and scale info
        call pfusio (fid, RE, 4, prmbuf(3), ierr)
        call pfui2f ( keep, prmbuf, offset, io1, ierr )
        call pfui2f ( keep, prmbuf(4), scale, io2, ierr )
c
c ----------------------------------------------------------------------
c       Find maximum power-of-ten offset and normalize to that offset
c ----------------------------------------------------------------------
c
c ..... Get log10 of offset, if zero, set to large negative value
c
        if (offset.ne.0) then
          lgoff = alog10(abs(offset)) + io1
        else
          lgoff = -1.0e8
        endif
c
c ..... Get log10 of (scale factor)*(2**15) (maximum range of data from
c ..... "offset")
c
        if (scale.ne.0) then
c
c ....... get FLOAT LIMITS
c
          call pf_flt_limits(pf2max, pf2min, pfrmin)
c
          lgsca  = alog10(abs(scale)) + io2
          lgrang = LOG215 + lgsca
c
c ....... find out if "offset" or "range" drives data upper bound; find
c ....... upper bound; select consistant power-of-ten offset.
c
          if (lgoff.gt.lgrang) then
            biglog = LOG2 + lgoff
            off10 = io1 + 1
          else
            biglog = LOG2 + lgrang
            off10 = io2 + 5
          endif
c
c ....... if possible overflow (or underflow w/ keep), set power-of-ten
c ....... offset to that calculated above; otherwise zero.
c
          if ( (biglog.ge.LOG2*pf2max) .OR.
     &         (keep .AND. biglog.le.LOG2*(pf2min+1))    ) then
            foff10 = off10
          else
            foff10 = 0
          endif
c
c ....... Adjust foff10 if scale is in an underflow state
c
          if (io2.lt.0) foff10 = min(io2,foff10)
c
c ....... adjust foff10 if "offset" or "scale" will overflow
c
          lgmax = max(lgoff, lgsca )
          if ( (lgmax - foff10) .ge. LOG2*pf2max ) then
            roff10 = lgmax - LOG2*pf2max
            if (roff10.ge.0) then
              foff10 = roff10 + 1
            else
              foff10 = roff10 + .0001
            endif
          endif
c
c ....... if necessary, adjust "offset" and "scale" to have the proper
c ....... power-of-ten offset
c
          if ( foff10.ne.io1 .AND. offset.ne.0 ) then
            offset = 10.**(lgoff - foff10)
          endif
          if ( foff10.ne.io2 .AND. scale.ne.0 ) then
            scale  = 10.**(lgsca - foff10)
          endif
c
c ...... if scale = 0.0, only need to worry about "offset"
c
        else
c
          foff10 = io1
c
        endif
c
      endif
c
c ... read length from file as a <LONG>
c
      call pfusio (fid, RE, 3, prmbuf, ierr)
      if ( ierr.ne.0 ) goto 999
c
      call pfui2l ( prmbuf, lenia, ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... compute # of values needed to fill do-list specification
c
      lenfa = ((idlist(2) - idlist(1))/idlist(3) + 1)*
     &        ((jdlist(2) - jdlist(1))/jdlist(3) + 1)*
     &        ((kdlist(2) - kdlist(1))/kdlist(3) + 1)
c
c ... Don't read more than the the destination array can hold
c
      nleft = ib_incr*min( lenia, lenfa )
      mxlbuf = (LPRBUF/ib_incr) * ib_incr
c
c ... read up to one buffer of data
c
      nread = min(nleft,mxlbuf)
      call pfusio (fid, RE, nread, prmbuf, ierr)
      if ( ierr.ne.0 ) goto 999
      ib = 1
c
      do k=kdlist(1),kdlist(2),kdlist(3)
        do j=jdlist(1),jdlist(2),jdlist(3)
          do i=idlist(1),idlist(2),idlist(3)
c
            if ( ib.gt.nread ) then
c
c ........... buffer used up, read up to one buffer more data if available
c
              nleft = nleft - nread
              nread = min(nleft,mxlbuf)
      if ( nread.le.0 ) goto 10
              call pfusio (fid, RE, nread, prmbuf, ierr)
              if ( ierr.ne.0 ) goto 999
              ib = 1
            endif
c
            if ( fp_mode.eq.FP_REDU ) then
c ........... compute floating array from integer array using offset and 
c ........... scale factor
              farray(i,j,k) = scale*prmbuf(ib) + offset
            else
              call pfmf4 (RE, 1, prmbuf(ib),farray(i,j,k), ierr)
            endif
            ib = ib + ib_incr
          end do
        end do
      end do
c
   10 continue

      if ( lenfa.lt.lenia ) then
c
c ..... process "insufficient storage" error; skip forward to end of array
c
        iloc = pfutel( fid, ierr ) + lenia - lenfa
        if ( ierr.ne.0 ) goto 999
        call pfusek( fid, iloc, ierr )
        if ( ierr.ne.0 ) goto 999
c
        ierr = 2
        call pfserr ( PFMODU, ierr, fid,
     &                'Array dimension exceeded -- array truncated' )
c
      else if ( lenfa.gt.lenia ) then
c
c ..... process "insufficient data" error
c
        ierr = 3
        call pfserr ( PFMODU, ierr, fid,
     &                'Insufficient data for do-list specification' )
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
