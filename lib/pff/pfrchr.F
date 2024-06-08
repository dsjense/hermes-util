      subroutine pfrchr (fid, str ,slen , nblen, ierr)
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
c       - This routine is a READ routine that reads a variable length
c         character string from a PFF file.
c       - This operation is ONLY ALLOWED in READ mode !!!
c       - This routine also returns the actual non-blank length of the
c         string read from the file and the non-blank length that is
c         returned in the destination string "str".  These two values
c         will be the same UNLESS the space allocated for "str" is not
c         sufficient to hold the entire string read from the file.
c       - if the string from the file is larger than the destination
c         string, the destination string will be truncated.
c       - if the string from the file is smaller than the destination
c         string, the destination string will left-justified and
c         blank-padded.
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
c       nblen   -  non-blank length of string from file
c       slen    -  non-blank length of string that fits in the allocated
c                  length of destination string "str"
c       str     -  character string to be read from PFF file
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   Illegal string length
c
c ----------------------------------------------------------------------
c
c     Internals:
c       ib      -  loop index over buffers
c       ic      -  pointer to character in string
c       ich     -  ascii value of character pointed to by "ic"
c       ichm    -  ascii value of character pointed to by "ic-1"
c       ilast   -  index of last word in buffer
c       ioff    -  offset between buffer location and full string location
c       iw      -  loop index over words in buffer
c       nw      -  TOTAL # of <INT> words to be read from file
c       stlen   -  destination string length
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
      integer   fid, ierr, nblen, slen
      character str*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFRCHR' )
      integer   TWO15
      parameter ( TWO15 = 32768 )
c
c ... External Block Data
      external  pffbd
c
c     Internal
      integer   ib, ic, ich, ichm, ilast, ioff, iw, nw, nw_arr(1), stlen
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
c ... find length of destination string
c
      stlen  = len(str)
c
c ... read length from file as a <INT>
c
      nw_arr = nw
      call pfusio (fid, RE, 1, nw_arr, ierr)
      nw = nw_arr(1)
      if ( ierr.ne.0 ) goto 999
c
c ... is string length in legal range ( 0 .le. NW .lt. 2**15 ) ?
c
      if ( nw.lt.0 .OR. nw.ge.TWO15 ) then
        ierr = 1
        call pfserr ( PFMODU, ierr, 0, 'Illegal string length' )
        goto 999
      endif
c
c ... initialize destination string
c
      str = ' '
      ich = 0
c
c ... read from file buffer by buffer and
c ...   unpack 2 characters from each <INT>
c
      do 12 ib=1,nw,LPRBUF
c
        ioff = ib - 1
        ilast = min(nw,ib+LPRBUF-1)
c
        call pfusio (fid, RE, ilast-ioff, prmbuf, ierr)
        if ( ierr.ne.0 ) goto 999
c
        do 11 iw=ib,ilast
          ic = 2*iw
          ichm = prmbuf(iw-ioff)/256
          ich  = prmbuf(iw-ioff) - 256*ichm
          if (ic.le.stlen) then
            str(ic  :ic  ) = char(ich)
            str(ic-1:ic-1) = char(ichm)
          else if (ic-1.le.stlen ) then
            str(ic-1:ic-1) = char(ichm)
          endif
   11   continue
c
   12 continue
c
c ... find # of characters in string
c
      nblen = nw*2
      if (ich.eq.32) nblen = nblen - 1
      slen = min(stlen,nblen)
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
