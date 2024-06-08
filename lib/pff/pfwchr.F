      subroutine pfwchr (fid, str, ierr)
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
c       - This routine is a WRITE routine that writes a variable length
c         character string to a PFF file.
c       - This operation is ONLY ALLOWED in WRITE mode !!!
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file
c       str     -  character string to be written to PFF file
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   Illegal string length
c
c ----------------------------------------------------------------------
c
c     Internals:
c       ib      -  loop index over buffers
c       ic      -  loop index over characters in string
c       ilast   -  index of last word in buffer
c       ioff    -  offset between buffer location and full string location
c       iw      -  loop index over words in buffer
c       nfw     -  # of FULL <INT> words to be written to file
c       nw      -  TOTAL # of <INT> words to be written to file
c       stlen   -  string length (including any trailing blanks)
c       tilen   -  string length (excluding any trailing blanks)
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfstfg  -  WR
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
      integer   fid, ierr
      character str*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFWCHR' )
      integer   TWO15
      parameter ( TWO15 = 32768 )
c
c     Internal
      integer   ib, ic, ilast, ioff, iw, nfw, nw, nw_arr(1), stlen,
     1          tilen
c
c ... External Block Data
      external  pffbd
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
c ----------------------------------------------------------------------
c
c ... find non-blank length of string
c
      stlen  = len(str)
c
      do 1 ic=stlen,1,-1
        if(str(ic:ic).ne.' ')then
          tilen = ic
          goto 2
        endif
    1 continue
c
      tilen = 0
c
    2 continue
c
c ... find # of corresponding full <INT>s and total <INT>s
c
      nfw = tilen/2
      nw  = (tilen+1)/2
c
c ... is string length in legal range ( NW .lt. 2**15 ) ?
c ... (Note that NW must be positive due to its method of computation)
c
      if( nw.ge.TWO15 ) then
        ierr = 1
        call pfserr ( PFMODU, ierr, 0, 'Illegal string length' )
        goto 999
      endif
c
c ... write length to file as a <INT>
c
      nw_arr = nw
      call pfusio (fid, WR, 1, nw_arr, ierr)
      nw = nw_arr(1)
      if ( ierr.ne.0 ) goto 999
c
c ... pack 2 characters to each <INT> and write to file buffer by buffer
c
      do 12 ib=1,nfw,LPRBUF
c
        ioff = ib - 1
        ilast = min(nfw,ib+LPRBUF-1)
c
        do 11 iw=ib,ilast
          ic = 2*iw
          prmbuf(iw - ioff) = ichar(str(ic-1:ic-1))*256
     &                                            + ichar(str(ic:ic))
   11   continue
c
        call pfusio (fid, WR, ilast-ioff, prmbuf, ierr)
        if ( ierr.ne.0 ) goto 999
c
   12 continue
c
c ... plus odd character, if any, with a blank character
c
      if(nw.gt.nfw)then
        prmbuf(1) = ichar(str(tilen:tilen))*256 + 32
        call pfusio (fid, WR, 1, prmbuf, ierr)
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
