      subroutine pfwlds ( fid, adr1st, lds, ierr )
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
c       - This routine is a WRITE routine that writes a dataset's length
c         BACK into its header after the entire dataset has been
c         written.
c       - This operation is ONLY ALLOWED in WRITE READ/WRITE mode !!!
c       - This routine should be call immediately AFTER all user data
c         has been written to the dataset.  In addition, it must be
c         supplied as input the file address of the dataset's header
c         framing word.  NOTE that this address is returned in the
c         variable "lstadr" from the call to PFWHED.
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file
c       adr1st  -  file address of the dataset's header framing word (in
c                  units of 16-bit words)
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       lds     -  dataset length (in 16-bit words)
c
c ----------------------------------------------------------------------
c
c     Internals:
c       adrend  -  file position pointer on subroutine exit
c       buf     -  buffer space for encoding <LONG>
c       lenloc  -  pointer to dataset-length field of header
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
      integer   fid, ierr, adr1st, lds
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFWLDS' )
c
c     Internal
      integer   lenloc, adrend
      integer   buf(3)
      integer   pfutel
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
c ... Find current file position
c
      adrend = pfutel ( fid, ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... compute address of dataset-length field and dataset length
c
      lenloc = adr1st + 1
      lds    = adrend - adr1st
c
c ... Encode dataset length to <LONG>
c
      call pful2i ( lds, buf, ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... Write length back to header location
c
      call pfuwbk (fid, lenloc, 3, buf, ierr)
c
c ----------------------------------------------------------------------
c
  999 continue
c
c     write status message if DEBUG is on and return
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
