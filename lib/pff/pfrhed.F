      subroutine pfrhed ( fid, lstadr, traw, tapp, vds, lds, nrfu,
     &                    st, nchrst, trunst, sc, nchrsc, trunsc,
     &                    ierr )
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
c       - This routine is a READ routine that reads a dataset's header
c         from a PFF file.
c       - This operation is ONLY ALLOWED in READ mode !!!
c       - If an error occurs, an attempt is made to reposition the file
c         pointer to its entry position (if it has been moved).
c       - Dataset Header Format:
c           <INT>          DFRAME       dataset framing word
c           <LONG>         Lds          dataset length (in 2-byte words)
c           <INT>          Traw         raw dataset type
c           <INT>          Vds          raw dataset type version #
c           <INT>          Tapp         application dataset type
c           10x<INT>       RFU          reserved for future use
c           <STRING>       TYPE         application dataset type label
c           <STRING>       TITLE        dataset title/comment
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
c       ierr    -  error status flag;
c                      0,   No error
c                     -1,   EOF reached  (Not really an error)
c                      1,   File Framing Error
c       lds     -  length of dataset in 16-bit words
c       lstadr  -  file pointer to dataset framing word, if found; on
c                  error, file position on subroutine entry
c       nchrsc  -  non-blank length of string "sc"
c       nchrst  -  non-blank length of string "st"
c       nrfu    -  # of non-default reserved integers
c       sc      -  character string to be loaded with dataset type label
c       st      -  character string to be loaded with dataset type label
c       tapp    -  # associated with application dataset type
c       traw    -  # associated with raw dataset type
c       trunsc  -  # of characters (if any) that were truncated when
c                  loading string "sc"
c       trunst  -  # of characters (if any) that were truncated when
c                  loading string "st"
c       vds     -  dataset version #
c
c ----------------------------------------------------------------------
c
c     Internals:
c       buf     -  buffer space for decoding of <LONG> and string values
c       i       -  loop index RFU array
c       iertmp  -  temporary error flag
c       nblen   -  non-blank length of string from file
c       nloc    -  address of dataset framing word
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfstfg  -  RE, DFAULT
c
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pfhrfu
c       r:    RFUMAX
c       w:    rfunum, rfuval(*)
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pferrs.inc"
#include "pfhrfu.inc"
c
#include "mdfpos.h"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ierr, lds, nchrsc, nchrst, nrfu
      integer   tapp, traw, trunsc, trunst, vds
      HU_FPOS_TYPE lstadr
      character st*(*), sc*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFRHED' )
c
c ... External Block Data
      external  pffbd
c
c     Internal
      integer   i, iertmp, nblen
      HU_FPOS_TYPE nloc
      integer   buf(3)
      HU_FPOS_TYPE pfutel
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
c ... Find entry (current) file position
c
      lstadr = -1
      lstadr = pfutel ( fid, ierr )
c
c ... Read dataset framing word
c
      call pfrdfw (fid, nloc, ierr)
c
c ... if this wasn't a framing word, no need to continue
c
      if ( ierr.ne.0 ) goto 20
c
c ... Read dataset length
c
c ... read length from file as a <LONG>
c
      call pfusio (fid, RE, 3, buf, ierr)
      if ( ierr.ne.0 ) goto 20
c
      call pfui2l ( buf, lds, ierr )
      if ( ierr.ne.0 ) goto 20
c
c ... read in the raw dataset type, version #, and app. dataset type
c
      call pfusio (fid, RE, 3, buf, ierr)
      if ( ierr.ne.0 ) goto 20
c
      traw = buf(1)
      vds  = buf(2)
      tapp = buf(3)
c
c ... read in the Reserved words (max of RFUMAX)
c
      call pfusio (fid, RE, RFUMAX, rfuval, ierr)
      if ( ierr.ne.0 ) goto 20
c
c ... Find out how many are being used
c
      nrfu = 0
      do 1 i=1,RFUMAX
        if (rfuval(i).eq.DFAULT) goto 2
        nrfu = i
    1 continue
c
    2 continue
c
      rfunum = nrfu
c
c ... Read dataset type label
c
      call pfrchr ( fid, st, nchrst, nblen, ierr )
      if ( ierr.ne.0 ) goto 20
      trunst = max ( 0, nblen - nchrst )
c
c ... Read dataset comment string
c
      call pfrchr ( fid, sc, nchrsc, nblen, ierr )
      if ( ierr.ne.0 ) goto 20
      trunsc = max ( 0, nblen - nchrsc )
c
c ... if no errors, set pointer to address of dataset framing word
c
      lstadr = nloc
c
      goto 999
c
   20 continue
c
      if ( lstadr.ge.0 ) then
        iertmp = 0
        call pfusek (fid, lstadr, iertmp )
      endif
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
