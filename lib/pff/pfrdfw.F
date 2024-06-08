      subroutine pfrdfw ( fid, lstadr, ierr )
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
c       - This routine is a READ routine that reads a dataset's Framing
c         word from a PFF file.
c       - This operation is ONLY ALLOWED in READ mode !!!
c       - If an error occurs, an attempt is made to reposition the file
c         pointer to its entry position (if it has been moved).
c       - If the dataset is positioned at the beginning of a null
c         region, an attempt is made to skip over the null region and
c         read a framing word.
c       - NULL Region Format:  (Lnul is length of NULL region in 16-bit
c                               words)
c           if (Lnul .lt. 4) :
c              (Lnul)x<INT>       Lnul*NULFLG
c
c           if (Lnul .ge. 4) :
c              <INT>              NULFLG      NULL Region Flag
c              <LONG>             Lnul        NULL Region length
c              (Lnul-4)x<INT>     JUNK
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
c       lstadr  -  file pointer to dataset framing word, if found; on
c                  error, file position on subroutine entry
c
c ----------------------------------------------------------------------
c
c     Internals:
c       buf     -  buffer space for decoding of <LONG> and string values
c       i       -  loop index over words in NULL Region
c       iertmp  -  temporary error flag
c       ifrm    -  framing word
c       nloc    -  provisional address of dataset framing word
c       nullen  -  length of NULL Region
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfstfg  -  RE, EOFFLG, DFRAME, NULFLG
c
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pferrs.inc"
c
#include "mdfpos.h"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ierr
      HU_FPOS_TYPE lstadr
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFRDFW' )
c
c ... External Block Data
      external  pffbd
c
c     Internal
      integer   i, iertmp, ifrm, ifrm_arr(1), nullen
      HU_FPOS_TYPE nloc
      integer   buf(3)
      HU_FPOS_TYPE pfutel
c
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
c ... Find entry (current) file position
c
      lstadr = -1
      lstadr = pfutel ( fid, ierr )
c
c ... Read framing word
c
      ifrm_arr = ifrm
      call pfusio (fid, RE, 1, ifrm_arr, ierr)
      ifrm = ifrm_arr(1)
      if ( ierr.ne.0 ) goto 20
c
c ... if this wasn't a framing word, need to look further
c
      if (ifrm.ne.DFRAME) then
c
c ..... Is it the EOF, a NULL Region, or a bonafide framing error ?
c
        if (ifrm.eq.EOFFLG) then
          ierr = -1
        else if (ifrm.eq.NULFLG) then
          call pfusio (fid, RE, 3, buf, ierr)
          if (buf(1).ge.0) then
c
            call pfui2l ( buf, nullen, ierr )
            if ( ierr.ne.0 ) goto 20
c
            nloc = lstadr + nullen
c
            call pfusek (fid, nloc, ierr )
            ifrm_arr = ifrm
            call pfusio (fid, RE, 1, ifrm_arr, ierr)
            ifrm = ifrm_arr(1)
c
            if (ifrm.eq.EOFFLG) then
              ierr = -1
            else if (ifrm.ne.DFRAME) then
              goto 15
            end if
c
          else
c
            do 1 i=1,3
              if (buf(i).eq.DFRAME) then
                nloc = lstadr + i
                goto 2
              else if (ifrm.eq.EOFFLG) then
                ierr = -1
                goto 20
              endif
    1       continue
            goto 15
    2       continue
            call pfusek (fid, nloc+1, ierr )
          end if
        else
          goto 15
        end if
c
        if ( ierr.ne.0 ) goto 20
c
        lstadr = nloc
c
      end if
c
      goto 999
c
   15 continue
c
        ierr = 1
        call pfserr ( PFMODU, ierr, fid, 'File Framing Error' )
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
