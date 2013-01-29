      subroutine pfucpy ( ifid, ofid, ib, ie, isk, ierr )
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
c       - This routine is a UTILITY routine that copies datasets from an
c         input file ("ifid") to an output file ("ofid").
c       - This routine assumes that the output file is properly
c         positioned BEFORE this routine is called.
c       - Datasets to be copied are specied by the do-list (ib,ie,isk):
c             if ib < 1,             then use 1 for # of first dataset
c             if ie < 1 OR ie > MAX, then use MAX for # of last dataset,
c                                    where MAX is the # of datasets in
c                                    the input file ("ifid")
c             if isk < 1,            then use a skip factor of 1
c       - This operation is ONLY ALLOWED for:
c             1)  Input file must be in READ or READ/WRITE mode !!!
c             2)  Output file must be in WRITE or READ/WRITE mode !!!
c       - The address is supplied in units of 16-bit words.
c
c ----------------------------------------------------------------------
c
c     Input:
c       ib      -  entry # of first dataset to be copied
c       ie      -  entry # of last dataset to be copied  (if ie = 0, use
c                  entry # of last dataset on file)
c       ifid    -  File ID of input (source) PFF file
c       isk     -  skip factor for dataset copying
c       ofid    -  File ID of output (destination) PFF file
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   Illegal File ID (FID)
c                      2,   File NOT Open
c                      3,   File not in proper mode for copy operation
c
c ----------------------------------------------------------------------
c
c     Internals:
c       ids     -  loop index over datasets on input file
c       idslen  -  input dataset length
c       idsloc  -  input dataset file position pointer
c       ilast   -  last word copied in transfer buffer
c       ip      -  loop index over words in dataset
c       kb      -  entry # of first dataset to be copied (modified)
c       ke      -  entry # of last dataset to be copied (modified)
c       ksk     -  skip factor for dataset copying (modified)
c       odsloc  -  output dataset file position pointer
c       sc      -  character string for dataset comment
c       st      -  character string for dataset type label
c       tapp    -  # associated with application dataset type
c       traw    -  # associated with raw dataset type
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  MXFILS
c     pfstfg  -  RE, WR, NOPEN
c
c ... Included common decks:
c ... pfbufm
c       r:    dircnt(*), wrflag(*)
c       w:    dircur(*)
c ... pfbufp
c       r:    LPRBUF
c       r/w:  prmbuf(*)
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pfbufm.inc"
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
      integer   ib, ie, ifid, isk, ofid, ierr
c
c     Internal
      integer   ids, idslen, ilast, ip, kb, ke, ksk
      integer   tapp, traw
      HU_FPOS_TYPE idsloc, odsloc
      character st*(MCHTYP), sc*(MCHTTL)
      HU_FPOS_TYPE pfutel
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFUCPY' )
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
c ... are FIDs legal ?
c
      if( ifid.lt.1 .OR. ifid.gt.MXFILS )then
c
        ierr = 1
        call pfserr ( PFMODU, ierr, ifid,
     &                             'Illegal Input File ID (FID)' )
        goto 999
c
      else if( ofid.lt.1 .OR. ofid.gt.MXFILS )then
c
        ierr = 1
        call pfserr ( PFMODU, ierr, ofid,
     &                             'Illegal Output File ID (FID)' )
        goto 999
c
      endif
c
c ... are files open ?
c
      if(wrflag(ifid).eq.NOPEN)then
c
        ierr = 2
        call pfserr ( PFMODU, ierr, ifid, 'Input File NOT Open' )
        goto 999
c
      else if(wrflag(ofid).eq.NOPEN)then
c
        ierr = 2
        call pfserr ( PFMODU, ierr, ofid, 'Output File NOT Open' )
        goto 999
c
      endif
c
c ... Are files in the proper mode ?
c
      if ( wrflag(ifid).eq.WR ) then
c
        ierr = 3
        call pfserr ( PFMODU, ierr, ifid,
     &             'Input File not in READ mode for copy operation' )
        goto 999
c
      else if ( wrflag(ofid).eq.RE ) then
c
        ierr = 3
        call pfserr ( PFMODU, ierr, ofid,
     &             'Output File not in WRITE mode for copy operation' )
        goto 999
c
      endif
c
c ... if 1st dataset is not positive, set to one
c
      kb = max( 1, ib )
c
c ... if last dataset is not positive or greater than # of datasets in
c     the file, set to # of datasets in file
c
      if ( ie.le.0 ) then
        ke = dircnt(ifid)
      else
        ke = min( ie, dircnt(ifid) )
      endif
c
c ... if skip factor is not positive, set to one
c
      ksk = max( 1, isk )
c
c ... loop over datasets to be copied
c
      do 2 ids=kb,ke,ksk
c
c ..... get directory info for this dataset
c
        call pfdget ( ifid, ids, traw, tapp, idslen, idsloc, st, sc,
     &                ierr )
c
c ..... position input file; save current address for output file
c
        call pfusek ( ifid, idsloc, ierr )
        odsloc = pfutel ( ofid, ierr )
c
        if (ierr.ne.0) goto 999
c
        do 1 ip=1,idslen,LPRBUF
c
c ....... find maximum # of word to read and write for this buffer
c
          ilast = min( LPRBUF, idslen - ip + 1 )
c
c ....... read bufferful of words from input file; write to output file
c
          call pfusio ( ifid, RE, ilast, prmbuf, ierr )
          call pfusio ( ofid, WR, ilast, prmbuf, ierr )
          if (ierr.ne.0) goto 999
c
    1   continue
c
c ..... store directory info for output file; update input file pointer
c
        call pfdput ( ofid, traw, tapp, idslen, odsloc, st, sc, ierr )
c
        dircur(ifid) = ids + 1
c
    2 continue
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
