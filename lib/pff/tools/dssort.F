      subroutine dssort (ifid, ofid, nchfsuf, max_bytes,
     &                   ib, ie, isk, subb, sube, exact, conflg, logflg,
     &                   ttyin, ttyout, mwork, iwork, ierr )
c
c **********************************************************************
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
C_Groups bldpff  
c **********************************************************************
c
c     Sorts datasets from an input file based on titles (or substrings
c     thereof) and copies sorted datasets it an output file.  Sorting
c     range of the input file is limited to supplied do-list.
c     An map array containing pointers to datasets for which a match is
c     found is returned.  A file's datasets are selectively scanned via
c     a supplied do-list.
c
c
c     Input:
c       conflg  -  flag for query (or confirmation) mode
c       exact   -  if .TRUE., search is case-sensitive; otherwise search
c                  is case-insensitive
c       ifid    -  File ID of input PFF file
c       ib      -  initial index for input file's dataset do-list
c                  if ib = 0, use 1
c       ie      -  final index for input file's dataset do-list
c                  if ie = 0, use last dataset
c       isk     -  skip factor for input file's dataset do-list
c       logflg  -  flag for logging mode
c       mwork   -  length of work array provided by calling routine
c       max_bytes - Max. length of an output file
c       ofid    -  File ID of output PFF file
c       nchfsuf -  # characters to use for multi-file suffixes
c       subb    -  initial index for title substring range (if 0, use 1)
c       sube    -  final index for title substring range (if 0, use
c                  MCHTTL)
c       ttyin   -  tty input file
c       ttyout  -  tty output file
c       ierr    -  if not zero, return with no operation
c
c     Work Space Arrays:  (contents may be destroyed by this routine)
c       iwork   -  (*)  work space
c
c     Output:
c       ierr    -  error status flag;
c                      0,          No error
c                      1,          Insufficient Buffer Size
c                      otherwise,  Error returned by called PFF routine
c
c ----------------------------------------------------------------------
c
c     Internal:
c       dspntr  -  current dataset pointer
c       count   -  # of datasets in file
c       i       -  loop index over scan-selected datasets
c       ids     -  dataset # of selected dataset
c       kb      -  initial dataset # in do-list
c       ke      -  final dataset # in do-list
c       kf      -  dataset # for start of scan
c       ksk     -  dataset do-list skip increment
c       lendir  -  length of dataset
c       locdir  -  file pointer to dataset
c       logans  -  flag:  if true, copy dataset
c       mmap    -  available storage for scan-selected map array
c       moff    -  offset address of map array in work array
c       n       -  loop index over elements in flag array
c       nds     -  flag array value of selected dataset
c       nf      -  1st element of flag array that has not been sorted
c       nflag   -  # of elements used in flag array
c       nmap    -  # of datasets returned in scan map array
c       ns      -  starting address in flag array for each pass of sort
c       rwmode  -  READ/WRITE status of PFF file
c       sb      -  initial index for title substring range (modified)
c       sc      -  comment (title) string of dataset
c       se      -  final index for title substring range (modified)
c       slen    -  length of supplied string
c       str80   -  temporary string
c       strl    -  substring length
c       tapp    -  application type of dataset
c       traw    -  raw type of dataset
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  MCHTTL
c
c ... Included common decks:
c
#include "pfbsiz.inc"
c
#include "mdfpos.h"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   ib, ie, ierr, ifid, isk, mwork
      integer   ofid, subb, sube, ttyin, ttyout
      integer   nchfsuf
      HU_FPOS_TYPE max_bytes
      integer   iwork(0:mwork)
      logical   conflg, exact, logflg
c
c     Internal
      integer   count, dspntr, i, ids, kb, ke, kf, ksk
      integer   lendir, mmap, moff
      integer   n, nds, nf, nflag, nmap, ns, rwmode
      integer   sb, se, slen, strl, tapp, traw
      HU_FPOS_TYPE locdir
      logical   logans
      character str80*80
      character sc*(MCHTTL)
c
c     External:
      integer   mstrng
      logical   yesno
c
c ----------------------------------------------------------------------
c
c
c ... return if error flag is not zero on entry
c
      if(ierr.ne.0) return
c
c ----------------------------------------------------------------------
c
c ... Get needed file information
c
      call pfgfin( ifid, rwmode, str80(1:1), count, dspntr, ierr)
c
c ... Modify do-list values to be legal
c
      kb = max(1,ib)
      if(ie.le.0)then
        ke = count
      else
        ke = min(count,ie)
      endif
      ksk = max(1,isk)
c
c ... Modify substring limits to be legal; get substring length
c
      sb = max(1,subb)
      if(sube.le.0)then
        se = MCHTTL
      else
        se = min(MCHTTL,sube)
      endif
      strl = se - sb + 1
c
c ... find upper limit of flag array; find out how much is left for
c ... scan-map array
c
      nflag = (ke - kb)/ksk
      moff  = nflag + 1
      mmap  = mwork - moff
c
c ... if work array is not large enough, return with error
c
      if ( mmap.le.0 ) then
        ierr = 1
        return
      endif
c
c ... initialize the flag array to zero
c
      do 1 n=0,nflag
        iwork(n) = 0
    1 continue
c
      ns = 0
c
c ... loop over sorting passes
c
   10 continue
c
c ..... find first flag value that hasn't been copied; get dataset #
c
        do 11 n=ns,nflag
          if(iwork(n).eq.0)then
            nf = n
            kf = kb + n*ksk
            goto 12
          endif
   11   continue
c
c ..... All sorted -- we're done !!!
c
          ierr = 0
          return
c
   12   continue
c
c ..... get comment string for 1st uncopied dataset
c
        call pfdget( ifid, kf, traw, tapp, lendir, locdir,
     &               str80(1:1), sc, ierr )
c
c ..... loop over scan calls until we've exhausted all matches
c
   20   continue
c
          call pfuscn( ifid, kf, ke, ksk, sb, se, sc(sb:se), strl,
     &                 mmap, .TRUE., exact, iwork(moff), nmap, ierr )
          if (ierr.gt.0) return
c
          kf = -ierr
          ierr = 0
c
c ....... loop over matched datasets
c
          do 30 i=1,nmap
c
            ids = iwork(nflag+i)
            nds = (ids-kb)/ksk
c
c ......... copy ONLY if this dataset hasn't been copied already
c
            if(iwork(nds).eq.0)then
c
c ........... get prompt string if log or con options on
c
              if ( logflg .OR. conflg ) then
                slen = mstrng(ifid,ids,10,str80,ierr)
                if(ierr.gt.0)then
                  write(ttyout,*) 'MSTRNG:  Error = ',ierr
                  return
                endif
                ierr = 0
              endif
c
c ........... do we need confirmation for copy
c
              if(conflg)then
                write(ttyout,'(1x,a)') str80(1:slen)
                logans = yesno(ttyin,ttyout,'Copy ?')
              else
                logans = .TRUE.
              endif
c
c ........... if copy is okay, do it
c
              if(logans)then
                call chk_outfile(nchfsuf,max_bytes,ttyout,ofid,ierr)
                call pfucpy(ifid,ofid,ids,ids,1,ierr)
                if (logflg)then
                  write(ttyout,'(1x,a)') str80(1:slen)
                  write(ttyout,*) 'Copied !'
                endif
              endif
c
c ........... set flag indicating dataset has been copied
c
              iwork(nds) = 1
c
            endif
c
   30     continue
c
c ....... have we scanned all datasets for this sort pass ?
c
          if ( kf.gt.0 ) goto 20
c
        continue
c
c ..... go back and see if any datasets remain to be copied
c
        ns = nf + 1
c
      goto 10
c
      end
