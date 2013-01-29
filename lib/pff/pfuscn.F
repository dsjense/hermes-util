      subroutine pfuscn ( fid, ib, ie, isk, subb, sube, s, slen, mmap,
     &                    match, exact, map, nmap, ierr )
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
c       - This routine is a UTILITY routine that scans dataset comment
c         strings (or substrings thereof) of an open PFF file for a
c         given string.  A map array is returned that contains pointers
c         to datasets for which a match is found or not found, depending
c         upon the value of the variable "match".
c       - A logical switch is provided to allow the string matches to be
c         either case-sensitive or case-insensitive.
c       - The file's datasets are selectively scanned via a supplied
c         do-list (ib,ie,isk):
c             if ib < 1,             then use 1 for # of first dataset
c             if ie < 1 OR ie > MAX, then use MAX for # of last dataset,
c                                    where MAX is the # of datasets in
c                                    the input file ("fid")
c             if isk < 1,            then use a skip factor of 1
c
c ----------------------------------------------------------------------
c
c     Input:
c       exact   -  if .TRUE., search is case-sensitive; otherwise search
c                  is case-insensitive
c       fid     -  File ID of PFF file
c       ib      -  entry # of first dataset to be copied
c       ie      -  entry # of last dataset to be copied  (if ie = 0, use
c                  entry # of last dataset on file)
c       isk     -  skip factor for dataset copying
c       exact   -  if .TRUE., search is case-sensitive; otherwise search
c                  is case-insensitive
c       match   -  if .TRUE., search for matches; otherwise search for
c                  failure to match
c       mmap    -  maximum dimension of MAP array
c       s       -  search string
c                  * is matched by 0 to n characters
c                  ? is matched by exactly 1 character
c                  ^ as a first character anchors the match to the beginning
c                    of the comment substring
c                  $ as a final character anchors the match to the end
c                    of the comment substring
c                  \ escapes * and ? to be used literally anywhere in the 
c                    search string and escapes ^ (and $) at the beginning 
c                    only (and end only) of the search string to force ^ 
c                    (or $) to be interpreted literally
c    WARNING:  S will be modified if it contains *? be in the order ?*
c       Note:  No matches will be found if the number of characters in the 
c              search string is greater than the number of characters in
c              the dataset comment.
c       slen    -  length of search string
c       subb    -  initial index for title substring range (if 0, use 1)
c       sube    -  final index for title substring range (if 0, use
c                  MCHTTL)
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       map     -  (*)  map array returning pointers to selected
c                  datasets
c       nmap    -  # of datasets selected.
c       ierr    -  error status flag;
c                        0,  No error
c                        1,  Illegal File ID (FID)
c                        2,  File NOT Open
c                      < 0,  Returns -K if MAP array is full and at
c                            least one dataset remains to be selected,
c                            where K is the index of the 1st dataset
c                            selected that is NOT returned in MAP
c
c ----------------------------------------------------------------------
c
c     Internals:
c       dslen   -  dataset length
c       dsloc   -  input dataset file position pointer
c       find    -  logical indicating whether match was actually found
c       i       -  loop index over characters in string
c       ic      -  ascii integer associated with a character
c       ids     -  loop index over datasets on file
c       kb      -  entry # of first dataset to be scanned (modified)
c       ke      -  entry # of last dataset to be scanned (modified)
c       ksk     -  skip factor for dataset scanning (modified)
c       sb      -  initial index for title substring range (modified)
c       sc      -  character string for dataset comment
c       se      -  final index for title substring range (modified)
c       st      -  character string for dataset type label
c       strl    -  length of search string (modified)
c       tapp    -  # associated with application dataset type
c       tmpstr  -  temporary string for case-insensitive match
c       traw    -  # associated with raw dataset type
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  MXFILS, MCHTTL
c     pfstfg  -  NOPEN
c
c ... Included common decks:
c ... pfbufm
c       r:    dircnt(*), wrflag(*)
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pfbufm.inc"
#include "pferrs.inc"
c
#include "mdfpos.h"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ib, ie, isk, mmap, nmap, slen, subb, sube, ierr
      integer   map(1:mmap)
      logical   exact, match
      character s*(*)
c
c     Internal
      integer   dslen, i, ic, ids, kb, ke, ksk
      integer   sb, se, strl, tapp, traw
      HU_FPOS_TYPE dsloc
      character sc*(MCHTTL), st*1, tmpstr*(MCHTTL)
      logical   find
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFUSCN' )
c
c     parameters for the generalized string search.
c
      integer lenwork (MCHTTL+1), lenstr(MCHTTL)
      integer lchat, lcdollar, istr
c
c ... External Block Data
      external  pffbd
c
c ----------------------------------------------------------------------
c
c ... write status message if DEBUG is on
c
      if (ludbg .ne. OFFLUN)then
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
c ... is FID legal ?
c
      if( fid.lt.1 .OR. fid.gt.MXFILS )then
c
        ierr = 1
        call pfserr ( PFMODU, ierr, fid,
     &                             'Illegal File ID (FID)' )
        goto 999
c
      endif
c
c ... is file open ?
c
      if(wrflag(fid).eq.NOPEN)then
c
        ierr = 2
        call pfserr ( PFMODU, ierr, fid, 'Input File NOT Open' )
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
        ke = dircnt(fid)
      else
        ke = min( ie, dircnt(fid) )
      endif
c
c ... if skip factor is not positive, set to one
c
      ksk = max( 1, isk )
c
c ... if 1st initial substring index is not positive, set to one
c
      sb = max(1,subb)
c
c ... if final substring index is not positive or greater than # of
c ... chars. in the title string, set to # of chars. in title string
c
      if(sube.le.0)then
        se = MCHTTL
      else
        se = min( MCHTTL, sube )
      endif
c
c ... if search string length is greater than # of characters in the
c ... search string, set to # of characters in search string
c
      strl = min( len(s), slen, MCHTTL )
c     strl = min(len(s), slen)
c
c ... initialize # of entries in map array
      nmap = 0 
      if (strl .eq. 0) go to 999
c
      call parse_sstring(s, strl, lchat, lcdollar, 
     &                   lenwork, lenstr, tmpstr, istr, mchttl)
c
c ... If we have no search string then return the entire set of data
      if (strl .lt. 1) then
        nmap = 0
        do ids=kb,ke,ksk
          if (nmap .eq. mmap) then
            ierr = -ids
            goto 999
          else
            nmap = nmap+1
            map(nmap) = ids
          endif
        enddo
        goto 999
      endif
c ... if case-insensitive matching is requested, convert search string
c ... to lower case
c
      if ( .NOT. exact ) then
c
        do 1 i=1,strl
          ic = ichar(tmpstr(i:i))
          if ( ic.ge.65 .AND. ic.le.90 ) then
            tmpstr(i:i) = char(ic + 32)
          endif
    1   continue
c
      endif
c
c ... loop over datasets to be scanned
c
      do 10 ids=kb,ke,ksk
c
c ..... get directory info for this dataset
        call pfdget ( fid, ids, traw, tapp, dslen, dsloc, st, sc, ierr )
c
c ..... if case-insensitive matching is requested, convert comment
c ..... string to lower case
c
        if ( .NOT. exact ) then
c
          do 2 i=sb,se
            ic = ichar(sc(i:i))
            if ( ic.ge.65 .AND. ic.le.90 ) then
              sc(i:i) = char(ic + 32)
            endif
    2     continue
c
        endif
c
      find = .true.
      call match_sstring(find, sc, sb, se, lchat, lcdollar, 
     &                   lenwork, lenstr, tmpstr, istr )
      

c        find = index( sc(sb:se), tmpstr(1:strl) )  .gt.  0
c
c ..... put dataset entry # in map only if match is found (or not found,
c ..... if "match" is FALSE)
c
        if ( find .EQV. match ) then
c
c ....... is map array full ?
c
          if(nmap.eq.mmap)then
c
c ......... send flag indicating the 1st dataset that was not mapped
c
            ierr = -ids
            return
c
          else
c
c ......... increment map entry counter; insert dataset into map array
c
            nmap = nmap + 1
            map(nmap) = ids
c
          endif
c
        endif
c
   10 continue
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


      subroutine parse_sstring(s, strl, lchat, lcdollar, 
     &   lenwork, lenstr, tmpstr, istr, mchttl)
c
c     parse the search string into a series of search strings
c
c     s        the search string
c             Caution: 
c               S will be modified if it contains '*?' to '?*'
c     strl     search string length - will be modified to final length
c     lchat    logical indicator that the string, s, begins with a '^'
c     lcdollar logical indicator that the string, s, ends with a '$'
c     lenwork  integer work array of length mchttl+1
c     lenstr   integer work array of length mchttl
c     tmpstr  character work array of length mchttl
c     istr     number of sub-strings 
c     mchttl  maximum size of search string
c
c    Techinque
c ... Generalize search string
c     Technique
c     1. All '*?' are converted to '?*'
c     2. Move characters to a temporary array.
c     3. Use lenwork as a pointer array to the first character of each
c        substring.
c     4. Use lenstr as a sub-string length indicator. If negative, this
c        indicates the number of characters to be skipped(ie. number of '?')
c     5. If a * follows a ? then more characters can be skipped.
c        If a * then lenwork for the ? is negative; if no *, then positive.
c     6. Leading * can be ignored.
c     7. All strings are assumed to be padded with an infinite number of spaces
c
c     parameters for the generalized string search.
c
      character *(*) s, tmpstr
      integer lenwork (*), lenstr(*), istr, mchttl, strl
      integer lchat, lcdollar
c
      integer lcesc, iwork, ii, i
      character bslash*1
c
      bslash = char(92)
c
      istr = 1
      lchat = 0
      lcdollar = 0
      lcesc = 0
      lenwork(1) = 1
      do i = 1, MCHTTL
        lenstr(i) = 0
      enddo
      i = 1
c skip then hat if next character is * or if the first character is *
      if (strl .gt. 1) then
        if ((s(1:2) .eq. '^*' ) .or. (s(1:1) .eq. '*')) i = 2
        ii = i
        iwork = index(s(ii:strl), '*?')
c order the '*?' to '?*'
        do while (iwork .gt. 0)
          if (s(ii+iwork-2:ii+iwork-2) .ne. bslash)  then
            s(ii+iwork-1:ii+iwork)= '?*'
          else
            ii = ii +iwork
          endif
          iwork = index(s(ii:strl), '*?')
        enddo
      endif
      iwork = 1
c  now sort the input string.
      do while((i .le. strl) .and. (iwork .le. MCHTTL) .and.
     $  (istr .le. MCHTTL))
        if (s(i:i) .eq. '*') then
          if (lcesc .gt. 0) then
            tmpstr(iwork-1:iwork-1) = '*'
          else if (iwork-lenwork(istr) .gt. 0) then
            lenstr(istr)= iwork-lenwork(istr)
            istr = istr+1
            lenwork(istr) = iwork
          else if (lenstr(istr) .lt. 0) then
            if (istr .eq. 1) then 
              lchat = 0
            else
              lenwork(istr) = -iwork
              istr = istr+1
              lenwork(istr) = iwork
            endif
          endif
        else if (s(i:i) .eq. '?') then
          if (lcesc .gt. 0) then
            tmpstr(iwork-1:iwork-1) = '?'
          else
            if (iwork-lenwork(istr) .gt. 0) then
              lenstr(istr)= iwork-lenwork(istr)
              istr = istr+1
              lenwork(istr) = iwork
              lenstr(istr)= -1
            else if (lenstr(istr) .lt. 0) then
              lenstr(istr) = lenstr(istr)-1
            else
              lenstr(istr) = -1
            endif
          endif
        else if (((i .eq. 1) .or. ((i .eq. 2) .and. (lcesc .gt. 0)))
     &    .and. (s(i:i) .eq. '^')) then
          if (lcesc .gt. 0) then
            tmpstr(iwork-1:iwork-1) = '^'
          else
            lchat = 1
          endif
        else if ((i .eq. strl) .and. (s(i:i) .eq. '$')) then
          if (lcesc .gt. 0) then
            tmpstr(iwork-1:iwork-1) = '$'
          else
c           trailing blanks must be removed.
            if (iwork .gt. 1) then 
              do while ((tmpstr(iwork-1:iwork-1) .eq. ' ') .and. 
     &           (iwork .gt. lenwork(istr)))
                iwork = iwork-1
              enddo
            endif
            if ((iwork .gt. lenwork(istr)).or.(lenstr(istr) .lt. 0)) 
     &        lcdollar = 1
          endif
        else
          if (lenstr(istr) .lt. 0) then
            istr = istr+1
            lenwork(istr) = iwork
          endif
          tmpstr(iwork:iwork)= s(i:i)
          iwork = iwork+1
        endif
        if (s(i:i) .eq. bslash) then
          lcesc = 1
        else
          lcesc = 0
        endif
        i = i+1
      enddo
      if (lenstr(istr) .lt. 0) then
        istr = istr+1
        lenwork(istr) = iwork
      else
        lenstr (istr) = iwork-lenwork(istr)
        if (lenstr(istr) .ne. 0) then
          istr = istr+1
          lenwork(istr) = iwork
        endif
      endif
      istr = istr-1
      strl = iwork-1
      if ((strl .le. 0) .and. (istr .gt. 0 )) then
        if (lenstr(istr) .lt. 0) strl = 1
      endif
      return
      end


      subroutine match_sstring(find, sc, sb, se, lchat, lcdollar, 
     &                         lenwork, lenstr, tmpstr, istr)
c
c     match a string to a parsed search string.
c 
c     find     logical variable. True if a match, false if no match
c     sc       the search string
c     sb       first character for the substring search
c     se       last character for the substring search
c     lchat    logical indicator that the string, s, begins with a '^'
c     lcdollar logical indicator that the string, s, ends with a '$'
c     lenwork  integer work array from call to parse_sstring 
c     lenstr   integer work array from call to parse_sstring 
c     tmpstr   character work string from call to parse_sstring
c     istr     number of sub-strings
c 
c  PASSED
      character *(*) sc, tmpstr
      logical find
      integer lenwork (*), lenstr(*), sb, se, istr

c  INTERNAL
      integer lchat, lcdollar, lcesc, iwork, ii, findit, lcagain
      integer i, isave, iwsave, strlen, lcexact
c
      iwork = sb
      isave = -1
      i = 1
      lcexact= 0
c ..... calculate the length minus trailing blanks
      strlen = se
      do while((sc(strlen:strlen) .eq. ' ') .and. (strlen .ge. iwork))
         strlen = strlen-1
      enddo
      find = .true.
c 
      lcagain = 0
      do  while  (find .and. (i .le. istr))
        if ((lchat .gt. 0) .and. (i .eq. 1)) then
c ......  Initial hat (^)
c         If we don't find the string, there is no match.
          if (lenstr(1) .lt. 0) then
            iwork = iwork-lenstr(i)
            if (iwork .gt. strlen+1) then
              find = .false.
            else
              if ((istr .eq. 1) .and. (lcdollar .gt. 0) .and. 
     &          (iwork .ne. strlen+1)) find = .false.
            endif
            lcexact = 1
          else
            ii = sb+lenstr(1)-1
            if (ii .gt. se) then
              find = .false.
            else
              findit=index(sc(iwork:ii),
     &          tmpstr(lenwork(1):abs(lenwork(2))-1)) 
              if (findit .gt. 0) then
                iwork = ii+1
                if ((istr .eq. 1) .and. (lcdollar .gt. 0)) then
c                 with both hat and dollar there cannot be a match
                  if (strlen .ne. ii) find = .false.
                endif
              else 
                find = .false.
              endif
            endif
          endif
        else if ((i .eq. istr) .and. (lcdollar .gt. 0) .and.
     &        (lenstr(i) .gt. 0)) then
c ......  Trailing dollar signal
          lcesc = strlen - lenstr(i)+1
          if (lcesc .lt. iwork) then
            find = .false.
          else if ((lcexact .gt. 0) .and. (lcesc .gt. iwork)) then
            lcagain =1
          else
            find=index(sc(lcesc:strlen),
     &          tmpstr(lenwork(i):abs(lenwork(i+1))-1)) .gt. 0
          endif
        else
          if (lenstr(i) .lt.  0) then
c .....     We have one or more ?
            iwork = iwork -lenstr(i)
c ......... Check on the non-blank length of the dataset commnet
            if (iwork .gt. strlen+1) find = .false.
            if (lenwork(i) .gt. 0) lcexact = 1
            if ((lcdollar .gt. 0) .and. (i .eq. istr) .and. 
     &         (iwork .lt. strlen+1)) lcagain = 1

          else 
c         normal search unless we had a previous ?
            if (lcexact .eq. 0) then 
              isave = i
              iwsave = iwork
              ii= se
            else 
              ii = iwork+lenstr(i)-1
            endif
            findit=index
     &         (sc(iwork:ii),
     &          tmpstr(lenwork(i):abs(lenwork(i+1))-1)) 
            if (findit .gt. 0) then
              iwork = findit-1+iwork+lenstr(i)
              find = .true.
            else 
              find = .false.
              lcagain = 1
            endif
            lcexact = 0
          endif
        endif
        if (lcagain .gt. 0)  then 
          if (isave .lt. 1) then
            find = .false.
          else 
            lcagain = 0
            i =  isave 
            iwork = iwsave+1
            if (iwork .gt. se) then
               find = .false.
            else 
               find = .true.
            endif
            lcexact = 0
          endif
        else 

          i = i+1
        endif
      enddo
      return
      end
