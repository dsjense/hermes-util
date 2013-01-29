      function mstrng (fid,den,mintit,str,ierr)
      integer mstrng
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
c     Returns a string (and its length) consisting of dataset title and
c     data type for file FID, dataset DEN.
c
c     Input:
c       den     -  pointer to directory element for which info is
c                  requested
c       fid     -  File ID of PFF file
c       mintit  -  minimum length of title truncation before leaving off
c                  the type string altogether
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       str     -  string returning title and type in the form:
c                  TITLE  (TYPE)
c                  trailing blanks are removed from both fields
c       ierr    -  error status flag;
c                     -1,   string was truncated to fit in supplied
c                           character variable (not really an error)
c                      0,   No error
c                      1,   Illegal File ID (FID)
c                      2,   File NOT Open
c                      3,   Illegal Dataset Entry # (DEN)
c
c     Return Value:
c         >  0,   length of returned string
c         <  0,   -(ierr) if ierr > 0
c
c ----------------------------------------------------------------------
c
c     Internal:
c       dspntr  -  current dataset pointer
c       count   -  # of datasets in file
c       lendir  -  length of dataset
c       locdir  -  file pointer to dataset
c       mode    -  indicates whether or not to return type-string as
c                  part of the returned string
c       rwmode  -  READ/WRITE status of PFF file
c       sc      -  comment (title) string of dataset
c       slen    -  length of supplied string
c       st      -  type string of dataset
c       tapp    -  application type of dataset
c       tilen   -  non-blank length of title string
c       tilenx  -  maximum non-blank length of title that will fit
c       traw    -  raw type of dataset
c       tylen   -  non-blank length of type string
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  MCHTTL, MCHTYP, MXFILS
c     pfstfg  -  NOPEN
c
c ... Included common decks:
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
c
#include "mdfpos.h"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, mintit, den, ierr
      character str*(*)
c
c     Internal
      integer   dspntr, count, lendir, mode, rwmode, slen
      integer   tapp, tilen, tilenx, traw, tylen
      HU_FPOS_TYPE locdir
      character str1, sc*(MCHTTL), st*(MCHTYP)
c
c     External:
      integer   lennb
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
c ... is FID legal ?
c
      if( fid.lt.1 .OR. fid.gt.MXFILS )then
        ierr = 1
        return
      endif
c
c ... Get needed file information
c
      call pfgfin( fid, rwmode, str1, count, dspntr, ierr)
c
c ... is file open ?
c
      if(rwmode.eq.NOPEN)then
        ierr = 2
        return
      endif
c
      slen = len(str)
c
c ... is dataset pointer legal ?
c
      if( den.lt.1 .OR. den.gt.count ) then
        ierr = 3
        return
      endif
c
c ... Get needed dataset information & find nonblank lengths
c
      call pfdget( fid, den, traw, tapp, lendir, locdir, st, sc, ierr )
      tilen = lennb(sc)
      tylen = lennb(st)
c
c ... compute total length of string
c
      mstrng = tilen + tylen + 4
c
c ... Will string fit in allotted space ?
c
      if(mstrng.gt.slen)then
        ierr = -1
c
c ..... find out how much the title needs to be truncated
c
        tilenx = slen - tylen - 4
c
c ..... if it's too much, switch to "no-type" mode (= 1)
c
        if(tilenx.ge.mintit)then
          mode = 0
          tilen = tilenx
          mstrng = slen
        else
          mode = 1
          tilen = min(slen,tilen)
          mstrng = tilen
        endif
      else
        mode = 0
      endif
c
c ... fill string, depending upon mode
c
      if(mode.eq.0)then
        str = sc(1:tilen) // '  (' // st(1:tylen) // ')'
      else
        str = sc(1:tilen)
      endif
c
      return
      end
