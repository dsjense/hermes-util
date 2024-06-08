      subroutine pfwifl ( fid, tapp, st, sc, ni, nfl, nfa,
     &                    iarray, flist, farray,           ierr )
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
c       - This routine is a WRITE routine that writes a IFL
c         (Integer/Float List) dataset to a PFF file.
c       - Floating data is divided into two groups:
c           1)  Float List -- each value is encoded independently as a
c               <FLOAT> at the full precision of this data type.
c           2)  Float Array -- the entire array is encoded as an
c               <FARRAY>.  This uses less space but has dynamic range
c               limitations for data with multi-order-of-magnitude
c               variations.
c       - An integer flag is used to indicate if the float array is
c         empty.  (flag = 0 means empty)
c       - This operation is ONLY ALLOWED in WRITE mode !!!
c       - Dataset Format:
c           <HEADER>       PFTIFL
c           <INT>          FLTFLG            (Float flag)
c           <LONG>         NFL               (length of Float List)
c           <IARRAY>       IARRAY(1:NI)      (integer array)
c           LOOP i=1,NFL
c             <FLOAT>        FLIST(i)        (ith value in Float List)
c           ENDLOOP
c           IF (FLTFLG.NE.0)
c             <FARRAY>       FARRAY(1:NFA)    (floating array)
c           ENDIF
c
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file
c       farray  -  (*) floating point array
c       flist   -  (*) floating point list
c       iarray  -  (*) integer array
c       nfa     -  length of floating array (farray)
c       nfl     -  length of floating list (flist)
c       ni      -  length of integer array (iarray)
c       sc      -  character string containing dataset comment
c       st      -  character string containing dataset type label
c       tapp    -  # associated with application dataset type
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                      0,          No error
c                      otherwise,  Error returned by called PFF routine
c
c ----------------------------------------------------------------------
c
c     Internals:
c       i       -  loop index over words in float list
c       ival    -  (*) work space for integer-encoded floats and longs
c       lds     -  length of dataset written (in 16-bit words)
c       lstadr  -  file position pointer for beginning of dataset
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfetyp  -  PFTIFL
c     pfstfg  -  FP_FULL, FP_REDU, WR, DFAULT
c
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pffils
c       r:    fpprec(*)
c
#include "pfbsiz.inc"
#include "pfetyp.inc"
#include "pfstfg.inc"
#include "pferrs.inc"
#include "pffils.inc"
c
#include "mdfpos.h"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ierr, nfa, nfl, ni, tapp
      integer   iarray(*)
      real      farray(*), flist(*)
      character st*(*), sc*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFWIFL' )
c
c     Internal
      integer   i, tlen
      HU_FPOS_TYPE lds, lstadr
      integer   ival(3)
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
c ... Write header
c
      call pfwhed ( fid, PFTIFL, tapp, DFAULT, DFAULT, DFAULT, st, sc,
     &              lstadr, ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... Write out Float Flag ( zero if nfa = 0 )
c
      if (nfa.eq.0) then
        call pfusio (fid, WR, 1, (/0/),      ierr )
      else
        call pfusio (fid, WR, 1, (/DFAULT/), ierr )
      endif
c
c ... Write out Float List length as <LONG>
c
      call pful2i ( nfl, ival, ierr )
      call pfusio ( fid, WR, 3, ival, ierr )
c
      if ( ierr.ne.0 ) goto 999
c
c ... Write out integer array
c
      call pfwia ( fid, ni, iarray, ierr )
c
c ... Write out float list, each value as a separate <FLOAT>
c
      do 1 i=1,nfl
        call pfuf2i ( flist(i), 0, ival, ierr )
        call pfusio ( fid, WR, 3, ival, ierr )
    1 continue
c
      if ( ierr.ne.0 ) goto 999
c
c ... if it is there, Write out float array
c
      if (nfa.ne.0) then
c
        if ( fpprec(fid).ne.FP_ALLFULL ) then
          call pfwfa ( fid, FP_REDU, nfa, farray, 0, ierr)
        else
          call pfwfa ( fid, FP_FULL, nfa, farray, 0, ierr)
        endif
c
      endif
c
c ----------------------------------------------------------------------
c
c ... now call routine to write dataset length into the header
c
      call pfwlds ( fid, lstadr, lds, ierr )
c
c ... put directory info into memory-resident directory structure
c
      tlen = lds
      call pfdput ( fid, PFTIFL, tapp, tlen, lstadr, st, sc, ierr )
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
