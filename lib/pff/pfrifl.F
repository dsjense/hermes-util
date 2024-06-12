      subroutine pfrifl ( fid, keep, idim, fldim, fadim, offdim,
     &                    ni, nfl, nfa, tapp, st, sc,
     &                    iarray, flist, floff, farray, foff10, ierr )
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
c       - This routine is a READ routine that reads a IFL (Integer/Float
c         List) dataset from a PFF file.
c       - Floating data is divided into two groups:
c           1)  Float List -- each value is encoded independently as a
c               <FLOAT> at the full precision of this data type.
c           2)  Float Array -- the entire array is encoded as an
c               <FARRAY>.  This uses less space but has dynamic range
c               limitations for data with multi-order-of-magnitude
c               variations.
c       - An integer flag is used to indicate if the float array is
c         empty.  (flag = 0 means empty)
c       - This operation is ONLY ALLOWED in READ mode !!!
c       - Dataset Format:
c           <HEADER>       PFTIFL
c           <INT>          FLTFLG            (Float flag)
c           <LONG>         NFL               (length of Float List)
c           <IARRAY>       IARRAY(1:NI)      (integer array)
c           LOOP i=1,NFL
c             <FLOAT>        FLIST(i)        (ith value in Float List)
c           ENDLOOP
c           IF (FLTFLG.NE.0)
c             <FARRAY>       FARRAY(1:NFA)   (floating array)
c           ENDIF
c
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file
c       fadim   -  dimensioned length of floating array "farray"
c       fldim   -  dimensioned length of floating list "flist"
c       idim    -  dimensioned length of integer array "iarray"
c       keep    -  logical flag indicating whether or not to keep a
c                  non-zero value for floating data in the case of
c                  underflow
c       offdim  -  dimensioned length of float list offsets "floff"
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       farray  -  (*) floating point array
c       flist   -  (*) floating point list
c       floff   -  (*) power-of-ten offset for floating list elements:
c                    for i=1,min(nfl,fldim)  {
c                      if (i .le. offdim) then {
c                         floff(i) is power-of-10 offset for flist(i)
c                      }
c                      else if (power-of-10 offset .ne. 0) then  {
c                         floff(offdim) = 1
c                      }
c                    }
c       foff10  -  power-of-ten offset for floating array "farray"
c       iarray  -  (*) integer array
c       nfa     -  length of floating array in dataset
c       nfl     -  length of floating list in dataset
c       ni      -  length of integer array in dataset
c       sc      -  character string to be loaded with dataset comment
c       st      -  character string to be loaded with dataset type label
c       tapp    -  # associated with application dataset type
c       ierr    -  error status flag;
c                     -1,   EOF marker encountered (Not really an error)
c                      0,   No error
c                      1,   Incorrect dataset type
c                      2,   Insufficient array dimension -- array
c                           truncated
c
c ----------------------------------------------------------------------
c
c     Internals:
c       i       -  loop index over words in float list
c       ioff    -  temporary power-of-ten offset value
c       ival    -  (*) work space for integer-encoded floats and longs
c       lds     -  length of dataset read (in 16-bit words)
c       lstadr  -  file pointer on subroutine entry
c       nchrsc  -  non-blank length of header comment string
c       nchrst  -  non-blank length of header datatype label string
c       nrfu    -  # of non-default header reserved integers
c       terr    -  temporary error flag
c       traw    -  # associated with raw datatype from input file
c       trunsc  -  # of characters truncated from header comment string
c       trunst  -  # of characters truncated from header datatype label
c                  string
c       vds     -  dataset version #
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfetyp  -  PFTIFL
c     pfstfg  -  RE
c
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pfetyp.inc"
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
      logical   keep
      integer   fadim, fldim, fid, foff10, idim, ierr
      integer   nfa, nfl, ni, offdim, tapp
      integer   iarray(1:idim), floff(1:offdim)
      real      farray(1:fadim), flist(1:fldim)
      character st*(*), sc*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFRIFL' )
c
c     Internal
      integer   terr
      integer   i, ioff, lds, nrfu, traw, vds
      integer   nchrst, nchrsc, trunst, trunsc
      HU_FPOS_TYPE lstadr
      integer   ival(3)
c
c ----------------------------------------------------------------------
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
c ... initialize temporary error flag
c
      terr = 0
c
c ... Read header
c
      call pfrhed ( fid, lstadr, traw, tapp, vds, lds, nrfu,
     &              st, nchrst, trunst, sc, nchrsc, trunsc,
     &              ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... return if this is not the right dataset type  (PFTIFL)
c
      if( traw.ne.PFTIFL ) then
        ierr = 1
        call pfserr ( PFMODU, ierr, fid,
     &                          'Incorrect dataset type' )
        goto 999
      endif
c
c ... Read in Float Flag
c
      call pfusio_scalar (fid, RE, 1, nfa, ierr)
      if ( ierr.ne.0 ) goto 999
c
c ... Read in Float List length as <LONG>
c
      call pfusio ( fid, RE, 3, ival, ierr )
      call pfui2l ( ival, nfl, ierr )
c
c ... read in integer array
c
      call pfria ( fid, idim, ni, iarray, ierr )
c
c ... if array truncation, set temp. error flag and clear IERR
c
      if (ierr.eq.1) then
        ierr = 0
        terr = 2
      else if (ierr.ne.0) then
        goto 999
      endif
c
c ... Read in float list, each value as a separate <FLOAT>
c
      do 1 i=1,min(nfl,fldim)
        call pfusio ( fid, RE, 3, ival, ierr )
        call pfui2f ( keep, ival, flist(i), ioff, ierr )
        if ( i.le.offdim ) then
          floff(i) = ioff
        else if (ioff.ne.0) then
          floff(offdim) = 1
        endif
    1 continue
      if ( ierr.ne.0 ) goto 999
c
c ... if array truncation, set temp. error flag
c
      if (nfl.gt.fldim) then
        terr = 2
      endif
c
c ... if it is there, read in float array
c
      if (nfa.ne.0) then
c
        call pfrfa( fid, fadim, keep, nfa, farray, foff10, farray, ierr)
c
c ..... if array truncation, set temp. error flag and clear IERR
c
        if (ierr.eq.1) then
          ierr = 0
          terr = 2
        endif
c
      endif
c
c ... return an error if any arrays were truncated
c
      if( terr.ne.0) then
        ierr = terr
        call pfserr ( PFMODU, ierr, fid,
     &              'Insufficient array dimensions -- array truncated' )
        goto 999
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
