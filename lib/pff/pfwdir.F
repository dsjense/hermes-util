      subroutine pfwdir ( fid, traw, tapp, st, sc, lendir, locdir,
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
c       - This routine is a WRITE routine that writes a directory (DIR)
c         dataset to a PFF file.
c       - This operation is ONLY ALLOWED in WRITE mode !!!
c       - Dataset Format:
c           <HEADER>       PFTDIR
c           <INT>          TRAW
c           <LONG>         LENDIR
c           <LONG>         LOCDIR
c
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file
c       lendir  -  length of dataset
c       locdir  -  pointer to dataset
c       sc      -  character string containing dataset comment
c       st      -  character string containing dataset type label
c       tapp    -  # associated with application dataset type
c       traw    -  # associated with raw dataset type
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                     0,           No error
c                     otherwise,   Error from called PFF utility routine
c
c ----------------------------------------------------------------------
c
c     Internals:
c       buf     -  work buffer for decoding <FLOAT>s, etc.
c       lds     -  length of dataset written (in 16-bit words)
c       lstadr  -  file pointer on subroutine entry
c                  string
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfetyp  -  PFTDIR
c     pfstfg  -  WR, DFAULT
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
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, ierr, traw, tapp, lendir, locdir
      character st*(*), sc*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFWDIR' )
      integer   BUFLEN
      parameter ( BUFLEN = 6 )
c
c     Internal
      integer   lds, lstadr
      integer   buf(0:BUFLEN)
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
c ... Write header
c
      call pfwhed ( fid, PFTDIR, tapp, DFAULT, DFAULT, DFAULT, st, sc,
     &              lstadr, ierr )
      if ( ierr.ne.0 ) goto 999
c
c ... Read TRAW, LENDIR, & LOCDIR
c
      if ( ierr.ne.0 ) goto 999
c
      buf(0) = traw
      call pful2i ( lendir, buf(1), ierr)
      call pful2i ( locdir, buf(4), ierr)
c
      call pfusio ( fid, WR, 7, buf, ierr)
c
c ----------------------------------------------------------------------
c
c ... write the dataset length (of directory dataset, NOT referenced
c ... dataset) back to the header
c
      call pfwlds ( fid, lstadr, lds, ierr )
c
c ... DON'T PUT DIRECTORY INFO INTO MEMORY-RESIDENT DIRECTORY STRUCTURE
c ... SINCE WE ARE PROCESSING A DIRECTORY DATASET
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
