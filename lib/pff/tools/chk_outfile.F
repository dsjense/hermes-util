      subroutine chk_outfile (nchfsuf, max_bytes, stdout, ofid, ierr)
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
c     Check that the output file has not exceeded the maximum byte size.
c     If it has, close the file, and reopen a new one with a suffix
c     appended to the "base" name.
c
c     Input:
c       nchfsuf   -  # characters to use for the suffix
c       max_bytes -  Max. # of bytes in a PFF file
c       stdout    -  standard output LU
c
c     Input/Output:
c       ofid    -  File ID of output file
c       ierr    -  error status flag;
c                      0,          No error
c                      otherwise,  Error returned by called PFF routine
c
c ----------------------------------------------------------------------
c
c     Internal:
c       ifsuf   - Multi-file suffix
c
c ----------------------------------------------------------------------
c     Call common blocks and parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfstfg    -  RE
c
c ... Included common decks:
c
#include "pfstfg.inc"
c
#include "mdfpos.h"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   ierr, nchfsuf, ofid, stdout
      HU_FPOS_TYPE max_bytes
c
c     Internal
      integer   count, dspntr, ndir, rwmode, fen
      integer   ifsuf, ix, lenb, lenf, loc
      HU_FPOS_TYPE fsize
      character ofname*132, basename*132, extname*20
      character fmt*6, cfsuf*9
c
c     Function calls
      integer  lennb, pfuopn, pfgfen
      HU_FPOS_TYPE pfutel
c
c ----------------------------------------------------------------------
c
c
c ... return immediately if:-
c       1. Error flag is not zero on entry
c       2. Max. file size not exceeded
c
      if (ierr.ne.0) return
      fsize = 2*pfutel(ofid,ierr)
      if (fsize .LT. max_bytes) return
c
c ----------------------------------------------------------------------
c
c ... Get current output file name, and then close it
c
      call pfgfin( ofid, rwmode, ofname, count, dspntr, ierr)
      fen = pfgfen(ofid,ierr)
      call pfucls( ofid, ierr)
      if (ierr.ne.0) return
c
c ... Extract basename and file extension (probably always ".pff")
c
      lenf = lennb(ofname)
      ix   = lenf
      do while (ofname(ix:ix).NE.'.' .AND. ix.GT.0)
        ix = ix - 1
      enddo
c
      if (ix .GT. 0) then
        extname  = ofname(ix:lenf)
        basename = ofname(1:ix-1)
        lenb     = ix - 1
      else
        basename = ofname
        lenb     = lenf
      endif
c
c ... Extract current multi-file suffix, if any
c
      ifsuf = 1
      if (lenb .GE. nchfsuf+2) then
        loc = lenb - nchfsuf
        if (basename(loc:loc) .EQ. '_') then
          write(fmt,'("(i",i1,")")') nchfsuf
          read(basename(loc+1:lenb),fmt,err=10) ifsuf
          lenb = loc - 1
          go to 20
   10     continue
          ifsuf = 1
          go to 20
        endif
      endif
c
c ... Build new file name
c
   20 continue
      ifsuf = ifsuf + 1
      write(fmt,'("(i",i1,".",i1,")")') nchfsuf, nchfsuf
      write(cfsuf,fmt) ifsuf
c
      ofname = basename(1:lenb) // '_' // cfsuf
      if (ix .GT. 0) then
        ofname = ofname(1:lenb+1+nchfsuf) // extname 
      endif
c
      ofid = pfuopn(ofname,WR,ierr,ndir)
      call pfsfen(ofid,fen,ierr)

      if (ierr .EQ. 0) then
        lenf = lennb(ofname)
        write(stdout,'(a,": ",a)') 'Opening file', ofname(1:lenf)
      endif
c
      return
      end
