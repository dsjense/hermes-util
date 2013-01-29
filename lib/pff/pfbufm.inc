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
c ----------------------------------------------------------------------
c
c   Summary:
c
c     Include file containing parameters and common block variables for
c     PFF's I/O buffer management.
c
c ----------------------------------------------------------------------
c
c   Parameters needed in this module, but not supplied:
c
c     -  LBUF, CHRFIL, MXFILS
c
c   Include file needed to supply these parameters:
c
c     -  pfbsiz.inc
c
c ----------------------------------------------------------------------
c
c   Explanation of the common-block variables:
c
c     currec  -  (*)  current record pointer for each open PFF file
c     dircnt  -  (*)  # of directory entries for each FID
c     dircur  -  (*)  current directory entry for each FID
c     eofre   -  (*)  logical flag indicating that EOF was encountered
c                     when attempting to load next buffer from file.
c     extflg  -  (*)  logical flag indicating that file has been
c                extended while open or that directory info must be
c                rewritten
c     fillun  -  (*)  logical unit # associated with each file
c     locbuf  -  (*)  current buffer pointer for each open PFF file
c     lstwrd  -  (*)  last word of data written to file (RW mode only)
c     maxrec  -  (*)  maximum record # read/written from/to file
c     mdflag  -  (*)  machine-dependent status flag for each PFF file
c     modrec  -  (*)  logical flag indicating that current buffer has
c                been modified (RW mode only)
c     ubuf    -  (*,*)  unpacked buffer space for each PFF file
c     wrflag  -  (*)  status flag for each PFF file
c
#include "mdfpos.h"
c ----------------------------------------------------------------------
c
c   Declare variables and define the common block:
c
      HU_FPOS_TYPE lstwrd
      integer   locbuf, currec, wrflag, fillun, ubuf, mdflag
      integer   dircur, dircnt, maxrec
      logical   eofre, extflg, modrec
c
      common /pfbufm8/ lstwrd(1:MXFILS)
      common /pfbufm/ eofre(1:MXFILS),
     &                extflg(1:MXFILS), modrec(1:MXFILS),
     &                locbuf(1:MXFILS), currec(1:MXFILS),
     &                wrflag(1:MXFILS), mdflag(1:MXFILS),
     &                fillun(1:MXFILS), maxrec(1:MXFILS),
     &                dircur(1:MXFILS), dircnt(1:MXFILS),
     &                ubuf(1:LBUF,1:MXFILS)
c
c***********************************************************************
