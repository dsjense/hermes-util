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
c     Include file containing common block variables for PFF's directory
c     management.
c
c ----------------------------------------------------------------------
c
c   Parameters needed in this module, but not supplied:
c
c     -  DBLKSZ, DBLKMX, DBLKMM, MXFILS, MCHTYP, MCHTTL
c
c   Include file needed to supply these parameters:
c
c     -  pfbsiz.inc
c
c ----------------------------------------------------------------------
c
c   Explanation of the common-block parameters:
c
c     PDAPP   -  application dataset type attribute
c     PDLEN   -  length of dataset attribute
c     PDLOC   -  file offset of dataset attribute
c     PDRAW   -  raw dataset type attribute
c     PDTTL   -  title string attribute
c     PDTYP  -  type string attribute
c
c   Explanation of the common-block variables:
c
#ifdef STATIC_MEM
c     nfdblk  -  # of free blocks in free directory block stack
c     tpdblk  -  top entry of free directory block stack
c     dblkpr  -  (*,*)  pointers from FID & directory entry to a
c                       directory block
c     dblksk  -  (*)  free directory block stack
#else
c     dblkpr  -  External function for dynamic operation
#endif
c     dirinfo -  (*,*,*) Array for storing saved information for all
c                        dataset directory entries
c
c ----------------------------------------------------------------------
c
c   Declare variables and define the common block:
c
      integer PDLEN, PDLOC, PDRAW, PDAPP, PDTYP, PDTTL, NDINFO
      parameter ( PDLEN=1, PDLOC=2, PDRAW=3, PDAPP=4, PDTYP=5,
     &            PDTTL=6, NDINFO=PDTTL )
c
      integer   dirinfo
c
#ifdef STATIC_MEM
      integer   tpdblk, nfdblk, dblksk, dblkpr
c
      common /pfbufd/ tpdblk, nfdblk,
     &                dblksk(1:DBLKMX),dblkpr(0:DBLKMM,1:MXFILS),
     &                dirinfo(1:NDINFO,1:DBLKSZ,1:DBLKMX)
#else
      integer dblkpr
      pointer (dirinfo_ptr, dirinfo(1:NDINFO,1:DBLKSZ,1:1))
c
      common /pfbufd/ dirinfo_ptr
#endif
c
c***********************************************************************
