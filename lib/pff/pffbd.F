      block data pffbd
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
c     Block data subprogram to initialize PFF common blocks.
c
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c     pfetyp
c     pfstfg
c
c ... Included common decks:
c ... pfbtyp
#ifdef STATIC_MEM
c ... pfbufd
#endif
c ... pfbufm
c ... pferrs
c ... pffils
c ... pfhrfu
c ... pftty
c
#include "pfbsiz.inc"
#include "pfetyp.inc"
#include "pfstfg.inc"
#include "pfbtyp.inc"
#ifdef STATIC_MEM
# include "pfbufd.inc"
#endif
#include "pfbufm.inc"
#include "pferrs.inc"
#include "pffils.inc"
#include "pfhrfu.inc"
#include "pftty.inc"
c
c     Internal variables:
c
c     Parameters:
#ifdef STATIC_MEM
      integer MXDINFO
      parameter (MXDINFO = NDINFO*MXDIRS )
#endif
c
c ----------------------------------------------------------------------
c
c
c ... initialize Raw dataset type information
c
c                            1234567890123456
      data rtypna(PFTDIR) / 'Directory       ' /
      data rtypln(PFTDIR)                         /  9 /
      data rtypna(PFTUF3) / 'Uniform 3D Float' /
      data rtypln(PFTUF3)                         / 16 /
      data rtypna(PFTUF1) / 'Uniform 1D Float' /
      data rtypln(PFTUF1)                         / 16 /
      data rtypna(PFTNF3) / 'Non-Unif. 3D FLT' /
      data rtypln(PFTNF3)                         / 16 /
      data rtypna(PFTNV3) / 'Non-Unif. 3D VEC' /
      data rtypln(PFTNV3)                         / 16 /
      data rtypna(PFTVTX) / 'Vertex List     ' /
      data rtypln(PFTVTX)                         / 11 /
      data rtypna(PFTIFL) / 'Int/Float List  ' /
      data rtypln(PFTIFL)                         / 14 /
      data rtypna(PFTNGD) / 'NonUn. MxN Float' /
      data rtypln(PFTNGD)                         / 16 /
      data rtypna(PFTNG3) / 'Non-Unif. 3D GRD' /
      data rtypln(PFTNG3)                         / 16 /
      data rtypna(PFTNI3) / 'Non-Unif. 3D INT' /
      data rtypln(PFTNI3)                         / 16 /
      data rtypna(PFTUNK) / 'Unknown         ' /
      data rtypln(PFTUNK)                         /  7 /
c
#ifdef STATIC_MEM
c ... initialize directory management arrays
c
      data nfdblk / DBLKMX /
      data tpdblk / DFAULT /
      data dblkpr / MXDPTR*0 /
      data dirinfo / MXDINFO*0 /
#endif
c
c ... initialize buffer management arrays
c
      data currec / MXFILS*0  /
      data eofre  / MXFILS*.FALSE. /
      data extflg / MXFILS*.FALSE. /
      data fillun / MXFILS*-1 /
      data lstwrd / MXFILS*0  /
      data locbuf / MXFILS*0  /
      data mdflag / MXFILS*0  /
      data modrec / MXFILS*.FALSE. /
      data wrflag / MXFILS*-1 /
      data dircnt / MXFILS*0 /
      data dircur / MXFILS*0 /
c
c ... initialize file info arrays
c
      data filcnt  / 0 /
      data filcur  / 0 /
      data stwrop  / 'new' /
      data fidmap  / MXFILS*0 /
      data fpprec  / DFAULT, MXFILS*DFAULT /
      data files   / MXFILS*' ' /
c
c ... initialize RFU array
c
      data rfunum  / 0 /
      data rfuval  / RFUMAX*DFAULT /
c
c ... initialize error flags, DEBUG/VERBOSE units and flags
c
      data ludbg,  luverb  / 2*OFFLUN /
      data vrbflg, v2dflg  / 2*.FALSE. /
      data errnum, errlun, errloc, errrec, errsta  / 5*0 /
      data errmod, errfil  / 2*' ' /
c
c ... initialize terminal I/O variables
c
      data luttyi, luttyo  / 5 , 6 /
      data ttycol          / 80 /
      data ttylin          / 24 /
c
      end
