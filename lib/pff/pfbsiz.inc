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
c     Include file containing parameters for PFF record and buffer sizes
c
c ----------------------------------------------------------------------
c
c   Parameters needed in this module, but not supplied:  NONE
c
c ----------------------------------------------------------------------
c
c   Explanation of the parameters:
c
c     CHRFIL  -  Maximum # of characters for PFF file names
c     DBLKMX  -  # of directory blocks
c     DBLKMM  -  # of directory blocks - 1
c     DBLKSZ  -  # of directory entrys in a directory block
c     LBUF    -  # of integers that are packed into each PFF file record
c     LFHDR   -  # of 2-byte words in file header (incl. framing word)
c     MCHTTL  -  # of characters dimensioned for dataset title strings
c     MCHTYP  -  # of characters dimensioned for type name strings
c     MXDIRS  -  maximum # of PFF directory entries
c     MXDPTR  -  maximum # of PFF directory block pointers
c     MXFILS  -  maximum # of PFF files that may simultaneously be open
c     PBPW    -  # of bytes into which one integer is packed in PFF file
c     PRECLB  -  # of bytes in a PFF file record
c
c ----------------------------------------------------------------------
c
c   Declare and define the parameters
c
c     fixed parameters:
c
      integer   CHRFIL, DBLKMX, DBLKSZ, LFHDR, MCHTTL, MCHTYP
      integer   MXFILS, PBPW, PRECLB
c
      parameter ( CHRFIL = 64 )
      parameter ( DBLKMX = 200 )
      parameter ( DBLKSZ = 50 )
      parameter ( LFHDR  = 16 )
      parameter ( MCHTYP = 16 )
      parameter ( MCHTTL = 64 )
      parameter ( MXFILS = 20 )
      parameter ( PBPW   = 2 )
      parameter ( PRECLB = 4096 )
c
c     computed parameters:
c
      integer DBLKMM, LBUF, MXDIRS, MXDPTR
c
      parameter ( DBLKMM = DBLKMX - 1 )
      parameter ( LBUF   = PRECLB/PBPW )
      parameter ( MXDIRS = DBLKMX*DBLKSZ )
      parameter ( MXDPTR = DBLKMX*MXFILS )
c
c***********************************************************************
