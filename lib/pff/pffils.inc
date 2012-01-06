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
c     Include file common block variables associated with non-buffer
c     related file information.
c
c ----------------------------------------------------------------------
c
c   Parameters needed in this module, but not supplied:
c
c     -  MXFILS
c
c   Include file needed to supply these parameters:
c
c     -  pfbsiz.inc
c
c ----------------------------------------------------------------------
c
c   Explanation of the common-block variables:
c
c     fidmap  -  (*)  array mapping file # to FID
c     filcnt  -  # of currently open PFF files
c     filcur  -  pointer to current PFF file (not FID)
c     files   -  (*) filename of each open PFF file
c     fpprec  -  (*) precision type for writing <FARRAY>'s. Legal values:
c                    FP_REDU - reduced (2-byte) precision (default)
c                    FP_FULL - full (4-byte) precision
c                fpprec(0) is the default value for a newly opened file.
c     stwrop  -  status value for Fortran open statements for files 
c                opened in WRITE_ONLY mode ('new' or 'unknown')
c
c ----------------------------------------------------------------------
c
c   Declare variables and define the common block:
c
      integer   filcnt, filcur, fidmap, fpprec
      character stwrop*8, files*(CHRFIL)
c
      common /pffils/ filcnt, filcur, fidmap(1:MXFILS), fpprec(0:MXFILS)
c
      common /pffilc/ stwrop, files(1:MXFILS)
c
c***********************************************************************
