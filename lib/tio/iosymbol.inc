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
c ... Common block for handling symbols in TIOLIB
c ... Both global and local symbols can be either scalar or arrays,
c     based on how many elements they have. For a scalar, the handle
c     to the value is for the equivalence string; for an array, the
c     handle is to an array of handles for the equiv. strings
c
c     Externally-defined parameters used in this module:
c       iocmdf.inc: MXCMFL
c
c-----------------------------------------------------------------------
c
c     MXGSYM   -  Max. # global symbols
c     MXLSYM   -  Max. # local symbols (for ALL command file levels)
c
      integer     MXGSYM, MXLSYM
      parameter  (MXGSYM  = 400)
      parameter  (MXLSYM  = 800)
c
c-----------------------------------------------------------------------
c
c ... Global symbols
c
c     ngsym     -  # global symbols
c     nelgsym   -  (*) # elements of each global symbol
c     hgsymnam  -  (*) Handles to global symbol names
c     hgsymval  -  (*) Handles to global symbol values
c
      integer  ngsym
      integer  nelgsym(1:MXGSYM)
      integer  hgsymnam(1:MXGSYM), hgsymval(1:MXGSYM)
      common /iosymn/ ngsym, nelgsym, hgsymnam, hgsymval
c
c ... Local symbols: Can only access local symbols defined at the
c     current command file (CMF) level, i.e. loclsym(cmf_lev) to nlsym
c
c     nlsym     -  (*) # local symbols at each CMF level
c     loclsym   -  (*) Pointer to first local symbol for CMF level
c     nellsym   -  (*) # elements of each local symbol
c     hlsymnam  -  (*) Handles to local symbol names
c     hlsymval  -  (*) Handles to local symbol values
c
      integer  nlsym(0:MXCMFL), loclsym(0:MXCMFL)
      integer  nellsym(1:MXLSYM)
      integer  hlsymnam(1:MXLSYM), hlsymval(1:MXLSYM)
      common /iosymn/ nlsym, loclsym, nellsym, hlsymnam, hlsymval
c
c-----------------------------------------------------------------------
