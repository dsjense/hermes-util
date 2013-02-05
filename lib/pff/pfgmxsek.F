      function pfgmxsek ()
c
#include "mdfpos.h"
c
      HU_FPOS_TYPE pfgmxsek
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
c       - This function is a GET routine that returns the maximum file
c         offset that can be passed to the PFUSEK function.
c       - Note that this sets the limit of a PFF file's length to
c         2 * PFGMXSEK.
c
c ----------------------------------------------------------------------
c
c     Input: None
c
c ----------------------------------------------------------------------
c
c     Output: None
c
c ----------------------------------------------------------------------
c
c     Return Value:  The maximum file offset that can be passed to the
c                    PFUSEK function.
c
c ----------------------------------------------------------------------
c
c     Internals:
c      two   -  integer 2 at HU_FPOS_TYPE precision
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
c
c     Internal
      HU_FPOS_TYPE two
c
c ... Parameters:
c
c ... External Block Data
      external  pffbd
c
c ----------------------------------------------------------------------
c
      two = 2
c
#if HU_FPOS_BYTES < 6
c ... file is limited by system file addressing:
c ...    (2^32 - 1) bytes -> (2^31 - 1) 2-byte words
c ...  if 32-bit signed integer, need to be careful about overflow
c
       pfgmxsek = two**30
       pfgmxsek = (pfgmxsek - 1) + pfgmxsek
#else
c ... file is limited by other constraints:
c ...    maximum pful2i lval:           2^44 - 1
c ...    maximum Fortran record value:  2^42 - 2049
c ... Consequently, the limit is 2^42 - 2049
c
      pfgmxsek = two**42 - 2049
#endif
c
      end
