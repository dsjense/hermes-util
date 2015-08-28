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
c> \file getbptr.F
c> \brief File containing function \ref getbptr of the STAK library.
c
c> \addtogroup PublicInterface
c>   \{  \}
c
c> \addtogroup PrivateInterface Documentation of Non-Public, Utility Functions
c> \brief The utility functions used in the STAK's Non-Public layer
c>        are intended for use by the STAK library itself and their use by
c>        application code should be avoided, since neither their
c>        functionality, calling interface, or even their continued
c>        existence in future versions of the library are guaranteed.
c>   \{
c
c> \brief This function returns a local pointer to the permanent pointer to a 
c>        specific memory bin.
c>
c> \param[in] bin  Bin number whose pointer is to be returned
c>
c> \returns Returns the value of the permanent pointer (type \b HU_PTR_tYPE)
c>          to memory bin \e bin
c
c ----------------------------------------------------------------------
#include "mdptr.h"
c following defines are related to doxygen
#define HU_PTR_tYPE  HU_PTR_TYPE
#define HU_SIZE_t  HU_SIZE_T
      function getbptr(bin)
c
c ----------------------------------------------------------------------
c
      implicit none
c
c ----------------------------------------------------------------------
c
c     Return value
      HU_PTR_tYPE getbptr
c
c     Passed variables
      integer bin
c
c     Common blocks
#include "stak.inc"
c
c     Local variables: NONE
c
c ----------------------------------------------------------------------
c
c ... return permanent bin pointer (clip bin to legal range)
c
      getbptr = binptr( max(1,min(NBINS,bin)) )
c
      return
      end
c>  \} END of Private Interface group
