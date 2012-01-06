      subroutine parsepar (
c ... INPUT
     &  iparam,
c ... OUTPUT
     2  oparam, nchpar, aindex)
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
c ... Utility routine to test a character parameter to see if it is
c     an array element expression of the form name[aindex], where
c     aindex is an integer.  If it is, return name in param, and the
c     index in integer AINDEX.  If not, return PARAM untouched. 
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT
c       iparam  -  Input parameter string
c
c     OUTPUT:
c       oparam  -  Output parameter string: IPARAM if it does not
c                  have an index substring; just the name if it does
c       nchpar  -  Non-blank length of OPARAM
c       aindex  -  Numerical value of array index if index substring
c                  at the end of IPARAM: -1 otherwise
c
      character*(*) iparam, oparam
      integer       aindex, nchpar
c
c-----------------------------------------------------------------------
c
c ... Include files
c
#include "ioctrl.inc"
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer       chrtoi, lennb, partyp
c
c-----------------------------------------------------------------------
c
c ... Local PARAMETERS and variables:
c
      character*80 idxstr
      integer      ierr, llbr
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
c ... Check for index substring at the end of PARAM
c
      aindex = -1
      oparam = iparam
      nchpar = lennb(oparam)
c
      if (oparam(nchpar:nchpar) .EQ. RBRAKT) then
        llbr = index (oparam, LBRAKT)
        if (llbr.GT.1 .AND. llbr.LT.nchpar-1) then
          idxstr = oparam(llbr+1:nchpar-1)
          if (partyp(idxstr) .EQ. INT) then
            oparam = oparam(1:llbr-1)
            nchpar = llbr - 1
            ierr   = 0
            aindex = chrtoi(idxstr, ierr)
          endif
        endif
      endif  
c
      return
      end
