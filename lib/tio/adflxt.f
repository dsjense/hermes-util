      character*(*) function adflxt (
c ... INPUT:
     1  fname, defext)
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
c     Add a default file extension to a file name.  If there is no
c     ".ext" in string "fname", it adds ".'defext'".
c
c-----------------------------------------------------------------------
c
c ... Function arguments:
c
c     INPUT:
c       fname   -  Root file name
c       defext  -  Default extension
c
      character*(*) fname, defext
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer  lennb
c
c-----------------------------------------------------------------------
c
c ... Local variables:
c
      integer  lnfnam, lenext
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      lnfnam = lennb(fname)
c
      if (lnfnam .GT. 0) then
        if (index(fname,'.') .GT. 0) then
          adflxt = fname(1:lnfnam)
        else
          lenext = lennb(defext)
          if (lenext .GT. 0) adflxt =
     1                       fname(1:lnfnam) // '.' // defext(1:lenext)
        endif
      else
        adflxt = ' '
      endif
c
      return
      end
