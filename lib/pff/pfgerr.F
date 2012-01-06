      subroutine pfgerr (modul, number, finfo, fname, clear)
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
c       - This routine is a UTILITY routine to retrieve the error
c         flags in the PFERRS common block.
c
c ----------------------------------------------------------------------
c
c     Input:
c       clear   -  if true, clear the error flags after reading them
c
c ----------------------------------------------------------------------
c
c     Output:
c       modul   -  module name of calling routine
c       number  -  error #
c       finfo   -  (*) information about file associated with error:
c                    1:  PFF File ID on which the error occurred
c                        (if .le. 0, no file associated with the error)
c                    2:  buffer loc. of file
c                    3:  Unit # of file
c                    4:  record # of file
c                    5:  I/O status of file
c       fname   -  file name associated with last reported error
c
c ----------------------------------------------------------------------
c
c     Internals:
c
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz:
c
c ... Included common decks:
c ... pferrs
c       r:    errfid, errfil, errloc, errfid, errmod, errnum, errrec,
c             errsta, errlun
c
#include "pfbsiz.inc"
#include "pferrs.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   finfo(5), number
      character fname*(*), modul*(*)
      logical   clear
c
c     Internal
c
c ... Parameters:
c
c ... External Block Data
      external  pffbd
c
c ----------------------------------------------------------------------
c
c
c ... extract info from error block
c
      modul = errmod
      number = errnum
      finfo(1) = errfid
      if ( errfid.gt.0 ) then
        finfo(2) = errloc
        finfo(3) = errlun
        finfo(4) = errrec
        finfo(5) = errsta
        fname = errfil
      endif
c
c ... if clear set, clear all flags
c
      if (clear) then
c
        errnum = 0
        errmod = ' '
        errfid = 0
        errloc = 0
        errlun = 0
        errrec = 0
        errsta = 0
        errfil = ' '
c
      endif
c
      return
c
      end
