      subroutine pfserr (modul, number, fid, string)
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
c       - This routine is a UTILITY routine to set (or clear) the error
c         flags in the PFERRS common block.  Also, if VERBOSE-MODE is
c         currently ON, appropriate error messages are printed to the
c         VERBOSE and/or the DEBUG output files.
c
c ----------------------------------------------------------------------
c
c     Input:
c       modul   -  module name of calling routine
c                  (if modul(1:1).eq.' ', error flags are cleared)
c       number  -  error # (if .le. 0, error flags are not modified)
c       fid     -  PFF File ID on which the error occurred
c                  (if .le. 0, no ID associated with the error)
c       string  -  error message text
c
c ----------------------------------------------------------------------
c
c     Output:
c       none
c
c ----------------------------------------------------------------------
c
c     Internals:
c
c       modlen  -  Non-blank length of modul
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz: MXFILS
c
c ... Included common decks:
c ... pfbufm
c       r:    currec(*), locbuf(*), wrflag(*),
c             fillun(*)
c ... pferrs
c       r:    OFFLUN, ludbg, luverb, v2dflg, vrbflg
c       w:    errfid, errfil, errloc, errfid, errmod, errnum, errrec,
c             errsta
c ... pffils
c       r:    files(*)
c
#include "pfbsiz.inc"
#include "pfbufm.inc"
#include "pferrs.inc"
#include "pffils.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, number
      character string*(*), modul*(*)
c
c     Internal
      integer   modlen
c
c ... Parameters:
c
c ... External Block Data
      external  pffbd
c
c ----------------------------------------------------------------------
c
c
c ... Find last non-blank character in module name
c
      modlen = index( modul, ' ' )
      if(modlen.le.0)then
        modlen = len(modul)
      else
        modlen = modlen - 1
      endif
c
c ... if MODUL(1:1) .eq. ' ', clear error flags and return
c
      if (modlen.eq.0) then
c
        errnum = 0
        errmod = ' '
        errfid = 0
        errloc = 0
        errrec = 0
        errsta = 0
        errfil = ' '
c
      else
c
c ..... if NUMBER .gt. 0, set error flags, otherwise, leave them be
c
        if (number.gt.0) then
c
          errnum = number
          errmod = modul
          errfid = fid
c
          if ( fid.ge.1 .AND. fid.le.MXFILS ) then
            errloc = locbuf(fid)
            errrec = currec(fid)
            errsta = wrflag(fid)
            errlun = fillun(fid)
            errfil = files(fid)
          else
            errloc = 0
            errrec = 0
            errsta = 0
            errlun = 0
            errfil = ' '
          endif
c
        endif
c
c ..... Now print message if in verbose mode
c ..... NOTE:  Write file info messages only if FID > 0
c
        if (vrbflg) then
c
          modlen = min(modlen,6)
c
c ....... write message to VERBOSE file (if on)
c
          if (luverb.ne.OFFLUN) then
            write (luverb,1000) modul(1:modlen),number,string
            if ( fid.ge.1 .AND. fid.le.MXFILS ) then
              write(luverb,2000) fid,files(fid),wrflag(fid),
     &                          currec(fid),locbuf(fid),fillun(fid)
            else if ( fid.ne.0 ) then
              write(luverb,3000) fid
            endif
          endif
c
c ....... write message to DEBUG file (if they are supposed to go there)
c
          if (v2dflg) then
            write (ludbg,1000) modul(1:modlen),number,string
            if ( fid.ge.1 .AND. fid.le.MXFILS ) then
              write(ludbg,2000) fid,files(fid),wrflag(fid),
     &                         currec(fid),locbuf(fid),fillun(fid)
            else if ( fid.ne.0 ) then
              write(ludbg,3000) fid
            endif
          endif
c
        endif
c
      endif
c
      return
c
 1000 format(' %PFF-',a,'-',i3.3,':  ',a)
 2000 format(' %%  File(',i3.3,')=',a,/,
     &       ' %%  Status=',i3,'  Rec. #=',i10,'  Buf-Loc=',i5,
     &          '  Unit=',i5)
 3000 format(' %%  PFF File ID = ',i3.3)
c
      end
