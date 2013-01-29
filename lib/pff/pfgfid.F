      function pfgfid ( fen, ierr )
      integer  pfgfid
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
c       - This routine is a GET routine to find the FID that maps to an
c         element in the open file list.
c
c     Input:
c       fen     -  entry from open file list; if "fen" = 0, then return
c                  the FID of PFF's current file
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   File not in open file list
c
c     Return Value:
c       pfgfid  =  < 0,    error has occurred, returns (-IERR)
c                  > 0,    returns File ID (FID) of file
c
c ----------------------------------------------------------------------
c
c     Internals:
c       tfen    -  temporary file # on open file list
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz
c
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pffils
c       r:    filcnt, filcur, fidmap(*)
c
#include "pfbsiz.inc"
#include "pferrs.inc"
#include "pffils.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fen, ierr
c
c     Internal
      integer  tfen
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFGFID' )
c
c ... External Block Data
      external  pffbd
c
c ----------------------------------------------------------------------
c
c
c     write status message if DEBUG is on
c
      if (ludbg.ne.OFFLUN)then
        write (ludbg,1000) PFMODU, 'Entry', ierr
      endif
c
c     return if error flag is not zero on entry
c
      if(ierr.ne.0)then
        call pfserr ( PFMODU, 0, 0, 'Error Flag Non-zero on Entry' )
        goto 999
      endif
c
c ----------------------------------------------------------------------
c
c     if fen=0, return FID of PFF's current file
c
      if ( fen.eq.0 ) then
        tfen = filcur
      else
        tfen = fen
      endif
c
c ... Is supplied # on open file list ??
c
      if ( tfen.ge.1 .AND. tfen.le.filcnt ) then
c
c ..... if on list, return mapped FID
c
        pfgfid = fidmap(tfen)
c
      else
c
c ..... otherwise, return error
c
        ierr   = 1
        call pfserr ( PFMODU, ierr, 0, 'File not in open file list' )
        pfgfid = -ierr
c
      endif
c
c ----------------------------------------------------------------------
c
  999 continue
c
c     write status message if DEBUG is on and return
c
      if (ludbg.ne.OFFLUN)then
        write (ludbg,1000) PFMODU, 'Exit', ierr
      endif
c
      return
c
 1000 format(' %PFF-',a,'-DBG:  ',a,' Error Status = ',i5)
c
      end
