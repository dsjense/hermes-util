      subroutine pfsfen ( fid, newfen, ierr )
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
c       - This routine is a SET routine to set the File Entry # (FEN)
c         on the open file list that maps to a specified PFF File ID
c         (FID). Note that the FID's associated with all FEN's between
c         the specified FID's original and requested FEN are shifted up
c         or down by one to accomodate the change. Also, if the current
c         file pointer indicates one of the the affected FID's, it is 
c         adjusted so that it points to the same FID as it originally did.
c
c     Input:
c       fid     -  File ID of PFF file; if "fid" = 0, then change the
c                  FEN of PFF's current file
c       newfen  -  The desired new FEN to be associated with the supplied
c                  FID
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   Illegal File ID (FID)
c                      2,   File NOT Open
c
c ----------------------------------------------------------------------
c
c     Internals:
c       i       -  loop index over open file list
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
c       r:    filcnt
c       rw:   filcur, fidmap(*)
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
      integer   fid, newfen, ierr
c
c     Internal
      integer   i, curfen
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFSFEN' )
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
      curfen = 0
c
c ... if fid=0, return FEN of PFF's current file
c
      if ( fid.eq.0 ) then
c
        if ( filcur.ge.1 .AND. filcur.le.filcnt ) then
          curfen = filcur
        endif
c
      else
c
c ..... is FID legal ?
c
        if( fid.lt.1 .OR. fid.gt.MXFILS )then
c
          ierr = 1
          call pfserr ( PFMODU, ierr, fid, 'Illegal File ID (FID)' )
          goto 999
c
        endif
c
c ..... loop over all open files looking for match
c
        do 1 i=1,filcnt
          if ( fid.eq.fidmap(i) ) then
            curfen = i
            goto 2
          endif
    1   continue
c
    2   continue
c
      endif
c
c ... was a match found?
c
      if (curfen.eq.0) then
        ierr = 2
        call pfserr ( PFMODU, ierr, fid, 'File NOT Open' )
        goto 999
      endif
c
c ... if FID is already mapped to the current FEN, then nothing needs to 
c     be changed
c
      if ( curfen.ne.newfen ) then
        if ( curfen.lt.newfen ) then
c ....... push intervening FIDs down on map          
          do i=curfen+1,newfen
            fidmap(i-1) = fidmap(i)
c ......... update current file pointer, if needed
            if ( i.eq.filcur ) filcur = filcur + 1
          end do
        else 
c ....... push intervening FIDs up on map          
          do i=curfen-1,newfen,-1
            fidmap(i+1) = fidmap(i)
c ......... update current file pointer, if needed
            if ( i.eq.filcur ) filcur = filcur + 1
          end do
        endif
c ..... Now deal with requested FID/FEN pair
        fidmap(newfen) = fid
        if ( filcur.eq.curfen ) filcur = newfen

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
