      subroutine pffprt ( luout, width, low, high, npoint, poifid,
     &                    cptr, ierr )
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
c
c     Summary:
c
c       - This routine is a FILE routine that prints some portion
c         of a PFF's open file list to a file (or terminal)
c
c ----------------------------------------------------------------------
c
c     Input:
c       cptr    -  (*) 2-character strings containing the pointer text
c                  for the user-defined pointers
c       luout   -  logical unit # for list output
c       high    -  upper limit on file entry for list
c       low     -  lower limit on file entry for list
c       npoint  -  # of user-defined FID pointers
c       poifid  -  (*) FIDs associated with user-defined pointers
c       width   -  maximum width of listing in characters
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                     0,   No error
c                     1,   Error writing to output file
c
c ----------------------------------------------------------------------
c
c     Internals:
c       actwid  -  line width used for output list (in characters)
c       blank   -  64-character blank string
c       dash    -  64-character string of dashes ('-')
c       fid     -  PFF File ID a
c       i       -  loop index over open files
c       ip      -  loop index over user-defined pointers
c       menmax  -  # of characters in title string printed
c       nblb    -  # of blanks printed before entry line
c       nblh    -  # of blanks printed before file line
c       tcfil   -  temporary string for file name
c       thigh   -  temporary upper limit on directory entry for list
c       tlow    -  temporary lower limit on directory entry for list
c       tptr    -  temporary string for pointer text
c       wrstat  -  2-character strings of file status (RE, WR, or RW)
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  CHRFIL, MXFILS
c     pfstfg  -  RE, WR, RW
c
c ... Included common decks:
c ... pfbufm
c       r:    dircnt(*), wrflag(*)
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pffils
c       r:    filcnt, filcur, fidmap(*), files(*)
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pfbufm.inc"
#include "pferrs.inc"
#include "pffils.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   high, ierr, low, luout, npoint, width
      integer   poifid(*)
      character cptr(*)*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFFPRT' )
      integer   L100, L400
      parameter ( L100 = 26 , L400 = 34 )
c
c ... Internal
      integer   actwid, fid, i, ip, menmax, nblb, nblh, thigh, tlow
      character blank*64, dash*64, tptr*2, tcfil*(CHRFIL)
      character wrstat(0:2)*2
c
      data blank /' '/
      data dash  /'-----------------------------------------------------
     &-----------'/
      data wrstat(RE) / 'RE' /
      data WRSTAT(WR) / 'WR' /
      data WRSTAT(RW) / 'RW' /
c
c ----------------------------------------------------------------------
c
c ... write status message if DEBUG is on
c
      if (ludbg.ne.OFFLUN)then
        write (ludbg,1000) PFMODU, 'Entry', ierr
      endif
c
c ... return if error flag is not zero on entry
c
      if(ierr.ne.0)then
        call pfserr ( PFMODU, 0, 0, 'Error Flag Non-zero on Entry' )
        goto 999
      endif
c
c ----------------------------------------------------------------------
c
c
c ... don't let limits exceed allowable limits
c
      tlow   = max(1,low)
c
      if (high.le.0) then
        thigh = filcnt
      else
        thigh  = min(MXFILS,filcnt,high)
      endif
c
c ... figure out how much we can print with the supplied line width
c
      actwid = min( width, max( CHRFIL + L400, L100 ) )
      nblh   = max( 0, actwid - L100 )/2 + 1
      menmax = min( CHRFIL, actwid - L400 )
      nblb   = max( 0, actwid - menmax - L400 ) + 1
c
c ... print header
c
      write(luout,100,err=50) blank(1:nblh)
      write(luout,300,err=50) blank(1:nblb),dash(10:menmax)
      write(luout,200,err=50) blank(1:nblb),blank(10:menmax)
      write(luout,300,err=50) blank(1:nblb),dash(10:menmax)
c
c ... print window of directory with pointer to current record
c
      do 10 i=tlow,thigh
c
        fid   = fidmap(i)
        tcfil = files(fid)
c
        if(npoint.eq.0)then
c
          if (i.eq.filcur) then
            tptr = '->'
          else
            tptr = '  '
          endif
c
        else
c
          do 1 ip=1,npoint
            if (poifid(ip).eq.fid) then
              tptr = cptr(ip)
              goto 2
            endif
    1     continue
c
            tptr = '  '
c
    2     continue
c
        endif
c
        write(luout,400,err=50) blank(1:nblb),tptr,i,tcfil(1:menmax),
     &                          wrstat(wrflag(fid)),dircnt(fid),fid
c
   10 continue
c
      write(luout,300,err=50) blank(1:nblb),dash(10:menmax)
c
      goto 999
c
   50 continue
c
      ierr = 1
      call pfserr ( PFMODU, ierr, fid, 'Error writing to output file' )
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
c
  100 format(/,a,'DIRECTORY of Active Files')
  200 format(a,'|     # | File Name',a,' | St | Entries | FID |')
  300 format(a,'+-------+----------',a,'-+----+---------+-----+')
  400 format(a,'| ',a,i3,' | ',a,' | ',a,' |  ',i5,'  | ',i3,' |')
c
 1000 format(' %PFF-',a,'-DBG:  ',a,' Error Status = ',i5)
c
      end
