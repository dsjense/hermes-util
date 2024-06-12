      subroutine pfdprt ( luout, fid, width, low, high, npoint,
     &                    poidir, cptr, ierr )
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
c       - This routine is a DIRECTORY routine that prints some portion
c         of a PFF file's directory structure to a file (or terminal)
c       - This routine can print contiguous directory entries, or,
c         alternately, in can list those provided in a directory list
c         array.
c
c ----------------------------------------------------------------------
c
c     Input:
c       cptr    -  (*) 2-character strings containing the pointer text
c                  for the user-defined pointers
c       luout   -  logical unit # for list output
c       fid     -  File ID of PFF file
c       high    -  if low.ge.0:  upper limit on directory entry for list
c                     low.lt.0:  directory entry list
c       low     -  if low.ge.0:  lower limit on directory entry for list
c                     low.lt.0:  minus of # of entries in the directory
c                                entry list
c       luout   -  logical unit # for list output
c       npoint  -  # of user-defined directory pointers
c       poidir  -  (*) directory #s associated with user-defined pointers
c       width   -  maximum width of listing in characters
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error status flag;
c                     0,   No error
c                     1,   Illegal file ID (FID)
c                     2,   File NOT open
c                     3,   Error retrieving string from NAME SERVER
c
c ----------------------------------------------------------------------
c
c     Internals:
c       actwid  -  line width used for output list (in characters)
c       blk     -  directory block of current directory entry
c       blank   -  64-character blank string
c       dash    -  64-character string of dashes ('-')
c       enddat  -  logical flag indicating EOD entry at end of list
c       i       -  loop index over directory entries
c       iblk    -  if low.ge.0:  loop index over directory blocks
c                     low.lt.0:  loop index over directory entry list
c       iblkhi  -  upper limit of "iblk" loop
c       iblklo  -  lower limit of "iblk" loop
c       ifirst  -  lower limit of directory entry loop
c       ilast   -  upper limit of directory entry loop
c       ioff    -  offset between directory entry and block entry
c       ip      -  loop index over user-defined pointers
c       lfil    -  # of characters printed in file name
c       mblk    -  # of block in memory-resident directory structure
c       menmax  -  # of characters in title string printed
c       nblb    -  # of blanks printed before entry line
c       nblh    -  # of blanks printed before file line
c       ptrflg  -  logical indicating whether or not to display a pointer
c       thigh   -  temporary upper limit on directory entry for list
c       tlow    -  temporary lower limit on directory entry for list
c       tptr    -  temporary string for pointer text
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  DBLKSZ, MCHTTL, MXFILS
c     pfstfg  -  NOPEN
c
c ... Included common decks:
c ... pfbufd
c       r:    dirinfo(*,*,*)
c ... pfbufm
c       r:    dircnt(*), dircur(*), wrflag(*)
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pffils
c       r:    files(*)
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pfbufd.inc"
#include "pfbufm.inc"
#include "pferrs.inc"
#include "pffils.inc"
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   fid, high(*), ierr, low, luout, npoint, width
      integer   poidir(*)
      character cptr(*)*(*)
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFDPRT' )
      integer   L100, L400
      parameter ( L100 = 29 , L400 = 32 )
      character EODSTR*16
      parameter ( EODSTR = 'END-OF-DATA     ' )
c
c ... Internal
      integer   actwid, blk
      integer   i, iblk, iblkhi, iblklo, ifirst, ilast, ioff, ip
      integer   lfil, mblk, menmax, nblb, nblh, thigh, tlow
      logical   enddat, ptrflg
      character blank*64, dash*64, tptr*2
      character sctmp*(MCHTTL), sttmp*(MCHTYP)
c
c     External functions
      integer   NS_getname
c
      data blank /' '/
      data dash  /'-----------------------------------------------------
     &-----------'/
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
c ... is this a legal FID ??
c
      if ( fid.lt.1 .OR. fid.gt.MXFILS ) then
        ierr = 1
        call pfserr ( PFMODU, ierr, fid, 'Illegal file ID (FID)' )
        goto 999
      endif
c
c ... is the file open ??
c
      if ( wrflag(fid).eq.NOPEN ) then
        ierr = 2
        call pfserr ( PFMODU, ierr, fid, 'File NOT open' )
        goto 999
      endif
c
c ... figure out how much we can print with the supplied line width
c
      lfil   = max( 1, index(files(fid),' ') - 1 )
      actwid = min( width, max(MCHTTL + L400, lfil + L100) )
      nblh   = max(0,actwid - lfil - L100)/2 + 1
      lfil   = min( lfil, actwid - L100 - nblh )
      menmax = min( MCHTTL, actwid - L400 )
      nblb   = max( 0, actwid - menmax - L400 ) + 1
c
c ... print header
c
      write(luout,100) blank(1:nblh),files(fid)(1:lfil)
      write(luout,300) blank(1:nblb),dash(5:menmax)
      write(luout,200) blank(1:nblb),blank(5:menmax)
      write(luout,300) blank(1:nblb),dash(5:menmax)
c
c ... set default value of "list EOD" flag
c
      enddat = .FALSE.
c
c ... Are we in contiguous or list mode ?
c
      if ( low.ge.0 ) then
c
c ..... don't let limits exceed allowable limits
c
        tlow   = max(1,low)
c
        if ( high(1).gt.dircnt(fid) .OR. high(1).le.0 ) then
          thigh  = dircnt(fid)
          enddat = .TRUE.
        else
          thigh  = high(1)
        endif
c
c ..... loop over all file directory blocks involved
c
        iblklo = (tlow - 1)/DBLKSZ
        iblkhi = (thigh - 1)/DBLKSZ
c
      else
c
c ..... let outer (block) loop be over entries in list array
c
        iblklo = 1
        iblkhi = -low
c
      endif
c
c ... loop over blocks (or entries in list)
c
      do 20 iblk=iblklo,iblkhi
c
c ..... Are we in contiguous or list mode ?
c
        if ( low.ge.0 ) then
c
c ....... find entry limits & actual directory block in this file block
c
          ioff   = iblk*DBLKSZ
          ifirst = max( tlow,ioff + 1 )
          ilast  = min( thigh, ioff + DBLKSZ )
          blk    = iblk
c
        else
c
c ....... loop over 1 entry only; find actual directory block and offset
c ....... for this entry
c
          ifirst = high(iblk)
          ilast  = high(iblk)
          blk    = (ifirst - 1)/DBLKSZ
          ioff   = blk*DBLKSZ
c
        endif
        mblk   = dblkpr(blk,fid)
c
c ..... loop over entries in this block
c
        do 10 i=ifirst,ilast
c
          if ( npoint.eq.0 ) then
c
c ......... compare entry to file's default (current) entry
c
            if (i.eq.dircur(fid)) then
              tptr = '->'
              ptrflg = .TRUE.
            else
              ptrflg = .FALSE.
            endif
c
          else
c
c ......... compare entry to supplied list of pointer entries
c
            do 1 ip=1,npoint
              if (poidir(ip).eq.i) then
                tptr = cptr(ip)
                ptrflg = .TRUE.
                goto 2
              endif
    1       continue
c
              ptrflg = .FALSE.
c
    2       continue
c
          endif
c
          ierr   = NS_getname(dirinfo(PDTTL,i-ioff,mblk),sctmp)
          if ( ierr.ge.0 ) 
     &      ierr = NS_getname(dirinfo(PDTYP,i-ioff,mblk),sttmp)
          if ( ierr.lt.0 ) then
            ierr = 3
            call pfserr ( PFMODU, ierr, fid,
     &                    'Error retrieving string from NAME SERVER' )
            goto 999
          endif
          ierr = 0
          if(ptrflg)then
            if(i.lt.10000) then
              write(luout,410) blank(1:nblb), tptr, i,
     &          sttmp(1:16), sctmp(1:menmax)
            else
              write(luout,420) blank(1:nblb), tptr, mod(i,10000),
     &          sttmp(1:16), sctmp(1:menmax)
            endif
          else
            write(luout,400) blank(1:nblb), i,
     &          sttmp(1:16), sctmp(1:menmax)
          endif
c
   10   continue
c
   20 continue
c
c ... if upper limit was past maximum, print an EOD line
c
      if ( enddat ) then
c
        i = dircnt(fid) + 1
c
        if ( npoint.eq.0 ) then
c
          if (i.eq.dircur(fid)) then
            tptr = '->'
            ptrflg = .TRUE.
          else
            ptrflg = .FALSE.
          endif
c
        else
c
          do 31 ip=1,npoint
            if (poidir(ip).eq.i) then
              tptr = cptr(ip)
              ptrflg = .TRUE.
              goto 32
            endif
   31     continue
c
            ptrflg = .FALSE.
c
   32     continue
c
        endif
c
        if (ptrflg) then
          if(i.lt.10000) then
            write(luout,410) blank(1:nblb), tptr, i, EODSTR,
     &                       blank(1:menmax)
          else
            write(luout,420) blank(1:nblb), tptr, mod(i,10000), EODSTR,
     &                       blank(1:menmax)
          endif
        else
          write(luout,400) blank(1:nblb), i, EODSTR, blank(1:menmax)
        endif
      endif
c
      write(luout,300) blank(1:nblb),dash(5:menmax)
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
  100 format(/,a,'DIRECTORY LISTING of File:  ',a)
  200 format(a,'|     # | Data Type        | Title',a,'|')
  300 format(a,'+-------+------------------+------',a,'+')
  400 format(a,'| ',i5,' | ',a,' | ',a,' |')
  410 format(a,'|',a,i4,' | ',a,' | ',a,' |')
  420 format(a,'|',a,i4.4,' | ',a,' | ',a,' |')
 1000 format(' %PFF-',a,'-DBG:  ',a,' Error Status = ',i5)
c
      end

      subroutine pfdprt_scalar(luout, fid, width, low, high, npoint,
     &                         poidir, cptr, ierr)
        integer luout, fid, width, low, high(*), npoint, ierr
        integer poidir
        integer poidirs(1)
        character cptr(*)*(*)
        poidirs(1) = poidir
        call pfdprt(luout, fid, width, low, high, npoint, poidirs, cptr,
     &              ierr)
        poidir = poidirs(1)
      end subroutine

