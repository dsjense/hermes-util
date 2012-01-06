#if defined(ABSOFTsys)
# define USE_FILE_BROWSER
#endif
      subroutine pfmopn ( fid, lun, stat, fname, ierr )
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
c       - This routine is a MACHINE-DEPENDENT routine to open a PFF file.
c         Opened file is an ANSI-standard direct access file.
c
c     Machine-Dependent Summary:
c
c       - Most Fortran compilers measure record length in bytes. However,
c         some measure record length in longwords (4 bytes).
c       - Use file browser if no file name supplied (fname=' ') on
c         platforms that support it.
c
c ----------------------------------------------------------------------
c
c     Input:
c       fid     -  File ID of PFF file opened by this routine
c       fname   -  character variable containing name of file to be
c                  opened
c       lun     -  logical unit # to be opened
c       stat    -  open status
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ierr    -  error flag;
c                      0,   No error
c                      1,   Error Opening File
c                      2,   Invalid Status
c                      3,   File Does Not Exist
#if defined(HU_POSIX)
c                      4,   File Already Open
#else
error "File system type not declared"
#endif
c
c ----------------------------------------------------------------------
c
c     Internals:
c       istat   -  status flag from open call
c       lexist  -  logical flag for existence of file to be opened
c       lopen   -  logical flag indicating if file is open
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  PRECLB
c     pfstfg  -  RE, WR, RW
c
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pffils
c       w:    stwrop
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pferrs.inc"
#include "pffils.inc"
c
c ----------------------------------------------------------------------
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFMOPN' )
c
      integer   IRECL, PBPRU
c
c     # of bytes one unit of units that records are measured in
      parameter ( PBPRU  = HU_RECLU_SIZE )
c
c     record length for open statement
      parameter ( IRECL  = PRECLB/PBPRU )
c
c     Declare variables:
c
c ... Passed:
      integer   fid, lun, ierr, stat
      character fname*(*)
c
c ... Internal
      integer   istat
      logical   lexist, lopen
c
c ----------------------------------------------------------------------
c
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
c ... open file as direct access
c
      if ( (stat .eq. RE) .OR. (stat .eq. RW) ) then
c
#if defined(USE_FILE_BROWSER)
        if ( fname.eq.' ' ) then
c
c ....... for ABSoft, if no file supplied, use file browser, then
c ....... inquire for name
c
          open(unit=lun,status='old',file='',form='unformatted',
     &         access='direct',recl=IRECL,err=20,iostat=istat)
          inquire(unit=lun,name=fname)
c
        else
#endif
c
c ....... If file is to be opened for read:  Does file exist ?
c .......                                    Is it already open ?
c
          inquire(file=fname,exist=lexist,opened=lopen )

          if ( .NOT.lexist ) then
            ierr = 3
            call pfserr ( PFMODU, ierr, lun, 'File Does Not Exist' )
            goto 999
          else if( lopen ) then
            ierr = 4
            call pfserr ( PFMODU, ierr, lun, 'File Already Open' )
            goto 999
          endif
c
          open(unit=lun,status='old',file=fname,form='unformatted',
     &         access='direct',recl=IRECL,err=20,iostat=istat)
#if defined(USE_FILE_BROWSER)
        endif
#endif
c
      elseif (stat .eq. WR ) then
c
        if ( fname.ne.' ' ) then
c ....... does file exist?
          inquire(file=fname,exist=lexist)
          if ( lexist ) then
c ......... if not in overwrite mode, it's an error
            if ( stwrop.eq.'new' ) goto 20
c ......... otherwise, open file in seq. access, write something short, 
c ......... and close to truncate the file, make sure we handle access
c ......... violation w/ error handling on the write statement
            open(unit=lun,status='old',file=fname)
            write(lun,'(a)',err=20) char(1)
            close(unit=lun)
          endif
        endif
#if defined(USE_FILE_BROWSER)
c
        if ( fname.eq.' ' ) then
c
c ....... If no file supplied, use file browser, then inquire for name
c
          open(unit=lun,status='new',file='',form='unformatted',
     &         access='direct',recl=IRECL,err=20,iostat=istat)
          inquire(unit=lun,name=fname)
c
        else
c
          open(unit=lun,status=stwrop,file=fname,form='unformatted',
     &         access='direct',recl=IRECL,err=20,iostat=istat)
c
        endif
c
#else
        open(unit=lun,status=stwrop,file=fname,form='unformatted',
     &       access='direct',recl=IRECL,err=20,iostat=istat)
c
#endif
      else
c
        ierr   = 2
        call pfserr ( PFMODU, ierr, 0, 'Invalid Status' )
        goto 999
c
      endif
c
      goto 999
c
c     error encountered when opening file
c
   20 continue
c
        ierr = 1
        call pfserr ( PFMODU, ierr, lun, 'Error Opening File' )
c
  999 continue
c
c ... write status message if DEBUG is on and return
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
