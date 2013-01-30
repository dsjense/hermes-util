      integer function mdotxf (
c ... INPUT
     1  rdonly, lutxf, fname, fstat, crgctl, deffnm, luerr)
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
c     Machine dependent routine to open a text file for reading or
c     writing.  It returns the following values:-
c
c        0  -  File opened successfully
c       -1  -  Error opening file
c       -2  -  Error in MDPATH getting full path name or blank file name
c
#if defined(ABSOFTsys)
c *** ABSoft MAC version.  Special features:
c
c        1.  READONLY not used.
c        2.  CARRIAGECONTROL keyword value not used.
c        3.  DEFAULTFILE keyword value not used.
c        4.  If supplied filename is blank, use file browser to query
#else
c *** UNIX version.  Special features:
c
c        1.  READONLY not used.
c        2.  CARRIAGECONTROL keyword value not used.
c        3.  DEFAULTFILE keyword value not used.
#endif
c
c-----------------------------------------------------------------------
c
c ... Function arguments
c
c     INPUT:
c       rdonly  -  Read only logical flag
c       lutxf   -  Logical unit # for text file
c       fname   -  File name
c       fstat   -  "STATUS" argument in OPEN statement
c       crgctl  -  "CARRIAGECONTROL" argument in OPEN statement (only
c                  used for files opened with write access)
c       deffnm  -  "DEFAULTFILE" argument in OPEN statement
c       luerr   -  Logical unit # for error messages (if luerr.LT.0, no 
c                  error messages are written)
c    
c
      character*(*)  crgctl, deffnm, fname, fstat
      logical        rdonly
      integer        lutxf, luerr
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer  mdpath
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character fulnam*200
      integer   i, ierr, lenfnm
c
c
c=======================================================================
c ... BEGIN:
c=======================================================================
c
      do 1 i=len(fname),1,-1
        if ( fname(i:i).ne.' ' ) then
          lenfnm = i
          goto 2
        endif
    1 continue
c
      if (luerr.ge.0) then
        write(luerr,9800) fname(1:1)
        mdotxf = -2
        return
      endif
c
c ... Get full file name
c
    2 continue
      ierr = mdpath (fname, fulnam)
c
      if (ierr .NE. 0) then
        if (luerr.ge.0) write(luerr,9800) fname(1:lenfnm)
        mdotxf = -2
        return
      endif
c
#if defined(ABSOFTsys)
c     *************************** ABSoft MAC ***************************
c
      if ( fulnam.eq.' ' ) then
        open (unit=lutxf, file='', status=fstat, err=9000,
     1        form='formatted')
        inquire(unit=lutxf,name=fname)
      else
        open (unit=lutxf, file=fulnam, status=fstat, err=9000,
     1        form='formatted')
      endif
#else
c     ****************************** UNIX ******************************
c
      open (unit=lutxf, file=fulnam, status=fstat, err=9000,
     1      form='formatted')
#endif
c
      mdotxf = 0
      return
c
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
c ... File failed to open
c
 9000 continue
      if (luerr.ge.0) then
        do i=len(fulnam),1,-1
          if ( fulnam(i:i).ne.' ' ) then
            lenfnm = i
            goto 3
          endif
        enddo
        lenfnm = 1
    3   continue
        write(luerr,9810) fulnam(1:lenfnm)
      endif
      mdotxf = -1
      return
c
 9800 format(/' *** MDOTXF: ERROR'/
     1'     Error processing file name: ',a/)
c
 9810 format(/' *** MDOTXF: ERROR'/
     1'     File failed to open'/
     2'     File name: ',a/)
c
      end
