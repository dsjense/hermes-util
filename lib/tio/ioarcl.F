      subroutine ioarcl (
c ... INPUT
     1 mode, uline,
c ... OUTPUT
     2 ierr )
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
c     IOARCL writes out lines to the archive file.  It operates in
c     one of two modes:-
c
c        1.  Archive the internal TIOLIB input line (i.e. unexpanded)
c
c        2.  Write out supplied user line ULINE
c
c     If MODE < 0, the lines are archived using mode ABS(MODE) WITH a
c     preceding comment character
c
c     NOTES:
c
c         1. IOARCL returns with IERR=-1 if unable to archive the
c            line (archive file not open, or archiving disabled
c            at this level).  NO ERROR MESSAGE WRITTEN TO ERR UNIT
c
c         2. If it encounters any errors, it shuts down archiving
c            (user must then reenable it explicitly)
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT
c       mode    -  Archive mode
c       uline   -  User line to be archived (in mode 1)
c
c     OUTPUT:
c       ierr    -  Error flag:-
c                            0  = normal return
c                            -1 = Unable to archive line (Archive file
c                                 not open, or archiving disabled)
c                            -2 = Error on write to archive file (?!!)
c
      character*(*) uline
      integer       ierr, mode
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "ioctrl.inc"
#include "ioinln.inc"
#include "iolun.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer  lennb, tioerh
c
c-----------------------------------------------------------------------
c
c ... Local variables:
c
      integer  i, lenuln, lmode
      logical  nocmnt
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      if (luarcv.LT.0 .OR. arclvl.LT.0) then
        ierr = -1
        return
      else
        ierr   = 0
        lmode  = abs(mode)
        nocmnt = mode .GT. 0
      endif
c
c-----------------------------------------------------------------------
c ... MODE 1: Archive TIOLIB internal input line
c-----------------------------------------------------------------------
c
      if (lmode .EQ. 1) then
c
        do 100 i=1,nuiln
          if (nocmnt) then
            write(luarcv,'(a)',err=9000)
     1            uinptl(locuil(i):locuil(i)+lenuil(i)-1)
          else
            write(luarcv,'(a,1x,a)',err=9000) cmtchr,
     1            uinptl(locuil(i):locuil(i)+lenuil(i)-1)
          endif
  100   continue
c
c-----------------------------------------------------------------------
c ... MODE 2: Archive user-supplied line
c-----------------------------------------------------------------------
c
      elseif (lmode .EQ. 2) then
c
        lenuln = max (1, lennb(uline))
c
        if (nocmnt) then
          write(luarcv,'(a)',err=9000) uline(1:lenuln)
        else
          write(luarcv,'(a,1x,a)',err=9000) cmtchr, uline(1:lenuln)
        endif
c
      endif
c
      return
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9000 continue
      ierr = tioerh ('TIO', 'IOARCL',2,'Write error archiving line', -2)
      arclvl = -1
      return
c
      end
