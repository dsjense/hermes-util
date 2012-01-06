      subroutine subcom (
c ... INPUT
     &  ncmd, cmdnam,
c ... OUTPUT
     &  comand, status)
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
c     SUBCOM processes subcommands by prompting with the list of
c     subcommands (if needed), reading the subcommand, and checking
c     against the list of subcommands.
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     INPUT:
c       ncmd   -  # subcommands recognized by calling program
c       cmdnam -  (*) Names of valid subcommands
c
c     OUTPUT:
c       comand  -  subcommand name
c       status  -  Status flag:-
c                     0  -  Normal return
c                    -1  -  No subcommand read
c                    -2  -  Syntax error in line
c                    -3  -  Error building prompt line
c                    -4  -  Unknown/ambiguous command
c
      integer        ncmd, status
      character*(*)  cmdnam(1:*)
      character*(*)  comand
c
c-----------------------------------------------------------------------
c
c ... Include PARAMETER and common block decks:
c
#include "iopar.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      character chrpar*132
      integer   lennb, lstmch, tioerh
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      integer   PLLEN
      parameter (PLLEN = 132)
      character errmsg*132, pline*(PLLEN)
      integer   cmdndx, i, lb, le, ln, nchcmd
c
c
      status = 0
c
c ... Construct prompt line from subcommand list
c
      lb = 1
      do 10 i=1, ncmd-1
        ln = lennb(cmdnam(i))
        le = lb + ln
        if(le .le. PLLEN) then
          pline(lb:) = cmdnam(i)(1:ln)//' '
        else
          status = tioerh ('TIO', 'SUBCOM', 2,
     1               'Too many application subcommands', -3)
          return
        endif
        lb = le + 1
   10 continue
c
      ln = lennb(cmdnam(ncmd))
      le = lb + ln + 4
      if(le .le. PLLEN) then
        pline(lb:) = 'or '//cmdnam(ncmd)(1:ln)//'?'
      else
        status = tioerh ('TIO', 'SUBCOM', 2,
     1             'Too many application subcommands', -3)
        return
      endif
c
c ... Get next parameter in input line, and see if it is a subcommand
c
      comand = chrpar (RQD, pline, ' ', ' ', status, nchcmd)
      if(status .ne. 0) return
c
c ... Check for a valid subcommand
c
      cmdndx = lstmch (comand, cmdnam, ncmd)
c
      if(cmdndx .gt. 0) then
        return
c
c ..... Ambiguous command
c
      else if(cmdndx .eq. -2) then
        ln = min(lennb(comand),
     &           len(errmsg) - len(' Ambiguous command: '))
        write(errmsg,2000) ' Ambiguous command: ', comand(1:ln)
        status = tioerh ('TIO', 'SUBCOM', 2, errmsg, -4)
        return
c
c ..... Unknown command
c
      else
        ln = min(lennb(comand),len(errmsg) - len(' Unknown command: '))
        write(errmsg,2000) ' Unknown command: ', comand(1:ln)
        status = tioerh ('TIO', 'SUBCOM', 2, errmsg, -4)
        return
      endif
c
 2000 format(2a)
c
      end
