      integer function tioerh (
c ... INPUT
     1 pakage, module, errlvl, errmsg, retval )
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
c     TIOERH handles errors encountered by TIO.  It prints out an
c     error message of the form:
c
c       %PAKAGE-MODULE-ERROR: errmsg
c
c     If input is from other than the terminal, it also prints the
c     line number of the file from which the offending input line
c     came.
c
c-----------------------------------------------------------------------
c
c ... Function arguments:
c
c     INPUT
c       pakage  -  "package" of modules containing module issuing call
c                  if pakage = ' ', continuation of message assumed
c       module  -  Module invoking TIOERH
c       errlvl  -  Error severity level:
c                    1 - Warning
c                    2 - Error
c       errmsg  -  Error message
c       retval  -  Return value for TIOERH (typically non-zero)
c
      character*(*) pakage, module, errmsg
      integer  errlvl, retval
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "iocmdf.inc"
#include "iolun.inc"
#ifdef USE_MPI
# include "mpi_tio.inc"
#endif
c
c-----------------------------------------------------------------------
c
c ... Function calls:
c
      integer lennb
      integer NS_getname

c
c-----------------------------------------------------------------------
c
c ... Local variables:
c
      character*132 cmfnam
      integer      lpak, lmod, lmsg, lvl, nch, ntrbk
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
#ifdef USE_MPI
      if ( node.ne.0 ) goto 2
#endif
      lmsg = max (1, lennb(errmsg))
      if ( pakage.ne.' ' ) then
        lpak = max (1, lennb(pakage))
        lmod = max (1, lennb(module))
c
        if (errlvl .EQ. 1) then
          write (luerr,100) pakage(1:lpak),module(1:lmod),errmsg(1:lmsg)
        elseif (errlvl .EQ. 2) then
          write (luerr,101) pakage(1:lpak),module(1:lmod),errmsg(1:lmsg)
        endif
c
        ntrbk = 0
        do 1 lvl=cmflvl,0,-1
          if (lucmd(lvl).EQ.luttyi .OR. hcmfnam(lvl).LE.0) go to 2
          nch = NS_getname (hcmfnam(lvl), cmfnam)
c
          if (ntrbk .EQ. 0) then
            write (luerr,200) ' At', lnumc0(lvl), cmfnam(1:nch)
          else
            write (luerr,200) '...', lnumc0(lvl), cmfnam(1:nch)
          endif
          ntrbk = ntrbk + 1
    1   continue
c
      else
c        
        write (luerr,'(a)') errmsg(1:lmsg)
c
      endif
c
    2 continue
      tioerh = retval
      return
c
c-----------------------------------------------------------------------
c
  100 format (' %',a,'-',a,'-WARNING: ',a)
  101 format (' %',a,'-',a,'-ERROR: ',a)
  200 format (3x,a,' line #',i5,' in file: ',a)
c
      end
