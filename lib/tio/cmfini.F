      subroutine cmfini (
c ... OUTPUT
     1 status)
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
c ... Initialize input from a command file:  This involves the
c     following:-
c
c     1.  Process input line:
c           * Get file name
c           * Load command file parameter values. Initially set up
c             parameter names as '1','2',etc., pass-by-value only
c     2.  Get logical unit for file
c     3.  Open the file
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     OUTPUT
c       status  -  Status Flag:
c                    0  -  Normal return (command file opened)
c                  !=0  -  Errors 
c
      integer status
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "iocmdf.inc"
#include "ioctrl.inc"
#include "iolun.inc"
#include "iopar.inc"
#include "iosymbol.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      character  chrpar*132
      integer    gtfrlu, lennb, mdotxf, tioerh
      integer    NS_putname
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character  cfpval(1:MXCMFP)*132
      character  cfpnam*4, cmdext*4, cmfnam*132, errmsg*132
      integer    arcerr, i, lstsym, lutmp, mcmfp, nch, nchnam
c
      data  cmdext / '.cmd' /
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      status = 0
c
c ... Ensure that we are not at maximum command level
c
      if (cmflvl .EQ. MXCMFL) then
        write(errmsg,9800) cmflvl
        status = tioerh ('TIO', 'CMFINI', 2, errmsg, -1)
        return
      endif
c
c-----------------------------------------------------------------------
c ... Process input
c-----------------------------------------------------------------------
c
c ... Get file name
c
      cmfnam = chrpar (RQD, 'file name', ' ', ' ', status, nchnam)
c
c ... Load command file parameter values into the temporary character
c     array CFPVAL, ensuring that we do not exceed either the command
c     file parameter or local symbol array space.
c
      lstsym = loclsym(cmflvl) + nlsym(cmflvl) - 1
      mcmfp  = min(MXCMFP-lstcfp, MXLSYM-lstsym)
c
      call ioloadpar (mcmfp, ' ', ncfpar(cmflvl+1), cfpval, status )
c
      if (status .NE. 0) then
        status = -2
        return
      endif
c
c-----------------------------------------------------------------------
c ... Open file
c-----------------------------------------------------------------------
c
c ... Get logical unit # for command file
c
      lutmp = gtfrlu (20, 90, 0)
c
      if (lutmp .LT. 0) then
        write(errmsg,9810) cmfnam(1:nchnam)
        status = tioerh ('TIO', 'CMFINI', 2, errmsg, -3)
        return
      endif
c
c ... And open it (adding default extension)
c
      status = mdotxf (.TRUE., lutmp, cmfnam, 'old', ' ', cmdext, luerr)
c
      if (status .NE. 0) then
        status = -3
        return
      endif
c
c-----------------------------------------------------------------------
c ... Complete setup for running command file
c-----------------------------------------------------------------------
c
c ... To support legacy command files, initially define command file
c     parameter names '1', '2', etc., passed by VALUE only (identified
c     by nellsym(lstsym+i) = 0). These default names can be overriden
c     by a "cmfparam name1 name2 ..." command on the FIRST line of
c     the command file (processed in subroutine CMFPDEF).
c
      do i=1,ncfpar(cmflvl+1)
        call ljusti (i, 0, cfpnam, nch, status)
        hlsymnam(lstsym+i) = NS_putname (cfpnam(1:nch))
        nch = lennb(cfpval(i))
        hlsymval(lstsym+i) = NS_putname (cfpval(i)(1:nch))
        nellsym(lstsym+i)  = 0
      enddo
c
c ... Set the command file control
c
      cmflvl = cmflvl + 1
      hcmfnam(cmflvl) = NS_putname (cmfnam(1:nchnam))
c
      lucmd(cmflvl)  = lutmp
      luin           = lucmd(cmflvl)
      lnumcf(cmflvl) = 0
      ncmdf(cmflvl)  = 0
c
      loccfp(cmflvl) = lstcfp + 1
      lstcfp         = lstcfp + ncfpar(cmflvl)
c
c ... Initialize pointer and count for local symbols at this level
c
      loclsym(cmflvl) = loclsym(cmflvl-1) + nlsym(cmflvl-1)
      nlsym(cmflvl)   = ncfpar(cmflvl)
c
      status = 0
      if(prepro) then
c
c ..... Only preprocess top-level command file
c
        if( (luin0.EQ.luttyi .AND. cmflvl.EQ.1) .OR.
     &      (luin0.NE.luttyi .AND. cmflvl.EQ.0)) then
c
c ....... Archive current command
c
          call ioarcl(1, ' ', arcerr)
c
c ....... Preprocess command file
c
          call preproc(0, status)
        endif
      endif
c
      return
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
 9800 format('Exceeded maximum # command file levels (',i2,')')
 9810 format('Unable to get free LU for command file: ',a)
c
      end
