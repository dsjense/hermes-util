      subroutine iocmfpdef (
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
c ... Define user names for parameters in a command file.  It processes
c     the first executable line of a command file of the form:
c
c        ^cmfparam parname1 parname2 ... parname_n
c
c     The list of supplied names is matched with the values passed
c     to subroutine IOCMFPDEF, which preloaded default names '1', '2', etc.
c     Parameters can be passed by value, which is the old default.
c     This routine allows parameter values to be read/write, by passing
c     by reference with the address character (@).
c
c-----------------------------------------------------------------------
c
c ... Subroutine arguments:
c
c     OUTPUT
c       status  -  Status Flag:
c                    0   -  Normal return
c                   !=0  -  Errors
c
      integer status
c
c-----------------------------------------------------------------------
c
c ... Include Parameter and Common block decks:
c
#include "iocmdf.inc"
#include "ioctrl.inc"
#include "iopar.inc"
#include "iosymbol.inc"
c
c-----------------------------------------------------------------------
c
c ... Function calls
c
      integer    lennb, tioerh
      integer    NS_find, NS_getname, NS_putname
      integer    NS_getarrval
c
c-----------------------------------------------------------------------
c
c ... Local variables
c
      character*32  cfpnam(1:MXCMFP)*32, symnam
      character*132 cmfnam, cfpval
      character*200 errmsg
      integer       arridx, i, ierr, isym, lcfp, lsym, nch, nerr,
     &              ncfpnam, nchnam, nsynam, strhandle
c
c
c=======================================================================
c     BEGIN:
c=======================================================================
c
      status = 0
c
      if (cmflvl .EQ. 0) then
        write(errmsg,9800)
        status = tioerh ('TIO', 'IOCMFPDEF', 2, errmsg, -1)
        return
      endif
c
      nchnam = NS_getname (hcmfnam(cmflvl), cmfnam)
c
      if (ncmdf(cmflvl) .GT. 1) then
        write(errmsg,9810) cmfnam(1:nchnam)
        go to 900
      endif
c
c ... Load user-supplied command file parameter names
c
      call ioloadpar (MXCMFP, ' ', ncfpnam, cfpnam, status )
      if (status .NE. 0) go to 900
c
      if (ncfpnam .NE. ncfpar(cmflvl)) then
        write(errmsg,9820) cmfnam(1:nchnam), ncfpnam, ncfpar(cmflvl)
        go to 900
      endif
c
c ... Match up the declared names and passed parameter values. Locate 
c     LVALUE parameters, and save the information required to construct
c     local symbols at the next lower level when this file completes.
c
      nerr = 0
      lsym = loclsym(cmflvl) - 1
      lcfp = loccfp(cmflvl) - 1
c
      do i=1,ncfpar(cmflvl)
c
c ..... Replace default parameter name with supplied name
c
        call NS_freename (hlsymnam(lsym+i))
        nch = lennb(cfpnam(i))
        hlsymnam(lsym+i) = NS_putname (cfpnam(i)(1:nch))
c
c ..... If first character of passed value is the ADDRESS character,
c       get the local symbol name, and array index.
c
        nch = NS_getname (hlsymval(lsym+i), cfpval)
c
        if (cfpval(1:1) .EQ. ADRCHR) then
          ierr = 0
          call parsepar(cfpval(2:), symnam, nsynam, arridx)
c
c ....... Look up the symbol on the local list of the command file
c         level that called this file
c
          isym = NS_find ( symnam(1:nsynam),
     &           hlsymnam(loclsym(cmflvl-1)), nlsym(cmflvl-1) )
c
c ....... Name is a current local variable: for a simple scalar or
c         an entire array, copy element count and handle into new
c         variable. For a single array element, check that the
c         array index is in-bounds, and create a new scalar with
c         the handle of the array element.
c
          if (isym .GT. 0) then
            isym = isym + loclsym(cmflvl-1) - 1
c
c ......... For a simple scalar or an entire array, copy element count
c           and handle into new variable
c
            if (arridx .EQ. -1) then
              nellsym(lsym+i)  = nellsym(isym)
              hlsymval(lsym+i) = hlsymval(isym)
c
c ......... For a single array element, check that the array index is
c           in-bounds, and create a new scalar with the handle of the
c           array element.
c
            else
              if (arridx.GE.0 .AND. arridx.LT.nellsym(isym)) then
                nellsym(lsym+i) = 1
                ierr = NS_getarrval(hlsymval(isym), arridx, strhandle)
                hlsymval(lsym+i) = strhandle
              else
                write(errmsg,9840) symnam(1:nsynam)
                status = tioerh ('TIO', 'IOCMFPDEF', 2, errmsg, -1)
                ierr   = ierr + 1
              endif
            endif
c
c ....... If this name is not on the list, this is a symbol created by
c         this file. Create it as a simple scalar with an undefined
c         value to start. Note that it is an error to pass an undefined
c         symbol with an array index.
c
          else
            if (arridx .EQ. -1) then
              nellsym(lsym+i)  = 1
              hlsymval(lsym+i) = -1
            else
              write(errmsg,9830) symnam(1:nsynam)
              status = tioerh ('TIO', 'IOCMFPDEF', 2, errmsg, -1)
              ierr   = ierr + 1
            endif
          endif
c
c ....... If there are no input errors, save the symbol name and array
c         index passed to the command file, to build local symbol from
c         the command file parameter data when this file exits
c
          if (ierr .EQ. 0) then
            hcfpsnm(lcfp+i) = NS_putname(symnam(1:nsynam))
            cfpaidx(lcfp+i) = arridx
c
c ....... If there were any errors, simply increase the error count.
c         NOTE that if there are errors, nellsym = 0, so when we call
c         CMFXIT to shut down this file, it will treat this parameter
c         as if it were a simple RVALUE
c
          else
            nerr = nerr + 1
          endif
        endif
      enddo
c
      if (nerr .GT. 0) go to 900
c
      return
c
c=======================================================================
c ... ERROR conditions and messages
c=======================================================================
c
c ... Force close of command file if errors occured
c
  900 continue
      fatalcfer = .TRUE.
      status    = tioerh ('TIO', 'IOCMFPDEF', 2, errmsg, -1)
      return
c
 9800 format('"^cmfparam" command not valid outside command file')
 9810 format('"^cmfparam" command not first command of file: ',a)
 9820 format('Parameter mismatch in file: ',a,
     &       ': n_file, n_passed = ',2i4)
 9830 format('Undefined local array: ',a)
 9840 format('Out-of-bounds index for array ',a,': iel,nel = ',2i4)
c
      end
