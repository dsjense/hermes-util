      function pfsfpr ( fid, prec, ierr )
      integer  pfsfpr
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
c       - This routine is a SET routine to set PFF the current precision 
c         for the writing of <FARRAY> data primitives to the specified 
c         PFF file.
c       - IF the file ID (fid) is zero, PFF's global precision state is 
c         set. This global state controls the default state of a file 
c         when it is opened. The default for this global state is FP_REDU.
c
c     Input:
c       fid   -  File ID of PFF file, if zero global default state for
c                opening new files is set to supplied value.
c       prec  -  desired precision. Legal values (see pfstfg.inc) are:
c                  FP_REDU    - reduced precision. Floating point array
c                               is linearly mapped to a 2-byte integer.
c                  FP_ALLFULL - all <FARRAY>s at full (4-byte) precision
c                  FP_ORDFULL - ordinate <FARRAY>s at full (4-byte)
c                               precision, other data at reduced prec.
c       ierr  -  if not zero, return with no operation
c
c     Output:
c       ierr  -  error status flag;
c                    0,   No error
c                    1,   Illegal File ID (FID)
c                    2,   File NOT Open
c                    3,   Illegal precision value
c
c     Return Value:
c       pfsfpr returns the previous precision value
c
c ----------------------------------------------------------------------
c
c     Internals:  NONE
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz: MXFILS
c     pfstfg: FP_REDU, FP_ALLFULL, FP_ORDFULL, NOPEN
c
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c ... pfbufm
c       r:    wrflag(*)
c ... pffils
c       r/w:  fpprec(*)
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
      integer   fid, prec, ierr
c
c     Internal
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFSFPR' )
c
c ... External Block Data
      external  pffbd
c
c ----------------------------------------------------------------------
c
c
      pfsfpr = 0
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
c ... is FID legal ?
c
      if( fid.lt.0 .OR. fid.gt.MXFILS )then
c
        ierr = 1
        call pfserr ( PFMODU, ierr, fid, 'Illegal File ID (FID)' )
        goto 999
c
      endif
c
      pfsfpr = fpprec(fid)
c
c ... is requested precision legal value ?
c
      if( prec.ne.FP_REDU .AND. prec.ne.FP_ALLFULL
     &   .AND. prec.ne.FP_ORDFULL )then
c
        ierr = 3
        call pfserr ( PFMODU, ierr, fid, 'Illegal precision value' )
        goto 999
c
      endif
c
c ... if a file, is it open ?
c
      if(fid.gt.0 .AND. wrflag(fid).eq.NOPEN)then
c
        ierr = 2
        call pfserr ( PFMODU, ierr, fid, 'File NOT Open' )
c
      else
c
        fpprec(fid) = prec
c
      endif
c
c ----------------------------------------------------------------------
c
  999 continue
C
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
