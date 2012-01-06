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
c ... Common block for handling command files in TIOLIB
c
c-----------------------------------------------------------------------
c
c ... Command files
c
c     MXCMFL  -  Maximum nest level for command files
c     MXCMFP  -  Maximum # command file parameters, over all levels
c
      integer     MXCMFL, MXCMFP
      parameter  (MXCMFL =   4)
      parameter  (MXCMFP = 100)
c
c     cmflvl    -  Current nest level of command file
c     fatalcfer -  Flag indicating a fatal command file error occurred,
c                  forcing the current level to be closed.
c     lstcfp    -  Pointer to last-used command file parameter 
c
c     hcmfnam -  (*) Handles to command file names
c     lucmd   -  (*) Logical unit # for each command file
c     lnumcf  -  (*) Line number in active input of current line
c     lnumc0  -  (*) Line number in active input of first line in command
c     ncmdf   -  (*) Counter for # of commands processed in each file
c
c     ncfpar  -  (*) # command file parameters for each level
c     loccfp  -  (*) Pointer locating command file parameters for
c                    each command file level
c     hcfpsnm -  (*) Handles to local symbol names for LVALUE command
c                    file parameters
c     cfpaidx -  (*) Array indices of LVALUE command file parameters
c
      integer  cmflvl, lstcfp
      integer  lucmd(0:MXCMFL)  , hcmfnam(0:MXCMFL)
      integer  lnumcf(0:MXCMFL) , lnumc0(0:MXCMFL), ncmdf(0:MXCMFL)
      integer  loccfp(1:MXCMFL) , ncfpar(0:MXCMFL)
      integer  cfpaidx(1:MXCMFP), hcfpsnm(1:MXCMFP)
      logical  fatalcfer
c
      common /iocmdf/ cmflvl, lstcfp, fatalcfer,
     &                lucmd , hcmfnam, lnumcf, lnumc0,
     &                ncmdf , loccfp, ncfpar, cfpaidx, hcfpsnm
c
c-----------------------------------------------------------------------
