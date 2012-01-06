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
c ----------------------------------------------------------------------
c
c   Summary:
c
c     Include file error reporting information for PFF modules.  In
c     VERBOSE mode, modules write a message if they encounter an error
c     or if they are called with the error flag set (non-zero).  In DEBUG
c     mode, they write an entry status and exit status message in
c     addition to the VERBOSE mode messages.  Modes are set by setting
c     the respective output unit #s to a positive integer value.
c
c ----------------------------------------------------------------------
c
c   Parameters needed in this module, but not supplied:
c
c     -  CHRFIL
c
c   Include file needed to supply these parameters:
c
c     -  pfbsiz.inc
c
c ----------------------------------------------------------------------
c
c   Explanation of the common-block parameters:
c
c     OFFLUN  -  value indicating that Verbose or Debug modes are off.
c
c   Explanation of the common-block variables:
c
c     errfid  -  PFF File ID of file associated with last reported error
c     errfil  -  file name associated with last reported error
c     errloc  -  buffer loc. of file associated with last reported error
c     errlun  -  Unit # of file associated with last reported error
c     errmod  -  module name of last reported error
c     errnum  -  # of last reported error
c     errrec  -  record # of file associated with last reported error
c     errsta  -  status of file associated with last reported error
c     ludbg   -  if .gt. 0, debug output unit #
c     luverb  -  if .gt. 0, verbose output unit #
c     vrbflg  -  flag indicating that VERBOSE output goes to one or both
c                of the error output files (VERBOSE & DEBUG)
c     v2dflg  -  flag indicating that VERBOSE output should go to the
c                DEBUG file
c
c ----------------------------------------------------------------------
c
c   Declare and define the parameters:
c
      integer   OFFLUN
      parameter ( OFFLUN = -100 )
c
c   Declare variables and define the common block:
c
      integer   errfid, errloc, errlun, errnum, errrec, errsta
      integer   ludbg, luverb
      logical   vrbflg, v2dflg
      character errfil*(CHRFIL), errmod*6
c
      common /pferrs/ errfid, errloc, errlun, errnum, errrec, errsta,
     &                ludbg,  luverb, vrbflg, v2dflg
c
      common /pferrc/ errfil, errmod
c
c***********************************************************************
