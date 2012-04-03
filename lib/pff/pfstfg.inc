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
c     Include file containing parameters for PFF's opcodes, file status
c     flags, and "special-meaning" dataset flags.
c
c ----------------------------------------------------------------------
c
c   Explanation of the parameters:
c
c     RE      -  READ opcode;  "open-for-readonly" file status
c     WR      -  WRITE opcode; "open-for-write" file status
c     RW      -  "open-for-read/write" file status
c     NOPEN   -  "not-open" file status
c     DFAULT  -  default data flag
c     EOFFLG  -  End-of-File flag
c     DFRAME  -  Dataset framing word
c     FFRAME  -  File Header framing word
c     NULFLG  -  NULL Region indicator flag
c     FP_FULL -  Array precision type: full precision on <FARRAY>
c     FP_REDU -  Array precision type: reduced precision on <FARRAY>
c     FP_ALLFULL - Dataset precision type: full precision on all
c                  <FARRAY>s in a dataset, ordinates and other data
c     FP_ORDFULL - Dataset precision type: full precision on ordinate
c                  <FARRAY>s, but reduced precision on other <FARRAY>s
c     INTP_4  -  indicator for 4-byte precision on <IARRAY>
c     INTP_2  -  indicator for 2-byte precision on <IARRAY>
c
c ----------------------------------------------------------------------
c
c   Declare and set the parameters:
c
      integer   RE, WR, RW, NOPEN
      integer   DFAULT, EOFFLG, DFRAME, FFRAME, NULFLG
      integer   FP_FULL, FP_REDU, INTP_4, INTP_2
      integer   FP_ALLFULL, FP_ORDFULL
c
      parameter ( RE = 0 , WR = 1 , RW = 2 , NOPEN = -1 )
      parameter ( DFRAME = -1 , EOFFLG = -2 , DFAULT = -3 )
      parameter ( FFRAME = -4 , NULFLG = -5 )
      parameter ( FP_FULL = -6 , FP_REDU = DFAULT )
      parameter ( FP_ALLFULL = FP_FULL , FP_ORDFULL = -7 )
      parameter ( INTP_4 = -6 , INTP_2 = DFAULT )
c
c***********************************************************************
