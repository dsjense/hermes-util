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
c ... Common block for handling FOR loops in TIOLIB
c
c     Externally defined parameters used in this module:
c       iocmdf.inc: MXCMFL
c
c-----------------------------------------------------------------------
c
c     MXLPCH  -  # characters in loop command line buffer
c     MXLPLN  -  # lines in a loop
c     MXLPLV  -  Maximum # nested loop levels for each command file
c
      integer    MXLPCH, MXLPLN, MXLPLV
      parameter (MXLPCH = 20000)
      parameter (MXLPLN = 500)
      parameter (MXLPLV = 4)
c
c     FORLOOPVAR  - Variable type index for FOR loop variable
c
      integer    FORLOOPVAR
      parameter (FORLOOPVAR = 3)
c
c ... Loop line storage management
c
c     beglpl  -  (*,*) Beginning line of loop in buffer
c     endlpl  -  (*,*) Ending line of loop in buffer
c     curlpl  -  (*,*) Current line in of loop in buffer
c     lplbuf  -  (*) FOR loop command line buffer
c     lplchb  -  (*) Beginning character of each loop line in buffer
c     lplche  -  (*) Ending character of each loop line in buffer (note
c                that to simplify code, we define lplche(0) = 0)
c     lstlch  -  Last character in use in loop line buffer
c     nlpln   -  # loop lines currently stored in buffer
c
      character*(MXLPCH) lplbuf
      common /iolpa/ lplbuf
c
      integer lstlch, nlpln
      integer lplchb(1:MXLPLN), lplche(0:MXLPLN)
      integer beglpl(1:MXLPLV,0:MXCMFL), endlpl(1:MXLPLV,0:MXCMFL) ,
     1        curlpl(1:MXLPLV,0:MXCMFL)
      common /iolpn/ lstlch, nlpln, lplchb, lplche,
     1               beglpl, endlpl, curlpl
c
c ... Control of file line numbers for loop lines
c
c     lnumcx  -  (*) Line # of the "^endfor" line of outer loop
c     lplnmf  -  (*) Line # in source file of each loop line
c
      integer lnumcx(0:MXCMFL)
      integer lplnmf(1:MXLPLN)
      common /iolpn/ lnumcx, lplnmf
c
c ... Control for loop iteration variable control
c
c     looplv  -  (*) Loop level at each command file level
c     lpstep  -  (*,*) FOR loop variable step value
c     loopvn  -  (*,*) FOR loop variable name
c     loopv   -  (*,*) Current FOR loop variable value (numeric)
c     loopvc  -  (*,*) Current FOR loop variable value (character string)
c     nchlpv  -  (*,*) # characters in LOOPVC
c     nlpit   -  (*,*) FOR loop iteration count
c
      character*32  loopvc(1:MXLPLV,0:MXCMFL),
     1              loopvn(1:MXLPLV,0:MXCMFL)
      common /iolpa/ loopvc, loopvn
c
      integer  looplv(0:MXCMFL)
      integer  lpstep(1:MXLPLV,0:MXCMFL), loopv(1:MXLPLV,0:MXCMFL),
     2         nchlpv(1:MXLPLV,0:MXCMFL) , nlpit(1:MXLPLV,0:MXCMFL)
      common /iolpn/ looplv, lpstep, loopv, nchlpv, nlpit
c
c-----------------------------------------------------------------------
