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
c ... Input line control for TIOLIB
c
c-----------------------------------------------------------------------
c
c ... User input line types
c
c     USRLIN  -  "normal" user input line
c     CONLIN  -   User line w/ trailing continuation character
c     CMTLIN  -  Comment line (nothing preceding comment char. in line)
c
      integer    CMTLIN    , CONLIN    , USRLIN
      parameter (CMTLIN = 3, CONLIN = 2, USRLIN = 1)
c
c-----------------------------------------------------------------------
c
c ... User input line buffer
c
c     MXICHL  -  Maximum # characters in a full user input line
c                (including continuation lines and embedded comments)
c     MXUILN  -  Maximum # input lines in a single user line
c
      integer    MXICHL, MXUILN
      parameter (MXICHL = 20000)
      parameter (MXUILN = 800)
c
c     lenuil  -  (*) Non-blank length of user input lines
c     lnauil  -  (*) Non-blank length of active part of user input lines
c     locuil  -  (*) Pointer locating user input lines in UINPTL buffer
c     nuiln   -  # (sub) lines in current user input line
c     uiltyp  -  (*) Type of user input lines
c     uinptl  -  (*) Character buffer for user input lines
c
      character       uinptl*(MXICHL)
      common /inptla/ uinptl
c
      integer   nuiln
      integer   lenuil(1:MXUILN), locuil(1:MXUILN),
     1          lnauil(1:MXUILN), uiltyp(1:MXUILN)
      common /inptln/ nuiln, lenuil, locuil, lnauil, uiltyp
c
c-----------------------------------------------------------------------
c
c ... Expanded input line character buffer
c
c     MXICHR  -  Maximum # input characters in an expanded input line
c
      integer    MXICHR
      parameter (MXICHR = 20000)
c
c     cuichr  -  Pointer for current char. in expanded input line
c     lenxln  -  Non-blank length of expanded input line
c
      character       uinptc*(MXICHR)
      common /inptla/ uinptc
c
      integer   cuichr, lenxln
      common /inptln/ cuichr, lenxln
c
c-----------------------------------------------------------------------
c
c ... Last parameter read
c
c     lstpar  -  Last parameter read
c     nchlsp  -  Non-blank length of last parameter read
c
      character*132   lstpar
      common /inptla/ lstpar
c
      integer         nchlsp
      common /inptln/ nchlsp
c
