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
c     Define PARAMETERS used in TIOLIB user interface
c
c-----------------------------------------------------------------------
c
c ... TIOLIB command parameter types
c
c       INT    -  Integer
c       REA    -  REAL number
c       DUBL   -  DOUBLE precision floating #
c       ALFA   -  Alphanumeric string
c       NUL    -  Null parameter
c       ENDLN  -  END-OF-LINE
c
      integer    INT, REA, DUBL, ALFA, NUL, ENDLN
      parameter (INT = 1, REA = 2, DUBL = 3, ALFA = 4,
     1           NUL = 5, ENDLN = 6 )
c
c ... command parameter status
c
c       RQD     -  Required
c       OPT     -  Optional
c
      integer     RQD    , OPT
      parameter  (RQD = 1, OPT = 2)
c
c-----------------------------------------------------------------------
c
c ... Exception return values for CMDINI's status flag
c
c       IOEOF    -  END-OF-FILE on input at command file level 0
c       IOEXIT   -  EXIT (exit command entered)
c       IOINTERR -  NON-fatal TIO error occurred processing command
c                   (e.g. undefined symbol)
c       IOBADCMD -  Bad command: either undefined or ambiguous
c       IOFTLERR -  FATAL error processing input
c
c ... NOTE that IOINTERR and IOBADCMD are only returned to the calling
c     routine in non-interactive mode.
c
      integer     IOEOF   , IOEXIT   , IOINTERR   , IOBADCMD
      parameter  (IOEOF=-1, IOEXIT=-2, IOINTERR=-3, IOBADCMD=-4 )
c
      integer     IOFTLERR
      parameter  (IOFTLERR=-9)
c
c-----------------------------------------------------------------------
c
c ... TIO symbol types
c
c       LOCALSYM  - Local  symbol
c       GLOBALSYM - Global symbol
c
      integer    LOCALSYM, GLOBALSYM
      parameter (LOCALSYM  = 1)
      parameter (GLOBALSYM = 2)
c
c-----------------------------------------------------------------------
