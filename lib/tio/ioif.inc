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
c ... Common block controlling TIO if-processing.
c
c        MXIFLV - Maximum nest level of IF statements
c        iflvl  - (*) IF nest level at current command file level
c        ifstat - (*,*) If-processing status for each level at each
c                 command file level :-
c                      1 - Accept lines: Block being processed
c                      2 - Reject input lines: Block not yet processed
c                      3 - Reject input lines: Block processed - drop
c                                              through to 'endif'
c
      integer MXIFLV
      parameter (MXIFLV = 4)
c
      integer iflvl(0:MXCMFL)
      integer ifstat(1:MXIFLV,0:MXCMFL)
c
      common /ioif/ iflvl, ifstat
c
