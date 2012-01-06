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
c ... Function type statements for all TIOLIB function calls
c
c       adflxt  -  Add default file extension
c       chrpar  -  Return next parameter as CHARACTER
c       chrtod  -  Convert alphanumeric string to DOUBLE PRECISION
c       chrtoi  -  Convert alphanumeric string to INTEGER
c       chrtor  -  Convert alphanumeric string to REAL
c       dblpar  -  Return next parameter as DOUBLE PRECISION
c       endofl  -  .TRUE. if at end of current line, .FALSE. otherwise
c       gtfrlu  -  Get free logical unit #
c       intpar  -  Return next parameter as INTEGER
c       lennb   -  Return non-blank length of a string
c       ljusti  -  Return left-justified integer character string
c       lookup  -  Match a string exactly against a list of strings
c       lstmch  -  Match a string against a list of strings without
c                  regard to case, and string length
c       nxttyp  -  Identify next parameter TYPE
c       partyp  -  Identify current parameter TYPE
c       reapar  -  Return next parameter as REAL
c       strmch  -  Compare two strings without regard to case and
c                  string length
c       tioini  -  Initialize TIOLIB
c
      character*80     adflxt, chrpar
      character*16     ljusti
      double precision chrtod, dblpar
      integer          chrtoi, gtfrlu, intpar, lennb, lookup, lstmch,
     1                 nxttyp, partyp
      logical          endofl, strmch
      real             chrtor, reapar
c
c-----------------------------------------------------------------------
