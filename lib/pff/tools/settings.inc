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
C_Groups
c***********************************************************************
c
c     Summary:  Machine-dependent settings for bldpff
c
c ----------------------------------------------------------------------
c
      logical NO_FILE_OK
c
#if defined(ABSOFTsys)
c     @(#)  ABSoft version 
c
      parameter ( NO_FILE_OK = .TRUE. )
#else
c     @(#)  Generic version
c
      parameter ( NO_FILE_OK = .FALSE. )
#endif
c