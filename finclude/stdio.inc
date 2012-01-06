c
c***********************************************************************
c   $Id$
c   
c   Copyright (2008) Sandia Corporation. Under the terms of
c   Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
c   Government retains certain rights in this software.
c   
c   Hermes is free software: you can redistribute it and/or modify
c   it under the terms of the GNU Lesser General Public License as
c   published by the Free Software Foundation, either version 3 of
c   the License, or (at your option) any later version.
c   
c   Hermes is distributed in the hope that it will be useful, but
c   WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU Lesser General Public License for more details.
c   
c   You should have received a copy of the GNU Lesser General
c   Public License along with Hermes.  If not, see
c   <http://www.gnu.org/licenses/>.
c   
c***********************************************************************
c
c     Summary:  Machine-dependent include file for assigning STDIN,
c               STDOUT, & STDERR and making sure that inquire will find
c               STDERR open.
c
      stdin =  5
      stdout = 6
c
#if defined (HPsys)
c     @(#)   HPUX Version
c
      stderr = 7
c
#elif defined (ABSOFTsys)
c     @(#)   ABSoft MAC Version
c
      stderr = 6
c
#else
c     @(#)   Default common version
c
      stderr = 0
c
#endif

#if defined (CRAsys) || defined(DECOSFsys)
c     For some systems a Fortran "inquire" statement testing whether or not
c     stderr is open will not return true unless the file as been written to.
c     For such systems, implicitly open STDERR by sending it a NULL character
c        UNICOS 6.1 (and later)
c        DECOSF F90
c
      write(stderr,'(a,$)') char(0)
#endif
