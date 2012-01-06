#if defined(ABSOFTsys) || defined(WIN32sys)
# define USE_FTN_UNLINK
#endif
      subroutine mddelf (file, ndel )
c
c **********************************************************************
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
c **********************************************************************
c
c     Machine-dependent routine to delete a file
c
c ----------------------------------------------------------------------
c
c     Input:
c       file    -  character variable containing the file name
c
c     Output:
c       ndel    -  # of files deleted
c
c     Internal:
c       istat   -  returned status of system calls
c       l       -  non blank length of string in "fulnam"
c
c ----------------------------------------------------------------------
c
c
      character file*(*)
      integer   ndel
c
      integer   istat
      integer   l
c
      integer lstchr
c
#if defined(USE_FTN_UNLINK)
c
      integer UNLINK
c
c ----------------------------------------------------------------------
c
      ndel = 0
c
c ... delete file
c
      l = lstchr(file)
      istat = UNLINK(file(1:l))
c
      if ( istat.eq.0 ) ndel = 1
#else
c
c *************************  Default Version  **************************
c
c     - Uses UNIX system calls to find files and delete them
c     - Supports UNIX environment variables and '~' directory specs.

c     UNIX specific internals:
c       fulnam  -  full file specification
c
      character fulnam*255
      integer   mdpath, facces, fsystm
c
c ----------------------------------------------------------------------
c
      ndel = 0
c
c ... get full file specification
c
      istat = mdpath( file, fulnam )
c
      if (istat.eq.0) then
c
c ..... does user have delete access ?
c
        istat = facces( fulnam, 'w' )
c
        if (istat.eq.0) then
c
c ....... delete file
c
          l = lstchr(fulnam)
          istat = fsystm( 'rm ' // fulnam(1:l) )
          if ( istat.eq.0 ) ndel = 1
c
        endif
c
      endif
#endif
c
      return
      end
