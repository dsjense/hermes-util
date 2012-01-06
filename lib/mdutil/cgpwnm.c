/*
-------------------------------------------------------------------------------
    Machine-dependent utilities:  cgpwnm.c
-------------------------------------------------------------------------------
    $Id$
    
    Copyright (2008) Sandia Corporation. Under the terms of
    Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
    Government retains certain rights in this software.
    
    Hermes is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of
    the License, or (at your option) any later version.
    
    Hermes is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General
    Public License along with Hermes.  If not, see
    <http://www.gnu.org/licenses/>.
    
C_Groups @(#)
-------------------------------------------------------------------------------

    FORTRAN interface to find home directory associated with a user; uses the 
    GETPWNAM system call

*/

#if !defined(ABSOFTsys) && !defined(WIN32sys)
# define GETPWNAM_AVAILABLE
#endif
#ifdef __STDC__
# include <stdlib.h>
#endif

#if defined(AIXsys) || defined(LINUX_X86sys) || defined(LINUX_IA64sys)
# include <sys/types.h>
#endif
#ifdef GETPWNAM_AVAILABLE
# include <pwd.h>
#endif
#include <string.h>
#include "mdutil.h" /* klf 08/12/92 */

#ifdef __STDC__

int cgpwnm(char *name, char *dir, int *vmax);

int cgpwnm(char *name, char *dir, int *vmax)

#else

int cgpwnm();

int cgpwnm(name, dir, vmax)

char  *name;
char  *dir;
int   *vmax;

#endif

{
  int    retval = 0;

#if defined(GETPWNAM_AVAILABLE)
  struct passwd *pw;

  pw = getpwnam(name);

  if ( pw != NULL )   {
    if ( strlen(pw->pw_dir)  >=  *vmax )   {
      retval = 1;
      strncpy(dir,pw->pw_dir,(*vmax)-1);
      dir[(*vmax)-1] = '\0';
    }
    else   {
      strcpy(dir,pw->pw_dir);
    }
  }
  else  {
    retval = 1;
    dir[0] = '\0';

  }
#else
  /* Not Implemented */
  retval = 1;
  dir[0] = '\0';
#endif

  return retval;
}
