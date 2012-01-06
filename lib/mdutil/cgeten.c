/*
-------------------------------------------------------------------------------
    Machine-dependent utilities:  cgeten.c
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

    FORTRAN interface to GETENV system call

*/

#ifdef __STDC__
#include <stdlib.h>
#endif

#if defined(ABSOFTsys) || defined(COUGARos)
# define GETENV_NOT_AVAILABLE
#else
# define GETENV_AVAILABLE
#endif 

#include <string.h>
#include "mdutil.h" /* klf 08/12/92 */

#ifdef __STDC__

int cgeten(char *name, char *value, int *vmax);

int cgeten(char *name, char *value, int *vmax)

#else

int cgeten();

int cgeten(name, value, vmax)
char  *name;
char  *value;
int   *vmax;

#endif

{
  int    retval = 0;
  char	*ival  =  NULL;

#if defined(GETENV_AVAILABLE)
  ival = getenv(name);
#endif

  if ( ival != NULL )   {
    if ( (int) strlen(ival)  >=  *vmax )   {
      retval = 1;
      strncpy(value,ival,(*vmax)-1);
      value[(*vmax)-1] = '\0';
    }
    else
      strcpy(value,ival);
  }
  else  {
    retval = 1;
    value[0] = '\0';
  }

  return retval;
}
