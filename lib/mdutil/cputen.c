/*
-------------------------------------------------------------------------------
    Machine-dependent utilities:  cputen.c
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
    
-------------------------------------------------------------------------------

    FORTRAN interface to PUTENV system call

*/

#ifdef __STDC__
#include <stdlib.h>
#endif

#if !defined(ABSOFTsys) && !defined(COUGARos)
# define PUTENV_AVAILABLE
#endif 

#include <string.h>
#include "mdutil.h"   /* klf 08/12/92 */

#ifdef __STDC__

int cputen(char *name, char *value);

int cputen(char *name, char *value)

#else

int cputen();

int cputen(name, value)
char  *name;
char  *value;

#endif

{
  int    retval = 0;
#if defined(PUTENV_AVAILABLE)
  char  *p     =  NULL;
  int    ln,lv;

  ln = strlen(name);
  lv = strlen(value);
  
  if ( ( p = (char *) malloc( ln + lv + 2 ) ) != NULL )   {
    strcpy(p,name);
    strcat(p,"=");
    strcat(p,value);    
  }
  else
    retval = 1;

  if ( putenv(p) != 0 )
    retval = 2;
#endif

  return retval;
}
