/*
-------------------------------------------------------------------------------
    Machine-dependent utilities:  cacces.c
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

    FORTRAN interface to ACCESS system call

*/

#ifdef __STDC__
# include <stdlib.h>
#endif

#if !defined(ABSOFTsys)
# define ACCESS_AVAILABLE
#endif 

#if defined(ACCESS_AVAILABLE)
# if defined(WIN32sys)
#  include <io.h>
#  define R_OK    4       /* Test for Read permission */
#  define W_OK    2       /* Test for Write permission */
#  define X_OK    0       /* Test for eXecute permission (not on NT !!) */
#  define F_OK    0       /* Test for existence of File */
#  define access _access
# else
#  include <unistd.h>
# endif
# include <string.h>
#endif
#include "mdutil.h" /* klf 08/12/92 */

#ifdef __STDC__

int cacces(char *path, char *amode);

int cacces(char *path, char *amode)

#else

int cacces();

int cacces(path,amode)

char  *path;
char  *amode;

#endif

{
#if defined(ACCESS_AVAILABLE)
  int    mode = F_OK;

  if ( strchr( amode, 'r' ) != NULL ) mode += R_OK;
  if ( strchr( amode, 'w' ) != NULL ) mode += W_OK;
  if ( strchr( amode, 'x' ) != NULL ) mode += X_OK;
  
  return access(path,mode);
#else
  return 0;
#endif
}
