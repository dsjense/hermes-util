/*
-------------------------------------------------------------------------------
    Machine-dependent utilities:  cvhelp.c
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

    FORTRAN interface to VHELP VAX help emulator package (DBS)

*/

#ifdef __STDC__
#include <stdlib.h>
#endif

#include <string.h>
#include "mdutil.h"       /* klf 08/12/92 */

#ifdef __STDC__

int vhelp  ( char *fname, char *first, int screen );
int cvhelp (char *file, char *string, int *lines );

int cvhelp (char *file, char *string, int *lines )

#else

int vhelp();
int cvhelp();

int cvhelp(file, string, lines )
char *file;
char *string;
int *lines;

#endif

{
  return vhelp ( file, string, *lines );
}
