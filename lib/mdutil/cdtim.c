/*
-------------------------------------------------------------------------------
    Machine-dependent utilities:  cdtim.c
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

*/

#include "mdutil.h"
#include <time.h>

#ifdef __STDC__

int cdtim ( char *cdate, int *len );

int cdtim ( char *cdate, int *len )

#else

int cdtim ();

int cdtim ( cdate, len )
char  *cdate;
int   *len;

#endif

{
  int        rval;
  time_t     x;
  

  if ( ( x = time( NULL ) ) <= 0 )   return 1;
  if ( (rval = 
        strftime( cdate, *len+1, "%m/%d/%y  %H:%M:%S" , localtime(&x) )) 
      == 0 )   {
    return 2;
  }
  return 0;
}
/*
#include <stdio.h>
#define SIZE  18
void main ()
{
  char dt[SIZE+1];
  int  sz = SIZE;
  printf("rval = %d",cdtim(dt,&sz));
  printf("  \"%s\"\n",dt);
}
*/
