/* C_Groups test winrtl
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
   
*/

#include "winrtl.h"
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32sys
# define STAT_STRUCT _stat
# define STAT_FUNCT _stat
# define S_ISREG(mode)	 ( (mode) & _S_IFREG )
static char pdelim = '\\';
#else
# define STAT_STRUCT stat
# define STAT_FUNCT stat
# include <unistd.h>
static char pdelim = '/';
#endif

char fullname[512];

int main(int argc, char *argv[])
{
  int err;
  DIR *dir;
  struct dirent *finfo;
  char defdir[] =".";
  char *dname = defdir;
  struct STAT_STRUCT buf;
  char *pappend;

  if ( argc > 1 ) dname = argv[1];

  dir = opendir(dname);

  strcpy(fullname,dname);
  pappend = fullname + strlen(dname);
  *(pappend++) = pdelim;
  
  finfo = readdir(dir);

  while ( finfo ) {
    strcpy(pappend,finfo->d_name);
    printf("%s",finfo->d_name);
    STAT_FUNCT(fullname, &buf);
    if ( S_ISREG(buf.st_mode) ) printf("\n");
    else printf("%c\n",pdelim);
    
    finfo = readdir(dir);
  }

  err = closedir(dir);
  if ( err ) {
    perror("dtest");
    return 1;
  }

  return 0;
}
