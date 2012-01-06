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
#include <stdlib.h>
#include <string.h>

int main ( int argc, char *argv[])
{
  int i,c, cnt=0;

  char ostr[] = ":abcqQA:B:C:";
  int off = 1;

  while ( (c=getopt(argc, argv, ostr+off )) != -1 ) {
    switch (c) {
    case 'a':
    case 'b':
    case 'c':
    case '?':
      printf("%c %c\n",c,optopt);
      break;
    case 'q':
      printf("%c %c\n",c,optopt);
      off = 0;
      break;
    case 'Q':
      printf("%c %c\n",c,optopt);
      opterr = 0;
      break;
    case 'A':
    case 'B':
    case 'C':
      printf("%c %c %s\n",c,optopt,optarg);
      break;
    default:
      printf("other: %c %c\n",c,optopt);
      break;
    }
  }

  printf("optind, argc: %d %d\n",optind,argc);
  argc -= optind;
  argv += optind;
  for(i=0;i<argc;++i) printf("ARG%d: %s\n",i,argv[i]);

  return 0;
}
