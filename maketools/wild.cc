//  C_Groups test
//  $Id$
//  
//  Copyright (2008) Sandia Corporation. Under the terms of
//  Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
//  Government retains certain rights in this software.
//  
//  Hermes is free software: you can redistribute it and/or modify
//  it under the terms of the GNU Lesser General Public License as
//  published by the Free Software Foundation, either version 3 of
//  the License, or (at your option) any later version.
//  
//  Hermes is distributed in the hope that it will be useful, but
//  WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Lesser General Public License for more details.
//  
//  You should have received a copy of the GNU Lesser General
//  Public License along with Hermes.  If not, see
//  <http://www.gnu.org/licenses/>.
//  

#include "ext_match_list.h"

#include <unistd.h>
#include <cstdio>

using std::string;
using std::list;

int main(int argc, char *argv[])
{
  int c;
  int error = 0;
  bool found = false;

  Ext_Match_List matcher;

  while ( (c=getopt(argc, argv, "s:")) != -1 ) {
    switch (c) {
    case 's':
      matcher.AddExtension(optarg);
      found = true;
      break;

    case '?':
      error = 1;
      break;

    default:
      error = 1;
      fprintf (stderr,"?? getopt returned character code 0%o ??\n", c);
    }    
  }

  if ( error || argc != optind ) {
    fprintf(stderr, "Usage: %s [-s extension ...]\n",argv[0]);
    return 1;
  }

  list<string> flist;
  int n = matcher.BldFileList(flist);
  printf("%d files found\n",n);
  list<string>::iterator iter;
  for(iter=flist.begin(); iter != flist.end(); iter++)
    printf("%s\n", (*iter).c_str());

  return 0;
}
