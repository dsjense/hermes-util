//  C_Groups
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

#include <cstdio>
#ifdef WIN32sys
# include "winrtl.h"
#else
# include <unistd.h>
#endif
#include "l_group.h"

using std::string;
using std::list;
using std::printf;
using std::fprintf;

int main(int argc, char *argv[])
{
  int c;
  L_Group lgrp;
  int error = 0;
  bool check = false;

  while ( (c=getopt(argc, argv, "m:n:")) != -1 ) {
    switch (c) {
    case 'm':
      lgrp.AddMatch(optarg);
      check = true;
      // printf ("option m %s\n", optarg);
      break;

    case 'n':
      lgrp.AddNomatch(optarg);
      check = true;
      // printf ("option n %s\n", optarg);
      break;
 
    case '?':
      error = 1;
      break;

    default:
      error = 1;
      fprintf (stderr,"?? getopt returned character code 0%o ??\n", c);
    }    
  }

  if ( error ) {
    fprintf(stderr,
            "Usage: %s [-m match ...] [-n nomatch ...] filelist\n",argv[0]);
    return 1;
  }

  for(int i=optind; i<argc; ++i) {
    if ( check ) {
      if ( lgrp.CheckFile(string(argv[i])) ) printf("%s\n", argv[i]);
    }
    else lgrp.List(string(argv[i]));
  }

  return 0;
}
