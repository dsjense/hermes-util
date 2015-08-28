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

#include <iostream>
#ifdef WIN32sys
# include "winrtl.h"
#else
# include <unistd.h>
#endif
#include "l_group.h"

using std::string;
using std::list;
using std::cout;
using std::cerr;
using std::endl;

#include "lgrp_usage.h"

int main(int argc, char *argv[])
{
  int c;
  L_Group lgrp;
  int error = 0;
  bool check = false;

  while ( (c=getopt(argc, argv, "hm:n:N:k:")) != -1 ) {
    switch (c) {
    case 'h':
      lgrp_usage(cout,0);
      std::exit(0);
      break;
    case 'm':
      lgrp.AddMatch(optarg);
      check = true;
      break;

    case 'n': case 'N':
      if (c == 'N') lgrp.AddNomatch(optarg,true);
      else lgrp.AddNomatch(optarg);
      check = true;
      break;
 
    case 'k':
      lgrp.SetKeyString(optarg);
      break;

    case '?':
      error = 1;
      break;

    default:
      error = 1;
      cerr << "?? getopt returned character code 0" << std::oct << c 
           << std::dec << " ??" << endl;
    }    
  }

  if ( error ) {
    lgrp_usage(cerr,1);
    std::exit(1);
  }

  for(int i=optind; i<argc; ++i) {
    if ( check ) {
      if ( lgrp.CheckFile(string(argv[i])) ) cout << argv[i] << endl;
    }
    else lgrp.List(string(argv[i]));
  }

  return 0;
}
