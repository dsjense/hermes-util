//  C_Groups hermes
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

#include "file_compare.h"

#include <cstring>

using std::string;

const int File_Compare::LBSIZ = 1024;

File_Compare::File_Compare(const std::string &f1, const std::string &f2)
  : file1(f1), file2(f2), U1(0), U2(0), status(0)
{
  if ( (U1 = fopen(f1.c_str(),"r")) == 0 ||
       (U2 = fopen(f2.c_str(),"r")) == 0 ||
       (L1 = new char[LBSIZ]) == 0 || (L2 = new char[LBSIZ]) )
    status = 1;

  if ( !U1 ) fprintf(stderr,"Error opening %s\n",f1.c_str());
  if ( !U2 ) fprintf(stderr,"Error opening %s\n",f2.c_str());
  if ( !L1 || !L2 ) fprintf(stderr,"Error allocating CMP buffers (%s)\n",
                            f1.c_str());
}

File_Compare::~File_Compare()
{
  if ( U1 ) fclose(U1);
  if ( U2 ) fclose(U2);
  delete [] L1;
  delete [] L2;
}

bool File_Compare::TheSame()
{
  bool have1 = false, have2 = false;
  char *p1, *p2;

  while (true) {
    have1 = have2 = false;
    while ( !have1 ) {
      p1 = fgets(L1, LBSIZ, U1);
      if ( p1 == 0 ) break;
      int loc = strspn(L1," \t");
      if ( L1[loc] != '\n' && L1[loc] != '#' ) have1 = true;
    }
    while ( !have2 ) {
      p2 = fgets(L2, LBSIZ, U2);
      if ( p2 == 0 ) break;
      int loc = strspn(L2," \t");
      if ( L2[loc] != '\n' && L2[loc] != '#' ) have2 = true;
    }
    if ( have1 && have2 ) {
      if ( strcmp(L1,L2) ) return false;
    }
    else if ( !have1 && !have2 ) return true;
    else return false;
  }
}
