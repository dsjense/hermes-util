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

#include <algorithm>
#include <cstdio>
#include <cstring>
#include <cstdlib>

#include "l_group.h"

using std::string;
using std::list;

const int L_Group::bufsiz = 256;

void L_Group::AddMatch(const string &mstring)
{
  mlist.push_back(" " + mstring + " ");
}

void L_Group::AddNomatch(const string &nstring)
{
  nlist.push_back(" " + nstring + " ");
}

int L_Group::CheckFile(const string &filename) const
{
  string str;
  if ( get_grp_string(filename, str) ) {
    list<string>::const_iterator iter;

    for(iter=mlist.begin(); iter != mlist.end(); iter++) {
      if ( str.find(*iter) == string::npos ) return 0;
    }
    for(iter=nlist.begin(); iter != nlist.end(); iter++) {
      if ( str.find(*iter) != string::npos ) return 0;
    }
    return 1;
  }
  return 0;
}

void L_Group::List(const string &filename) const
{
  string str = "";
  if ( get_grp_string(filename, str) ) { 
    printf("%s:%s\n",filename.c_str(), str.c_str());
  }
}

bool L_Group::get_grp_string(const string &filename, string &str) const
{
  char line[bufsiz];
  FILE *file = fopen(filename.c_str(), "r");
  if ( ! file ) return false;
  string blank = " ";

  char *mat = 0;
  str = blank;
  while ( fgets(line, bufsiz, file) ) {
    mat = strstr(line,"C_Groups");
    if ( mat ) {
      mat += 8;
      if ( mat[0] == '\n' ) break;      
      int l = strlen(mat);
      if ( l > 0 )mat[l-1] = ' ';
      char *tok = strtok(mat," \t\r");
      if ( ! tok ) break;
      if ( tok == mat ) continue;
      do {
        if ( ! strchr("@%",tok[0]) ) str += string(tok) + blank;
        tok = strtok(NULL," \t\r");
      }
      while ( tok );
      break;
    }
  }
  fclose(file);
  if ( mat ) return true;
  return false;
}
