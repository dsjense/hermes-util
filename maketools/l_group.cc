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
#include <iostream>
#include <fstream>
#include <sstream>

#include "l_group.h"

using std::cout;
using std::endl;
using std::string;
using std::list;

const int L_Group::bufsiz = 256;

L_Group::L_Group()
{
  no_key_okay = false;
  key_string = "C_Groups";
  key_len = key_string.size();
}

void L_Group::printit()
{
  list<string>::iterator iter;
  cout << "mList:\n";
  for(iter=mlist.begin(); iter != mlist.end(); iter++) {
    cout << " " << (*iter) << "\n";
  }
  cout << "nList:\n";
  for(iter=nlist.begin(); iter != nlist.end(); iter++) {
    cout << " " << (*iter) << "\n";
  }
  cout << "keyString: \"" << key_string << "\"\n";
  cout << "no_key_okay: " << no_key_okay << endl;
}

void L_Group::SetKeyString(const std::string &kstring)
{
  key_string = kstring;
  key_len = key_string.size();
}

void L_Group::AddMatch(const string &mstring)
{
  mlist.push_back(" " + mstring + " ");
}

void L_Group::AddNomatch(const string &nstring, bool noKey)
{
  nlist.push_back(" " + nstring + " ");
  if (noKey) no_key_okay = true;
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
  else if (no_key_okay && mlist.empty()) return 1;
  return 0;
}

void L_Group::List(const string &filename) const
{
  string str = "";
  if ( get_grp_string(filename, str) ) cout << filename << ":" << str << endl;
}

bool L_Group::get_grp_string(const string &filename, string &str) const
{
  string blank = " ";
  str = blank;
  // open input file
  std::ifstream inp(filename.c_str(),std::ios::in);
  if ( ! inp ) return false;

  bool rval = false;
  string tstr, s;
  string::size_type idx;
  while ( getline(inp,tstr) ) {
    if ( (idx=tstr.find(key_string)) == string::npos ) continue;
    // backup 1 char in case key_string is not a WS-delimited token
    if (idx > 0) --idx;
    // make sure there isn't any whitespace at end of string!
    string::size_type last = tstr.find_last_not_of(" \t");
    string::size_type len = last - idx + 1;
    if (idx > 0 || last < tstr.size()-1) tstr = tstr.substr(idx,len);
    std::istringstream t(tstr);
    t >> s;
    if ( s != key_string ) continue; // key_string not WS-delimited
    // found good key_string, now strip off the keys
    while (!t.eof()) {
       t >> s;
       // skip if it starts  w/ either '@' or '%'
       if ( s[0] != '@' && s[0] != '%' ) str += s + blank;
    }
    rval = true;
    break;
  }
  inp.close();
  return rval;
}
