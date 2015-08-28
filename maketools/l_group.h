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

#ifndef L_Group_h
#define L_Group_h 1

#include <list>
#include <string>

class L_Group
{
 public:
  L_Group();
  void AddMatch(const std::string &mstring);
  void AddNomatch(const std::string &nstring, bool noKey=false);
  void SetKeyString(const std::string &kstring);
  int CheckFile(const std::string &filename) const; 
  void List(const std::string &filename) const;
  void printit();
  
 private:
  bool get_grp_string(const std::string &filename, std::string &str) const;
  std::list<std::string> mlist;
  std::list<std::string> nlist;
  const static int bufsiz;
  bool no_key_okay;
  std::string key_string;
  int key_len;
};

#endif
