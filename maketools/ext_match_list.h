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

#ifndef Ext_Match_List_h
#define Ext_Match_List_h 1

#include <list>
#include <map>
#include <string>

class Ext_Match_List
{
 public:
  Ext_Match_List();
  void AddSearchDir(const std::string &dir);
  void AddExtension(const std::string &ext);
  void AddExcludePattern(const std::string &pat);
  int BldFileList(std::list<std::string> &filelist);
  int BldFileMap(std::map<std::string,std::string> &filemap);
  void printit();

 private:
  int fextmatch(const char *ext, const char *string);
  std::list<std::string> mat_exts;
  std::list<std::string> search_dirs;
  std::list<std::string> exclude_pattern;

};

#endif
