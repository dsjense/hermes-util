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

#ifndef File_Compare_h
#define File_Compare_h 1

#include <cstdio>
#include <string>

class File_Compare {
 public:
  File_Compare(const std::string &f1, const std::string &f2);
  ~File_Compare();
  bool TheSame();
  int Status() { return status; }

 private:
  static const int LBSIZ;
  std::string file1, file2;
  FILE *U1, *U2;
  int status;
  char *L1, *L2;
};

#endif
