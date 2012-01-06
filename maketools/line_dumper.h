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

#ifndef Line_Dumper_h
#define Line_Dumper_h 1

#include <cstdio>
#include <string>

class Line_Dumper
{
 public:
  Line_Dumper(FILE *f, int width=78);
  ~Line_Dumper();
  void Start_String(const std::string &start);
  void Cont_String(const std::string &cont);
  void Reset(int extralines=0);
  void SendPhrase(const std::string &phrase);

 private:
  FILE* file;
  int lmax;
  int cnt;
  int slen;
  int csav;
  bool is_cont;
  char *contstr;
  char *startstr;
  char *buff;
};

#endif
