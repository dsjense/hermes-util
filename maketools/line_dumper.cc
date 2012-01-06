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

#include "line_dumper.h"

using std::string;

Line_Dumper::Line_Dumper(FILE *f, int width)
  : file(f), lmax(width), cnt(0), is_cont(false), buff(0)
{
  startstr = new char[2];
  strcpy(startstr,"\t");
  slen = 8;
    
  contstr = new char[3];
  strcpy(contstr," \\");
  csav = 2;

  buff = new char[width+1];
  buff[0] = '\0';
}

Line_Dumper::~Line_Dumper()
{
  Reset();
  delete [] buff;
  delete [] contstr;
  delete [] startstr;
}

void Line_Dumper::Start_String(const string &start)
{
  delete [] startstr;
  int l = start.length();
  startstr = new char[l+2];
  strcpy(startstr,start.c_str());
  if ( l > 0 ) {
    char last = startstr[l-1];
    if ( last != ' ' && last != '\t' ) { ++l; strcat(startstr," "); }
  }
  slen = 0;
  for(int i=0;i<l;++i) {
    if (startstr[i] == '\t' ) slen = ((slen/8)+1)*8;
    else ++slen;
  }
}

void Line_Dumper::Cont_String(const string &cont)
{
  delete [] contstr;
  int l = cont.length();
  contstr = new char[l+2];
  strcpy(contstr,cont.c_str());
  if ( l > 0 ) {
    char last = contstr[l-1];
    if ( last != ' ' && last != '\t' ) { ++l; strcat(contstr," "); }
  }
  csav = 0;
  for(int i=0;i<l;++i) {
    if (contstr[i] == '\t' ) csav = ((csav/8)+1)*8;
    else ++csav;
  }
}

void Line_Dumper::Reset(int extralines)
{
  if ( cnt ) fprintf(file,"%s\n",buff);
  for(int i=0;i<extralines;++i) fprintf(file,"\n");
  cnt = 0;
  buff[0] = '\0';
  is_cont = false;
}

void Line_Dumper::SendPhrase(const string &phrase)
{
  int len = phrase.size();
  int shft = 0;
  if ( cnt + len + csav > lmax ) {
    fprintf(file,"%s%s\n", buff, contstr);
    strcpy(buff,startstr);
    cnt = slen;
    is_cont = true;
    if ( phrase.c_str()[0] == ' ' ) shft = 1;
  }
  strcat(buff,phrase.c_str()+shft);
  cnt += len;
}

