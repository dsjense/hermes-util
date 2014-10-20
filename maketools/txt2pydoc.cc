//  C_Groups
//  $Id$
//  
//  Copyright (2014) David Seidel.
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
#include <string>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>

#ifdef WIN32sys
# include "winrtl.h"
#else
# include <unistd.h>
#endif

using std::cerr;
using std::cout;
using std::endl;
using std::string;
using std::ostream;

static string suffix = "\"";
static string slash_n = "\\n";

// Tool for converting Python docstrings in a text file into C
// character strings definitions in an include (.h) file. The text
// file also includes two types of "special" lines, indicated by a
// special first character in the line. A "!" character is the default
// comment character (can be changed via the `-c' option), indicating
// that the rest of the line is a comment and are ignored anywhere
// they occur within the text file. A "@" first character, immediately
// followed by a legal C variable name, indicates the name of the C
// character variable which will contain the docstring defined by the
// following lines, up to the next "@" line. Note that the first
// non-comment line of the file MUST be a "@" line, and that each "@"
// line must be followd by at least one line of text.

static void usage(const string &cmd, int status)
{
  ostream *o = &cout;
  if ( status ) o = &cerr;

  *o << "Usage: " << cmd << " [-h] [-c comment_char] text_file [out_file]"
     << endl;

  exit(status);
}

void process_last_line(std::ofstream &out, string &outline)
{
  string::size_type idx;
  while ( (idx = outline.rfind(slash_n)) != string::npos )
    outline.erase(idx,2);
  out << outline << suffix << ';' << endl;
}

int main(int argc, char *argv[])
{
  char cmmnt = '!';  // '!' is default comment character
  int c;
  int error = 0;

  // get command "basename"
  string cmd = argv[0];
  string::size_type idx = cmd.rfind('/');
  if ( idx != string::npos ) cmd.erase(0,idx+1);

  // check for valid options
  while ( (c=getopt(argc, argv, "hc:")) != -1 ) {
    switch (c) {
    case 'h':
      usage(cmd,0);
      break;
    case 'c':
      cmmnt = optarg[0];
      break;
 
    case '?':  // invalid option
      error = 1;
      break;

    default:  // getopt should never return something that goes here !! 
      error = 1;
      cerr <<"?? getopt returned unexpected character code: " <<c <<endl;
    }    
  }

  // shift away the option arguments
  argv += optind;
  argc -= optind;

  if ( error || argc < 1 || argc > 2 ) usage(cmd,1);

  string txtfile = argv[0];  // input text file
  string ofile;              // output C or C++ include file
  if(argc == 1) {
    ofile = txtfile;
    idx = txtfile.rfind('.');
    if ( idx != string::npos ) ofile = txtfile.substr(0,idx);
    ofile +=  ".h";
  }
  else ofile = argv[1];

  // open input and output files
  std::ifstream inp(txtfile.c_str(),std::ios::in);
  if ( ! inp ) {
    cerr << cmd << ": cannot open input file \"" << txtfile << "\"" << endl;
    exit(1);
  }
  std::ofstream out(ofile.c_str());
  if ( ! out ) {
    cerr << cmd << ": cannot open output file \"" << ofile << "\"" << endl;
    exit(1);
  }

  const int bufsize = 1024;
  char line[bufsize];
  string prefix = "  \"";

  char *buf = line;

  bool datamode = false;
  int lcnt = 0;
  string outline("");
  while ( inp.getline(line,bufsize) ) {
    lcnt++;
    if ( line[0] == cmmnt ) { // skip lines w/ comment char in col 1
      continue;  
    }
    if ( line[0] == '@' ) {
      char *vname = strtok(line+1," \t");
      if ( !vname ) {
        cerr << "Error parsing '@' directive at line " << lcnt << endl;
        exit(1);
      }
      if (!outline.empty()) {
        process_last_line(out, outline);
        outline.clear();
        out << '\n';
      }
      out << "static char " << vname << "[] =" << endl;
      datamode = false;
      continue;
    }
    string sline(line);
    if ( sline.find_first_not_of(" \t") == string::npos ) {
      outline += slash_n;
      continue;
    }
    if ( datamode) out << outline << suffix << endl;
    datamode = true;
    string::size_type loc = 0;
    // need to escape special characters
    while ( (idx = sline.find_first_of("\\\"",loc)) != string::npos ) {
      sline.insert(idx,1,'\\');
      loc = idx + 2;// ++top;
    }
    outline = prefix + sline + slash_n; 
  }
  //cout << "'" << outline << "'" << endl;
  process_last_line(out, outline);

  return 0;
}
