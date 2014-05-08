// $Id$
// 
// Copyright (2008) Sandia Corporation. Under the terms of
// Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
// Government retains certain rights in this software.
// 
// Hermes is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
// 
// Hermes is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General
// Public License along with Hermes.  If not, see
// <http://www.gnu.org/licenses/>.
// 
//  C_Groups pffdiff main

/*! \file pffdiff.cc
 *  \brief Main program for PFFDIFF utility.
 */

#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <cstdio>
#include "code_types.h"
#include "comparators.h"
#include "syntax.h"
#include "test_environment.h"
#include "token.h"
#include "token_separator.h"
#include "token_stream.h"
#include "token_enum.h"

using std::string;
using std::istream;
using std::ifstream;
using std::cin;
using std::cerr;
using std::cout;
using std::endl;
using std::ios;

// need prototypes for manpage and copyright functions
void pffdiff_manpage(std::ostream &);
extern "C" void dump_copyright(FILE *);

int main(int argc, char *argv[])
{
  istream *input = &cin;

  string command = string(argv[0]);
  string::size_type locslash = command.rfind('/');
  if ( locslash != string::npos ) command.erase(0,locslash+1);

  // No options, print man page and exit
  if ( argc == 1 ) {
    pffdiff_manpage(cout);
    return 0;
  }

  // Too many arguments ??
  if ( argc > 2 ) {
    std::cerr << "Usage: " << command << " [ control_file | - ]" << endl;
    return 1;
  }

  bool control_file_open = false;
  // if control file name supplied, need to open it
  // (defaults to stdin if file name is "-")
  string infile(argv[1]);
  if ( infile != "-" ) {
    input = new ifstream(argv[1]);
    if ( input->fail() ) {
      std::cerr << "error opening input file: " << argv[1] << endl;
      return 1;
    }
    control_file_open = true;
  }

  // ignored token delimiters
  string delims = " ,;()[]";
  // keyword delimiters (syntax is <keyword><delimiter><value>)
  string kw_delims = "=:";
  // Variable delimiters (syntax is <left_delimiter><value><right_delimiter>)
  string val_delims = "{}";
  // flag characters (syntax is <flag_char><flag_keyword>)
  string flag_chrs = "/";
  // characters indicating that remainder of input line should not be parsed
  string comment_chrs = "#";

  string rtn_delims = kw_delims + val_delims + flag_chrs;

  bool is_false = false;

  // set minimum substring match charactor to '*'
  Token::Set_Substring_Match_Char('*');

  Token_Separator grammar(false, delims, rtn_delims, comment_chrs);

  Token_Stream *token_stream = new Token_Stream(*input, 0, grammar);

  Syntax syntax(kw_delims, val_delims, flag_chrs);

  Test_Environment test_env(&syntax);

  Generic_Test *current_test = 0;
  int test_cnt = 0;
  int parse_errors = 0;

  cout << "                  Hermes Utilities -- PFFDIFF" << endl;
  cout <<
    " *********************************************************************"
       << endl;
  dump_copyright(stdout);
  cout <<
    " *********************************************************************"
       << endl;

  //cout << std::scientific << std::showpoint << std::setprecision(3);
  // use these instead since showpoint and scientific manipulators don't work
  // properly on tflop
  cout.setf(ios::showpoint);
  cout.setf(ios::scientific,ios::floatfield);
  cout.precision(3);

  while ( 1 ) {
    if ( current_test ) {
      delete current_test;
      current_test = 0;
    }
    try {
      Token token = token_stream->Lookahead();

      if ( token.Type()==TK_EXIT ) {
        // cout << "EOF encountered" << endl;
        break;
      }
      else if ( token.Type()!=TK_IDENTIFIER ) {
        token_stream->Parse_Error("Control file syntax error",
                                  "Identifier expected");
      }

      // loop over supported commmands -- test constructors perform parsing
      if ( token == "BASE*FILE" ) {
        token_stream->Pop();
        test_env.Process_File_Command(token_stream,
                                      Test_Environment::BASE_FILE);
      }
      else if ( token == "TEST*FILE" ) {
        token_stream->Pop();
        test_env.Process_File_Command(token_stream,
                                      Test_Environment::TEST_FILE);
      }
      else if ( token == "MAT*CH" ) {
        token_stream->Pop();
        test_env.Process_Match_Command(token_stream);
      }
      else {
        current_test = test_env.Build_Tester(token_stream);
        if ( ! current_test ) token_stream->Parse_Error("Unknown command",
                                                        token.As_String());

      }

      // if we processed a command that constructed a test object, run it
      if ( current_test ) {
        try {
          ++test_cnt;
          if ( !current_test->Run_Test() ) test_env.Increment_Error_Count();
        }
        catch ( Test_Error &test_err ) {
          test_env.Increment_Error_Count();
          current_test->Print_Result_Header(&is_false);
          cout <<  " -- " << test_err.what() << endl;
        }
        delete current_test;
        current_test = 0;
      }
    }
    catch ( Parsing_Error &err ) {
      ++parse_errors;
      cerr << "Parsing Error";
      if ( argc > 1 ) cerr << " in " << argv[1] << "[" << err.line_number
                           << "]";
      cerr << ": " << err.what() << endl;
      if ( err.extra.size() ) {
        cerr << "   " << err.extra << endl;
      }
      token_stream->Skip_Rest_of_Line();
    }
  }
  cout << "\nError Count: (" << test_env.Error_Count() << "/" << test_cnt 
       << ")";
  if ( parse_errors ) cout << "   " << parse_errors << " Parsing Error"
                           << ((parse_errors==1) ? "":"s") << " encountered";
  cout << endl;

  delete token_stream;
  if ( control_file_open ) delete input;
  return test_env.Error_Count() + parse_errors;
}
