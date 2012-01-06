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
//  C_Groups pffdiff

/*! \file test_environment.cc
 *  \brief Implementation for the Test_Environment class.
 */

#include <vector>
#include "test_environment.h"
#include "token_stream.h"
#include "PFFfile.h"
#include "PFFdataset.h"
#include "comparators.h"
#include "metrics.h"

using std::string;
using std::vector;

/*****************************************************************************/
Test_Environment::Test_Environment(const std::string &keyword_delims,
                                   const std::string &flag_chars)
  : kw_delims(keyword_delims), flag_chrs(flag_chars), error_count(0),
    basefile(0), testfile(0)
/*****************************************************************************/
{
}

/*****************************************************************************/
Test_Environment::~Test_Environment()
/*****************************************************************************/
{
  delete basefile;
  delete testfile;
}

/*****************************************************************************/
Generic_Test *Test_Environment::Build_Tester(Token_Stream *token_stream)
/*****************************************************************************/
{
  Token token = token_stream->Pop();

  Generic_Metric::Metric_Type type = Generic_Metric::Parse_Metric_Type(token);
  if ( type == Generic_Metric::UNKNOWN ) return 0;

  Token flag = token_stream->Lookahead();
  bool is_grid = false;
  if ( flag.Type() == TK_STRING &&
       flag_chrs.find(flag.As_String()) != string::npos ) {
    token_stream->Pop();
    flag = token_stream->Lookahead();
    if ( flag =="GRID" ) {
      is_grid = true;
      token_stream->Pop();
    }
    else token_stream->Parse_Error("Unknown Flag", flag.Force_As_String());
  }

  if ( is_grid )
    return new Grid_Test(token_stream, this, type, kw_delims,flag_chrs);
  else
    return new Attr_Test(token_stream, this, type, kw_delims,flag_chrs);
}

/*****************************************************************************/
void Test_Environment::Process_File_Command(Token_Stream *tok_stream,
                                            Data_File_Type type)
/*****************************************************************************/
{
  Token token = tok_stream->Lookahead();
  if ( token.Type() == TK_STRING &&
       flag_chrs.find(token.As_String()) != string::npos ) {
    tok_stream->Pop();
    token = tok_stream->Lookahead();
    if ( token == "C*LOSE" ) {
      tok_stream->Pop();
      Close_File(type);
    }
    else tok_stream->Parse_Error("Unknown option flag", token.As_String());
  }
  else if ( token.Type() == TK_IDENTIFIER ) {
    if ( token == "F*ILE" ) {
      tok_stream->Pop();
      token = tok_stream->Lookahead();
      if ( token.Type() == TK_STRING &&
           kw_delims.find(token.As_String()) != string::npos ) {
        tok_stream->Pop();
        string fname = tok_stream->Parse_String();
        if ( Open_File(type, fname) ) {
          tok_stream->Parse_Error("Error opening file", fname);
        }
      }
      else tok_stream->Parse_Error("Missing keyword delimiter");
    }
    else tok_stream->Parse_Error("Unrecognized Keyword", token.As_String());
  }
  else tok_stream->Parse_Error("Unrecognized syntax", token.Force_As_String());
}

/*****************************************************************************/
int Test_Environment::Open_File(Data_File_Type type, const string &fname)
/*****************************************************************************/
{
  // if string is empty, this closes the file if it was open

  PFF_File *tmp = 0;
  if ( !fname.empty() ) {
    tmp = new PFF_File(fname);
    if ( tmp->Status() ) return 1;
  }

  PFF_File **file = 0;

  switch ( type ) {
  case BASE_FILE:
    file = &basefile;
    break;
  case TEST_FILE:
    file = &testfile;
    break;
  }

  if ( *file ) delete *file;
  *file = tmp;

  return 0;
}

/*****************************************************************************/
void Test_Environment::Close_File(Data_File_Type type)
/*****************************************************************************/
{
  PFF_File **file = 0;

  switch ( type ) {
  case BASE_FILE:
    file = &basefile;
    break;
  case TEST_FILE:
    file = &testfile;
    break;
  }

  if ( *file ) delete *file;
  *file = 0;
}

/*****************************************************************************/
bool Test_Environment::Is_File(Data_File_Type type) const
/*****************************************************************************/
{
  bool is_file = false;

  switch ( type ) {
  case BASE_FILE:
    is_file = basefile != 0;
    break;
  case TEST_FILE:
  int Get_Dataset_Number(Data_File_Type type, const std:: string &ds_name);
    is_file = testfile != 0;
    break;
  }
  return is_file;
}

/*****************************************************************************/
int Test_Environment::Dataset_Count(Data_File_Type type)
/*****************************************************************************/
{

  PFF_File * const *file = 0;

  switch ( type ) {
  case BASE_FILE:
    file = &basefile;
    break;
  case TEST_FILE:
    file = &testfile;
    break;
  }

  if ( *file == 0 ) return -1;
  return (*file)->Dataset_Count();
}

/*****************************************************************************/
PFF_Dataset<Real> *Test_Environment::Get_Dataset(Data_File_Type type,
                                                 int ds_number) const
/*****************************************************************************/
{
  PFF_File * const *file = 0;

  switch ( type ) {
  case BASE_FILE:
    file = &basefile;
    break;
  case TEST_FILE:
    file = &testfile;
    break;
  }

  if ( *file == 0 ) return 0;
  PFF_Dataset<Real> *tmp = PFF_Dataset<Real>::Read_Dataset(*file,ds_number);
  if ( tmp == 0 || (*file)->Status() ) {
    (*file)->Clear_Error();
    if ( tmp ) delete tmp;
    return 0;
  }
  return tmp;
}

/*****************************************************************************/
int Test_Environment::Get_Dataset_Number(Data_File_Type type,
                                         const std:: string &ds_name) const
/*****************************************************************************/
{
  PFF_File * const *file = 0;

  switch ( type ) {
  case BASE_FILE:
    file = &basefile;
    break;
  case TEST_FILE:
    file = &testfile;
    break;
  }

  if ( *file == 0 ) return -2; // file not open

  vector<int> found;
  int nf = (*file)->Find_Datasets(found, ds_name); // Exact, non-appending
  if ( nf == 0 ) return 0; // no matches
  if ( nf >  1 ) return -1; // more than one match
  return found[0];
}
