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
#include "syntax.h"

using std::string;
using std::vector;

/*****************************************************************************/
Test_Environment::Test_Environment(const Syntax *sntx)
  : syntax(sntx), error_count(0), basefile(0), testfile(0)
/*****************************************************************************/
{
  match[MATCH_INDEX] = true;
  match[MATCH_TITLE] = true;
  title_substr[0] = title_substr[1] = 0;
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
  if ( syntax->isOptionFlag(flag,token_stream) ) {
    flag = token_stream->Lookahead();
    if ( flag =="GRID" ) {
      is_grid = true;
      token_stream->Pop();
    }
    else token_stream->Parse_Error("Unknown Flag", flag.Force_As_String());
  }

  if ( is_grid )
    return new Grid_Test(token_stream, this, type, syntax);
  else
    return new Attr_Test(token_stream, this, type, syntax);
}

/*****************************************************************************/
void Test_Environment::Process_File_Command(Token_Stream *tok_stream,
                                            Data_File_Type type)
/*****************************************************************************/
{
  Token token = tok_stream->Lookahead();
  if ( syntax->isOptionFlag(token, tok_stream) ) {
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
      bool need_rhs;
      if ( syntax->isKeywordDelimiter(token, tok_stream, need_rhs ) ) {
        string fname = tok_stream->Parse_String();
        if ( Open_File(type, fname) ) {
          tok_stream->Parse_Error("Error opening file", fname);
        }
        if ( need_rhs) {
          token = tok_stream->Lookahead();
          if ( !syntax->isRightValueDelimiter(token, tok_stream) )
            tok_stream->Parse_Error("Unmatched value delimiter");
        }
      }
      else tok_stream->Parse_Error("Missing keyword delimiter");
    }
    else tok_stream->Parse_Error("Unrecognized Keyword", token.As_String());
  }
  else tok_stream->Parse_Error("Unrecognized syntax", token.Force_As_String());
}

/*****************************************************************************/
void Test_Environment::Process_Match_Command(Token_Stream *tok_stream)
/*****************************************************************************/
{
  int ntyp = 0;
  int ntgl = 0;
  int ss[] = { 0,0 };
  Match_Type type = MATCH_TITLE;
  bool is_on = true;

  while (1) {
    Token token = tok_stream->Lookahead();
    if ( syntax->isOptionFlag(token, tok_stream) ) {
      token = tok_stream->Lookahead();
      if ( token.Type() != TK_IDENTIFIER )
        tok_stream->Parse_Error("Illegal flag syntax", token.Force_As_String());
      if ( token == "I*NDEX" ) {
        type = MATCH_INDEX;
        ++ntyp;
      }
      else if ( token == "TI*TLE" ) {
        type = MATCH_TITLE;
        ++ntyp;
      }
      else if ( token == "ON" ) {
        is_on = true;
        ++ntgl;
      }
      else if ( token == "OF*F" ) {
        is_on = false;
        ++ntgl;
      }
      else tok_stream->Parse_Error("Unknown option flag",
                                   token.Force_As_String());
      tok_stream->Pop();
    }
     
    else if (token.Type() == TK_IDENTIFIER ) {
      Token key = token;
      tok_stream->Pop();
      token = tok_stream->Pop();
      bool need_rhs;
      if ( syntax->isKeywordDelimiter(token, tok_stream, need_rhs, false ) ) {
        if ( key == "SUB*STRING" ) {
          if ( ss[0] != 0 )
            tok_stream->Parse_Error("Multiple substring specifications");
          ss[0] = std::max(1,tok_stream->Parse_Integer());
          token = tok_stream->Pop();
          if ( token.As_String() != ":" )
            tok_stream->Parse_Error("Missing substring delimiter");
          else ss[1] = std::max(0,tok_stream->Parse_Integer());
          if ( ss[1] != 0 && ss[1] < ss[0] )
            tok_stream->Parse_Error("Illegal substring specification");
          else if ( ss[1] == 0 && ss[0] == 1 ) ss[0] = -1; // full string match
        }
        else tok_stream->Parse_Error("Unknown keyword",
                                     token.Force_As_String());
        if ( need_rhs) {
          token = tok_stream->Lookahead();
          if ( !syntax->isRightValueDelimiter(token, tok_stream) )
            tok_stream->Parse_Error("Unmatched value delimiter");
        }
      }
      else { // otherwise, put both tokens back on the stack and return
        tok_stream->Push_Back(token);
        tok_stream->Push_Back(key);
        break;
      }
    }
    else if (token.Type() == TK_EXIT ) break;
    else tok_stream->Parse_Error("Invalid keyword/flag syntax",
                                 token.Force_As_String());
  }
  if ( ntgl>1 ) tok_stream->Parse_Error("Multiple on/off flags specified");
  if ( ntyp>1 ) tok_stream->Parse_Error("Multiple match types specified");
  if ( ss[0] != 0 ) {
    if ( type == MATCH_INDEX )
      tok_stream->Parse_Error("SUBSTRING only available for /TITLE option");
    if ( ntgl > 0 ) tok_stream->Parse_Error("SUBSTRING keyword not compatible"
                                            " with on/off option flags");
    if ( ss[0] < 0 ) ss[0] = 0;
    is_on = true;
  }
  match[type] = is_on;
  title_substr[0] = ss[0]; title_substr[1] = ss[1];
}

/*****************************************************************************/
bool Test_Environment::Index_Match(int i1, int i2)
/*****************************************************************************/
{
  if ( !match[MATCH_INDEX] || i1 == i2 ) return true;
  return false;
}

/*****************************************************************************/
bool Test_Environment::Title_Match(const std::string &s1, const std::string &s2)
/*****************************************************************************/
{
  if ( !match[MATCH_TITLE] ) return true;
  if ( title_substr[0] == 0 ) return s1 == s2;
  int indx = title_substr[0] - 1;
  int cnt = 0;
  if ( s1.size() <= indx ) ++cnt;
  if ( s2.size() <= indx ) ++cnt;
  if ( cnt == 1 ) return false;
  if ( cnt == 2 ) return true;
  if ( title_substr[1] == 0 ) return s1.substr(indx) == s2.substr(indx);
  int len = title_substr[1] - indx;
  return s1.substr(indx,len) == s2.substr(indx,len);
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
