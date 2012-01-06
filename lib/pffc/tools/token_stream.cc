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

/*! \file token_stream.cc
 *  \brief Implementation for the Token_Stream class.
 */

#ifdef C_UTILS_NOT_IN_STD 
# include <ctype.h>
# include <stdlib.h>
#else
# include <cctype>
 using std::isalnum;
 using std::isalpha;
 using std::isdigit;
 using std::islower;
 using std::isupper;
 using std::tolower;
 using std::toupper;
# include <cstdlib>
 using std::atof;
 using std::atoi;
#endif
#include <algorithm>
#include <iostream>

#include "token_stream.h"

using std::endl;
using std::istream;
using std::ostream;
using std::string;
using std::transform;

/*****************************************************************************/
Token_Stream::Token_Stream(istream &in, ostream *out,
                           const Token_Separator &separator)
/*****************************************************************************/
  // Create a Token_Stream whose text is taken from in and echoed to out.
  //
  // verb indicates the verbosity level for diagnostic messages.  A value
  // of zero, the default, causes only the basic diagnostic message to print.
  // A higher value enables printing of supplemental information, such as a
  // list of recognized keywords or recommendations for correcting the 
  // diagnosed problem.
  //
  // Upon successful construction, the Token_Stream points to the first token 
  // in the input stream.  In other words, Lookahead() returns the first token 
  // in the input stream, and Pop() returns this token and advances to the 
  // second token in the input stream.  
  //
  // in must already be opened and in a good state for reading.  out must 
  // already be opened and in a good state. 
: output(out),
  line_number(0),
  error_count(0),
  this_lines_tok_cnt(0),
  line_buffer(""),
  last_buffer(""),
  sep(separator),
  tok(line_buffer,sep)
{
  input = &in;
  tok_iterator = tok.end();
}

/*****************************************************************************/
int Token_Stream::Increment_Line_Number(void)
/*****************************************************************************/
{ 
  line_number++;
  return line_number;
}

/*****************************************************************************/
Token Token_Stream::Pop(void)
/*****************************************************************************/
  // Return the current token in the stream and advance to the next token.
{ 
  if ( lookahead.empty() ) return read_token();
  else {
    Token tmp = lookahead.back();
    lookahead.pop_back();
    return tmp;
  }
}

/*****************************************************************************/
void Token_Stream::Push_Back(Token &token)
/*****************************************************************************/
  // Push a token back on the stream.
{ 
  lookahead.push_back(token);
}

/*****************************************************************************/
Token Token_Stream::Lookahead(void)
/*****************************************************************************/
  // Return the current token on the stream, but do not advance the stream.
{
  if ( lookahead.empty() ) lookahead.push_back(read_token());

  return lookahead.back();
}

/*****************************************************************************/
void Token_Stream::Parse_Error(const string &s, const string &v)
  throw(Parsing_Error)
/*****************************************************************************/
{
  Semantics_Error(s, v);

  // lookahead.clear();

  throw Parsing_Error(s, Line_Number(), v);
}

/*****************************************************************************/
void Token_Stream::Semantics_Error(const string &s, const string &v)
/*****************************************************************************/
{
  error_count++;
  if ( output ) {
    *output << "\n***** ERROR Parsing: " << s << endl;
    if ( v.size() ) *output << v << endl;
    *output << std::flush;
  }
}

/*****************************************************************************/
bool Token_Stream::At_Integer()
/*****************************************************************************/
{
  Token token = Lookahead();
  return token.Type() == TK_INTEGER;
}

/*****************************************************************************/
int Token_Stream::Parse_Integer()
/*****************************************************************************/
  // Parse an integer quantity.  Unlike real quantities, integers do not have 
  // units.
{
  int retval = 0;
  Token token = Pop();

  try {
    retval = token.As_Int();
  }
  catch ( Parsing_Error &err ) {
    Parse_Error("Integer expected");
  }
  return retval;
}

/*****************************************************************************/
bool Token_Stream::At_Real()
/*****************************************************************************/
{
  Token token = Lookahead();
  return token.Type() == TK_REAL || token.Type() == TK_INTEGER;
}

/*****************************************************************************/
Real Token_Stream::Parse_Real()
/*****************************************************************************/
  // Parse a real value. If type is INTEGER do a type conversion
{
  Real retval;
  Token token = Pop();

  try {
    retval = token.As_Real();
  }
  catch ( Parsing_Error &err ) {
    Parse_Error("real number expected");
  }
  return retval;
}

/*************************************************************************/ 
bool Token_Stream::At_String()
/*************************************************************************/ 
{
  Token token = Lookahead();
  return token.Type()==TK_STRING || token.Type()==TK_IDENTIFIER;
}

/*************************************************************************/ 
bool Token_Stream::At_Identifier()
/*************************************************************************/ 
{
  Token token = Lookahead();
  return token.Type()==TK_IDENTIFIER;
}

/*************************************************************************/ 
string Token_Stream::Parse_String()
/*************************************************************************/ 
{
  Token token = Pop();

  try {
    return token.As_String();
  }
  catch ( Parsing_Error &err ) {
    Parse_Error("String expected");
  }
  return string(""); // never gets here, but avoids compiler warnings
}

/*************************************************************************/ 
string Token_Stream::Parse_Identifier(char conv)
/*************************************************************************/ 
{
  // Warning: this permanently converts the case of the token if case
  //          conversion is requested
  Token token = Pop();
  if ( token.Type() != TK_IDENTIFIER ) Parse_Error("Identifier expected");

  string s = token.As_String();
  if (islower(conv)) 
    transform(s.begin(), s.end(), s.begin(), tolower);
  else if (isupper(conv))
    transform(s.begin(), s.end(), s.begin(), toupper);

  return s;
}

/*************************************************************************/ 
Token_Stream::~Token_Stream()
/*************************************************************************/ 
{
}

/*****************************************************************************/
string Token_Stream::get_next_token_string(Token_IOStatus &status)
/*****************************************************************************/
{
  const int bufsiz = 132;
  char buf[bufsiz];

  static bool start_of_line = false;

  while (1) {
    if ( tok_iterator == tok.end() ) {
      input->getline(buf,bufsiz,'\n');
      Increment_Line_Number();

      if ( input->eof() ) {
        status = IO_EOF;
        return string("");
      }
      if ( input->fail() ) {
        status = IO_ERROR;
        return string("");
      }
      if ( this_lines_tok_cnt ) last_buffer = line_buffer;
      line_buffer = string(buf);
      this_lines_tok_cnt = 0;

      tok.assign(line_buffer);
      start_of_line = true;
      // tok_end = tok.end();
    }
    try {
      if ( start_of_line ) {
        start_of_line = false;
        tok_iterator = tok.begin();
      } else ++tok_iterator;
    }
    catch ( Tokenizer_Error const &err ) {
      status = SYNTAX_ERROR;
      tok_iterator = tok.end();
      return string("");
    }
    if ( tok_iterator == tok.end() ) continue;

    string s = *tok_iterator;
    if ( s.size() ) {
      status = IO_OK;
      return s;
    } else std::cout << "** EMPTY TOKEN **" << endl;
  }
 }

/*****************************************************************************/
Token Token_Stream::read_token()
/*****************************************************************************/
  // Routine read_token checks for comments, ints, floats, and identifiers.
  // Checks for whitespace and eof
{
  
  Token_IOStatus status;
  string tok_string("");

  // loop so we if we have an empty token (because of a comment), we will get
  // the next one
  while ( 1 ) {
    // Get the next token.
    tok_string = get_next_token_string(status);

    // process EOF and errors
    if ( status == IO_EOF ) return Token(TK_EXIT, Token::no_value);
    if ( status == IO_ERROR ) Parse_Error("Error reading from file");
    if ( status == SYNTAX_ERROR ) Parse_Error("Syntax error encountered");

    break; // since we don't have an empty token
  }
  // if we've gotten this far, we are going to return a token
  ++this_lines_tok_cnt;

  // Now decide what kind of token we have
  const char *cstring = tok_string.c_str();

  //check for indication of numeric value
  const char *pc = cstring;
  char c = *pc;
  bool is_real = false;
  bool maybe_number = true;
  if (c=='+' || c=='-' || c=='.' || isdigit(c)) {
    if (c=='-' || c=='+') c = *(++pc); // leading +/- is okay

    int digit_cnt = 0;
    // read integer (or integer part of real)
    while (isdigit(c)) { ++digit_cnt; c = *(++pc); }
    if ( c=='.' ) {
      // Must be a float if a number at all
      is_real = true;
      c = *(++pc);
      // read fractional part of real
      while (isdigit(c)) { ++digit_cnt; c = *(++pc); }
    }
    if ( digit_cnt && maybe_number ) {
      if ( c=='e' || c=='E') { 
        // it's a float; look for tail, if any
        is_real = true;
        c = *(++pc);
        if (c=='-' || c=='+') c = *(++pc); // leading +/- is okay
        // need at least 1 digit
        if ( c=='\0' || !isdigit(c) ) maybe_number = false;
        while (isdigit(c)) c = *(++pc); // read exponent
      }
      if (maybe_number && c=='\0') {
        if ( is_real ) {
          Real fval = atof(cstring);
          return Token(fval);
        }
        else           return Token(atoi(cstring));
      }
    }
  }

  //check for identifier string:
  //  1st char:  alpha or '_'
  //  remaining: alphanumeric or '_' 
  c = *(pc = cstring);
  if (isalpha(c) || c=='_') {
    c = *(++pc);
    while (isalnum(c) || c=='_') c = *(pc++); // read rest of legal chars
    if (c=='\0') {
      return Token(tok_string, true);
    }
  }

  // must be a string
  return Token(tok_string, false);
}

/*****************************************************************************/
 string Token_Stream::Skip_Rest_of_Line()
/*****************************************************************************/
  // Skip the remainder of the current line from the input stream.
{
  if ( lookahead.size() > this_lines_tok_cnt ) {
    while (lookahead.size() > this_lines_tok_cnt ) lookahead.pop_back();
    return last_buffer;
  }
  else {
    tok_iterator = tok.end();
    lookahead.clear();
    return line_buffer;
  }
}
