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

/*! \file token.cc
 *  \brief Implementation for the Token class.
 */
#ifdef C_UTILS_NOT_IN_STD 
# include <ctype.h>
#else
# include <cctype>
 using std::tolower;
#endif
#include <sstream>
#include <algorithm>
#include "token.h"

using std::string;
using std::ostringstream;
using std::ios;

char Token::substring_match_char = '\0';

/*****************************************************************************/
Token::Token() : type(TK_NONE) 
/*****************************************************************************/
{
}

/*****************************************************************************/
Token::Token(Token_Type t, Token_Value& sv)
/*****************************************************************************/
:
  type(t)
{
  switch (type){
    case TK_IDENTIFIER:
    case TK_STRING:
      s.sval = new string(*(sv.sval));
      break;
    default:
      s = sv;
      break;
  }
}

/*****************************************************************************/
Token::Token(const string &str, bool is_id)
/*****************************************************************************/
: type(TK_STRING)
{
  if ( is_id ) type = TK_IDENTIFIER;

  s.sval = new string(str);
}

/*****************************************************************************/
Token::Token(const char *cstr, bool is_id)
/*****************************************************************************/
: type(TK_STRING)
{
  if ( is_id ) type = TK_IDENTIFIER;

  s.sval = new string(cstr);
}

/*****************************************************************************/
Token::Token(Real fval)
/*****************************************************************************/
: type(TK_REAL)
{
  s.fval = fval;
}

/*****************************************************************************/
Token::Token(int ival)
/*****************************************************************************/
: type(TK_INTEGER)
{
  s.ival = ival;
}

/*****************************************************************************/
Token::Token(Token_Type t, No_Value)
/*****************************************************************************/
:
  type(t)
{
}

/*****************************************************************************/
Token::Token(const Token& t)
/*****************************************************************************/
:
  type(t.type)
{
  switch (type){
    case TK_IDENTIFIER:
    case TK_STRING:
      s.sval = new string(*(t.s.sval));
      break;
    default:
      s = t.s;
      break;
  }
}

/*****************************************************************************/
Token& Token::operator=(const Token& t)
/*****************************************************************************/
{
  switch (type){
    case TK_IDENTIFIER:
    case TK_STRING:
      if(s.sval) delete s.sval;
    break;
    default: break;
  }

  switch (t.type){
    case TK_IDENTIFIER:
    case TK_STRING:
      s.sval = new string(*(t.s.sval));
      break;
    default:
      s = t.s;
      break;
  }
  type = t.type;
  return *this;
}

/*****************************************************************************/
Real Token::As_Real() const throw(Parsing_Error)
/*****************************************************************************/
{
  if (Type() == TK_INTEGER) return Real(s.ival);
  else if (Type() == TK_REAL)   return s.fval;

  throw Parsing_Error("Real expected",0);
  // return Token::0.0;
}

/*****************************************************************************/
std::string Token::Force_As_String() const
/*****************************************************************************/
{
  switch (type){
    case TK_EXIT:
      return string("<<EOF>>");
      // break;
    case TK_NONE:
      return string("<<NONE>>");
      // break;
    case TK_INTEGER:
      {
        ostringstream tmp;
        tmp << s.ival;
        return tmp.str();
      }
      // break;
    case TK_REAL:
      {
        ostringstream tmp;
        tmp.setf(ios::showpoint);
        tmp.setf(ios::scientific,ios::floatfield);
        tmp << s.fval;
        return tmp.str();
      }
      // break;
    case TK_STRING:
    case TK_IDENTIFIER:
      return As_String();
      // break;
  }
  return string(""); // we should never get here !
}

/*****************************************************************************/
Token::~Token(void)
/*****************************************************************************/
{
  switch (type){
    case TK_IDENTIFIER:
    case TK_STRING:
      if(s.sval) delete s.sval;
      break;
    default:
      break;
  }
}

/*****************************************************************************/
bool Token::Token_Match(const Token &tok, const char *k)
/*****************************************************************************/
{
  bool rval = false;
  string sk = string(k);

  switch (tok.Type()) {
    case TK_STRING:
      // Matches two character strings according to the following rules:
      //   strings are identical
      rval = (tok.As_String() == sk);
      break;
    case TK_IDENTIFIER:
      // Matches two character identifiers according to the following rules:
      //   strings are case-insensitively identical over length of token string
      //   Token string must be a minimum # of characters (specified by
      //       substring_match_char class member)
      //       For example, if substring_match_char='*' and k="ABC*DEFG", then
      //          t="abc"       match
      //          t="ab"        no match
      //          t="abcde"     match
      //          t="abcdx"     no match
      {
        const string t = tok.As_String();
        string::size_type mc = 0;
        if ( ! substring_match_char ||
             (mc = sk.find(substring_match_char)) == string::npos ) {
          rval = case_insensitive_match(t,sk);
        }
        else {
          int sublen = std::max(mc,t.size());
          sk.erase(sk.begin()+mc);
          rval = case_insensitive_match(t,sk,sublen);
        }
      }
      break;
    default:
      break;
  }
  return rval;
}

/*****************************************************************************/
bool Token::case_insensitive_match(const string &s1, const string &s2,
                                   int minlen)
/*****************************************************************************/
{
  string::const_iterator last = s1.end();
  string::const_iterator loc1 = s1.begin();
  string::const_iterator loc2 = s2.begin();
  int len1 = s1.size();
  int len2 = s2.size();
  if ( minlen > 0 ) {
    if ( len1 != len2 && std::min(len1,len2) < minlen ) return false;
    if ( len1 > minlen ) last = loc1 + minlen;
  }
  else if ( len1 != len2 ) return false;

  for( ; loc1 != last; ++loc1,++loc2) {
    if ( tolower(*loc1) != tolower(*loc2) ) return false;
  }
  return true;
}

/*****************************************************************************/
bool operator==(const Token &a, const Token &b)
/*****************************************************************************/
{
  if (a.type != b.type) return false;
  switch (a.type){
    case TK_IDENTIFIER:
      return Token::case_insensitive_match(a.As_String(), b.As_String());
   case TK_STRING:
      return a.As_String() == b.As_String();
    case TK_INTEGER:
      return a.s.ival == b.s.ival;
    case TK_REAL:
      return a.s.fval == b.s.fval;
    default:
      return true;
  }
}


