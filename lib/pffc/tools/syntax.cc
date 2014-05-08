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
// C_Groups

/*! \file syntax.cc
 *  \brief Implementation for the Syntax class.
  */

#include "syntax.h"
#include "token.h"
#include "token_stream.h"

using std::string;

/*****************************************************************************/
Syntax::Syntax(const std::string &keyword_delims,
               const std::string &value_delims,
               const std::string &flag_chars )
  : kw_delims(keyword_delims), val_delims(value_delims),
    flag_chrs(flag_chars)
/*****************************************************************************/
{  }

/*****************************************************************************/
Syntax::~Syntax()
/*****************************************************************************/
 {  }

/*****************************************************************************/
bool Syntax::isKeywordDelimiter(const Token &token, Token_Stream *tok_stream,
                                bool &need_right_delimiter, bool pop_it) const
/*****************************************************************************/
{
  bool is_kw = token.Type() == TK_STRING &&
    kw_delims.find(token.As_String()) != string::npos;
  if ( is_kw ) {
    if ( pop_it ) tok_stream->Pop();
    Token tryit = tok_stream->Lookahead();
    need_right_delimiter =
      tryit.Type() == TK_STRING && val_delims.find(tryit.As_String()) == 0;
    if ( need_right_delimiter ) tok_stream->Pop();
  }
  return is_kw;
}

/*****************************************************************************/
bool Syntax::isRightValueDelimiter(const Token &token, Token_Stream *tok_stream,
                                   bool pop_it) const
/*****************************************************************************/
{
  bool is_rhs =
    token.Type() == TK_STRING && val_delims.find(token.As_String()) == 1;
  if ( is_rhs && pop_it ) tok_stream->Pop();
  return is_rhs;

}

/*****************************************************************************/
bool Syntax::isOptionFlag(const Token &token, Token_Stream *tok_stream,
                          bool pop_it) const
/*****************************************************************************/
{
  bool is_flg = token.Type() == TK_STRING &&
    flag_chrs.find(token.As_String()) != std::string::npos;
  if ( is_flg && pop_it ) tok_stream->Pop();
  return is_flg;
}
