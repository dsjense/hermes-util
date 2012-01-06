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
// ------------------------------------------------------------------
// 
// The Hermes Token_Separator class is based upon the Boost
// escaped_list_separator class.
//
// Copyright John R. Bandela 2001. 
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)


//  C_Groups pffdiff

/*! \file token_separator.cc
 *  \brief Implementation for the Token_Separator class.
 */

#include "token_separator.h"

using std::string;

/*****************************************************************************/
bool Token_Separator::operator()(iterator& next, iterator end, string& tok)
/*****************************************************************************/
{
  bool bInQuote = false;
  tok = string();

  if (next == end) return false;

  for ( ;next != end; ++next) {
    if (is_c(*next)) {
      if ( !bInQuote ) {
        // If we are not in quote, then we are done
        ++next;
        // The last character was a c, that means there is
        // 1 more blank field
        if ( !blanks_ok_ ) while ( is_c(*next) ) ++next;
        if ( next == end ) break;
        if ( is_cmnt(*next) ) {
          next = end;
          return blanks_ok_ || tok.size() != 0;
        }
        if ( discard_delims_ ) {
          if ( blanks_ok_ ) discard_delims_ = false;
          --next;
          continue;
        }
        return true;
      }
      else tok += *next;
    }
    else {
      discard_delims_ = false;
      if ( is_escape(*next) ) {
        do_escape(next, end, tok);
      }
      else if ( is_rc(*next) ) {
        if ( !bInQuote ) {
          // if we've found something previously (that wasn't a delimiter,
          // we need to return it and get the returnable token on the next call
          if ( tok.size() != 0 ) return true;

          tok += *next;   // add returned delimiter
          // advance to next character, and if delimiter, next again
          if ( is_c(*(++next)) ) ++next;
          // if no blank tokens, eat any more delimiters
          if ( !blanks_ok_ ) while ( is_c(*next) ) ++next;
          // always true, since we have the returned delimiter
          return true;
        }
        else tok += *next;
      }
      else if ( is_cmnt(*next) ) {
        if ( !bInQuote ) {
          // If we are not in quote, then we are done with entire sequence
          next = end;
          return blanks_ok_ || tok.size() != 0;
        }
        else tok += *next;
      }
      else if ( is_quote(*next) ) {
        bInQuote = !bInQuote;
      }
      else {
        tok += *next;
      }
    }
  }
  return blanks_ok_ || tok.size() != 0;
}

/*****************************************************************************/
void  Token_Separator::do_escape(iterator& next, iterator end,
                                 string& tok) const throw (Tokenizer_Error)
/*****************************************************************************/
{
  if (++next == end)
    throw Tokenizer_Error(std::string("cannot end with escape"));
  if ( *next == 'n' ) {
    tok += '\n';
    return;
  }
  else if (is_quote(*next)) {
    tok += *next;
    return;
  }
  else if (is_c(*next)) {
    tok += *next;
    return;
  }
  else if (is_rc(*next)) {
    tok += *next;
    return;
  }
  else if (is_cmnt(*next)) {
    tok += *next;
    return;
  }
  else if (is_escape(*next)) {
    tok += *next;
    return;
  }
  else
    throw Tokenizer_Error(std::string("unknown escape sequence"));
}
