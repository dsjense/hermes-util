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

/*! \file syntax.h
 *  \brief Interface definitions for the Syntax class.
  */

#ifndef SYNTAXH
#define SYNTAXH 1
 
#include <string>

class Token;
class Token_Stream;

/*! \brief This class codifies the parsing of option flag, keyword, and value
 *  delimiters.
 */
struct Syntax
{
  /*! \brief Constructor.
   *
   *  \param keyword_delims  String containing all keyword delimiters.
   *  \param value_delims    Two-character string containing the left and
   *                         right value delimiters.
   *  \param flag_chars      String containing all option flag characters.
   */
  Syntax(const std::string &keyword_delims, const std::string &value_delims,
         const std::string &flag_chars );

  //! Destructor
  ~Syntax();

  /*! \brief Checks supplied token to see if it is a keyword delimiter.
   *
   *  \param token       Token to be checked for keyword delimiter status.
   *  \param tok_stream  Token_Stream from which the token came.
   *  \param need_right_delimiter
   *                     Returns true if a left value delimiter was found
   *                     after the keyword delimiter.
   *  \param pop_it      If true, and the supplied token is a keyword delimiter,
   *                     a token will be popped from the token stream.
   *                 
   *  \return true if supplied token is a keyword delimiter.
   *  \note   pop_it should be false if the supplied token has already been
   *          popped from the token stream
   */
  bool isKeywordDelimiter(const Token &token, Token_Stream *tok_stream,
                          bool &need_right_delimiter, bool pop_it=true) const;

  /*! \brief Checks supplied token to see if it is a right value delimiter.
   *
   *  \param token       Token to be checked for right value delimiter status.
   *  \param tok_stream  Token_Stream from which the token came.
   *  \param pop_it      If true, and the supplied token is a right value
   *                     delimiter, a token will be popped from the token
   *                     stream.
   *                 
   *  \return true if supplied token is a right value delimiter.
   *  \note   pop_it should be false if the supplied token has already been
   *          popped from the token stream
   */
  bool isRightValueDelimiter(const Token &token, Token_Stream *tok_stream,
                             bool pop_it=true) const;

  /*! \brief Checks supplied token to see if it is a option flag.
   *
   *  \param token       Token to be checked for option flag status.
   *  \param tok_stream  Token_Stream from which the token came.
   *  \param pop_it      If true, and the supplied token is an option flag,
   *                     a token will be popped from the token stream.
   *                 
   *  \return true if supplied token is a option flag.
   *  \note   pop_it should be false if the supplied token has already been
   *          popped from the token stream
   */
  bool isOptionFlag(const Token &token, Token_Stream *tok_stream,
                    bool pop_it=true) const;

  //! Accessor method for string containing all keyword delimiters.
  std::string KeyWordDelimiters() const { return kw_delims; }

  //! Accessor method for string containing left and right value delimiters.
  std::string ValueDelimiters() const { return val_delims; }

  //! Accessor method for string containing all option flag characters.
  std::string OptionFlagCharacters() const { return flag_chrs; }

 private:
  //! String containing keyword delimiters.
  std::string kw_delims;

  //! Two-character String containing the left and right value delimiters.
  std::string val_delims;

  //! String containing option flag characters
  std::string flag_chrs;
};

#endif
