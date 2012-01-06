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
// ------------------------------------------------------------------
// 
// The Hermes Tokenizer_Error and Token_Separator classes are based upon
// the Boost escaped_list_error and escaped_list_separator classes,
// respectively.
//
// Copyright John R. Bandela 2001. 
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

/*! \file token_separator.h
 *  \brief Interface definitions for the Token_Separator class.
 */

#ifndef token_separatorH
#define token_separatorH
 
#include <string>
#include <stdexcept>
#include <algorithm> // for find_if

//! \brief An exception class, derived from std::runtime_error, for errors
//!        encountered by methods of the Token_Separator class.
struct Tokenizer_Error : public std::runtime_error
{
  /*! \brief Constructor.
   *
   *  \param what      A string that describes the error.
   */
  Tokenizer_Error(const std::string& what):std::runtime_error(what) { }
};

/*! \brief Class to provide the syntactical framework for a Tokenizer<> object
 *         to parse an input line.
 *
 *  The syntactical rules implemented by this class are:
 *    \li Tokens are delimited by specified dilimiter characters, of two
 *        varieties. <b> Ignorable delimiters </b> delimit tokens, but are not
 *        themselves returned as tokens. Furthermore, consecutive ignorable
 *        delimiters delimit empty (or blank) tokens, which may or may not be
 *        returned to the Tokenizer object as tokens. <b> Returnable delimiters
 *        </b> are delimiters that are themselves returned as tokens.
 *        Consecutive returnable tokens are considered separate tokens, and
 *        are each returned to the Tokenizer object.
 *    \li Quote characters can be specified to delimit strings, in which all
 *        other special characters (delimiters or comment characters) are
 *        interpreted literally. To use a quote character or an escape
 *        character literally within a quoted string, preceed it with an
 *        escape character (see below).
 *    \li Comment characters can be specified. If an unquoted or unescaped
 *        comment character is encountered, it and the remainder of the input
 *        line are discarded.
 *    \li Escape characters can be specified which cause the special character
 *        that follows to be interpreted literally, in which case the escape
 *        character itself is discarded. Characters that can be escaped are
 *        limited to either type of delimiter, quote characters, comment
 *        characters, escape characters, and the character 'n', which indicates
 *        a newline character. An escape character preceding any other
 *        character, or at the end of an input line, is considered an error and
 *        a Tokenizer_Error object is thrown.
 *
 *  Modelled after the escaped_list_separator class in boost -- \n
 *  copyright John R. Bandela 2001 \n
 *  See http://www.boost.org/libs/tokenizer for documentation.
 */
class Token_Separator {
 private:
  //! An alias for the string iterator
  typedef std::string::const_iterator iterator;

 public:
  //! \name Constructors.
  //@{
  /*! \brief Single character-based constructor (includes default constructor).
   *
   *  \param blanks_ok  If true empty (or blank tokens) will be considered
   *                    tokens and returned as such. (default: true)
   *  \param c          The ignorable delimiter (default: ',')
   *  \param rc         The returnable delimiter (default: none)
   *  \param k          The comment character (default: '#')
   *  \param q          The quote character (default: '"')
   *  \param e          The escape character (default: '\')
   */
  explicit Token_Separator(bool blanks_ok = true, char c = ',', char rc = '\0',
                           char k = '#', char  q = '\"', char  e = '\\')
    : escape_(1,e), c_(1,c), quote_(1,q), cmnt_(1,k), rc_(1,rc),
      blanks_ok_(blanks_ok), discard_delims_(true)
  {
  }
    
  /*! \brief String-based constructor
   *
   *  \param blanks_ok  If true empty (or blank tokens) will be considered
   *                    tokens and returned as such.
   *  \param c          String containing the ignorable delimiters
   *                    (default: ",")
   *  \param rc         String containing the returnable delimiters
   *                    (default: "")
   *  \param k          String containing the comment characters
   *                    (default: "#")
   *  \param q          String containing the quote characters (default: "\"")
   *  \param e          String containing the escape characters (default: "\\")
   */
  Token_Separator(bool blanks_ok, std::string c = ",", std::string rc = "",
                  std::string k = "#", std::string q = "\"",
                  std::string e = "\\")
    : escape_(e), c_(c), quote_(q), cmnt_(k), rc_(rc), blanks_ok_(blanks_ok),
      discard_delims_(true)
  {
  }
  //@}   

  //! Method (required by Tokenizer) to reset the to the beginning of the line
  void reset()
  {
    discard_delims_ = true;
  }

  //! \name Access Functions
  //@{
  //! Returns a string containing the ignorable delimiters
  std::string delim_chars() const { return c_; }

  //! Returns a string containing the returnable delimiters
  std::string ret_delim_chars() const { return rc_; }

  //! Returns a string containing the escape characters
  std::string escape_chars() const { return escape_; }

  //! Returns a string containing the quote characters
  std::string quote_chars() const { return quote_; }

  //! Returns a string containing the comment characters
  std::string comment_chars() const { return cmnt_; }

  //! Returns true if empty strings are to be considered returnable tokens
  bool blanks_returned() const { return blanks_ok_; }
  //@}

  /*! \brief Processes the input stream from its current position and discovers
   *         the next token.
   *
   *  \param next Reference to an iterator pointing the the current position
   *              in the input line, which is updated as the next token is
   *              processed by this method.
   *  \param end  Iterator pointing the the end of the input line
   *  \param tok  Reference to a string in which the next token is constructed
   *              and returned.
   *
   *  \return Returns true if the end of the input line has \b not yet been
   *          reached.
   */
  bool operator()(iterator& next, iterator end, std::string& tok);

 private:
  /*! \brief Determines whether a character is a legal escape character.
   *
   *  \param e  Character to be tested.
   */
  bool is_escape(char e) const {
    return escape_.find(e) != std::string::npos;
  }

  /*! \brief Determines whether a character is a legal ignorable delimiter.
   *
   *  \param e  Character to be tested.
   */
  bool is_c(char e) const {
    return c_.find(e) != std::string::npos;
  }

  /*! \brief Determines whether a character is a legal repeatable delimiter.
   *
   *  \param e  Character to be tested.
   */
  bool is_rc(char e) const {
    return rc_.find(e) != std::string::npos;
  }

  /*! \brief Determines whether a character is a legal quote character.
   *
   *  \param e  Character to be tested.
   */
  bool is_quote(char e) const {
    return quote_.find(e) != std::string::npos;
  }

  /*! \brief Determines whether a character is a legal comment character.
   *
   *  \param e  Character to be tested.
   */
  bool is_cmnt(char e) const {
    return cmnt_.find(e) != std::string::npos;
  }

  /*! \brief Method that processes a character following an escape character.
   *
   *
   *  \param next Reference to an iterator pointing the the current position
   *              in the input line, which is updated as the next character is
   *              processed by this method.
   *  \param end  Iterator pointing the the end of the input line
   *  \param tok  Reference to a string in which the next tiken is constructed
   *              and returned.
   */
  void do_escape(iterator& next, iterator end, std::string& tok) const
    throw (Tokenizer_Error);

  //! String containing the escape characters.
  std::string  escape_;

  //! String containing the ignorable delimiters.
  std::string  c_;

  //! String containing the quote characters.
  std::string  quote_;      

  //! String containing the comment characters.
  std::string  cmnt_;

  //! String containing the returnable delimiters.
  std::string  rc_;

  //! \brief Indicates whether or not empty (or blank tokens) will be
  //!        considered tokens and returned as such.
  bool blanks_ok_;

  //! \brief Indicates whether the processing of the input line is in a state
  //!        in which igorable delimiters should be discarded. 
  bool discard_delims_;
};

#endif 
