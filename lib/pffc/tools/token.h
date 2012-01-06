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

/*! \file token.h
 *  \brief Interface definitions for the Token class.
 */

#ifndef tokenH
#define tokenH
 
#include "code_types.h"
#include <string>
#include <stdexcept>
#include "token_enum.h"
#include "token_value.h"

//! \brief An exception class, derived from std::runtime_error, for errors
//!        encountered by methods of the Token and Token_Stream classes.
class Parsing_Error : public std::runtime_error
{
 public:
  //! Extra string of error information
  std::string extra;
  //! line number at the time the exception was thrown
  int line_number;

  /*! \brief Constructor.
   *
   *  \param what      A string that describes the error.
   *  \param line_num  Current line number.
   *  \param v         A second, optional, string for describing the error.
   */
  Parsing_Error(const std::string& what, int line_num,
                const std::string& v ="")
    : std::runtime_error(what), extra(v), line_number(line_num)
  { }
  //! Destructor
  virtual ~Parsing_Error() throw() { }
};


//! \brief Represents a token, the atom of input recognized by the Token_Stream
//!        class.
class Token
{ 
 public:
  /*! \brief Default Constructor.
   *
   *  The type of the constructed token is TK_NONE.
   */
  Token();

  /*! \brief Constructor.
   *
   *  In general, a token consists of a type and an associated semantic value.
   *  \param t  The token type, one of enum Token_Type.
   *  \param sv The token value, represented as a Token_Value object.
   */
  Token(Token_Type t, Token_Value& sv); 

  /*! \brief Constructor.
   *
   *  This constructor creates a token containing a string or identifier.
   *  \param str   String to be used to initialize the token's value.
   *  \param is_id If true, the the type of the constructed token will be
   *               TK_IDENTIFIER. If false or not supplied, the token type
   *               will be TK_STRING.
   */
  Token(const std::string &str, bool is_id = false); 

  /*! \brief Constructor.
   *
   *  This constructor creates a token containing a string or identifier.
   *  \param cstr  C-style string to be used to initialize the token's value.
   *  \param is_id If true, the the type of the constructed token will be
   *               TK_IDENTIFIER. If false or not supplied, the token type
   *               will be TK_STRING.
   */
  Token(const char *cstr, bool is_id = false); 

  /*! \brief Constructor.
   *
   *  This constructor creates a token containing a floating point value.
   *  \param fval  The floating point value for token.
   */
  Token(Real fval); 

  /*! \brief Constructor.
   *
   *  This constructor creates a token containing a integer value.
   *  \param ival  The integer value for token.
   */
  Token(int ival); 

  //! Dummy enumeration to avoid implicit conversion types to Token.
  enum No_Value
  {
    no_value //!< dummy value
  };

  /*! \brief Constructor.
   *
   *  \param t   The token type, one of enum Token_Type.
   *  \param dum Must be Token::no_value
   *
   *  Not all tokens have an associated semantic value. However, we do NOT want
   *  an implicit conversion from integral types to Token, so we add a dummy
   *  argument.
   *  \sa No_Value
   */
  Token(Token_Type t, No_Value dum);

  //! Copy constructor.
  Token(const Token &src);

  //! Destructor.
  ~Token();

  //! Assignment operator
  Token& operator=(const Token&);

  //! Returns token's type
  Token_Type Type() const {return type;}

  /*! \brief Returns token's value as an integer.
   *
   *  If token's type is not integer (TK_INTEGER), Parsing_Error is thrown
   */
  int As_Int() const throw(Parsing_Error)
  {
    if ( Type() !=  TK_INTEGER )
      throw Parsing_Error("Integer expected",0);
    return s.ival;
  }

  /*! \brief Returns token's value as an floating point number.
   *
   *  If token's type is not integer (TK_REAL), Parsing_Error is thrown
   */
  Real As_Real() const throw(Parsing_Error);

  /*! \brief Returns token's value as a (char *) string.
   *
   *  If token's type is not a string type (TK_STRING or TK_IDENTIFIER),
   *  Parsing_Error is thrown.
   */
  const char* As_CString() const throw(Parsing_Error)
  {
    if (Type() != TK_STRING && Type() != TK_IDENTIFIER)
      throw Parsing_Error("String expected",0);
    return s.sval->c_str();
  }

  /*! \brief Returns token's value as a (std::string) string.
   *
   *  If token's type is not a string type (TK_STRING or TK_IDENTIFIER),
   *  Parsing_Error is thrown.
   */
  std::string As_String() const throw(Parsing_Error)
  {
    if (Type() != TK_STRING && Type() != TK_IDENTIFIER)
      throw Parsing_Error("String expected",0);
    return *(s.sval);
  }

  /*! \brief Returns token's value as a (std::string) string.
   *
   *  If token's type is not a string type (TK_STRING or TK_IDENTIFIER),
   *  its value is converted to a representative string.
   */
  std::string Force_As_String() const;

  //! Method to set the static data member substring_match_char.
  static void Set_Substring_Match_Char(char cval)
  {
    substring_match_char = cval;
  }

  /*! \name Equality/Inequality methods between two Token objects.
   *
   * \param a  First token object to be compared.
   * \param b  Second token object to be compared.
   *
   * \return  true is returned if the matching criteria are meant.
   *
   *  Match Criteria:
   * \li If tok is an identifier token (TK_IDENTIFIER), then the tokens match
   *     if they are \b case-insensitively identical over their entire lengths.
   * \li If tok is a string token (TK_STRING), then the tokens match if they
   *     are \b case-sensitively identical over their entire lengths.
   *     matches the char string if they are case-sensitively identical.
   * \li If tok is of any other type, tok and k to NOT match.
   */
  //@{
  //! Inequality operator between two Tokens
  friend bool operator==(const Token &a, const Token &b);
  //! Equality operator between two Tokens
  friend bool operator!=(const Token &a, const Token &b)
  {
    return !operator==(a, b);
  }
  //@}

  /*! \name Equality/Inequality methods between Token objects and char strings.
   *
   *  All these methods ultimately use the static Token_Match() method, and 
   *  equality is determined according to its match criteria.
   *
   *  \sa Token_Match
   */
  //@{
  //! Equality operator between a token and a character string
  friend bool operator==(const Token& tk, const char *c)
  { 
    return Token_Match(tk, c);
  }
  //! Equality operator between a character string and a token
  friend bool operator==(const char *c, const Token& tk)
  { 
    return operator==(tk,c);
  }
  //! Inequality operator between a token and a character string
  friend bool operator!=(const Token& tk, const char *c)
  { 
    return !operator==(tk,c);
  }
  //! Inequality operator between a character string and a token
  friend bool operator!=(const char *c, const Token& tk)
  {
    return !operator==(tk,c);
  }
  //@}

  private:
  /*! \brief Static method to compare a Token object with a character string.
   *
   *  \param tok  Token object to be compared with char string.
   *  \param k    Char string to be compared with Token object.
   *
   *  \return  True is returned if the matching criteria are meant.
   *
   *  Match Criteria:
   *  \li If tok is an identifier token (TK_IDENTIFIER), then the tok's string
   *      value matches the char string if they are \b case-insensitively
   *      identical over the length of tok's string. Token string must be a
   *      minimum number of characters (specified by the substring_match_char
   *      class member). \n For example, if substring_match_char='*' and
   *      k="ABC*DEFG", then t="abc" and t="abcde" match, but t="ab",
   *      t="abcdx", and t="abcdefgh" do not match.
   *  \li If tok is a string token (TK_STRING), then the tok's string value
   *      matches the char string if they are \b case-sensitively identical.
   *  \li If tok is of any other type, tok and k to NOT match.
   *  \sa substring_match_char
   */
  static bool Token_Match(const Token &tok, const char *k);

  /*! \brief compares two strings for \b case-insensitive equality over 
   *         their first \b len characters.
   *
   *  \param t       First string to be compared.
   *  \param k       Second string to be compared.
   *  \param minlen  Maximum number of characters to compare
   *
   *  \note  If minlen less than or equal to zero, the two strings must match
   *         over there entire length.
   */
  static bool case_insensitive_match(const std::string &s1,
                                     const std::string &s2, int minlen=0);

  //! token type
  Token_Type type;

  //! token value 
  Token_Value s;

  /*! \brief Static data member that determines minimum substring match.
   *
   *  \sa Token_Match
   */
  static char substring_match_char;
};

#endif

