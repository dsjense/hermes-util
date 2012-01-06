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

/*! \file token_stream.h
 *  \brief Interface definitions for the Token_Stream class.
 */

#ifndef token_streamH
#define token_streamH
 
#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include "code_types.h"
#include "tokenizer.h"
#include "token.h"
#include "token_separator.h"

/*! \brief Class that represents a stream of tokens
 */
class Token_Stream 
{
 private:
  //! Typedef for class that generates tokens from the input file
  typedef  Tokenizer<Token_Separator> Token_Generator;
  //! Typedef for iterator for class that generates tokens from the input file
  typedef  Token_Generator::iterator Token_Generator_Iterator;
  //! Enumeration of return status values from get_next_token_string() method.
  typedef enum
  {
    IO_OK=0,     //!< get_next_token_string() successfully processed a token.
    IO_EOF,      //!< get_next_token_string() encountered an End-of-File.
    IO_ERROR,    //!< get_next_token_string() encountered an error reading
                 //!<     from the input istream.
    SYNTAX_ERROR //!< get_next_token_string() caught a Tokenizer_Error object.
  } Token_IOStatus;

 public:
  /*! \brief  Constructor.
   *
   *  \param in        Reference to the istream object to read to obtain
   *                   tokens.
   *  \param out       If nonzero, a pointer to a ostream object where the
   *                   error processing routines will write a message.
   *  \param separator Object that contains syntax rules for parsing tokens.
   */
  Token_Stream(std::istream &in, std::ostream *out,
               const Token_Separator &separator);

  //! Destructor
  ~Token_Stream();

  //! \name Access Functions
  //@{
  //! Returns number of errors processed by error-processing methods.
  int Error_Count() const {return error_count;}
  //! Returns the current line number of the input stream.
  int Line_Number() const {return line_number;}
  //@}
 
  /*! \brief Examine the next token in the stream without extracting it.
   *
   *  \return The extracted token, in the form of a Token object.
   */
  Token Lookahead();

  /*! \brief Extract the next token in the stream.
   *
   *  \return The extracted token, in the form of a Token object.
   */
  Token Pop();

  /*! \brief Push a token back on the stream.
   *
   *  \param token  The token to be pushed back, in the form of a Token object.
   */
  void Push_Back(Token &token);

  /*! \brief Processes errors for which processing can continue from the point
   *         of the error.
   *
   *  The following actions are performed:
   *  \li The error count is incremented.
   *  \li If the attribute \b output is not zero, a message is written to the
   *      ostream object pointed to by \b output.
   *
   *  \param s  Message to be written
   *  \param v  If not empty, a second line of message to be written
  */
  void Semantics_Error(const std::string &s, const std::string &v = "");

  /*! \brief Processes errors for which an exception should be thrown.
   *
   *  The following actions are performed:
   *  \li The Semantics_Error() method is called.
   *  \li A Parsing_Error object is constructed and thrown.
   *
   *  \param s  Message to be written
   *  \param v  If not empty, a second line of message to be written
   */
  void Parse_Error(const std::string &s, const std::string &v = "")
    throw(Parsing_Error);

  /*! \brief Skips the rest of the current line in the associated input stream.
   *
   *  \return The contents of the entire line currently being processed.
   */
  std::string Skip_Rest_of_Line();

  //! \name Specific elementary parsing methods
  //@{
  //! Tests to see if next token is a legal integer.
  bool At_Integer();

  /*! \brief Extracts an integer from the stream.
   *
   *  If the next token is not an integer, a Parsing_Error object is thrown.
   */
  int Parse_Integer();

  //! Tests to see if next token is a legal real value.
  bool At_Real();

  /*! \brief Extracts a real value (or an integer converted to a real) from the
   *         stream.
   *
   *  If the next token is not a legal real value (which includes a legal
   *  integer), a Parsing_Error object is thrown.
   */
  Real Parse_Real();

  /*! \brief Tests to see if next token is a legal identifier.
   *
   *  \note  An identifier is a special form of string that is composed
   *         entirely of alphanumeric characters and underscores ('_'), with
   *         the additional restriction that the first character cannot be a 
   *         numeral (0-9).
   */
  bool At_Identifier();

  /*! \brief Extracts an identifier from the stream.
   *
   *  If the next token is not a legal identifier, a Parsing_Error object is
   *  thrown.
   */
  std::string Parse_Identifier(char conv='\0');

  //! \brief Tests to see if next token is a legal string (including identifier
  //!        strings).
  bool At_String();

  /*! \brief Extracts a string or identifier from the stream.
   *
   *  If the next token is not a legal string or identifier, a Parsing_Error
   *  object is thrown.
   */
  std::string Parse_String();

 private:
  //! Increments the line number and returns its new value.
  int Increment_Line_Number();

  //! Reads a token from the stream and returns it as a Token object.
  Token read_token();

  /*! \brief Reads a token from the stream and returns it as a string object.
   *
   *  \param Success status of method.
   *  \return The next token, in the form of a string object.
   */
  std::string get_next_token_string(Token_IOStatus &status);

  /*! \name Disallowed Methods.
   *
   *  A Token_Stream is not copyable or assignable. So the copy constructor
   *  and the assignment constructor are declared as private but not defined.
   */
  //@{
  //! Copy constructor (not defined).
  Token_Stream(const Token_Stream&);
  //! Assignment constructor (not defined).
  Token_Stream& operator=(const Token_Stream&);
  //@}

  //! Pointer to the input istream object. (not owned)
  std::istream *input;

  //! Pointer to the output ostream object, or zero for no output. (not owned)
  std::ostream *output;

  //! \brief Vector to hold tokens that have been read from the istream, but
  //!        not yet extracted from this Token_Stream object.
  std::vector<Token> lookahead;

  //! current line number
  int line_number;

  //! Error Count (number of calls to Semantics_Error() and Parse_Error())
  int error_count;

  //! Number of tokens that have been read from the current input line.
  unsigned int this_lines_tok_cnt;

  //! Buffer containing input line currently being processed.
  std::string line_buffer;

  //! Buffer containing input line that was last processed.
  std::string last_buffer;

  //! \brief Object containing the syntax rules for parsing tokens from the
  //!        input stream.
  Token_Separator sep;

  //! \brief The container object that uses input lines and the Token_Separator
  //!        to generate tokens.
  Token_Generator tok;

  //! Iterator for the Token_Generator container.
  Token_Generator_Iterator tok_iterator;
};

#endif

