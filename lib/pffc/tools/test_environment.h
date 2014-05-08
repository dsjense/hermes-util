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

/*! \file test_environment.h
 *  \brief Interface definitions for the Test_Environment class.
 */

#ifndef TEST_ENVIRONMENTH
#define TEST_ENVIRONMENTH

#include "code_types.h"
#include <string>

template <typename T> class PFF_Dataset;
class PFF_File;
class Token_Stream;
class Generic_Test;
class Syntax;

//! \brief Class that hold the test environment information and provides an
//!        interface to the PFF files.
class Test_Environment
{
 public:
  //! Enumeration of the two types of PFF files used.
  typedef enum
  {
    BASE_FILE = 0, //!< PFF file referred to is the \b baseline file.
    TEST_FILE      //!< PFF file referred to is the \b test file.
  } Data_File_Type;

  /*! \brief Constructor.
   *
   *  \param sntx            Pointer to Syntax object for handling keyword and 
   *                         value delimiters as well as option flags.
   */
  Test_Environment(const Syntax *sntx);

  //! Destructor.
  ~Test_Environment();

  /*! \brief A method to process commands that describe metric comparison
   *         tests.
   *
   *  The method also processes any optional flags that further specify
   *  the test. Currently, the only recognized flag is "/GRID". Upon
   *  successful return, the token stream is positioned at the next keyword
   *  to be processed.
   *
   *  \param tok_stream  Token_Stream object to parse input command from.
   *
   *  \return Pointer to a Generic_Test object that encapsulates the requested
   *          test. If 0 is returned, the command token does not represent a
   *          valid metric test.
   */
  Generic_Test *Build_Tester(Token_Stream *token_stream);

  /*! \brief A method to process commands related to opening/closing PFF files.
   *
   *  \param tok_stream  Token_Stream object from which to parse the command.
   *  \param type        The type of PFF file (TEST or BASE).
   */
  void Process_File_Command(Token_Stream *tok_stream, Data_File_Type type);

  /*! \brief A method to process the MATCH command, related to comparing PFF
   *         datasets.
   *
   *  By default, comparisons between two datasets fail if either their dataset
   *  comments to not match or if their dataset indices do not match. This
   *  command allows the user to change that behavior.
   *
   *  \param tok_stream  Token_Stream object from which to parse the command.
   */
  void Process_Match_Command(Token_Stream *tok_stream);

  /*! \brief Method compare the indices of two datasets, based on current
   *         environment settings.
   *
   *  \param i1  File index of first dataset.
   *  \param i2  File index of second dataset.
   *
   *  \return   true if the indicies match or index matching is turned OFF.
   *            Otherwise, false is returned.
   *  \sa Process_Match_Command
   */
  bool Index_Match(int i1, int i2);

  /*! \brief Method compare the titles of two datasets, based on current
   *         environment settings.
   *
   *  \param i1  Title of first dataset.
   *  \param i2  Title of second dataset.
   *
   *  \return true if the titles match (or substrings of those titles, depending
   *          on current environment settings) or title matching is turned OFF.
   *          Otherwise, false is returned.
   *  \sa Process_Match_Command
   */
  bool Title_Match(const std::string &s1, const std::string &s2);

  /*! \brief Method to increment the error count for the test environment.
   *
   *  \return   The updated error count is returned.
   */
  int Increment_Error_Count() { return ++error_count; }


  /*! \brief Method to access the error count for the test environment.
   *
   *  \return   The current error count is returned.
   */
  int Error_Count() const { return error_count; }


  /*! \brief Method to determine if a PFF file is open.
   *
   *  \param type   The type of PFF file.
   *  \return       True if the file is open.
   */
  bool Is_File(Data_File_Type type) const;

  /*! \brief Method that returns the number of datasets in the specified file.
   *
   *  \param type   The type of PFF file.
   *  \return If the file is open, the number of contained datasets is
   *          returned. Otherwise, -1 is returned.
   */
  int Dataset_Count(Data_File_Type type);

  /*! \brief Method to obtain a specified dataset from an open PFF file.
   *
   *  \param type      The type of PFF file.
   *  \param ds_number The index of the dataset in the file.
   *  \return          Pointer to the requested dataset object.
   */
  PFF_Dataset<Real> *Get_Dataset(Data_File_Type type, int ds_number) const;

  /*! \brief Method to obtain the dataset index of a dataset with the
   *         specified title in an open PFF file.
   *
   *  \param type     The type of PFF file.
   *  \param ds_name  The title of the requested dataset.
   *  \return         The index of the requested dataset.
   */
  int Get_Dataset_Number(Data_File_Type type,
                         const std:: string &ds_name) const;

 private:

  //! Enumeration of the two types of match used by the MATCH command.
  typedef enum
  {
    MATCH_INDEX = 0,  //!< dataset index.
    MATCH_TITLE,      //!< dataset title.
    MATCH_TYPES       //!< Number of match types used.
  } Match_Type;

  /*! \brief Method to open a PFF file
   *
   *  \param type   The type of PFF file.
   *  \param fname  Name of file to be opened.
   *  \return       Zero on successful completion
   */
  int Open_File(Data_File_Type type, const std::string &fname);

  /*! \brief Method to close a PFF file
   *
   *  \param type   The type of PFF file.
   */
  void Close_File(Data_File_Type type);

  //! Pointer to object for handling keyword/value delimiters and option flags.
  const Syntax *syntax;

  //! Error counter
  int error_count;

  //! Baseline PFF file
  PFF_File *basefile;

  //! Test PFF file
  PFF_File *testfile;

  //! Flags indicating whether the various match types are turned on or off.
  bool match[MATCH_TYPES];

  /*! \brief Flags indicating the current substring limits for matching titles.
   *
   *  If the second index is set to a non-positive value, the substring used is
   *  from the character denoted by the first index to the end of the string.
   *
   *  \note The index of the first character in the string is by convention ONE
   *        (NOT ZERO).
   */
  int title_substr[2];
};

#endif
