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
   *  \param keyword_delims String containing keyword delimiters.
   *  \param flag_chars     String containing option flag characters
   */
  Test_Environment(const std::string &keyword_delims="",
                   const std::string &flag_chars="");

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

  /*! \brief A method to process commands related to PFF files.
   *
   *  \param tok_stream  Token_Stream object to parse input command from.
   *  \param type        The type of PFF file.
   */
  void Process_File_Command(Token_Stream *tok_stream, Data_File_Type type);

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

  //! String containing keyword delimiters.
  std::string kw_delims;

  //! String containing option flag characters
  std::string flag_chrs;

  //! Error counter
  int error_count;

  //! Baseline PFF file
  PFF_File *basefile;

  //! Test PFF file
  PFF_File *testfile;
};

#endif
