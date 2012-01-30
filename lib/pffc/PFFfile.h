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

/*! \file PFFfile.h
 *  \brief Interface definitions for the PFF_File class.
 */
 
#ifndef PFFfile_h
#define PFFfile_h 1

#include "pff_ds.hh"
#include <set>
#include <string>
#include <vector>

template <typename T> class PFF_Dataset;

//! An abstraction for a PFF file.
class PFF_File 
{
  friend class PFF_Dataset<float>;
  friend class PFF_Dataset<double>;
 public:

  //! Enumeration over the possible PFF file modes.
  typedef enum {READ=0, WRITE, READWRITE, WRITE_MP} PFF_File_Modes;

  //! Enumeration over the possible PFF floating-point precision settings.
  typedef enum {REDUCED=0, FULL, ORD_FULL} PFF_FP_Precision;

  //! Enumeration over supported string matching modes for Find_Datasets().
  typedef enum { EXACT_MATCH = 0,
                 SUBSTRING_MATCH,
                 LEADING_MATCH } PFF_Match_Mode;

  //! Enumeration over supported string match options for Find_Datasets_Wild().
  typedef enum { EXACT_CASE = 1,
                 INVERT_MATCH = 2,
                 NO_APPEND = 4 } PFF_Match_Opts;

  /*! \brief Constructor
   * 
   *  Opens the specified file with the specified mode.
   *  \param fname File name (including complete path).
   *  \param mode File mode.
   */
  PFF_File(const std::string &fname, PFF_File_Modes mode = READ);

  /*! \brief Destructor.
   * 
   *  Closes the file.
   */
  ~PFF_File();

  //! Returns the number of datasets in the file.
  int Dataset_Count() const;

  //! Returns the index of the current dataset.
  int Current_Dataset() const;

  /*! \brief Sets the file's current dataset to that specified by the
   *         supplied index.
   * 
   *  \param dataset The new current dataset for the file.
   */
  void Current_Dataset(int dataset);

  //! Returns the current file error status. Non-zero indicates error.
  int Status() const
  {
    return last_error;
  }

  //! This method clears the file's error status.
  void Clear_Error()
  {
    last_error = 0;
  }

  //! Returns the file's I/O mode.
  PFF_File_Modes Mode() const
  {
    return file_mode;
  }

  //! Returns the file name of the PFF file.
  std::string FileName() const;

  /*! \brief Finds all datasets whose titles match the supplied string using
   *         the specified match criteria.
   *
   *  \param found  Reference to vector in which indices of datasets whose
   *                titles meet the specified matching criteria are returned.
   *  \param match  String used in matching against dataset titles.
   *  \param mode   Indicates whether match should be exact, substring,
   *                leading, etc.
   *  \param invert If true, indicates that datasets whose titles do NOT meet
   *                the matching criteria are returned.
   *  \param append If true, found datasets are appended (uniquely) to any
   *                already in the "found" vector. Otherwise, any previous
   *                contents of "found" vector are overwritten.
   */  
  int Find_Datasets(std::vector<int> &found, const std::string &match,
                    PFF_Match_Mode mode = EXACT_MATCH, bool invert = false,
                    bool append = false);

  /*! \brief Finds all datasets whose titles match the supplied string using
   *         the specified match criteria.
   *
   *  Limited wildcard capability is supported. The following characters have
   *  special meaning when encountered in the search string:
   *  \li   * is matched by 0 to n characters
   *  \li   ? is matched by exactly 1 character
   *  \li   ^ as a first character anchors the match to the beginning
   *        of the comment substring
   *  \li   $ as a final character anchors the match to the end
   *        of the comment substring
   *  \li   \ escapes * and ? to be used literally anywhere in the 
   *        search string and escapes ^ (and $) at the beginning 
   *        only (and end only) of the search string to force ^ 
   *        (or $) to be interpreted literally
   *
   *  \param found        Reference to vector in which indices of datasets
   *                      whose titles meet the specified matching criteria
   *                      are returned.
   *  \param match        Search string used in matching against dataset titles.
   *  \param dslist       Set of dataset indicies to be examined for a match.
   *                      If not supplied, all datasets in the file are
   *                      examined.
   *  \param matchOptions Contains all single-bit match options:
   *                       \li PFFfile::EXACT_CASE,
   *                       \li PFFfile::INVERT_MATCH,
   *                       \li PFFfile::NO_APPEND.
   *
   *                      If not supplied, none of the options will be set.
   *  \param subIndices   Array whose first two values are assumed to be
   *                      beginning and ending indices defining a substring of
   *                      the dataset comment string to be use for matching.
   *                      If not supplied, the entire string  will be used.
   */  
  int Find_Datasets_Wild(std::vector<int> &found, const std::string &match,
                         std::set<int> *dslist = 0, int matchOptions = 0,
                         int *subIndices = 0);

  /*! \brief Sets the PFF floating point precision for any datasets written to
   *         this file after a call to this method.
   *
   *  \param setting  PFF floating-point precision (PFF_File::REDUCED,
   *                  PFF_File::FULL, or PFF_File::ORD_FULL)
   */  
  void Set_File_Precision(PFF_FP_Precision setting);

  /*! \brief returns a pointer to the underlying C PFFfid structure.
   * 
   *  This method is provided to allow an application to access functionality
   *  of the underlying C PFF library that have not yet been implemented in
   *  the PFF_File class.
   *
   *  \note Extreme care should be exercised in using this method!
   */
  PFF::PFFfid *Get_BaseFID() const { return fid; }

  /*! \brief Static method to set the default PFF floating point precision for
   *         any files opened after a call to this method.
   *
   *  \param setting  PFF floating-point precision (PFF_File::REDUCED,
   *                  PFF_File::FULL, or PFF_File::ORD_FULL)
   */  
  static void Set_PFF_Precision(PFF_FP_Precision setting);

 private:

  /*! \brief Read a dataset from the file and return a pointer to it.
   *
   *  \param dataset Dataset to read (use 0 for the current dataset).
   *  \param keep Indicates whether or not to keep non-zero floating
   *              point values if underflow occurs.
   */  
  PFF::PFFds_any *Read_Dataset(int &type, int dataset = 0, bool keep = false);

  /*! \brief Write a dataset to the file.
   *
   *  \param dataset Pointer to dataset object to be written.
   *
   *  \return  0 for normal completion, otherwise, the PFF error status.
   */  
  int Write_Dataset(PFF::PFFds_any *any_dataset);

  // Data Members for Class Attributes

  //! PFF file id returned by the open call.      
  PFF::PFFfid *fid;
 
  //! Filename of PFF file.
  std::string filename;

  //! Last returned error status.
  int last_error;

  //! File's I/O mode.
  PFF_File_Modes file_mode;

  //! List of titles for all datasets.
  std::vector<std::string> titles;
};

#endif
