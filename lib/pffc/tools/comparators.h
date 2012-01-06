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

/*! \file comparators.h
 *  \brief Interface definitions for the Comparators base class and the test
 *         classes derived from it.
 */

#ifndef COMPARATORSH
#define COMPARATORSH 1
 
#include <list>
#include <string>
#include <stdexcept>
#include "token.h"
#include "token_stream.h"
#include "code_types.h"
#include "PFFdataset.h"
#include "test_error.h"
#include "metrics.h"

class Test_Environment;
class Generic_Metric;

/*! \brief An abstraction for a test to be performed on PFF datasets from
 *         supplied "test" and/or "base" files.
 *
 *  The model assumes that if datasets from both files are involved, the
 *  dataset titles and the indices of the datasets in their respective files
 *  are the same. It further assumes that the data in each dataset is of the
 *  same type and has the same shape (i.e., same spatial and attribute 
 *  dimensionality, same number of blocks, and same spatial extent in each
 *  block  .
 */
class Generic_Test
{
 public:
  //! Enumeration of supported test data types
  typedef enum {
    //! Grid data (UF1, UF3, NG3, NI3, NF3, NV3, and NGD w/ Nd>0)
    GRID = 0,
    //!  Attribute data (UF1, UF3, NF3, NV3, and NGD w/ Nd>0 & Na>0)
    ATTRIBUTE
  } Data_Type;

  //! \name Constructors.
  //@{
  /*! \brief Constructor that parses remainder of PFFDIFF input line.
   *
   *  \param tok_stream      Token_Stream object to parse input command from.
   *  \param test_env        Test_Environment object that counts errors,
   *                         provides PFF datasets from the "base" and/or
   *                         "test" files, etc.
   *  \param type            Type of metric to be used
   *  \param keyword_delims  Keyword delimiters for parsing.
   *  \param flag_chars      For parsing, characters that prefix option flags.
   */
  Generic_Test(Token_Stream *tok_stream, Test_Environment* test_env, 
               Generic_Metric::Metric_Type type,
               const std::string &keyword_delims,
               const std::string &flag_chars="");

  /*! \brief Alternate constructor.
   *
   *  \param test_env  Test_Environment object that counts errors, provides PFF
   *                   datasets from the "base" and/or "test" files, etc.
   *  \param ds_number Index of dataset in file/s to supply data for test.
   *  \param type      Type of metric to be used
   *  \param lims      If nonzero, a pointer to the value/values to be used
   *                   to set the "success" limits on the metric. For MAXIMUM,
   *                   MEAN, and RMS metric types, one value, representing the
   *                   maximum deviation of the data in the base and test
   *                   files, must be provided. For the RANGE metric type, two
   *                   values, representing the minimum and maximum limits of
   *                   the data in the test file, must be provided. If zero, 
   *                   no limits are applied.
   */
  Generic_Test(Test_Environment* test_env, int ds_num,
               Generic_Metric::Metric_Type type, Real *lims);
  //@}

  //! Destructor.
  virtual ~Generic_Test();

  /*! \brief Method that runs the test after the object construction is
   *         complete.
   *
   *  \return Returns true upon the successful completion of the test.
   */
  virtual bool Run_Test();

  /*! \brief Method to print the beginning portion of the line decribing the
   *         test result.
   *
   *  \param success Pointer to success flag, if zero, test status (pass/FAIL)
   *                 will not be printed in the message header.
   */
  void Print_Result_Header(bool *success) const;

 protected:
  //! Enumeration describing type of keyword found when parsing command line
  typedef enum
  {
    //! An identifier followed by a keyword delimiter was found
    IS_KEYWORD = 0,
    //! An option flag followed by an identifier was found
    IS_FLAG,
    //! Something else was found.
    IS_OTHER
  } Keyword_Status;

  /*! \brief An abstraction of the input associated with the specification of
   *         subrange for a single spatial direction.
   */
  struct SubRangeInput {
    //! constuctor
    SubRangeInput() : dir(-1), mode(0), low(0.0), high(0.0) { };

    //! destructor
    ~SubRangeInput() { }

    //! spatial direction for the subrange (0,...)
    int dir;

    /*! \brief Bit-mapped indicator of the specified subrange bounds.
     *
     *  If lowest bit (1) is set, then lower bound was supplied, if next
     *  lowest bit (2) is set, then upper bound was supplied. Consequently, 
     *  the only legal values for this variable are 1, 2, and 3, since at
     *  least one bound must be specified.
     */
    unsigned char mode;

    //! Lower bound value
    Real low;

    //! Upper bound value
    Real high;
  };

  /*! \brief Structure that contains all the information needed for any
   *         Generic_Test object to apply any spatial sub-range constraints
   *         to its associated test.
   */
  struct SubRangeInfo {
    /*! \Constructor
     *
     *  \param  d     Dataset to be used to access grid information for test.
     *  \param  srl   List of sub-range input specification objects.
     */
    SubRangeInfo(PFF_Dataset<Real> *d, const std::list<SubRangeInput *> &srl);

    //! destructor
    ~SubRangeInfo();

    /*! \brief Method to determine the indical limits which specify the
     *         portion of the sub-range within a single data block.
     *
     *  \param  block Block index (0,...) for which indical limits are
     *                to be returned.
     *  \param  max   Array containing the spatial dimensions of this block.
     *  \param  lo    Lower loop limits for each spatial dimension.
     *  \param  hi    Upper loop limits for each spatial dimension.
     *
     *  \return  Returns true if block intersects the sub-range,
     *           false otherwise.
     *
     *  \note  The max, low, and high arrays SHOULD NOT be deleted by the
     *         calling program.
     */
    bool GetBlockIndicies(int block, int *&max, int *&lo, int *&hi);

    //! Spatial dimesionality of the data
    int ndim;

    //! PFF dataset containing the grid information for the test.
    PFF_Dataset<Real> *ds;

    /*! \brief Array of bit-mapped indicators of the specified subrange
     *         bounds in each spatial direction.
     *
     *  If lowest bit (1) is set, then lower bound was supplied, if next
     *  lowest bit (2) is set, then upper bound was supplied. Legal values
     *  for these indicators are 0, 1, 2, and 3, although at least one must
     *  be nonzero.
     */
    unsigned char *modes;

    //! Array of lower bounds for each spatial dimension.
    Real *low;

    //! Array of upper bounds for each spatial dimension.
    Real *high;

    /*! \brief Work buffer used to temporarily hold the max, low, and high
     *         arrays returned by GetBlockIndicies() method.
     */
    int *work;
  };

  /*! \brief Method to initialize the test.
   *
   *  \li Obtains needed datasets from the Test_Environment object.
   *  \li Checks that datasets are available, the data types are the same,
   *      and that dataset numbers and/or name are consistant.
   */
  virtual void Initialize() throw(Test_Error);

  /*! \brief Method to determine if the shapes of the two datasets are
   *         conformal.
   *
   *  Requirements for conformality are:
   *
   *  \li The number of blocks match.
   *  \li The number of spatial dimensions match.
   *  \li The number of attribute (vector) dimensions match.
   *  \li For each block, the grid lengths for each spatial dimension match.
   */
  virtual void Check_Shape() const throw(Test_Error);

  /*! \brief Method to loop over all data, one datum at a time, for the
   *         dataset/s.
   *
   *   At each iteration, the Apply method of the metric object is called
   *   with the real values at that grid location. At the end, the Analyze
   *   method of the metric object is called to evaluate the metric.
   */
  virtual bool Analyze_Data();

  /*! \brief Method to parse keywords and option flags
   *
   *  It looks for legal keyword and/or option flag syntax. If it finds legal
   *  syntax, it first looks to see if the base class knows how to handle the
   *  keyword or flag, and processes that keyword. It keeps looping until it
   *  finds a keyword or flag it doesn't know about, in which case it returns
   *  the name of the keyword of flag, back to the calling program for it to
   *  process. In the case of any errors encountered, it throws and exception
   *  by calling Token_Stream::Parse_Error.
   *
   *  \param  keyword  Token object containing the keyword of flag returned.
   *
   *  \return Keyword status, one of: IS_KEYWORD, IS_FLAG, or IS_OTHER.
   */
  virtual Keyword_Status Parse_Keyword(Token &keyword);

  /*! \brief Method to parse metric-related keywords and option flags
   *
   *  This method should be called by the constructor for the most derived
   *  test class.
   */
  virtual void Parse_Metric_Keywords();

  /*! \brief Pure virtual method to set the name printed for the the test
   *         performed by this object.
   *
   *  This method should be called by the constructor for the most derived
   *  test class.
   */
  virtual void Set_Test_Name() = 0;

  /*! \brief Utility method used to recursively access a linear array
   *         that represents a multi-dimemsional block of data over a
   *         subrange of that data, and apply the metric associated with
   *         the test over that sub-range.
   *
   *  \param  order   On initial call, the number of spatial dimensions. On
   *                  recursion, the number of remaining spatial dimensions.
   *  \param  rbase   Base linear array containing multidimensional data.
   *  \param  rtest   Test linear array containing multidimensional data.
   *  \param  max     Array containing the spatial dimensions of this block.
   *  \param  low     Lower loop limits for each spatial dimension.
   *  \param  high    Upper loop limits for each spatial dimension.
   *  \param  offset  On initial call, this should be zero. On recursion,
   *                  this is the offset in the linear array for the current
   *                  level and index of the recursion.
   *
   *  \return  Returns the number of data values for which the test metric
   *           was applied.
   */
  int Apply_Recursive(int order, const Real *rbase, const Real *rtest,
                      const int *max, const int *low, const int *high,
                      int offset=0);

  //! Token_Stream object from which to parse input command.
  Token_Stream *token_stream;

  /*! \brief Test_Environment object that counts errors, provides PFF
   *         datasets from the "base" and/or "test" files, etc.
   */
  Test_Environment *env;

  //! The object that knows how to compute the desired metric from the data
  Generic_Metric *metric;

  //! Type of metric used for this test
  Generic_Metric::Metric_Type test_type;

  //! Keyword delimiters for parsing.
  std::string kw_delims;

  //! For parsing, characters that prefix option flags.
  std::string flag_chrs;

  /*! \brief If true, the test performed by this object requires data from a
   *         "test" file.
   */
  bool use_base;

  /*! \brief If true, the test performed by this object requires data from a
   *        "base" file.
   */
  bool use_test;

  //! Count of grid points iterated over by Data_Loop() method.
  int loop_count;

  //! Index of requested dataset within the PFF file/files.
  int ds_number;

  //! Title name of requested dataset.
  std::string ds_name;

  //! Pointer to PFF file object for the "base" PFF file.
  PFF_Dataset<Real> *base_ds;

  //! Pointer to PFF file object for the "test" PFF file.
  PFF_Dataset<Real> *test_ds;

  //! Data type of this dataset
  PFF_Dataset<Real>::PFF_DS_Type ds_type;

  //! Name of test -- must be set by derived class
  std::string test_name;

  //! List of input sub-range specifications associated with this test
  std::list<SubRangeInput *> sublist;

  //! Processed sub-range information for this test
  SubRangeInfo * sr_info;
};

/*! \brief An abstraction testing GRID data based on some metric, to be
 *         performed on the PFF datasets from the supplied "test" and "base"
 *         files.
 *
 *  This test object operates on the PFF dataset types:
 *    UF1, UF3, NG3, NI3, NF3, NV3, and NGD (w/ Nd>0).
 */
class Grid_Test : public Generic_Test
{
 public:
  //! \name Constructors.
  //@{
  /*! \brief Constructor that parses grid-specific part of PFFDIFF input line.
   *
   *  \param tok_stream      Token_Stream object to parse input command from.
   *  \param test_env        Test_Environment object that counts errors,
   *                         provides PFF datasets from the "base" and/or
   *                         "test" files, etc.
   *  \param type            Type of metric to be used
   *  \param keyword_delims  Keyword delimiters for parsing.
   *  \param flag_chars      For parsing, characters that prefix option flags.
   */
  Grid_Test(Token_Stream *tok_stream, Test_Environment* test_env, 
            Generic_Metric::Metric_Type type,
            const std::string &keyword_delims,
            const std::string &flag_chars="");

  /*! \brief Alternate constructor.
   *
   *  \param test_env  Test_Environment object that counts errors, provides PFF
   *                   datasets from the "base" and/or "test" files, etc.
   *  \param ds_number Index of dataset in file/s to supply data for test.
   *  \param type      Type of metric to be used
   *  \param lims      If nonzero, a pointer to the value/values to be used
   *                   to set the "success" limits on the metric. For MAXIMUM,
   *                   MEAN, and RMS metric types, one value, representing the
   *                   maximum deviation of the data in the base and test
   *                   files, must be provided. For the RANGE metric type, two
   *                   values, representing the minimum and maximum limits of
   *                   the data in the test file, must be provided. If zero, 
   *                   no limits are applied.
   *  \param dir       Index of coordinate direction over which to examine grid
   *                   data. If zero, all coordinate directions in dataset are
   *                   examined.
   *  \param blk       Index of single block in which to examine grid data.
   *                   If zero, all blocks in dataset are examined.
   */
  Grid_Test(Test_Environment* test_env, int ds_num,
            Generic_Metric::Metric_Type type, Real *lims = 0, int dir = 0,
            int blk = 0);
  //@}

  //! Destructor.
  virtual ~Grid_Test();

 private:
  /*! \brief Method to initialize the test.
   *
   *  \li It first calls Generic_Test::Initialize()
   *  \li Next, it checks to make sure that this test object supports the
   *      dataset type for the requested dataset.
   */
  virtual void Initialize() throw(Test_Error);

  /*! \brief Method to loop over all data, one datum at a time, for the
   *         dataset/s.
   *
   *   This deriviation knows how to traverse through a PFF dataset's grid
   *   data.
   *   At each iteration, the Apply method of the metric object is called
   *   with the real values at that grid location. At the end, the Analyze
   *   method of the metric object is called to evaluate the metric.
   */
  virtual bool Analyze_Data();

  /*! \brief Method to parse keywords and option flags
   *
   *  It looks for legal keyword and/or option flag syntax. If it finds legal
   *  syntax, it first looks to see if the base class knows how to handle the
   *  keyword or flag, and processes that keyword. It keeps looping until it
   *  finds a keyword or flag it doesn't know about, in which case it returns
   *  the name of the keyword of flag, back to the calling program for it to
   *  process. In the case of any errors encountered, it throws and exception
   *  by calling Token_Stream::Parse_Error.
   *
   *  \param  keyword  Token object containing the keyword of flag returned.
   *
   *  \return Keyword status, one of: IS_KEYWORD, IS_FLAG, or IS_OTHER.
   */
  virtual Keyword_Status Parse_Keyword(Token &keyword);

  /*! \brief Virtual method to set the name printed for the the test
   *         performed by this object.
   *
   *  This method should be called by the constructor for the most derived
   *  test class.
   */
  virtual void Set_Test_Name();

  //!  Block to be examined. If zero, all blocks are examined.
  int block;

  //!  Coord. direction to be examined. If zero, all directionss are examined.
  int direction;
};

/*! \brief An abstraction testing attrbute data based on some metric, to be
 *         performed on the PFF datasets from the supplied "test" and "base"
 *         files.
 *
 *  This test object operates on the PFF dataset types:
 *    UF1, UF3, NF3, NV3, and NGD (w/ Nd>0 and Na>0).
 */
class Attr_Test : public Generic_Test
{
 public:
  //! \name Constructors.
  //@{
  /*! \brief Constructor that parses attribute-specific part of PFFDIFF input
   *         line.
   *
   *  \param tok_stream      Token_Stream object to parse input command from.
   *  \param test_env        Test_Environment object that counts errors,
   *                         provides PFF datasets from the "base" and/or
   *                         "test" files, etc.
   *  \param type            Type of metric to be used
   *  \param keyword_delims  Keyword delimiters for parsing.
   *  \param flag_chars      For parsing, characters that prefix option flags.
   */
  Attr_Test(Token_Stream *tok_stream, Test_Environment* test_env, 
            Generic_Metric::Metric_Type type,
            const std::string &keyword_delims,
            const std::string &flag_chars="");

  /*! \brief Alternate constructor.
   *
   *  \param test_env  Test_Environment object that counts errors, provides PFF
   *                   datasets from the "base" and/or "test" files, etc.
   *  \param ds_number Index of dataset in file/s to supply data for test.
   *  \param type      Type of metric to be used
   *  \param lims      If nonzero, a pointer to the value/values to be used
   *                   to set the "success" limits on the metric. For MAXIMUM,
   *                   MEAN, and RMS metric types, one value, representing the
   *                   maximum deviation of the data in the base and test
   *                   files, must be provided. For the RANGE metric type, two
   *                   values, representing the minimum and maximum limits of
   *                   the data in the test file, must be provided. If zero, 
   *                   no limits are applied.
   *  \param dir       Index of coordinate direction over which to examine grid
   *                   data. If zero, all coordinate directions in dataset are
   *                   examined.
   *  \param blk       Index of single block in which to examine grid data.
   *                   If zero, all blocks in dataset are examined.
   *
   *  \note Currently, this class only implements dir = blk = 0;
   */
  Attr_Test(Test_Environment* test_env, int ds_num,
            Generic_Metric::Metric_Type type, Real *lims = 0, int dir = 0,
            int blk = 0);
  //@}

  //! Destructor.
  virtual ~Attr_Test();

 private:
  /*! \brief Method to initialize the test.
   *
   *  \li It first calls Generic_Test::Initialize()
   *  \li Next, it checks to make sure that this test object supports the
   *      dataset type for the requested dataset.
   */
  virtual void Initialize() throw(Test_Error);

  /*! \brief Method to loop over all data, one datum at a time, for the
   *         dataset/s.
   *
   *   This deriviation knows how to traverse through a PFF dataset's attribute
   *   data.
   *   At each iteration, the Apply method of the metric object is called
   *   with the real values at that grid location. At the end, the Analyze
   *   method of the metric object is called to evaluate the metric.
   */
  virtual bool Analyze_Data();

  /*! \brief Method to parse keywords and option flags
   *
   *  It looks for legal keyword and/or option flag syntax. If it finds legal
   *  syntax, it first looks to see if the base class knows how to handle the
   *  keyword or flag, and processes that keyword. It keeps looping until it
   *  finds a keyword or flag it doesn't know about, in which case it returns
   *  the name of the keyword of flag, back to the calling program for it to
   *  process. In the case of any errors encountered, it throws and exception
   *  by calling Token_Stream::Parse_Error.
   *
   *  \param  keyword  Token object containing the keyword of flag returned.
   *
   *  \return Keyword status, one of: IS_KEYWORD, IS_FLAG, or IS_OTHER.
   */
  virtual Keyword_Status Parse_Keyword(Token &keyword);

  /*! \brief Virtual method to set the name printed for the the test
   *         performed by this object.
   *
   *  This method should be called by the constructor for the most derived
   *  test class.
   */
  virtual void Set_Test_Name();

  /*! \brief Block to be examined. If zero, all blocks are examined.
   *
   *  \note This value is not currently used. All blocks are examined.
   */
  int block;

  /*! \brief Coordinate direction to be examined. If zero, all directions are
   *         examined.
   *
   *  \note This value is not currently used. All directions are examined.
   */
  int direction;
};

#endif
