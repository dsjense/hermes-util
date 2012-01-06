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

/*! \file metrics.h
 *  \brief Interface definitions for the Generic_Metric base class and the
 *         metric classes derived from it.
 */

#ifndef METRICSH
#define METRICSH 1

#include "code_types.h"
#include "test_error.h"

class Generic_Test;
class Token;

/*! \brief An abstract class that defines the inteface for an object that
 *         knows how to process individual pairs of data of PFF datasets
 *         from supplied "test" and/or "base" files to determine a specific
 *         metric.
 */
class Generic_Metric
{
 public:
  //! Enumeration of supported test metric types
  typedef enum {
    //! Unknown type
    UNKNOWN = 0,
    //! Find maximum deviation
    MAXIMUM,
    //! Find mean deviation
    MEAN,
    //! Find Root-Mean-Squared (RMS) deviation
    RMS,
    //! Find the range of the data
    RANGE
  } Metric_Type;
  /*! \brief Constructor.
   *
   *  \param test   The parent test object of this metric.
   */
  Generic_Metric(const Generic_Test *test);

  //! Destructor.
  virtual ~Generic_Metric();


  /*! \brief Pure virtual method for performing the necessary
   *         calculations, appropriate to the metric encapsulated in
   *         the derived class, at a single spatial location.
   *
   *  Deriving classes should perform whatever operation is
   *  appropriate to the test.

   *
   *  \param base_val  Data value from base dataset.
   *  \param test_val  Data value from test dataset.
   */
  virtual void Apply(Real base_val, Real test_val) = 0;

  /*! \brief Pure virtual method that analyzes the data for the entire
   *         spatial grid.
   *
   *  Deriving classes should perform whatever is appropriate to determine
   *  the final metrics and status (success or failure) of the test.
   *
   *  \return Returns true upon the successful completion of the test.
   */
  virtual bool Analyze(int mean_count, const char *extra = 0) = 0;

  /*! \brief Static method that processes an input stream command
   *         token to determine which, if any, of the known metric
   *         types it is requesting.
   *
   *  \param token  The input stream token to be processed.
   *
   *  \return Returns the value from the Metric_Type enumeration
   *          representing the requested metric, or UNKNOWN if the
   *          token does not represent a known test.
   */
  static Metric_Type Parse_Metric_Type(const Token &token);

 protected:
  //! The parent test object of this metric.
  const Generic_Test *parent_test;

  /*! \brief Flag indicating the first time that the Apply method is called
   *         for a specific analysis.
   */
  bool first_time;

  //! Minimum value of data in the analysis range "base" dataset.
  Real range_min;

  //! Maximum value of data in the analysis range "base" dataset.
  Real range_max;
};


/*! \brief An abstraction for a Maximum Deviation metric.
 *
 *  This test object computes the maximum deviation of the "test" data from
 *  the "base" data, over a range of data defined by the parent test. This
 *  deviation is normalized by the overall range of the "base" data.
 */
class Maximum_Metric : public Generic_Metric
{
 public:
  /*! \brief Constructor.
   *
   *  \param test             The parent test object of this metric.
   *  \param deviation_limit  Maximum deviation relative to range of data.
   *
   *  \note  If deviation_limit <= 0, no limit test is performed, and only
   *         the measured maximum deviation is reported.
   *                          
   */
  Maximum_Metric(const Generic_Test *test, Real deviation_limit);

  //! Destructor.
  virtual ~Maximum_Metric();

  /*! \brief Performs the necessary calculations at a single spatial location
   *         for determining the normalized maximum deviation.
   *
   *  \param base_val  Data value from base dataset.
   *  \param test_val  Data value from test dataset.
   *
   *  \li  Checks to see if supplied "base" datum is the maximum or minumum of
   *       data encountered so far, and sets range_max or range_min as
   *       appropriate.
   *  \li  Checks to see if difference between the "base" and "test" data
   *       {abs(base_val - test_val)} exceeds the current maximum, and if so,
   *       sets it to this new value.
   */
  virtual void Apply(Real base_val, Real test_val);

  /*! \brief Analyzes the data collected by Apply() for the entire
   *         spatial grid.
   *
   *  \param mean_count  A count of the data pairs examined by Apply().
   *  \param extra       If non-zero, a character string to be inserted
   *                     between the printed message's header and the test
   *                     result.
   *
   *  \li  Computes normalized maximum deviation.
   *  \li  If limit has been provided, determines test status.
   *  \li  Prints a message to standard out summarizing result of test.
   *
   *  \return Returns true upon the successful completion of the test.
   */
  virtual bool Analyze(int mean_count, const char *extra = 0)
    throw(Test_Error);

 private:
  //! \brief Maximum value of normalized deviation allowed for the test's
  //!        status to be "pass". If negative, no limit has been provided.
  Real max_deviation_limit;

  //! Actual value of the maximum normalized deviation found for this test.
  Real max_deviation;
};


/*! \brief An abstraction for a Mean Deviation metric.
 *
 *  This test object computes the mean deviation of the "test" data from
 *  the "base" data, over a range of data defined by the parent test. This
 *  deviation is normalized by the overall range of the "base" data.
 */
class Mean_Metric : public Generic_Metric
{
 public:
  /*! \brief Constructor.
   *
   *  \param test             The parent test object of this metric.
   *  \param deviation_limit  Mean deviation relative to range of data.
   *
   *  \note  If deviation_limit <= 0, no limit test is performed, and only
   *         the measured mean deviation is reported.
   *                          
   */
  Mean_Metric(const Generic_Test *test, Real deviation_limit);

  //! Destructor.
  virtual ~Mean_Metric();

  /*! \brief Performs the necessary calculations at a single spatial location
   *         for determining the normalized mean deviation.
   *
   *  \param base_val  Data value from base dataset.
   *  \param test_val  Data value from test dataset.
   *
   *  \li  Checks to see if supplied "base" datum is the maximum or minumum of
   *       data encountered so far, and sets range_max or range_min as
   *       appropriate.
   *  \li  Adds the difference between the "base" and "test" data
   *       (base_val - test_val) to the running total (mean_deviation).
   */
  virtual void Apply(Real base_val, Real test_val);

  /*! \brief Analyzes the data collected by Apply() for the entire
   *         spatial grid.
   *
   *  \param mean_count  A count of the data pairs examined by Apply().
   *  \param extra       If non-zero, a character string to be inserted
   *                     between the printed message's header and the test
   *                     result.
   *
   *  \li  Computes normalized mean deviation (divides running total by
   *       loop_count to get mean, then normalize to range of "base" data).
   *  \li  If limit has been provided, determines test status.
   *  \li  Prints a message to standard out summarizing result of test.
   *
   *  \return Returns true upon the successful completion of the test.
   */
  virtual bool Analyze(int mean_count, const char *extra = 0)
    throw (Test_Error);

 private:
  //! \brief Maximum value of normalized mean deviation allowed for the test's
  //!        status to be "pass". If negative, no limit has been provided.
  Real mean_deviation_limit;

  /*! \brief Actual value of the normalized mean deviation found for this test.
   *
   *  Also used by Apply() to store running total of the deviation.
   */
  Real mean_deviation;
};


/*! \brief An abstraction for a RMS (Root-Mean-Squared) Deviation metric.
 *
 *  This test object computes the RMS deviation of the "test" data from
 *  the "base" data, over a range of data defined by the parent test. This
 *  deviation is normalized by the overall range of the "base" data.
 */
class RMS_Metric : public Generic_Metric
{
 public:
  /*! \brief Constructor.
   *
   *  \param test             The parent test object of this metric.
   *  \param deviation_limit  RMS deviation relative to range of data.
   *
   *  \note  If deviation_limit <= 0, no limit test is performed, and only
   *         the measured RMS deviation is reported.
   *                          
   */
  RMS_Metric(const Generic_Test *test, Real deviation_limit);

  //! Destructor.
  virtual ~RMS_Metric();

  /*! \brief Performs the necessary calculations at a single spatial location
   *         for determining the normalized RMS deviation.
   *
   *  \param base_val  Data value from base dataset.
   *  \param test_val  Data value from test dataset.
   *
   *  \li  Checks to see if supplied "base" datum is the maximum or minumum of
   *       data encountered so far, and sets range_max or range_min as
   *       appropriate.
   *  \li  Adds the square of the difference between the "base" and "test" data
   *       {(base_val - test_val)^2} to the running total (rms_deviation).
   */
  virtual void Apply(Real base_val, Real test_val);

  /*! \brief Analyzes the data collected by Apply() for the entire
   *         spatial grid.
   *
   *  \param mean_count  A count of the data pairs examined by Apply().
   *  \param extra       If non-zero, a character string to be inserted
   *                     between the printed message's header and the test
   *                     result.
   *
   *  \li  Computes normalized RMS deviation (divides running total by
   *       loop_count, then takes the root of that value to get RMS value,
   *       then normalizes to range of "base" data).
   *  \li  If limit has been provided, determines test status.
   *  \li  Prints a message to standard out summarizing result of test.
   *
   *  \return Returns true upon the successful completion of the test.
   */
  virtual bool Analyze(int mean_count, const char *extra = 0);

 private:
  /*! \brief Maximum value of normalized RMS deviation allowed for the test's
   *         status to be "pass".
   *
   *  If negative, no limit has been provided.
   */
  Real rms_deviation_limit;

  /*! \brief Actual value of the normalized RMS deviation found for this test.
   *
   *  Also used by Apply() to store running total of the deviation^2.
   */
  Real rms_deviation;
};


/*! \brief An abstraction for a data "Range" metric.
 *
 *  This test object computes the range of the "test" data over all spatial 
 *  grid locations. For the test to pass, the computed range of the "test" data
 *  must fall within the specified range limits.
 *
 *  \note  Only data from the "test" dataset is used in computing this metric.
 */
class Range_Metric : public Generic_Metric
{
 public:
  /*! \brief Constructor.
   *
   *  \param test       The parent test object of this metric.
   *  \param min_limit  Minimum limit for range test
   *  \param max_limit  Maximum limit for range test
   *
   *  \note  If both min_limit and max_limit = 0, no limit test is performed,
   *         and only the measured range of the data is reported.
   *                          
   */
  Range_Metric(const Generic_Test *test, Real min_limit, Real max_limit);

  //! Destructor.
  virtual ~Range_Metric();

  /*! \brief Performs the necessary calculations at a single spatial location
   *         for determining the range of the "test" data.
   *
   *  \param base_val  Data value from base dataset.
   *  \param test_val  Data value from test dataset.
   *
   *  \li  Checks to see if supplied "test" datum is the maximum or minumum of
   *       data encountered so far, and sets range_max or range_min as
   *       appropriate.
   *
   *  \note Only data from the "test" dataset (test_val) is used.
   */
  virtual void Apply(Real base_val, Real test_val);

  /*! \brief Analyzes the data collected by Apply() for the entire
   *         spatial grid.
   *
   *  \param mean_count  A count of the data pairs examined by Apply().
   *  \param extra       If non-zero, a character string to be inserted
   *                     between the printed message's header and the test
   *                     result.
   *
   *  \li  If range limits have been provided, determines test status.
   *  \li  Prints a message to standard out summarizing result of test.
   *
   *  \return Returns true upon the successful completion of the test.
   */
  virtual bool Analyze(int mean_count, const char *extra = 0);

 private:
  //! Minimum range limit for successful completion of the test.
  Real range_min_limit;

  //! Maximum range limit for successful completion of the test.
  Real range_max_limit;
};

#endif
