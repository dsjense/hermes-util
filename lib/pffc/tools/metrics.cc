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
//  C_Groups pffdiff

/*! \file metrics.cc
 *  \brief Implementation for the Generic_Metric base class and the metric
 *         classes derived from it.
 */
 
#include <algorithm>
#include <iostream>
#include <cmath>
#include "metrics.h"
#include "comparators.h"
#include "token.h"

using std::fabs;
using std::sqrt;

/*****************************************************************************/
Generic_Metric::Generic_Metric(const Generic_Test *test)
  : parent_test(test), first_time(true)
/*****************************************************************************/
{
}

/*****************************************************************************/
Generic_Metric::~Generic_Metric()
/*****************************************************************************/
{
}

/*****************************************************************************/
Generic_Metric::Metric_Type Generic_Metric::Parse_Metric_Type
(const Token &token)
/*****************************************************************************/
{
  Metric_Type test_type = UNKNOWN;

  if ( token == "MAX*_DEVIATION" ) test_type = MAXIMUM;
  else if ( token == "MEAN*_DEVIATION" ) test_type = MEAN;
  else if ( token == "RMS*_DEVIATION" ) test_type = RMS;
  else if ( token == "RAN*GE_DEVIATION" ) test_type = RANGE;

  return test_type;
}

/*****************************************************************************/
Maximum_Metric::Maximum_Metric(const Generic_Test *test, Real deviation_limit)
  : Generic_Metric(test), max_deviation_limit(deviation_limit),
    max_deviation(0.0)
/*****************************************************************************/
{
}

/*****************************************************************************/
Maximum_Metric::~Maximum_Metric() { }
/*****************************************************************************/

/*****************************************************************************/
void Maximum_Metric::Apply( Real base_val, Real test_val )
/*****************************************************************************/
{
  if ( first_time ) {
    range_max = base_val;
    range_min = base_val;
    first_time = false;
  }
  else {
    range_max = std::max(range_max, base_val);
    range_min = std::min(range_min, base_val);
  }
  max_deviation = std::max(max_deviation, Real(fabs(base_val - test_val)));    
}

/*****************************************************************************/
bool Maximum_Metric::Analyze(int mean_count, const char *extra)
  throw(Test_Error)
/*****************************************************************************/
{
  if ( first_time ) throw Test_Error("Dataset contains no data");

  Real drange = range_max - range_min;
  if ( drange != 0.0 ) max_deviation /= drange;

  bool success = true;

  if ( max_deviation_limit > 0.0 ) {
    success = max_deviation < max_deviation_limit;
    parent_test->Print_Result_Header(&success);
    if ( extra ) std::cout << " (" << extra << ")";
    std::cout << " -- Deviation = " << max_deviation
              << ", Limit = " << max_deviation_limit << std::endl;
  }
  else {
    parent_test->Print_Result_Header(0);
    if ( extra ) std::cout << " (" << extra << ")";
    std::cout << "  Deviation = " << max_deviation << std::endl;
  }
  first_time = true; // in case we use this metric for more than one dataset
  max_deviation = 0.0;
  return success;
}

/*****************************************************************************/
Mean_Metric::Mean_Metric(const Generic_Test *test, Real deviation_limit)
  : Generic_Metric(test), mean_deviation_limit(deviation_limit),
    mean_deviation(0.0)
/*****************************************************************************/
{
}

/*****************************************************************************/
Mean_Metric::~Mean_Metric() { }
/*****************************************************************************/

/*****************************************************************************/
void Mean_Metric::Apply( Real base_val, Real test_val )
/*****************************************************************************/
{
  if ( first_time ) {
    range_max = base_val;
    range_min = base_val;
    first_time = false;
  }
  else {
    range_max = std::max(range_max, base_val);
    range_min = std::min(range_min, base_val);
  }
  mean_deviation += (base_val - test_val);    
}

/*****************************************************************************/
bool Mean_Metric::Analyze(int mean_count, const char *extra) throw (Test_Error)
/*****************************************************************************/
{
  if ( first_time ) throw Test_Error("Dataset contains no data");

  Real drange = range_max - range_min;
  mean_deviation = fabs(mean_deviation);
  mean_deviation /= Real(mean_count);
  if ( drange != 0.0 ) mean_deviation /= drange;

  bool success = true;

  if ( mean_deviation_limit > 0.0 ) {
    success = mean_deviation < mean_deviation_limit;
    parent_test->Print_Result_Header(&success);
    if ( extra ) std::cout << " (" << extra << ")";
    std::cout << " -- Deviation = " << mean_deviation
              << ", Limit = " << mean_deviation_limit << std::endl;
  }
  else {
    parent_test->Print_Result_Header(0);
    if ( extra ) std::cout << " (" << extra << ")";
    std::cout << "  Deviation = " << mean_deviation << std::endl;
  }
  first_time = true; // in case we use this metric for more than one dataset
  mean_deviation = 0.0;
  return success;
}

/*****************************************************************************/
RMS_Metric::RMS_Metric(const Generic_Test *test, Real deviation_limit)
  : Generic_Metric(test), rms_deviation_limit(deviation_limit),
    rms_deviation(0.0)
/*****************************************************************************/
{
}
  
/*****************************************************************************/
RMS_Metric::~RMS_Metric() { }
/*****************************************************************************/

/*****************************************************************************/
void RMS_Metric::Apply( Real base_val, Real test_val )
/*****************************************************************************/
{
  if ( first_time ) {
    range_max = base_val;
    range_min = base_val;
    first_time = false;
  }
  else {
    range_max = std::max(range_max, base_val);
    range_min = std::min(range_min, base_val);
  }
  Real delta = base_val - test_val;
  rms_deviation += (delta*delta);    
}

/*****************************************************************************/
bool RMS_Metric::Analyze(int mean_count, const char *extra)
/*****************************************************************************/
{
  if ( first_time ) throw Test_Error("Dataset contains no data");

  Real drange = range_max - range_min;
  rms_deviation /= Real(mean_count);
  rms_deviation = sqrt(rms_deviation);
  if ( drange != 0.0 ) rms_deviation /= drange;

  bool success = true;

  if ( rms_deviation_limit > 0.0 ) {
    success = rms_deviation < rms_deviation_limit;
    parent_test->Print_Result_Header(&success);
    if ( extra ) std::cout << " (" << extra << ")";
    std::cout << " -- Deviation = " << rms_deviation
              << ", Limit = " << rms_deviation_limit << std::endl;
  }
  else {
    parent_test->Print_Result_Header(0);
    if ( extra ) std::cout << " (" << extra << ")";
    std::cout << "  Deviation = " << rms_deviation << std::endl;
  }
  first_time = true; // in case we use this metric for more than one dataset
  rms_deviation = 0.0;
  return success;
}
  
/*****************************************************************************/
Range_Metric::Range_Metric(const Generic_Test *test, Real min_limit,
                           Real max_limit)
  : Generic_Metric(test), range_min_limit(min_limit),
    range_max_limit(max_limit)
/*****************************************************************************/
{
}

/*****************************************************************************/
Range_Metric::~Range_Metric() { }
/*****************************************************************************/

/*****************************************************************************/
void Range_Metric::Apply( Real base_val, Real test_val )
/*****************************************************************************/
{
  if ( first_time ) {
    range_max = test_val;
    range_min = test_val;
    first_time = false;
  }
  else {
    range_max = std::max(range_max, test_val);
    range_min = std::min(range_min, test_val);
  }
}

/*****************************************************************************/
bool Range_Metric::Analyze(int mean_count, const char *extra)
/*****************************************************************************/
{
  if ( first_time ) throw Test_Error("Dataset contains no data");

  bool success = true;

  if ( range_min_limit != 0.0 || range_max_limit != 0.0 ) {
    success = range_min >= range_min_limit && range_max <= range_max_limit;
    parent_test->Print_Result_Header(&success);
    if ( extra ) std::cout << " (" << extra << ")";
    std::cout << " -- Range = [" << range_min << ", " << range_max << "]"
              << ", Limits = [" << range_min_limit << ", " << range_max_limit
              << "]" << std::endl;
  }
  else {
    parent_test->Print_Result_Header(0);
    if ( extra ) std::cout << " (" << extra << ")";
    std::cout << "  Range = [" << range_min << ", " << range_max << "]"
              << std::endl;
  }
  first_time = true; // in case we use this metric for more than one dataset
  return success;
}
