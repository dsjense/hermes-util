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

/*! \file test_error.h
 *  \brief Interface definitions for the Test_Error base class and the test
 *         classes derived from it.
 */

#ifndef TEST_ERRORH
#define TEST_ERRORH 1
 
#include <string>
#include <stdexcept>

//! \brief An exception class, derived from std::runtime_error, for errors
//!        encountered by Generic_Test::Run_Test() (and any derived virtual
//!        functions it calls)
struct Test_Error : public std::runtime_error
{
  /*! \brief Constructor.
   *
   *  \param what  A string that describes the error.
   *  \param more  A second, optional, string for describing the error.
   */
  Test_Error(const std::string& what,const std::string& more ="" )
    : std::runtime_error(what), extra(more)
  {
    what_string = std::runtime_error::what() + std::string(":  ") + extra;
  }

  //! Destructor
  virtual ~Test_Error() throw() { }

  /*! \brief virtual operation to override std::runtime_error's what() method.
   *
   *  \return The string obtained by appending the "extra" attribute to the
   *          string returned by std::runtime_error::what().
   */
  virtual const char* what() const throw()
  {
    return what_string.c_str();
  }
 private:
  /*! \brief string to hold the output of the what method
   *
   *  It is used so the c_string returned by the "what" method will not be
   *  deallocated.
   */
  std::string what_string;

  /*! \brief Extra string of error information
   *
   *  It will be appended to the string returned by std::runtime_error::what()
   *  and then returned by Test_Error::what().
   */
  std::string extra;
};

#endif
