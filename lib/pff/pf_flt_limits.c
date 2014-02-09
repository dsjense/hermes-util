/* **********************************************************************
    PFF MD parameter initialization
    C_Groups @(#)
    $Id$
    
    Copyright (2008) Sandia Corporation. Under the terms of
    Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
    Government retains certain rights in this software.
    
    Hermes is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of
    the License, or (at your option) any later version.
    
    Hermes is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General
    Public License along with Hermes.  If not, see
    <http://www.gnu.org/licenses/>.
    
   ******************************************************************* */

#include <float.h>

/* function name mangling for F77 linkage */
#include "mdf77mangle.h"
#if !defined(HU_F77_MANGLING_L00)
# define pf_flt_limits      HU_F77_FUNC_( pf_flt_limits, PF_FLT_LIMITS )
#endif

/*! \brief Initializes MD parameters for Fortran floating point limits.
 *
 *  \note this needs to be called in PFUOPN, and in any other module that
 *        includes pfmdpr.inc and can be called without an open FID (like
 *        pfui2f)
 *
 *  \param pf2max Maximum power-of-2 exponent that is in range for this
 *                machine's default REAL data type
 *  \param pf2min Minimum power-of-2 exponent that is in range for this
 *                machine's default REAL data type
 *  \param pfrmin Minimum floating point number that can be inverted 
 *                without floating point overflow
 *  \param pfrmax Maximum floating point number that can be represented
 *                without floating point overflow
 */
void pf_flt_limits( int *pf2max, int *pf2min, float *pfrmin, float *pfrmax )
{
  *pf2max = FLT_MAX_EXP - 1;
  *pf2min = FLT_MIN_EXP;
  *pfrmax = FLT_MAX;
  *pfrmin = 1.0/FLT_MAX;
  if (FLT_MIN > *pfrmin ) *pfrmin = FLT_MIN;
}
