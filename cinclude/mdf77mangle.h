#ifndef MD_F77_MANGLE__H
#define MD_F77_MANGLE__H

/*
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
   
C_Groups @(#)
   ----------------------------------------------------------------------------
*/

/*! \file mdf77mangle.h
 *  \brief File containing macros to properly decorate C functions to be
 *         callable from Fortran.
 *
 *   These definitions will select:
 *   \li (L) lower case or (U) upper case mangling,
 *   \li with (mn) m-leading underscores and n-trailing underscores.
 *
 *   For example, L01 is lower case with one trailing underscore.
 *
 *  \li HU_F77_FUNC is used for function names NOT containing underscores
 *  \li HU_F77_FUNC_WITH_UNDERSCORES is used for function names containing
 *      underscores
 *  \li HU_F77_FUNC_ is shorthand for HU_F77_FUNC_WITH_UNDERSCORES

 *  \note FORTRAN is case insensitive
 */

/*! \brief More informatively-named alias for HU_F77_FUNC_ macro */
#define HU_F77_FUNC_WITH_UNDERSCORES HU_F77_FUNC_

/*
  all combinations seem to have the same upper or lower case
  and no leading underscores (first digit 0).
  so here are the short cut mapping: Xij  -->  X0i_X0j
*/

/*
 * More combinations can be added if
 * they are required by any compiler 
 */

#if defined( HU_F77_MANGLING_L00_L00 ) || defined( HU_F77_MANGLING_L00  )
/*! \brief Macro for function names that DO NOT contain underscores
 *         (value for no-decoration case is shown) */
#  define HU_F77_FUNC(name,NAME) name
/*! \brief Macro for function names that DO contain underscores
 *         (value for no-decoration case is shown) */
#  define HU_F77_FUNC_(name,NAME) name
#elif defined( HU_F77_MANGLING_L00_L01 ) || defined( HU_F77_MANGLING_L01  )
#  define HU_F77_FUNC(name,NAME) name
#  define HU_F77_FUNC_(name,NAME) name##_
#elif defined( HU_F77_MANGLING_L00_L02 ) || defined( HU_F77_MANGLING_L02  )
#  define HU_F77_FUNC(name,NAME) name
#  define HU_F77_FUNC_(name,NAME) name##__
#elif defined( HU_F77_MANGLING_L01_L00 ) || defined( HU_F77_MANGLING_L10  )
#  define HU_F77_FUNC(name,NAME) name##_
#  define HU_F77_FUNC_(name,NAME) name
#elif defined( HU_F77_MANGLING_L01_L01 ) || defined( HU_F77_MANGLING_L11  )
#  define HU_F77_FUNC(name,NAME) name##_
#  define HU_F77_FUNC_(name,NAME) name##_
#elif defined( HU_F77_MANGLING_L01_L02 ) || defined( HU_F77_MANGLING_L12  )
#  define HU_F77_FUNC(name,NAME) name##_
#  define HU_F77_FUNC_(name,NAME) name##__
#elif defined( HU_F77_MANGLING_U00_U00 ) || defined( HU_F77_MANGLING_U00  )
#  define HU_F77_FUNC(name,NAME) NAME
#  define HU_F77_FUNC_(name,NAME) NAME
#else
/*! \cond NEVER */
   error "*** F77 name mangling ***"
   error "  mangling has not been specified, eg, HU_F77_MANGLING_L11"
   error "  or a new case must be added here."
#  define HU_F77_FUNC(name,NAME) name
#  define HU_F77_FUNC_(name,NAME) name
/*! \endcond */
#endif

#endif /* MD_F77_MANGLE__H */
