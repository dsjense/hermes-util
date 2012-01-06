/*
-------------------------------------------------------------------------------
    Machine-dependent utilities:  mdutil.h
-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------

*/

/* function name mangling for F77 linkage */
#include "mdf77mangle.h"
#if !defined(HU_F77_MANGLING_L00)
#define  cacces HU_F77_FUNC(  cacces,  CACCES )
#define  cdtim  HU_F77_FUNC(  cdtim ,  CDTIM  )
#define  cexit  HU_F77_FUNC(  cexit ,  CEXIT  )
#define  cgeten HU_F77_FUNC(  cgeten,  CGETEN )
#define  cgettm HU_F77_FUNC(  cgettm,  CGETTM )
#define  cgpwnm HU_F77_FUNC(  cgpwnm,  CGPWNM )
#define  cputen HU_F77_FUNC(  cputen,  CPUTEN )
#define  csystm HU_F77_FUNC(  csystm,  CSYSTM )
#define  cvhelp HU_F77_FUNC(  cvhelp,  CVHELP )
#endif

