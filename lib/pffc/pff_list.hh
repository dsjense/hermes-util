/*
-------------------------------------------------------------------------------
    PFF I/O utility:   pff_list.hh
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
  
-------------------------------------------------------------------------------
*/

#ifndef PFF_LIST_HH
#define PFF_LIST_HH

/* Wrap list_defs.h in PFF namespace */

namespace PFF {
# define USING_PFF_NAMESPACE
# include "list_defs.h"
# undef USING_PFF_NAMESPACE
}

#endif
