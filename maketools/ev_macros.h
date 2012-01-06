/* C_Groups hermes
 * $Id$
 * 
 * Copyright (2008) Sandia Corporation. Under the terms of
 * Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
 * Government retains certain rights in this software.
 * 
 * Hermes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 * 
 * Hermes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General
 * Public License along with Hermes.  If not, see
 * <http://www.gnu.org/licenses/>.
 * 
 */
#ifndef EV_MACROS_H
#define EV_MACROS_H

void EV_Add(const char *name);
int EV_Init();
char *EV_Subst_Macro(char *string);

#endif
