/*
-------------------------------------------------------------------------------
    HERMES NAMESERVER software -- C interface definitions (nserver_c.h)
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

#ifndef NSERVER_C_H
#define NSERVER_C_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#if defined(__STDC__) || defined(__cplusplus)

  EXTERN char   *nsc_getname    ( int handle );
  EXTERN int     nsc_putname    ( const char *name );

#else

  extern char   *nsc_getname    ();
  extern int     nsc_putname    ();

#endif

#endif
