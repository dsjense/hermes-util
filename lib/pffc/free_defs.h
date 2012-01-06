/*
-------------------------------------------------------------------------------
    PFF I/O utility:   free_defs.h
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

#ifndef FREE_DEFS_H
#define FREE_DEFS_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#if defined(__STDC__) || defined(__cplusplus)

  EXTERN void   pf_free_dirlist    ( PFFdir *dir,  int *ierr );
  EXTERN void   pf_free_header     ( PFFhead *head, int *ierr );
  EXTERN void   pf_free_ds         ( PFFds_any *ds, int *ierr );
  EXTERN void   pf_free_dir        ( PFFds_any *dir, int *ierr );
  EXTERN void   pf_free_ifl        ( PFFds_any *ifl, int *ierr );
  EXTERN void   pf_free_nonuniform ( PFFds_any *nonuniform, int *ierr );
  EXTERN void   pf_free_uniform    ( PFFds_any *uniform, int *ierr );
  EXTERN void   pf_free_vertex     ( PFFds_any *vertex, int *ierr );

#else

  extern void   pf_free_dirlist    ();
  extern void   pf_free_header     ();
  extern void   pf_free_ds         ();
  extern void   pf_free_dir        ();
  extern void   pf_free_ifl        ();
  extern void   pf_free_nonuniform ();
  extern void   pf_free_uniform    ();
  extern void   pf_free_vertex     ();

#endif

#endif
