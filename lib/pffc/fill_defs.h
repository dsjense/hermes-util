/*
-------------------------------------------------------------------------------
    PFF I/O utility:   fill_defs.h
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

#ifndef FILL_DEFS_H
#define FILL_DEFS_H

# include "ds_structs.h"

#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif

#if defined(__STDC__) || defined(__cplusplus)

  EXTERN void pf_fill_nblock ( PFFds_nonuniform *nonuniform, int iblk, 
                               int nspare, int alloc_method, int *spare, 
                               long *nx, float **x, int goff10, void **data, 
                               int foff10, char **glabel, char **dlabel, 
                               char *blabel, int new_labs, int new_arrs, 
                               int *ierr );
  EXTERN void pf_fill_ublock ( PFFds_uniform *uniform, int iblk, int nspare, 
                               int alloc_method, int *spare, long *nx, 
                               float *x0, float *dx, int goff10, float **data, 
                               int foff10, char **glabel, char **dlabel, 
                               char *blabel, int new_labs, int new_arrs, 
                               int *ierr );

# else

  extern void pf_fill_nblock ();
  extern void pf_fill_ublock ();

# endif

#endif
