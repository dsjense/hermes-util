/*
-------------------------------------------------------------------------------
    PFF I/O utility:   bld_defs.h
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

#ifndef BLD_DEFS_H
#define BLD_DEFS_H

# include "ds_structs.h"

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#if defined(__STDC__) || defined(__cplusplus)

  EXTERN PFFds_dir        *pf_bld_dir
                             ( PFFhead *head, long offset, int new_head, 
                               int *ierr );
  EXTERN PFFhead          *pf_bld_header
                             ( int raw, int app, char *title, char *type, 
                               long len, int ver, int new_strs, int *ierr );
  EXTERN PFFds_ifl        *pf_bld_ifl
                             ( int app, char *title, char *type, long ni, 
                               long nfl, long nf, int *iarr, float *flist, 
                               int *floff, float *farr, int faoff, 
                               int new_strs, int new_arrs, int *ierr );
  EXTERN PFFds_nonuniform *pf_bld_nonuniform
                             ( int app, char *title, char *type, 
                               int pf_ds_type, int dims, int dimd, int nblk, 
                               int new_strs, int *ierr );
  EXTERN PFFds_uniform    *pf_bld_uniform
                             ( int app, char *title, char *type, int dims,
                               int dimd, int nblk, int new_strs, int *ierr );
  EXTERN PFFds_vertex     *pf_bld_vertex
                             ( int app, char *title, char *type, int dims,
                               int dimd, long nv, int nspare, int alloc_method,
                               int *spare, char **vlabel, char **dlabel,
                               float *vert, int voff, float **data, int *doff,
                               int new_strs, int new_labs, int new_arrs,
                               int *ierr );

# else

  extern PFFds_dir        *pf_bld_dir        ();
  extern PFFhead          *pf_bld_header     ();
  extern PFFds_ifl        *pf_bld_ifl        ();
  extern PFFds_nonuniform *pf_bld_nonuniform ();
  extern PFFds_uniform    *pf_bld_uniform    ();
  extern PFFds_vertex     *pf_bld_vertex     ();

# endif

#endif
