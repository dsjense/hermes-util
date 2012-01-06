/*
-------------------------------------------------------------------------------
    PFF I/O utility:   wr_defs.h
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

#ifndef WR_DEFS_H
# define WR_DEFS_H

# include "ds_structs.h"

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#if defined(__STDC__) || defined(__cplusplus)

  EXTERN long       pf_wr_fltarray  ( PFFfid *fid, long len, float *farray, 
                                      int foff10, int *ierr );
  EXTERN void       pf_wr_header    ( PFFfid *fid, PFFhead *head, 
                                      long *lstadr, int *ierr );
  EXTERN void       pf_wr_intarray  ( PFFfid *fid, long len, int *iarr, 
                                      int *ierr );
  EXTERN void       pf_wr_lds       ( PFFfid *fid, long adr1st, long *lds, 
                                      int *ierr );

  EXTERN void       pf_wr_ds        ( PFFfid *fid, PFFds_any *ds, int *ierr );
  EXTERN void       pf_wr_dir       ( PFFfid *fid, PFFds_any *ds, int *ierr );
  EXTERN void       pf_wr_ifl       ( PFFfid *fid, PFFds_any *ds, int *ierr );
  EXTERN void       pf_wr_ngd       ( PFFfid *fid, PFFds_any *ds, int *ierr );
  EXTERN void       pf_wr_nonuniform( PFFfid *fid, PFFds_any *ds, int *ierr );
  EXTERN void       pf_wr_uniform   ( PFFfid *fid, PFFds_any *ds, int *ierr );
  EXTERN void       pf_wr_vertex    ( PFFfid *fid, PFFds_any *ds, int *ierr );

# else

  extern long       pf_wr_fltarray  ();
  extern void       pf_wr_header    ();
  extern void       pf_wr_intarray  ();
  extern void       pf_wr_lds       ();

  extern void       pf_wr_ds        ();
  extern void       pf_wr_dir       ();
  extern void       pf_wr_ifl       ();
  extern void       pf_wr_ngd       ();
  extern void       pf_wr_nonuniform();
  extern void       pf_wr_uniform   ();
  extern void       pf_wr_vertex    ();

# endif

#endif
