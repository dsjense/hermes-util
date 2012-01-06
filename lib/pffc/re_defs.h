/*
-------------------------------------------------------------------------------
    PFF I/O utility:   re_defs.h
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

#ifndef RE_DEFS_H
#define RE_DEFS_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#if defined(__STDC__) || defined(__cplusplus)

  EXTERN void        pf_re_ds_frame   ( PFFfid *fid, long *offset, 
                                        int *ierr );
  EXTERN float      *pf_re_fltarray   ( PFFfid *fid, int keep, long *len, 
                                        int *foff10, int *ierr );
  EXTERN PFFhead    *pf_re_header     ( PFFfid *fid, long *offset, 
                                        int *ierr );
  EXTERN int        *pf_re_intarray   ( PFFfid *fid, long *len, int *ierr );

  EXTERN PFFds_any  *pf_re_ds         ( PFFfid *fid, int keep, int *ierr );
  EXTERN PFFds_any  *pf_re_dir        ( PFFfid *fid, int keep, int *ierr );
  EXTERN PFFds_any  *pf_re_ifl        ( PFFfid *fid ,int keep, int *ierr );
  EXTERN PFFds_any  *pf_re_ngd        ( PFFfid *fid, int keep, int *ierr );
  EXTERN PFFds_any  *pf_re_nonuniform ( PFFfid *fid, int keep, int *ierr );
  EXTERN PFFds_any  *pf_re_uniform    ( PFFfid *fid, int keep, int *ierr );
  EXTERN PFFds_any  *pf_re_vertex     ( PFFfid *fid, int keep, int *ierr );

#else

  extern void        pf_re_ds_frame   ();
  extern float      *pf_re_fltarray   ();
  extern PFFhead    *pf_re_header     ();
  extern int        *pf_re_intarray   ();

  extern PFFds_any  *pf_re_ds         ();
  extern PFFds_any  *pf_re_dir        ();
  extern PFFds_any  *pf_re_ifl        ();
  extern PFFds_any  *pf_re_ngd        ();
  extern PFFds_any  *pf_re_nonuniform ();
  extern PFFds_any  *pf_re_uniform    ();
  extern PFFds_any  *pf_re_vertex     ();

#endif

#endif
