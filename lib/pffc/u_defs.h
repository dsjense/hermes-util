/*
-------------------------------------------------------------------------------
    PFF I/O utility:   u_defs.h
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

#ifndef U_DEFS_H
#define U_DEFS_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#if defined(__STDC__) || defined(__cplusplus)

  EXTERN void       pf_u_close            ( PFFfid *fid, int *ierr );
  EXTERN void       pf_u_f2i              ( float xval, int off10, int *ival,
                                            int *ierr );
  EXTERN void       pf_u_f4               ( int iop, long len, int *iarr,
                                            float *farr, int *ierr );
  EXTERN void       pf_u_i4               ( int iop, long len, int *ibuf,
                                            int *iarr, int *ierr );
  EXTERN void       pf_u_4to2             ( int iop, long len, int *ibuf,
                                            short int *shrt);
  EXTERN void       pf_u_processor_toggle ( PFFfid *fid,  long offset,
                                            int *ierr );
  EXTERN void       pf_u_i2f              ( int keep, int *ival, float *xval,
                                            int *off10, int *ierr );
  EXTERN void       pf_u_i2io             ( FILE *file, PFFfid *fid, int iop,
                                            long len, int *iarr, int *ierr );
  EXTERN void       pf_u_i2l              ( int *ival, long *lval, int *ierr );
  EXTERN void       pf_u_l2i              ( long lval, int *ival, int *ierr );
  EXTERN PFFfid    *pf_u_open             ( const char *name, int mode,
                                            int *nds, int *ierr );
  EXTERN void       pf_u_seek             ( PFFfid *fid,  long offset,
                                            int *ierr );
  EXTERN void       pf_u_sio              ( PFFfid *fid, int iop, long len,
                                            int *iarr, int *ierr );
  EXTERN void       pf_u_string_io        ( PFFfid *fid, int iop, int mlen,
                                            char *str[], int *ierr );
  EXTERN long       pf_u_tell             ( PFFfid *fid,  int *ierr );
  EXTERN void       pf_u_wrback           ( PFFfid *fid, long offset, long len,
                                            int *iarr, int *ierr);

#else

  extern void       pf_u_close            ();
  extern void       pf_u_f2i              ();
  extern void       pf_u_i4               ();
  extern void       pf_u_f4               ();
  extern void       pf_u_4to2             ();
  EXTERN void       pf_u_processor_toggle ();
  extern void       pf_u_i2f              ();
  extern void       pf_u_i2io             ();
  extern void       pf_u_i2l              ();
  extern void       pf_u_l2i              ();
  extern PFFfid    *pf_u_open             ();
  extern void       pf_u_seek             ();
  extern void       pf_u_sio              ();
  extern void       pf_u_string_io        ();
  extern long       pf_u_tell             ();
  extern void       pf_u_wrback           ();

#endif

#endif
