/*
-------------------------------------------------------------------------------
    PFF I/O utility:   get_defs.h
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

#ifndef GET_DEFS_H
#define GET_DEFS_H

#ifdef __cplusplus
# define EXTERN extern "C"
#else
# define EXTERN extern
#endif

#if defined(__STDC__) || defined(__cplusplus)

  EXTERN PFFdir    *pf_get_direntry ( PFFfid *fid,  int entry, int *ierr );
  EXTERN PFFfid    *pf_get_fid      ( int entry, int *ierr );
  EXTERN int        pf_get_fp_precision ( PFFfid *fid, int *ierr );

#else

  extern PFFdir    *pf_get_direntry ();
  extern PFFfid    *pf_get_fid      ();
  extern int        pf_get_fp_precision ();

#endif

#endif
