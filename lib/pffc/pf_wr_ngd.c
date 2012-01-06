/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_wr_ngd.c
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

#include "pff.h"
#include "ds_structs.h"
#include "workspace.h"
#include  "bld_defs.h"
#include  "dir_defs.h"
#include   "wr_defs.h"
#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_wr_ngd ( PFFfid   *fid ,  PFFds_any *ds, int *ierr );

void pf_wr_ngd ( PFFfid   *fid ,  PFFds_any *ds, int *ierr )

#else

void pf_wr_ngd ();
void pf_wr_ngd ( fid, ds, ierr )

PFFfid   *fid; 
PFFds_any *ds;
int      *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine is a WRITE routine that writes a NGD dataset to a 
         PFF file.
        - This data is stored in a PFFds_nonuniform data structure
        - This operation is ONLY ALLOWED in WRITE mode !!!
        - Legal nonuniform datasets can be written in this format IF
          they have one block only.  Warning: block labels are lost in this
          translation.
        - NGD Dataset Format:
            <HEADER>       PFTNGD
            <INT>          M                 (Space dimensionality)
            <INT>          N                 (Vector dimensionality)
#ifdef NGD_VERSION_ORIGINAL
            <INT>xM        NX                (# grid points for each
                                              dimension of space)
#else
            <LONG>xM       NX                (# grid points for each
                                              dimension of space)
#endif
            <IARRAY>       ISPARE(1:NSPARE)  (Reserved for application) 
            <STRING>xM     ALABEL            (coordinate labels for
                                              each axis of m-D space)
            <STRING>xN     VLABEL            (labels for each component
                                              of vector data)
            LOOP i=1,M
              <FARRAY>     Xi(1:NXi)         (Grid points for Xi axis)
            ENDLOOP
            LOOP j=1,N
              <FARRAY>     Vj(1:MG)          (j'th component of vector at
            ENDLOOP                          each grid point)

------------------------------------------------------------------------

    Input:
      fid     -  pointer to PFF file structure
      ds      -  general dataset structure to be written to file [needs to
                 be cast to (PFFds_nonuniform *) before using]
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,        Normal return
                   =  1,        Incorrect dataset type
                   =  2,        Inconsistent dataset type
                   =  3,        NGD datasets must be one block only (to write)
                   otherwise,   Error from called PFF utility routine
*/
{
  static char    *module    = "PF_WR_NGD";
  static char    *null_string = "";
  char           *p;
  long            nv, offset, lds;
  PFFds_nonuniform   *nonuniform;
  PFFblock_nonuniform   *block;
  PFFds_dir      *dsdir;
  register int    i, j;
  int             NUNF_type = DFAULT;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  nonuniform = (PFFds_nonuniform *) ds;

  /* check to see if this is a NONUNIFORM dataset that matches an NGD */
  if ( ds->type != PFTNGD )   {
    for ( i=0; PFF_legal_nonuniform[i] != DFAULT; i++ )  {
      /* if it does, is it also single block */
      if ( ds->type == PFF_legal_nonuniform[i] )  {
        NUNF_type = ds->type;
        (ds->head)->rawtype = PFTNGD;
        ds->type = PFTNGD;
      }
    }
  }

  /* if still no match or type not consistent with header, then ERROR */
  if ( ds->type != PFTNGD || ds->type != (ds->head)->rawtype )  {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Incorrect dataset type");
  }
  /* Make sure its only one block */
  if ( nonuniform->nblk != 1 )  {
    *ierr = 3;
    pf_wr_err ( module, *ierr, fid, 
                "NGD datasets must be one block only (to write)");
  }

  if( *ierr == 0 )   {
    /* Write header */
    (ds->head)->length = DFAULT;
#ifndef VERTEX_VERSION_ORIGINAL
    (ds->head)->ds_version = 1; /* Now writing Version 1 of this dataset !!! */
#endif
    pf_wr_header ( fid, ds->head, &offset, ierr );
  }

  /* if NUNF, need to set the header type back to its original value */
  if ( NUNF_type != DFAULT )   {
    (ds->head)->rawtype = NUNF_type;
    ds->type = NUNF_type;
  }
  if( *ierr != 0 ) return;

/* --------------------------------------------------------------------- */

  /*  Write out M, N, & grid lengths */
  PFF_work_buf[0] = nonuniform->dims;
  PFF_work_buf[1] = nonuniform->dimd;
  pf_u_sio( fid, WR, 2, PFF_work_buf, ierr );
  block = nonuniform->block[0];
  nv = 1;
  for ( j=0; j<nonuniform->dims; j++ )  {
    nv *= block->nx[j];
#ifdef NGD_VERSION_ORIGINAL
    pf_u_sio( fid, WR, 1, block->nx[j], ierr );
#else
    /* Use <LONG>s instead of <INT>s for axis dimensions */
    pf_u_l2i( block->nx[j], PFF_work_buf, ierr);
    pf_u_sio( fid, WR, 3, PFF_work_buf, ierr );
#endif
  }

  /* write out spare words */
  pf_wr_intarray( fid, block->nspare, block->spare, ierr );

  /* Write out grid labels */
  for ( j=0; j<nonuniform->dims; j++ )   {
    p = (block->glabel == NULL) ? null_string : block->glabel[j];
    pf_u_string_io ( fid, WR, 0, &p, ierr );
  }

  /* write out data labels */
  for ( j=0; j<nonuniform->dimd; j++ )   {
    p = (block->dlabel == NULL) ? null_string : block->dlabel[j];
    pf_u_string_io ( fid, WR, 0, &p, ierr );
  }

  /* Write out grid arrays */
  for ( j=0; j<nonuniform->dims; j++ )   {
    pf_wr_fltarray ( fid, block->nx[j], block->x[j], block->goff10, ierr );
  }

  /* Write out data arrays */
  for ( j=0; j < nonuniform->dimd; ++j) {
    pf_wr_fltarray ( fid, nv, block->data[j], block->foff10, ierr );
  }

/* --------------------------------------------------------------------- */

  /* write the dataset length back to the header */
  pf_wr_lds ( fid, offset, &lds, ierr );

  /* build a directory dataset -- don't destroy the NONUNIFORM's header */
  (ds->head)->length = lds;
  dsdir = pf_bld_dir ( ds->head, offset, TRUE, ierr );

  /* put directory info into memory-resident directory structure -- 
     okay to discard dsdir structure since no longer needed */
  pf_dir_put ( fid, dsdir, FALSE, ierr );

  return;
}
