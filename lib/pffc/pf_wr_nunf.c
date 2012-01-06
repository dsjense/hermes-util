/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_wr_nunf.c
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

void pf_wr_nonuniform ( PFFfid   *fid ,  PFFds_any *ds, int *ierr );

void pf_wr_nonuniform ( PFFfid   *fid ,  PFFds_any *ds, int *ierr )

#else

void pf_wr_nonuniform ();
void pf_wr_nonuniform ( fid, ds, ierr )

PFFfid   *fid; 
PFFds_any *ds;
int      *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine is a WRITE routine that writes a NONUNIFORM 
         dataset to a PFF file.
        - This operation is ONLY ALLOWED in WRITE mode !!!
        - Several dataset types map to this structure depending upon the 
          dimensionality of the grid (NDIMS), the dimensionality of the data 
          (NDIMD), and the type of the data (float or integer).  The 
          following convention is used:

              Dataset type        Ndims          Ndimd     Data Type
              ------------        -----          -----     ---------
                 PFTNF1             1              1         float
                 PFTNG3             3              0         float
                 PFTNF3             3              1         float
                 PFTNV3             3              3         float
                 PFTNI3             3              1        integer
               etc.

        - Dataset Format:
            <HEADER>       PFTNF3, PFTNV3, etc.
            <INT>          NBLKS  (# of data blocks)
            LOOP nb=1,NBLKS
              LOOP i=1,NDIMS
                <LONG>         NX(1:i)
              ENDLOOP
              <INT>x5        Reserved for application (ISPARE)
              LOOP i=1,NDIMS
                <FARRAY>       X(1:NX) for ith spatial dimension
              ENDLOOP
              LOOP i=1,NDIMS
                <STRING>       XLABEL  for ith spatial dimension
              ENDLOOP
              <STRING>       BLABEL
              LOOP i=1,NDIMD
                IF ( TYPE = PFTNI3 ) THEN
                  <IARRAY>       IARRAY( 1 : NX(1)*NX(2)*...*NX(NDIMS) )
                ELSE
                  <FARRAY>       FARRAY( 1 : NX(1)*NX(2)*...*NX(NDIMS) )
                ENDIF
              ENDLOOP
            ENDLOOP

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
                   otherwise,   Error from called PFF utility routine
*/
{
  static char    *module    = "PF_WR_NONUNIFORM";
  static char    *null_string = "";
  char           *p;
  long            nv, offset, lds;
  PFFds_nonuniform   *nonuniform;
  PFFblock_nonuniform   *block;
  PFFds_dir      *dsdir;
  register int    i, j, ib, itype;
  int             NGD_mode = FALSE;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  nonuniform = (PFFds_nonuniform *) ds;

  itype = DFAULT;
  i = 0;
  /* check to see if this is a legal NONUNIFORM dataset */
  while ( PFF_legal_nonuniform[i]  != DFAULT )  {
    if ( ds->type == PFF_legal_nonuniform[i] )
      itype = i;
    i++;
  }

  /* check to see if this is an NGD that matches an NONUNIFORM dataset */
  if ( itype == DFAULT && ds->type == PFTNGD )   {
    for ( i=0; (j=PFF_legal_nonuniform[i]) != DFAULT; i++ )  {
      /* if it does, temporarily set header type to approp. value */
      if ( PFF_ds_dims[j] == nonuniform->dims && 
            PFF_ds_dims[j] == nonuniform->dimd   )  {
        itype = i;
        (ds->head)->rawtype = j;
        ds->type = j;
        NGD_mode = TRUE;
      }
    }
  }

  /* if still no match or type not consistent with header, then ERROR */
  if ( itype == DFAULT || ds->type != (ds->head)->rawtype )  {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Incorrect dataset type");
  }

  /* if multiblock NONUNIFORM check for consistent dimensionality */
  if ( NGD_mode == FALSE &&  
       ( nonuniform->dims != PFF_ds_dims[ds->type] ||
         nonuniform->dimd != PFF_ds_dimd[ds->type]   )  )  {
    *ierr = 2;
    pf_wr_err ( module, *ierr, fid, "Inconsistent dataset type");
  }
  if( *ierr != 0 ) return;

  /* Write header */
  (ds->head)->length = DFAULT;
  pf_wr_header ( fid, ds->head, &offset, ierr );

  /* if NGD, need to set the header type back to its original value */
  if ( NGD_mode )   {
    (ds->head)->rawtype = PFTNGD;
    ds->type = PFTNGD;
  }
  if( *ierr != 0 ) return;

/* --------------------------------------------------------------------- */

  /*  Write out number of blocks */
  PFF_work_buf[0] = nonuniform->nblk;
  pf_u_sio( fid, WR, 1, PFF_work_buf, ierr );

  /* loop over blocks */
  for ( ib=0; ib<nonuniform->nblk; ib++ )  {
    block = nonuniform->block[ib];

    /* Write out grid sizes & spare words */
    nv = 1;
    for( j=0; j<nonuniform->dims; j++ )   {
      nv *= block->nx[j];
      pf_u_l2i( block->nx[j], PFF_work_buf+3*j, ierr );
    }
    if ( PFF_fixed_spare_length[ds->type] ) {
      j = 0;  i = 3*nonuniform->dims;
      if ( block->spare != NULL )  {
        while ( j < MIN(block->nspare,NONUNIF_MAXSPARE) )
          PFF_work_buf[i++] = block->spare[j++];
      }
      while ( j++ < NONUNIF_MAXSPARE )  PFF_work_buf[i++] = DFAULT; 
      pf_u_sio( fid, WR, i, PFF_work_buf, ierr );
    }
    else {
      pf_u_sio( fid, WR, 3*nonuniform->dims, PFF_work_buf, ierr );
      pf_wr_intarray ( fid, block->nspare, block->spare, ierr );
    }

    /* Write out grid arrays */
    for ( j=0; j<nonuniform->dims; j++ )   {
      pf_wr_fltarray ( fid, block->nx[j], block->x[j], block->goff10, ierr );
    }

    /* Write out grid labels */
    for ( j=0; j<nonuniform->dims; j++ )   {
      p = (block->glabel == NULL) ? null_string : block->glabel[j];
      pf_u_string_io ( fid, WR, 0, &p, ierr );
    }
    /* Write out block label */
    pf_u_string_io ( fid, WR, 0, &(block->blabel), ierr );

    /* Write out data arrays */
    for ( j=0; j < nonuniform->dimd; ++j) {
      if ( nonuniform->type == PFTNI3 )  {
        pf_wr_intarray ( fid, nv, block->idata[j], ierr );
      }
      else  {
        pf_wr_fltarray ( fid, nv, block->data[j], block->foff10, ierr );
      }
    }

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
