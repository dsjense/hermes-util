/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_bld_nunf.c
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

#include <stdlib.h>
#include "pff.h"
#include "bld_defs.h"
#include "free_defs.h"

/*  Declare function */

#ifdef __STDC__

PFFds_nonuniform *pf_bld_nonuniform ( int app, char *title, char *type, 
                                      int pf_ds_type, int dims, int dimd, 
                                      int nblk, int new_strs, int *ierr );

PFFds_nonuniform *pf_bld_nonuniform ( int app, char *title, char *type, 
                                      int pf_ds_type, int dims, int dimd, 
                                      int nblk, int new_strs, int *ierr )

#else

PFFds_nonuniform *pf_bld_nonuniform ();

PFFds_nonuniform *pf_bld_nonuniform ( app, title, type, pf_ds_type, dims, 
                                      dimd, nblk, new_strs, ierr )

int    app, pf_ds_type, dims, dimd, nblk;
int    new_strs;
char  *title, *type;
int   *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine is a BLD routine that builds a PFF NONUNIFORM dataset 
         structure from supplied data.
       - Although it allocates the BLOCK structures needed for the dataset,
         it does not actually populate those BLOCKs with data.  That can be
         done, for example, by using the function pf_fill_nblock.

------------------------------------------------------------------------

    Input:
      app        -  application dataset type
      title      -  dataset title string
      type       -  dataset type string
      pf_ds_type -  PFF raw dataset type (see defs. in pff.h)
      dims       -  dimensionality of grid (used only if pf_ds_type=PFTNGD)
      dimd       -  dimensionality of data (used only if pf_ds_type=PFTNGD)
      nblk       -  number of blocks in dataset (not used if pf_ds_type=PFTNGD)
      new_strs   -  flag indicating that new title and type strings must be 
                    created because the supplied strings must be preserved
                    WARNING: if new_strs is FALSE, it is critical that the 
                    strings be later freed by the calling program.  Immediately 
                    setting them to NULL will help prevent this.
      ierr       -  If not zero, return with no operation

    Output:
      ierr      -  error flag:
                     =  0,  Normal return
                     =  1,  Incorrect dataset type
                     =  2,  Error allocating memory for NONUNIFORM structure
*/
{
  static char          *module      = "PF_BLD_NONUNIFORM";
  PFFds_nonuniform     *nonuniform  =  NULL;
  PFFblock_nonuniform  *block       =  NULL;
  int                   i, j;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  /* get memory for NONUNIFORM structure */

  if ( (nonuniform = (PFFds_nonuniform *) malloc( sizeof(PFFds_nonuniform) )) 
       == NULL )  {
    *ierr = 2;
    pf_wr_err ( module, *ierr, NULL, 
                "Error allocating memory for NONUNIFORM structure");
    return NULL;
  }

  /* is this a legal NONUNIFORM dataset raw type ? */
  nonuniform->type  = DFAULT;
  nonuniform->head  = NULL;
  nonuniform->block = NULL;
  if ( pf_ds_type != PFTNGD )   {
    for ( j=0; PFF_legal_nonuniform[j] != DFAULT; j++ )  {
      /* if it does, is it also single block */
      if ( pf_ds_type == PFF_legal_nonuniform[j] )  {
        nonuniform->type = pf_ds_type;
        nonuniform->dims = PFF_ds_dims[pf_ds_type];
        nonuniform->dimd = PFF_ds_dimd[pf_ds_type];
        nonuniform->nblk = nblk;
      }
    }
  }
  else  {
    nonuniform->type = PFTNGD;
    nonuniform->dims = dims;
    nonuniform->dimd = dimd;
    nonuniform->nblk = 1;
  }
  if ( nonuniform->type == DFAULT )  {
    *ierr = 1;
    pf_wr_err ( module, *ierr, NULL, "Incorrect dataset type");
  }

  nonuniform->head = pf_bld_header ( nonuniform->type, app, title, type, 
                                     DFAULT, DFAULT,  new_strs, ierr );
  if( *ierr != 0 )   {
    CHKFREE(nonuniform);
    return NULL;
  }

  if ( 
        ( nonuniform->block = 
            (PFFblock_nonuniform **) malloc( 
                nonuniform->nblk*sizeof(PFFblock_nonuniform *) ) ) == NULL
      )  {
    *ierr = 2;
    pf_wr_err ( module, *ierr, NULL, 
            "Error allocating memory for NONUNIFORM structure or contents");
  }
  else   {
    for( i=0; i<nonuniform->nblk; i++ )  nonuniform->block[i] = NULL;

    for (i=0; *ierr == 0 && i < nonuniform->nblk; ++i ) {

      if ( ( nonuniform->block[i] =
              (PFFblock_nonuniform *) malloc( sizeof(PFFblock_nonuniform) )
                                                          ) == NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
              "Error allocating memory for NONUNIFORM structure or contents");
      }
      else   {
        block = nonuniform->block[i];
        block->nspare = 0;
        block->spare  = NULL;
        block->nx     = NULL;
        block->glabel = NULL;
        block->dlabel = NULL;
        block->blabel = NULL;
        block->x      = NULL;
        block->goff10 = 0;
        block->data   = NULL;
        block->idata  = NULL;
        block->foff10 = 0;
        block->alloc_method = DFAULT;
      }
    }
  }

  if ( *ierr != 0 )   {
    i = 0;
    pf_free_nonuniform( (PFFds_any *) nonuniform, &i );
    return NULL;
  }

  return nonuniform;
}
