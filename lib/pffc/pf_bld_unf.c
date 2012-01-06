/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_bld_unf.c
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

/* Temporary define */
#define PFTUGD    (999)

#include <stdlib.h>
#include "pff.h"
#include "bld_defs.h"
#include "free_defs.h"

/*  Declare function */

#ifdef __STDC__

PFFds_uniform *pf_bld_uniform ( int app, char *title, char *type, 
                  int dims, int dimd, int nblk, int new_strs, int *ierr );

PFFds_uniform *pf_bld_uniform ( int app, char *title, char *type, 
                  int dims, int dimd, int nblk, int new_strs, int *ierr )

#else

PFFds_uniform *pf_bld_uniform ();

PFFds_uniform *pf_bld_uniform ( app, title, type, dims, dimd, nblk, 
                                new_strs, ierr )

int    app, dims, dimd, nblk;
int    new_strs;
char  *title, *type;
int   *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine is a BLD routine that builds a PFF UNIFORM dataset 
         structure from supplied data.
       - Although it allocates the BLOCK structures needed for the dataset,
         it does not actually populate those BLOCKs with data.  That can be
         done, for example, by using the function pf_fill_ublock.

------------------------------------------------------------------------

    Input:
      app      -  application dataset type
      title    -  dataset title string
      type     -  dataset type string
      dims     -  dimensionality of grid
      dimd     -  dimensionality of data
      nblk     -  number of blocks in dataset
      new_strs -  flag indicating that new title and type strings must be 
                  created because the supplied strings must be preserved
                  WARNING: if new_strs is FALSE, it is critical that the 
                  strings be later freed by the calling program.  Immediately 
                  setting them to NULL will help prevent this.
      ierr     -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  Normal return
                   =  2,  Error allocating memory for UNIFORM structure
*/
{
  static char          *module      = "PF_BLD_UNIFORM";
  PFFds_uniform     *uniform  =  NULL;
  PFFblock_uniform  *block       =  NULL;
  int                   i, j;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  /* get memory for UNIFORM structure */

  if ( (uniform = (PFFds_uniform *) malloc( sizeof(PFFds_uniform) )) 
       == NULL )  {
    *ierr = 2;
    pf_wr_err ( module, *ierr, NULL, 
                "Error allocating memory for UNIFORM structure");
    return NULL;
  }

  /* is this a legal UNIFORM dataset raw type ? */
  uniform->type  = DFAULT;
  uniform->head  = NULL;
  uniform->block = NULL;
  for ( i=0; (j=PFF_legal_uniform[i]) != DFAULT; i++ )  {
    if ( PFF_ds_dims[j] == dims && PFF_ds_dimd[j] == dimd   )  {
      uniform->type = j;
    }
  }
  /* If not, set it to UGD dataset raw type  */
  if ( uniform->type == DFAULT )  uniform->type = PFTUGD;

  uniform->head = pf_bld_header ( uniform->type, app, title, type, 
                                     DFAULT, DFAULT,  new_strs, ierr );
  if( *ierr != 0 )   {
    CHKFREE(uniform);
    return NULL;
  }

  uniform->dims = dims;
  uniform->dimd = dimd;
  uniform->nblk = nblk;

  if ( 
        ( uniform->block = 
            (PFFblock_uniform **) malloc( 
                uniform->nblk*sizeof(PFFblock_uniform *) ) ) == NULL
      )  {
    *ierr = 2;
    pf_wr_err ( module, *ierr, NULL, 
            "Error allocating memory for UNIFORM structure or contents");
  }
  else   {
    for( i=0; i<uniform->nblk; i++ )  uniform->block[i] = NULL;

    for (i=0; *ierr == 0 && i < uniform->nblk; ++i ) {

      if ( ( uniform->block[i] =
              (PFFblock_uniform *) malloc( sizeof(PFFblock_uniform) )
                                                          ) == NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
              "Error allocating memory for UNIFORM structure or contents");
      }
      else   {
        block = uniform->block[i];
        block->nspare = 0;
        block->spare  = NULL;
        block->nx     = NULL;
        block->glabel = NULL;
        block->dlabel = NULL;
        block->blabel = NULL;
        block->x0     = NULL;
        block->dx     = NULL;
        block->goff10 = 0;
        block->data   = NULL;
        block->foff10 = 0;
        block->alloc_method = DFAULT;
      }
    }
  }

  if ( *ierr != 0 )   {
    i = 0;
    pf_free_uniform( (PFFds_any *) uniform, &i );
    return NULL;
  }

  return uniform;
}
