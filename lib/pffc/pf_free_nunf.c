/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_free_nonuniform.c
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
#include "ds_structs.h"

#include "free_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_free_nonuniform ( PFFds_any *nonuniform, int *ierr );

void pf_free_nonuniform ( PFFds_any *nonuniform, int *ierr )

#else

void       pf_free_nonuniform    ();

void pf_free_nonuniform ( nonuniform, ierr )

PFFds_any   *nonuniform; 
int         *ierr;

#endif

/* Release a PFF NONUNIFORM dataset structure

------------------------------------------------------------------------

   Memory associated with internal pointers must also be freed:

        PFFhead*              nonuniform->head
        LOOP i=1,nonuniform->nblk
          ( block = nonuniform->block[i-1] )
          int*                  block->spare
          long*                 block->nx
          char*                 block->blabel
          LOOP j=1,nonuniform->dims
            char**                block->glabel[j-1]
            float*                block->x[j-1]
          ENDLOOP
          char**                block->glabel
          char**                block->dlabel
          float**               block->x
          LOOP j=1,nonuniform->dimd
            char**                block->dlabel[j-1]
            float*                block->data[j-1]
          ENDLOOP
          float**               block->data
          PFFblock_nonuniform*  block
        ENDLOOP
        PFFblock_nonuniform** nonuniform->block

------------------------------------------------------------------------

    Input:
      nonuniform  -  pointer to NONUNIFORM dataset structure to be 
                     released
      ierr        -  If not zero, return with no operation

    Output:
      NONE
*/
{
  /* static char         *module  = "PF_FREE_NONUNIFORM"; */
  register int         i, ib;
  int                  gmax, dmax;
  PFFds_nonuniform    *tmpnonuniform; 
  PFFblock_nonuniform *block   = NULL;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  /* need a temporary pointer to NONUNIFORM structure */
  tmpnonuniform = ( PFFds_nonuniform* ) nonuniform;

  /* Delete the structure only if it is not null */

  if ( tmpnonuniform != NULL )   {

    pf_free_header (tmpnonuniform->head, ierr );

    if ( tmpnonuniform->block != NULL )   {

      for ( ib=0; ib < tmpnonuniform->nblk; ++ib )  { 

        if (  ( block = tmpnonuniform->block[ib] )  !=  NULL )   {

          CHKFREE (block->spare);
          CHKFREE (block->nx);
          CHKFREE (block->blabel);

          if ( block->alloc_method != DFAULT )   {
            if (block->glabel   != NULL )    {
              for ( i=0; i < tmpnonuniform->dims; ++i )
                CHKFREE (block->glabel[i]);
              free (block->glabel);
            }
            if (block->dlabel   != NULL )    {
              for ( i=0; i < tmpnonuniform->dimd; ++i )
                CHKFREE (block->dlabel[i]);
              free (block->dlabel);
            }

            switch ( block->alloc_method )  {
              case  1:     gmax = dmax = 1; break;
              case  2:     gmax = dmax = 0; break;
              default:     gmax = tmpnonuniform->dims; 
                           dmax = tmpnonuniform->dimd; break;
            }
            if (block->x        != NULL )    {
              for ( i=0; i < gmax; ++i )
                CHKFREE (block->x[i]);
              free (block->x);
            }
            if (block->data     != NULL )    {
              for ( i=0; i < dmax; ++i )
                CHKFREE (block->data[i]);
              free (block->data);
            }
            if (block->idata    != NULL )    {
              for ( i=0; i < dmax; ++i )
                CHKFREE (block->idata[i]);
              free (block->idata);
            }
          }
          free(block);

        }   /* end of non-NULL block structure */

      }   /* end of loop over all grid blocks */

      free (tmpnonuniform->block );

    }   /* end of non-NULL block array pointer */

    free(tmpnonuniform);
    nonuniform = NULL;

  }   /* end of non-NULL dataset structure */

  return;
}
