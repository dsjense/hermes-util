/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_free_uniform.c
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

void pf_free_uniform ( PFFds_any *uniform, int *ierr );

void pf_free_uniform ( PFFds_any *uniform, int *ierr )

#else

void       pf_free_uniform    ();

void pf_free_uniform ( uniform, ierr )

PFFds_any     *uniform; 
int           *ierr;

#endif

/* Release a PFF UNIFORM dataset structure

------------------------------------------------------------------------

   Memory associated with internal pointers must also be freed:

        PFFhead*              uniform->head
        LOOP i=1,uniform->nblk
          ( block = uniform->block[i-1] )
          int*                  block->spare
          long*                 block->nx
          char*                 block->blabel
          float*                block->x0
          float*                block->dx
          LOOP j=1,uniform->dims
            char**                block->glabel[j-1]
          ENDLOOP
          char**                block->glabel
          char**                block->dlabel
          LOOP j=1,uniform->dimd
            char**                block->dlabel[j-1]
            float*                block->data[j-1]
          ENDLOOP
          float**               block->data
          PFFblock_uniform*     block
        ENDLOOP
        PFFblock_uniform**    uniform->head

------------------------------------------------------------------------

    Input:
      uniform     -  pointer to UNIFORM dataset structure to be 
                     released
      ierr        -  If not zero, return with no operation

    Output:
      NONE
*/
{
  /* static char      *module  = "PF_FREE_UNIFORM"; */
  register int      i, ib;
  int               dmax;
  PFFds_uniform    *tmpuniform; 
  PFFblock_uniform *block   = NULL;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  /* need a temporary pointer to UNIFORM structure */
  tmpuniform = ( PFFds_uniform* ) uniform;

  /* Delete the structure only if it is not null */

  if ( tmpuniform != NULL )   {

    pf_free_header (tmpuniform->head, ierr );

    if ( tmpuniform->block != NULL )   {

      for ( ib=0; ib < tmpuniform->nblk; ++ib )  { 

        if (  ( block = tmpuniform->block[ib] )  !=  NULL )   {

          CHKFREE (block->spare);
          CHKFREE (block->nx);
          CHKFREE (block->blabel);
          CHKFREE (block->x0);
          CHKFREE (block->dx);

          if ( block->alloc_method != DFAULT )   {
            if (block->glabel   != NULL )    {
              for ( i=0; i < tmpuniform->dims; ++i )
                CHKFREE (block->glabel[i]);
              free (block->glabel);
            }
            if (block->dlabel   != NULL )    {
              for ( i=0; i < tmpuniform->dimd; ++i )
                CHKFREE (block->dlabel[i]);
              free (block->dlabel);
            }

            switch ( block->alloc_method )  {
              case  1:     dmax = 1; break;
              case  2:     dmax = 0; break;
              default:     dmax = tmpuniform->dimd; break;
            }
            if (block->data     != NULL )    {
              for ( i=0; i < dmax; ++i )
                CHKFREE (block->data[i]);
              free (block->data);
            }
          }
          free(block);

        }   /* end of non-NULL block structure */

      }   /* end of loop over all grid blocks */

      free (tmpuniform->block );

    }   /* end of non-NULL block array pointer */

    free(tmpuniform);
    uniform = NULL;

  }   /* end of non-NULL dataset structure */

  return;
}
