/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_free_vertex.c
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

void pf_free_vertex ( PFFds_any *vertex, int *ierr );

void pf_free_vertex ( PFFds_any *vertex, int *ierr )

#else

void       pf_free_vertex    ();

void pf_free_vertex ( vertex, ierr )

PFFds_any   *vertex; 
int         *ierr;

#endif

/* Release a PFF VERTEX dataset structure

------------------------------------------------------------------------

   Memory associated with internal pointers must also be freed:

       PFFhead*   vertex->head
       int*       vertex->spare
       char**     vertex->vlabel
       char**     vertex->dlabel
       float*     vertex->vert
       float**    vertex->data
       int*       vertex->aoff10

------------------------------------------------------------------------

    Input:
      vertex  -  pointer to VERTEX dataset structure to be released
      ierr    -  If not zero, return with no operation

    Output:
      NONE
*/
{
  /* static char    *module    = "PF_FREE_VERTEX"; */
  register int    i, lmax;
  PFFds_vertex   *tmpvertex; 

  /* need a temporary pointer to VERTEX structure */
  tmpvertex = ( PFFds_vertex* ) vertex;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  /* Delete the structure only if it is not null */

  if ( tmpvertex != NULL )   {
    pf_free_header (tmpvertex->head, ierr );
    CHKFREE (tmpvertex->spare);
    CHKFREE (tmpvertex->vert);
    CHKFREE (tmpvertex->aoff10);

    if (tmpvertex->vlabel   != NULL )    {
      for ( i=0; i < tmpvertex->dims; ++i )
        CHKFREE (tmpvertex->vlabel[i]);
      free (tmpvertex->vlabel);
    }
    if (tmpvertex->dlabel   != NULL )    {
      for ( i=0; i < tmpvertex->dimd; ++i )
        CHKFREE (tmpvertex->dlabel[i]);
      free (tmpvertex->dlabel);
    }
    if (tmpvertex->data     != NULL )    {
      switch ( tmpvertex->alloc_method )  {
        case  1:  lmax = 1; break;
        case  2:  lmax = 0; break;
        default:  lmax = tmpvertex->dimd; break;
      }
      for ( i=0; i < lmax; ++i ) CHKFREE (tmpvertex->data[i]);
      free (tmpvertex->data);
    }

    free(tmpvertex);
    vertex = NULL;
  }

  return;
}
