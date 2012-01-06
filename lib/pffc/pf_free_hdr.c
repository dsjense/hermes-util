/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_free_header.c
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

/*  Declare function */

#ifdef __STDC__

void pf_free_header ( PFFhead *head, int *ierr );

void pf_free_header ( PFFhead *head, int *ierr )

#else

void       pf_free_header    ();

void pf_free_header ( head, ierr )

PFFhead        *head; 
int            *ierr;

#endif

/* Release a PFF header structure

------------------------------------------------------------------------

   Memory associated with internal header pointers must also be freed:

       head->rfu
       head->title
       head->type_name

------------------------------------------------------------------------

    Input:
      head    -  pointer to header structure to be released
      ierr    -  If not zero, return with no operation

    Output:
      NONE
*/
{
  /* static char    *module    = "PF_FREE_HEADER"; */

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  /* Checkthe error flag is set already */
  if( head != NULL )        {
    CHKFREE (head->rfu);
    CHKFREE (head->type_name);
    CHKFREE (head->title);
    free (head);
    head = NULL;
  }
  return;
}
