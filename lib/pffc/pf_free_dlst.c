/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_free_dirlist.c
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

void pf_free_dirlist ( PFFdir *dir,  int *ierr );

void pf_free_dirlist ( PFFdir *dir,  int *ierr )

#else

void       pf_free_dirlist      ();

void pf_free_dirlist ( dir, ierr )

PFFdir  *dir;
int     *ierr;

#endif

/* Recursively removes and frees the memory associated with an entire PFF 
   directory list, from the directory element "dir" supplied by calling 
   routine down thru to the bottom element in the list.  

    Input:
      dir     -  pointer to PFF directory element structure at the top of the 
                 directory list to be deleted.
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   = 0,  Normal return
*/
{
  /* static char        *module    = "PF_FREE_DIRLIST"; */

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( dir != NULL )    {

    pf_free_dirlist ( dir->down, ierr);

    CHKFREE(dir->title);
    CHKFREE(dir->type_name);
    free(dir);
  }

  return;
}
