/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_dir_get.c
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
#include <string.h>
#include "pff.h"
#include "ds_structs.h"
#include "typenames.h"

/*  Declare function */

#ifdef __STDC__

PFFds_dir *pf_dir_get ( PFFdir   *dir, int keep, int *ierr );

PFFds_dir *pf_dir_get ( PFFdir   *dir, int keep, int *ierr )

#else

PFFds_dir *pf_dir_get ();

PFFds_dir *pf_dir_get ( dir, keep, ierr )

PFFdir   *dir;
int       keep;
int      *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine builds and returns a Directory (DIR) dataset from 
         a specified directory list element.

------------------------------------------------------------------------

    Input:
      dir     -  pointer to Directory list entry structure
      keep    -  flag indicating whether or not the the strings in "dir"
                 need to be kept (this requires that duplicate strings be 
                 constructed)
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  Normal return
                   = -1,  EOF reached  (not really an error)
                   =  1,  Incorrect dataset type
                   =  2,  Error allocating memory for DIR structure
*/
{
  static char    *module    = "PF_DIR_GET";
  char           *p;
  int             traw, not_default;
  PFFhead        *head =  NULL; 
  PFFds_dir      *ds   =  NULL;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  if ( ((head = (PFFhead *)   malloc( sizeof(PFFhead)  )) == NULL) ||
       ((ds   = (PFFds_dir *) malloc( sizeof(PFFds_dir))) == NULL)   )  {
    *ierr = 2;
    pf_wr_err ( module, *ierr, NULL, 
                     "Error allocating memory for DIR structure");
    return NULL;
  }
  /* copy info from directory entry to directory dataset */
  ds->type     = PFTDIR;
  ds->head     = head;
  ds->ref_type = dir->rawtype;
  ds->length   = dir->length;
  ds->offset   = dir->offset;

  head->rawtype    = PFTDIR;
  head->apptype    = dir->apptype;
  head->length     = 0;
  head->ds_version = DFAULT;
  head->nwords_rfu = 0;
  head->rfu        = NULL;

  /* check to see if type name is default -- just set pointer to NULL */
  traw = ( dir->rawtype < 0 || dir->rawtype > PFFMAXDS ) ?
          PFFMAXDS+1 : dir->rawtype;
  if ( (not_default = strcmp( dir->type_name, PFF_type_names[traw])) == 0 )
    head->type_name = NULL;

  /* Need to make copies of strings if "keep" is set */
  if ( keep )   {
    if ( ( p = (char *) malloc ( strlen( dir->title ) + 1 ) )  
               ==  NULL )  {
      *ierr = 2;
      pf_wr_err ( module, *ierr, NULL, 
                        "Error allocating memory for DIR structure");
      return NULL;
    }
    strcpy ( p, dir->title );
    head->title = p;

    if ( not_default )   {
      if ( ( p = (char *) malloc ( strlen( dir->type_name ) + 1 ) )  
                  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                          "Error allocating memory for DIR structure");
        return NULL;
      }
      strcpy ( p, dir->type_name );
      head->type_name = p;
    }
  }
  /* Just transfer strings if "keep" is not set, but need to set
     original string pointers to NULL so that they won't be freed later */
  else    {
    head->title     = dir->title;
    dir->title      = NULL;

    if ( not_default )   {
      head->type_name = dir->type_name;
      dir->type_name  = NULL;
    }
  }

  return ds;
}
