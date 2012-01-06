/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_dir_put.c
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

void pf_dir_put ( PFFfid *fid, PFFds_dir *ds, int keep, int *ierr );

void pf_dir_put ( PFFfid *fid, PFFds_dir *ds, int keep, int *ierr )

#else

void pf_dir_put ();

void pf_dir_put ( fid, ds, keep, ierr )

PFFfid         *fid; 
PFFds_dir      *ds; 
int             keep;
int            *ierr;

#endif

/* Release any PFF dataset structure

------------------------------------------------------------------------

    Input:
      fid     -  pointer to PFF file structure
      ds      -  pointer to a PFF directory dataset structure to be 
                 inserted into the file's directory structure list.
      keep    -  flag indicating whether or not ds should be freed after 
                 it is entered into the file's directory structure list.
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  Normal return
                   =  1,  Illegal PFF file ID (fid)
                   =  2,  Directory dataset structure is empty
                   =  3,  Error allocating directory entry structure
*/
{
  static char    *module    = "PF_DIR_PUT";
  char           *p;
  PFFdir         *dptr;
  int             not_default, traw;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  /* Is FID illegal ? -- Is directory dataset structure null ? */

  if ( fid == NULL )   {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid,
                "Illegal PFF file ID (fid)");
    return;
  }
  if ( ds == NULL )   {
    *ierr = 2;
    pf_wr_err ( module, *ierr, fid,
                "Directory dataset structure is empty");
    return;
  }

  if ( fid->directory == NULL )   {
    dptr = (PFFdir *) malloc ( sizeof(PFFdir) );
    if ( dptr == NULL )  {
      *ierr = 3;
      pf_wr_err ( module, *ierr, fid,
                  "Error allocating directory entry structure");
      return;
    }
    dptr->up = NULL;
    dptr->down = fid->dirtop;
    if ( dptr->down == NULL)
      dptr->count = 1;
    else   {
      dptr->count = (dptr->down)->count + 1;
      (dptr->down)->up = dptr;
    }
  }
  else   {
    dptr = fid->directory;
    CHKFREE ( dptr->title );
    CHKFREE ( dptr->type_name );
  }

  dptr->rawtype = ds->ref_type;
  dptr->apptype = (ds->head)->apptype;
  dptr->length  = ds->length;
  dptr->offset  = ds->offset;

  /* check to see if type name is default -- Need to copy in either case */
  if ( (ds->head)->type_name == NULL || (ds->head)->type_name[0] == '\0' )   {
    traw = ( dptr->rawtype < 0 || dptr->rawtype > PFFMAXDS ) ?
             PFFMAXDS+1 : dptr->rawtype;
    if ( ( p = (char *) malloc ( strlen( PFF_type_names[traw] )+ 1 ) )  
         ==  NULL )  {
      *ierr = 3;
      pf_wr_err ( module, *ierr, fid,
                  "Error allocating directory entry structure");
      return;
    }
    strcpy ( p, PFF_type_names[traw] );
    dptr->type_name = p;
    not_default = 0;
  }
  else  not_default = 1;

  if ( keep )   {
    if ( ( p = (char *) malloc ( strlen( (ds->head)->title ) + 1 ) )  
               ==  NULL )  {
      *ierr = 3;
      pf_wr_err ( module, *ierr, fid,
                  "Error allocating directory entry structure");
      return;
    }
    strcpy ( p, (ds->head)->title );
    dptr->title = p;

    if ( not_default )  {
      if ( ( p = (char *) malloc ( strlen( (ds->head)->type_name )+ 1 ) )  
           ==  NULL )  {
        *ierr = 3;
        pf_wr_err ( module, *ierr, fid,
                    "Error allocating directory entry structure");
        return;
      }
      strcpy ( p, (ds->head)->type_name );
      dptr->type_name = p;
    }
  }
  else    {
    dptr->title     = (ds->head)->title;
    if ( not_default )  dptr->type_name = (ds->head)->type_name;
    CHKFREE ( (ds->head)->rfu );
    CHKFREE ( ds->head );
    free ( ds );
  }

  fid->directory = dptr->up;
  if ( dptr->up == NULL )
    fid->dirtop = dptr;
  if ( dptr->down == NULL )
    fid->dirbottom = dptr;

  return;
}
