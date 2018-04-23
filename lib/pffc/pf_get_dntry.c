/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_get_direntry.c
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

/*  Declare function */

#ifdef __STDC__

PFFdir *pf_get_direntry ( PFFfid *fid,  int entry, int *ierr );

PFFdir *pf_get_direntry ( PFFfid *fid,  int entry, int *ierr )

#else

PFFdir      *pf_get_direntry      ();

PFFdir *pf_get_direntry ( fid, entry, ierr )

PFFfid  *fid;
int      entry;
int     *ierr;

#endif

/* This routine returns a directory list element pointer assocated with a 
   specific entry number.

    Input:
      fid     -  pointer to PFF file structure
      entry   -  # of directory entry 
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Illegal File ID (FID)
                   = 2,  Entry # is out of range
*/
{
  static char        *module    = "PF_GET_DIRENTRY";
  PFFdir             *dir;
  int                ds_count;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  if ( ( fid == NULL ) || ( fid->stream == NULL ) ) {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Illegal File ID (FID)" );
    return NULL;
  }

  if (  (dir = fid->dirtop)  ==  NULL ) ds_count = 0;
  else ds_count = (fid->dirtop)->count;

  /* if entry is one greater that count, return NULL with no error to 
     indicate End-of-Data */
  if ( entry == ds_count + 1 ) return NULL;

  if ( ( entry < 1 )  ||  ( entry > ds_count ) )    {
    *ierr = 2;
    pf_wr_err ( module, *ierr, fid, "Entry # is out of range" );
    return NULL;
  }

  if ( (entry - 1) < (ds_count - entry) )   {
    dir = fid->dirbottom;
    while ( dir->count < entry  && dir != NULL )
      dir = dir->up;
  }
  else    {
    while ( dir->count > entry  && dir != NULL )
      dir = dir->down;
  }

  return dir;
}
