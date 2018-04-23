/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_set_dsp.c
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

#include  "get_defs.h"
#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_set_dsp ( PFFfid *fid,  int entry, int *ierr );

void pf_set_dsp ( PFFfid *fid,  int entry, int *ierr )

#else

void       pf_set_dsp      ();

void pf_set_dsp ( fid, entry, ierr )

PFFfid  *fid;
int      entry;
int     *ierr;

#endif

/* Sets the file's current directory pointer to the specified directory entry
   and positions the file to the first word of the associated dataset.  This 
   operation is NOT permissible in WriteOnly (WR) mode.

    Input:
      fid     -  pointer to PFF file structure
      entry   -  # of directory entry 
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Illegal File ID (FID)
                   = 2,  Attempt to Position File Open in WRITE mode
                   = 3,  Attempt to Position File to End-of-Data when open 
                         in READ mode
*/
{
  static char        *module    = "PF_SET_DSP";
  PFFdir             *dir;
  long                offset;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( fid == NULL ) *ierr = 1;
  else {
    if ( fid->mp_current == 0 ) return;
    if ( fid->stream == NULL ) *ierr = 1;
  }
  if ( *ierr != 0 ) {
    pf_wr_err ( module, *ierr, fid, "Illegal File ID (FID)" );
    return;
  }

  if ( fid->mode == WR )  {
    *ierr = 2;
    pf_wr_err ( module, *ierr, fid, 
                            "Attempt to Position File Open in WRITE mode" );
    return;
  }

  /* Get the directory structure associated with this entry */

  dir = pf_get_direntry ( fid, entry, ierr );
  if( *ierr != 0 ) return;

  /* update current file position */

  if ( dir == NULL ) {
    if ( fid->mode == RE )  {
      *ierr = 3;
      pf_wr_err ( module, *ierr, fid, 
       "Attempt to Position File to End-of-Data when open in READ mode" );
      return;
    }
    offset = fid->last_word;
  }
  else  offset = dir->offset;

  pf_u_seek( fid, offset, ierr );

  if ( *ierr == 0 ) 
    fid->directory = dir;

  return;
}
