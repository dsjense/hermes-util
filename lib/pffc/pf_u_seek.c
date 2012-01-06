/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_seek.c
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

void pf_u_seek ( PFFfid *fid,  long offset, int *ierr );

void pf_u_seek ( PFFfid *fid,  long offset, int *ierr )

#else

void       pf_u_seek      ();

void pf_u_seek ( fid, offset, ierr )

PFFfid  *fid;
long     offset;
int     *ierr;

#endif

/* sets the offset for "fid" in 2-byte words, relative to "beginning of file"

    Input:
      fid     -  pointer to PFF file structure
      offset  -  offset (in 2-byte words)
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Illegal File ID (FID)
                   = 2,  Attempt to Position File Open in WRITE mode
                   = 3,  FSEEK error
*/
{
  static char        *module    = "PF_U_SEEK";
  FILE               *stream;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( ( fid == NULL ) || ( (stream = fid->stream) == NULL ) ) {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Illegal File ID (FID)" );
    return;
  }

  if ( fid->mode == WR )  {
    *ierr = 2;
    pf_wr_err ( module, *ierr, fid, 
                            "Attempt to Position File Open in WRITE mode" );
    return;
  }

  if ( fseek( stream, 2*offset, SEEK_SET ) != 0 )  {
    *ierr = 3;
    pf_wr_err ( module, *ierr, fid, "FSEEK error" );
    return;
  }

  /* update current file position */
  fid->position = offset;

  return;
}
