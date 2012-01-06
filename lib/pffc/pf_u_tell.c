/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_tell.c
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

long pf_u_tell ( PFFfid *fid,  int *ierr );

long pf_u_tell ( PFFfid *fid,  int *ierr )

#else

long       pf_u_tell      ();

long pf_u_tell ( fid, ierr )

PFFfid *fid; 
int    *ierr;

#endif

/* returns offset in "fid" in 2-byte words or -1 on error

    Input:
      fid     -  pointer to PFF file structure
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Illegal File ID (FID)
*/
{
  static char        *module    = "PF_U_TELL";

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return (-1);

  if ( ( fid == NULL ) || ( fid->stream == NULL ) ) {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Illegal File ID (FID)" );
    return (-1);
  }

  return (fid->position);
}
