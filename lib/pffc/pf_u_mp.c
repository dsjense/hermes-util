/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_mp.c
-------------------------------------------------------------------------------
  $Id: pf_u_mp.c,v 1.1.1.1 2012/01/06 19:48:15 mfpasik Exp $
  
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

#include <assert.h>

#include "pff.h"
#include "workspace.h"

#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_u_processor_toggle ( PFFfid *fid, long offset, int *ierr);

void pf_u_processor_toggle ( PFFfid *fid, long offset, int *ierr)

#else

void pf_u_processor_toggle ();

void pf_u_processor_toggle ( fid, offset, ierr )

PFFfid   *fid; 
long      offset;
int      *ierr;

#endif

/* Toggle the Parallel "current" status of a PFF file. Specifically, this
   means:

   If the file is "current" ( fid->mp_current = 1) --
     1) close the file (fid->stream)
     2) set fid->mp_current to 0

   If the file is NOT "current" ( fid->mp_current = 0) --
     1) open the file (fid->stream)
     2) set fid->mp_current to 1
     3) set the file's current position to "offset"

------------------------------------------------------------------------

    Input:
      fid     -  pointer to PFF file structure
      offset  -  When toggling to "current", the offset to position the file
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  No Error
                   =  1,  Illegal File ID (FID)
                   =  2,  Error Opening or Closing File
                   =  3,  FSEEK error
                   =  otherwise,  Error from called PFF routine
*/
{
  FILE           *stream;
  static char    *module    = "PF_U_PROCESSOR_TOGGLE";

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( fid == NULL ) {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Illegal File ID (FID)" );
    return;
  }

  if ( fid->mp_current == 0 ) {  /* toggle from "not-current" to "current" */
    assert(fid->stream == NULL);
    stream = fopen(fid->name,"rb+");
    if (stream == NULL)  {
      *ierr = 2;
      pf_wr_err ( module, *ierr, NULL, "Error Opening File" );
    }
    else {
      if ( fseek( stream, 2*offset, SEEK_SET ) != 0 )  {
        *ierr = 3;
        pf_wr_err ( module, *ierr, fid, "FSEEK error" );
        fclose(stream);
        return;
      }
      /* update current file position */
      fid->position = offset;
      if ( (fid->mode == RW) && (offset > fid->last_word) )
        fid->last_word = offset;
      fid->stream = stream;
      fid->mp_current = 1;
    }
  }
  else { /* toggle from "current" to "not-current" */
    assert(fid->stream != NULL);
    fclose(fid->stream);
    fid->stream = NULL;
    fid->mp_current = 0;
  }
  return;
}
