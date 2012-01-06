/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_wrback.c
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

void pf_u_wrback ( PFFfid *fid, long offset, long len, int *iarr, int *ierr);

void pf_u_wrback ( PFFfid *fid, long offset, long len, int *iarr, int *ierr)

#else

void       pf_u_wrback    ();

void pf_u_wrback ( fid, offset, len, iarr, ierr )

PFFfid  *fid;
long     offset, len;
int      *iarr, *ierr;

#endif

/* writes a block of integers as 2-byte integers to a position 
   before the current position in a PFF file 

    Input:
      fid     -  input file ID
      offset  -  word offset for write-back operation
      len     -  number of integers to be read or written
      iarr    -  pointer to target buffer
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Illegal File ID (FID)
                   = 2,  Attempt to Overwrite File Open in READONLY mode
                   = 3,  Attempt to Overwrite File Outside its Extent
                   = 4,  Error encountered while writing to file
                   = 5,  FSEEK error
*/
{
  register long       i;
  register short int *buf;
  long                last = 0;
  static char        *module    = "PF_U_WRBACK";
  FILE               *stream;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  /* Does FID point to a file stream ? */
  if ( ( fid == NULL ) || ( (stream = fid->stream) == NULL ) ) {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Illegal File ID (FID)" );
    return;
  }

  /* Can't write if file is READONLY */
  if ( fid->mode == RE )  {
    *ierr = 2;
    pf_wr_err ( module, *ierr, fid,
                   "Attempt to Overwrite File Open in READONLY mode" );
    return;
  }

  /* find last word in file */
  if ( fid->mode == WR )
    last = fid->position;
  else if ( fid->mode == RW )
    last = fid->last_word;

  /* Is overwrite outside of current file extent ? */
  if ( ( offset < 0 ) ||  ( (offset + len) > last ) )  {
    *ierr = 3;
    pf_wr_err ( module, *ierr, fid,
                   "Attempt to Overwrite File Outside its Extent" );
    return;
  }

  /* Go to offset position in file */
  if ( fseek( stream, 2*offset, SEEK_SET ) != 0 )  {
    *ierr = 5;
    pf_wr_err ( module, *ierr, fid, "FSEEK error" );
  }

  /* Set internal buffer to first part of source buffer */
  buf = (short int *) iarr;

  /* Pack source array into internal buffer values */
  for (i=0; i<len; ++i)
#ifdef HU_ENDIAN_IS_LSB_FIRST
    buf[i] = ((255&iarr[i])<<8) | (255&(iarr[i]>>8));   
#else
    buf[i] = iarr[i];
#endif

  /* Load binary data */
  i = fwrite( (char *)buf, 2, len, stream );
  /* Check for error */
  if ( i < len )    {
    *ierr = 4;
    pf_wr_err ( module, *ierr, fid, 
                            "Error encountered while writing to file" );
    return;
  }

  /* Expand internal buffer values to target buffer */
  for (i=len-1; i>=0; --i)
#ifdef HU_ENDIAN_IS_LSB_FIRST
    iarr[i] = (short) ((255&buf[i])<<8) | (255&(buf[i]>>8));   
#else
    iarr[i] = buf[i];
#endif

  /* Go back to original position in file */
  if ( fseek( stream, 2*fid->position, SEEK_SET ) != 0 )  {
    *ierr = 5;
    pf_wr_err ( module, *ierr, fid, "FSEEK error" );
  }

}
