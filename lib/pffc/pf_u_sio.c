/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_sio.c
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

#include "u_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_u_sio ( PFFfid *fid, int iop, long len, int *iarr, int *ierr);

void pf_u_sio ( PFFfid *fid, int iop, long len, int *iarr, int *ierr)

#else

void       pf_u_sio       ();

void pf_u_sio ( fid, iop, len, iarr, ierr )

PFFfid  *fid;
int      iop, *iarr, *ierr;
long     len;

#endif

/* reads/writes a block of integers from/to a PFF file as 2-byte integers

    Input:
      fid     -  input file ID
      iop     -  opcode  ( RE or WR )
      len     -  number of integers to be written  (iop = WR only)
      iarr    -  pointer to source buffer (iop = WR only)
      ierr    -  If not zero, return with no operation

    Output:
      len     -  number of integers read (iop = RE only)
      iarr    -  pointer to target buffer (iop = RE only)
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Illegal File ID (FID)
                   = 2,  Illegal Opcode
                   = 3,  Attempt to Read File Open in WRITE mode
                   = 4,  EOF encountered while reading from file
                   = 5,  Attempt to Write File Open in READONLY mode
*/
{
  register int        i;
  static char        *module    = "PF_U_SIO";
  int                 workspace[3];
  FILE               *stream;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( fid == NULL ) {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Illegal File ID (FID)" );
    return;
  }

  if ( fid->mp_current == 0 ) return;

  if ( (stream = fid->stream) == NULL ) {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Illegal File ID (FID)" );
    return;
  }

  if ( iop == RE )  {

    if ( fid->mode == WR )  {
      *ierr = 3;
      pf_wr_err ( module, *ierr, fid, 
                          "Attempt to Read File Open in WRITE mode" );
      return;
    }
    pf_u_i2io ( stream, fid, iop, len, iarr, ierr);

    if ( *ierr == -1 ) {
      *ierr = 4;
      pf_wr_err ( module, *ierr, fid, 
                             "EOF encountered while reading from file" );
      return;
    }
    else if ( *ierr != 0 ) return;

    /* update current file position */
    fid->position += len;
  }
  else if ( iop == WR )  {

    if ( fid->mode == RE )  {
      *ierr = 5;
      pf_wr_err ( module, *ierr, fid, 
                              "Attempt to Write File Open in READONLY mode" );
      return;
    }
    pf_u_i2io ( stream, fid, iop, len, iarr, ierr);

    /* update current file position */
    fid->position += len;

    /* update last_word if in Read/Write mode */
    if ( (fid->mode == RW) && (fid->position > fid->last_word) )  {
      fid->last_word = fid->position;
      if ( fid->extend_flag == FALSE )     {
        fid->extend_flag = TRUE;
        for (i=0; i<3; ++i)
          workspace[i] = DFAULT;
        pf_u_wrback ( fid, 1, 3, workspace, ierr );
      }
    }
  }
  else    {
    
    *ierr=2;
    pf_wr_err ( module, *ierr, fid, "Illegal Opcode" );
  }
  return;
}
