/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_string_io.c
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
#include "pffmp.h"

#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_u_string_io ( PFFfid *fid, int iop, int mlen, char *str[], int *ierr);

void pf_u_string_io ( PFFfid *fid, int iop, int mlen, char *str[], int *ierr)

#else

void       pf_u_string_io ();

void pf_u_string_io ( fid, iop, mlen, str, ierr )

PFFfid  *fid;
int      iop, mlen, *ierr;
char   *str[];

#endif

/* reads/writes a character string from/to a PFF file as 2-byte integers

    Input:
      fid     -  input file ID
      iop     -  opcode  ( RE or WR )
      mlen    -  RE:  maximum # of characters that can be read into supplied 
                      array (if mlen < 1, string is allocated)
                 WR:  not used
      str     -  pointer to string pointer  (iop = WR only)
      ierr    -  If not zero, return with no operation

    Output:
      str     -  pointer to string pointer  (iop = RE only)
      ierr    -  error flag:
                   =  0,  Normal return
                   =  1,  Illegal File ID (FID)
                   =  2,  Illegal Opcode
                   =  3,  Attempt to Read File Open in WRITE mode
                   =  4,  EOF encountered while reading from file
                   =  5,  Error encountered while reading from file
                   =  6,  Attempt to Write File Open in READONLY mode
                   =  7,  Error encountered while writing to file
                   =  8,  Error allocating string
                   =  9,  Illegal string length
                   = 10,  GLOBAL mode not allowed for strings
*/
{
  register int        i;
  int                 wlen, c_len;
  char                blank     = ' ';
  static char        *module    = "PF_U_STRING_IO";
  static int          two15     = 32768;
  FILE               *stream;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( fid == NULL ) {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Illegal File ID (FID)" );
    return;
  }

  if ( fid->mp_mode == PFFMP_GLOBAL ) {
    *ierr = 10;
    pf_wr_err ( module, *ierr, fid, "GLOBAL mode not allowed for strings");
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
    /* read in string length */
    pf_u_sio ( fid, RE, 1, &wlen, ierr );
    if ( *ierr != 0 )   return;

    c_len = 2*wlen;

    if ( mlen > 0 ) 
      c_len = MIN ( c_len, mlen-1 );
    else   {
      CHKFREE (*str);
      if ( (*str = (char *) malloc ( c_len + 1 )) == NULL )  {
        *ierr = 8;
        pf_wr_err ( module, *ierr, fid, "Error allocating string");
        return;
      }
    }
    if ( c_len > 0 )  {
      /* Load character data */
      i = fread( *str, 1, c_len, fid->stream );
      /* Check for error */
      if ( i < c_len )   { 
        if ( feof(fid->stream) != 0 ) {
          *ierr = 4;
          pf_wr_err ( module, *ierr, fid, 
                                  "EOF encountered while reading from file" );
          return;
        }
        else if ( ferror(fid->stream) != 0 ) {
          *ierr = 5;
          pf_wr_err ( module, *ierr, fid, 
                                  "Error encountered while reading from file" );
          return;
        }
      }
      if ( (*str)[c_len-1] == blank ) c_len -= 1;
    }
    (*str)[c_len] = '\0';

    /* update current file position */
    fid->position += wlen;
  }
  else if ( iop == WR )  {

    if ( fid->mode == RE )  {
      *ierr = 6;
      pf_wr_err ( module, *ierr, fid, 
                              "Attempt to Write File Open in READONLY mode" );
      return;
    }
    /* write out length of non-blank string */
    if ( *str != NULL ) c_len = strlen(*str);
    else                c_len = 0;

    while ( c_len > 0 && (*str)[c_len-1] == blank ) c_len--;

    wlen  = (c_len + 1)/2;
    if ( wlen >= two15 )  {
      *ierr = 9;
      pf_wr_err ( module, *ierr, fid,
                              "Illegal string length");
      return;
    }

    pf_u_sio ( fid, WR, 1, &wlen, ierr );
    if ( *ierr != 0 )   return;

    if ( c_len > 0 )   {

      /* Output character data */
      i = fwrite( *str, 1, c_len, stream );
      /* Check for error */
      if ( i < c_len )    {
        *ierr = 7;
        pf_wr_err ( module, *ierr, fid, 
                                "Error encountered while writing to file" );
        return;
      }

      /* Load last character if necessary */
      if ( c_len < 2*wlen )  {
        i = fwrite( &blank, 1, 1, stream );
        /* Check for error */
        if ( i < 1 )    {
          *ierr = 7;
          pf_wr_err ( module, *ierr, fid, 
                                  "Error encountered while writing to file" );
          return;
        }
      }
    }

    /* update current file position */
    fid->position += wlen;

    /* update last_word if in Read/Write mode */
    if ( (fid->mode == RW) && (fid->position > fid->last_word) )  {
      fid->last_word = fid->position;
      fid->extend_flag = TRUE;
    }
  }
  else    {
    
    *ierr=2;
    pf_wr_err ( module, *ierr, fid, "Illegal Opcode" );
    return;
  }
}
