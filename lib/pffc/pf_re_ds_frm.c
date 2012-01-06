/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_re_ds_frame.c
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
#include "workspace.h"
#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_re_ds_frame ( PFFfid *fid, long *offset, int *ierr );

void pf_re_ds_frame ( PFFfid *fid, long *offset, int *ierr )

#else

void       pf_re_ds_frame ();

/*  void pf_re_ds_frame ( PFFfid *fid, long *offset, int *ierr )  */
void pf_re_ds_frame ( fid, offset, ierr )

PFFfid   *fid; 
long     *offset;
int      *ierr;

#endif

/* Attempt to Read a dataset framing word from a PFF file: 

------------------------------------------------------------------------

   This routine will attempt to skip over any null regions on the file.

   NULL Region Format:  (Lnul is length of NULL region in 16-bit 
                         words)
     if (Lnul .lt. 4) :
        (Lnul)x<INT>       Lnul*NULFLG
      if (Lnul .ge. 4) :
        <INT>              NULFLG      NULL Region Flag
        <LONG>             Lnul        NULL Region length
        (Lnul-4)x<INT>     JUNK        

------------------------------------------------------------------------

    Input:
      fid     -  pointer to PFF file structure
      ierr    -  If not zero, return with no operation

    Output:
      offset  -  file offset to the framing word  (on error, it is set 
                 to the file offset at function entry)
      ierr    -  error flag:
                   =  0,  Normal return
                   = -1,  EOF reached  (not really an error)
                   =  1,  File Framing Error
*/
{
  static char    *module    = "PF_RE_DS_FRAME";
  register int    i;
  long            nloc = 0, null_len;
  int             tframe;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  *offset = pf_u_tell ( fid, ierr );

  pf_u_sio( fid, RE, 1, &tframe, ierr );
  if ( tframe != DFRAME )  {
    if ( tframe == EOFFLG ) 
      *ierr = -1;
    else if ( tframe == NULFLG )  {
      pf_u_sio( fid, RE, 3, PFF_work_buf, ierr );
      if ( PFF_work_buf[0] >= 0 )   {
        pf_u_i2l( PFF_work_buf, &null_len , ierr);
        nloc = *offset + null_len;
        pf_u_seek ( fid, nloc, ierr);
        pf_u_sio( fid, RE, 1, &tframe, ierr );
        if ( tframe == EOFFLG ) 
          *ierr = -1;
        else if ( tframe != DFRAME )   {
          pf_wr_err ( module, *ierr, fid, "File Framing Error");
          *ierr = 1;
        }
      }
      else   {
        for(i=0;i<3;i++)   {
          if ( PFF_work_buf[i] == DFRAME )  {
            nloc = *offset + i + 1;
            break;
          }
          else if ( PFF_work_buf[i] == EOFFLG )  {        
            *ierr = -1;
            pf_wr_err ( module, *ierr, fid, "File Framing Error");
            break;
          }
        }
        if ( i > 2 )   {
          *ierr = 1;
          pf_wr_err ( module, *ierr, fid, "File Framing Error");
        }
        else
          pf_u_seek ( fid, nloc+1, ierr );
      }
    }
    else  {
      *ierr = 1;
      pf_wr_err ( module, *ierr, fid, "File Framing Error");
    }

    if ( *ierr == 0 ) 
      *offset = nloc;
  }

  return;
}
