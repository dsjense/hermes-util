/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_open.c
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
#include "ds_structs.h"
#include "workspace.h"
#include "filestack.h"

#include  "dir_defs.h"
#include "free_defs.h"
#include   "re_defs.h"
#include  "set_defs.h"
#include    "u_defs.h"

#include "init_data.h"

/*  Declare function */

#ifdef __STDC__

PFFfid *pf_u_open ( const char *name, int mode, int *nds, int *ierr );

PFFfid *pf_u_open ( const char *name, int mode, int *nds, int *ierr )

#else

PFFfid    *pf_u_open      ();

PFFfid *pf_u_open ( name, mode, nds, ierr )

char *name;
int   mode;
int  *nds, *ierr;

#endif

/* open a PFF I/O file and setup appropriate structures - 
   returns a pointer to the file's PFF file structure, or NULL on error

    Input:
      name    -  ASCII name of file
      mode    -  I/O mode  (RE, WR, or RW)
      ierr    -  If not zero, return with no operation

    Output:
      nds     -  # of datasets found in file  (RE or RW mode only)
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Invalid File Mode
                   = 2,  Error Opening File
                   = 3,  Unable To Allocate PFF File ID
                   = 4,  File has no PFF header
                   = 5,  File directory cannot be loaded
*/
{
  static char    *module    = "PF_U_OPEN";
  register int    i;
  FILE           *tmpfile = NULL;
  PFFfid         *tmpfid = NULL;
  PFFhead        *head;
  PFFds_dir      *dsdir;
  char           *p = NULL;
  char            tmode[4];
  long            dir_loc, dloc, dlen = 0;
  int             repair     = FALSE;
  int             terr;
  int             mp_on = FALSE;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  if ( PFFMP_procs == 0 ) {
#ifdef USE_MPI
    PFFMP_comm = MPI_COMM_WORLD;
    MPI_Comm_size(PFFMP_comm, &PFFMP_procs);
    MPI_Comm_rank(PFFMP_comm, &PFFMP_rank);
#else
    PFFMP_comm = 0;
    PFFMP_procs = 1;
    PFFMP_rank  = 0;
#endif
  }

  switch (mode)  {
    case RE:
      strcpy ( tmode, "rb" );
      break;
    case WR_MP:
      mp_on = TRUE;
    case WR:
      strcpy ( tmode, "wb" );
      break;
    case RW:
      strcpy ( tmode, "rb+" );
      break;
    default:
      *ierr = 1;
      pf_wr_err ( module, *ierr, NULL, "Invalid File Mode" );
      return NULL;
  }
  if ( mode == WR_MP ) mode = WR;

  if ( ! mp_on || PFFMP_rank == 0 ) {
    tmpfile = fopen(name,tmode);
    if (tmpfile == NULL)  {
      *ierr = 2;
      pf_wr_err ( module, *ierr, NULL, "Error Opening File" );
    }
  }

  tmpfid = (PFFfid *) malloc(sizeof(PFFfid));
  p = (char *) malloc(strlen(name)+1);
  if (tmpfid == NULL || p == NULL)   {
    *ierr = 3;
    pf_wr_err ( module, *ierr, NULL, "Unable To Allocate PFF File ID" );
  }
  else if ( *ierr == 0 ) {
    strcpy(p,name);
    tmpfid->up           = NULL;
    tmpfid->down         = PFF.top;
    tmpfid->name         = p;
    tmpfid->stream       = tmpfile;
    tmpfid->mp_on        = mp_on;
    tmpfid->mode         = mode;
    if ( tmpfile != 0 )    tmpfid->mp_current = 1;
    else                   tmpfid->mp_current = 0;
    tmpfid->mp_mode      = PFFMP_CURRENT;
    tmpfid->last_word    = 0;
    tmpfid->position     = 0;
    tmpfid->dirtop       = NULL;
    tmpfid->directory    = NULL;
    tmpfid->dirbottom    = NULL;
    tmpfid->fp_precision = PFF_fp_precision;
  }

  PFFMP_Allreduce_1(tmpfid, *ierr, terr, MPI_INT, MPI_MAX);
  if ( *ierr != 0 ) {
    if ( tmpfile != NULL ) fclose(tmpfile);
    CHKFREE(p);
    CHKFREE(tmpfid);
    return NULL;
  }

  if (mode == WR )  {
    tmpfid->extend_flag = TRUE;
    PFF_work_buf[0] = FFRAME;
    for (i=1;i<PFF_HEADERLEN;i++)
      PFF_work_buf[i] = DFAULT;
    pf_u_sio( tmpfid, WR, PFF_HEADERLEN, PFF_work_buf, ierr );
    *nds = 0;
  }
  else if ( tmpfid->mp_current == 1 )   {
    pf_u_sio( tmpfid, RE, PFF_HEADERLEN, PFF_work_buf, ierr );
    if (*ierr != 0)   {
      fclose(tmpfile);
    }
    else if (PFF_work_buf[0] != FFRAME)   {
      *ierr = 4;
      pf_wr_err ( module, *ierr, NULL, "File has no PFF header" );
      fclose(tmpfile);
    }
    else {
      if (PFF_work_buf[1] == DFAULT)   {
        repair = TRUE;
      }
      else    {
        pf_u_i2l ( (PFF_work_buf+1) , &dir_loc, ierr );
      }

      /*  Load directory information here */

      if ( repair )  {
        dloc = PFF_HEADERLEN;
        dlen = 0;
      }
      else  {
        pf_u_seek ( tmpfid, dir_loc, ierr );
      }

      while ( *ierr == 0 )  {
        if ( repair )  {
          pf_u_seek ( tmpfid, dloc + dlen, ierr);
          head = pf_re_header ( tmpfid, &dloc, ierr);
          if ( (dsdir = (PFFds_dir *) malloc( sizeof(PFFds_dir) )) == NULL )  {
            *ierr = 5;
            pf_wr_err ( module, *ierr, tmpfid, 
                        "File directory cannot be loaded" );
          }
          else  {
            dsdir->type     = PFTDIR;
            dsdir->ref_type = head->rawtype;
            dlen            = head->length;
            dsdir->length   = dlen;
            dsdir->offset   = dloc;
            dsdir->head     = head;
            head = NULL;
          }
        }
        else
          dsdir = ( PFFds_dir* ) pf_re_dir ( tmpfid, TRUE, ierr );

        if ( *ierr == 0  )
          pf_dir_put (tmpfid, dsdir, FALSE, ierr );
        else if ( *ierr != -1  &&  !repair )  {
          repair = TRUE;
          dloc = PFF_HEADERLEN;
          dlen = 0;
          pf_free_dirlist ( tmpfid->dirtop, ierr );
          tmpfid->dirtop    = NULL;
          tmpfid->directory = NULL;
          tmpfid->dirbottom = NULL;
          *ierr = 0;
        }
      }

      if ( *ierr != -1 )   {
        *ierr = 5;
        pf_wr_err ( module, *ierr, tmpfid, "File directory cannot be loaded" );
      }
      else  {
        *ierr = 0;
        if ( tmpfid->dirtop ) {
          pf_set_dsp ( tmpfid, 1, ierr );
          *nds = tmpfid->dirtop->count;
        }
        else *nds = 0;
        if ( repair )  {
          if ( tmpfid->dirtop == NULL )
            tmpfid->last_word = PFF_HEADERLEN;
          else
            tmpfid->last_word = dloc;
          if ( tmpfid->mode == RW )
            tmpfid->extend_flag = TRUE;
        }
        else     {
          tmpfid->extend_flag = FALSE;
          tmpfid->last_word = dir_loc - 1;
        }
      }
    }
  }

  PFFMP_Allreduce_1(tmpfid, *ierr, terr, MPI_INT, MPI_MAX);
  if ( *ierr != 0 ) {
    CHKFREE(p);
    CHKFREE(tmpfid);
    return NULL;
  }

  if(PFF.top==NULL) 
    tmpfid->count = 1;
  else  {
    tmpfid->count = PFF.top->count + 1;
    PFF.top->up   = tmpfid;
  }

  PFF.top       = tmpfid;
  PFF.current   = tmpfid;

  return tmpfid;
}
