/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_re_ngd.c
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

#include <math.h>
#include <stdlib.h>
#include "pff.h"
#include "ds_structs.h"
#include "workspace.h"

#include "free_defs.h"
#include   "re_defs.h"
#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

PFFds_any *pf_re_ngd ( PFFfid   *fid ,int keep, int *ierr );

PFFds_any *pf_re_ngd ( PFFfid   *fid ,int keep, int *ierr )

#else

PFFds_any   *pf_re_ngd    ();

PFFds_any *pf_re_ngd ( fid, keep, ierr )

PFFfid   *fid; 
int       keep;
int      *ierr;

#endif

/* 
------------------------------------------------------------------------

        - This routine is a READ routine that reads a NGD dataset 
          type dataset from a PFF file.
        - This data is stored in a PFFds_nonuniform data structure
        - This operation is ONLY ALLOWED in READ mode !!!
        - Dataset Format:
            <HEADER>       PFTNGD
            <INT>          M                 (Space dimensionality)
            <INT>          N                 (Vector dimensionality)
#ifdef NGD_VERSION_ORIGINAL
            <INT>xM        NX                (# grid points for each
                                              dimension of space)
#else
            <LONG>xM       NX                (# grid points for each
                                              dimension of space)
#endif
            <IARRAY>       ISPARE(1:NSPARE)  (Reserved for application) 
            <STRING>xM     ALABEL            (coordinate labels for
                                              each axis of m-D space)
            <STRING>xN     VLABEL            (labels for each component
                                              of vector data)
            LOOP i=1,M
              <FARRAY>     Xi(1:NXi)         (Grid points for Xi axis)
            ENDLOOP
            LOOP j=1,N
              <FARRAY>     Vj(1:MG)          (j'th component of vector at
            ENDLOOP                          each grid point)

------------------------------------------------------------------------

    Input:
      fid     -  pointer to PFF file structure
      keep    -  flag indicating whether or not to keep a non-zero value 
                 for floating data in the case of underflow
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  Normal return
                   = -1,  EOF reached  (not really an error)
                   =  1,  Incorrect dataset type
                   =  2,  Error allocating memory for NONUNIFORM structure or 
                          contents
                   =  3,  Internal inconsistancy detected while reading 
                          NONUNIFORM structure
*/
{
  static char           *module    = "PF_RE_NGD";
  register int           i;
  register int           j;
  int                   *p = NULL;
  int                    ib, tlen;
  char                 **pl;
  long                   nv, tnv, offset, ltmp;
  int                    temp = 0;
  float                  pow10;
  PFFhead               *head =  NULL; 
  PFFds_nonuniform      *nonuniform  =  NULL;
  PFFblock_nonuniform   *block  =  NULL;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  head = pf_re_header ( fid, &offset, ierr );
  if( *ierr != 0 ) return NULL;

  if ( head->rawtype != PFTNGD )  {
    *ierr=1;
    pf_wr_err ( module, *ierr, fid, "Incorrect dataset type");
  }

  else    {

    /* get memory for NONUNIFORM structure */

    if ( (nonuniform = 
              (PFFds_nonuniform *) malloc( sizeof(PFFds_nonuniform) ))
                         == NULL )  {
      *ierr = 2;
      pf_wr_err ( module, *ierr, fid, 
                "Error allocating memory for NONUNIFORM structure or contents");
    }
    else  {

      nonuniform->type = head->rawtype;
      nonuniform->head = head;
      head = NULL;
      nonuniform->block   = NULL;

      nonuniform->nblk    = 1;

      /* read in dimensionality info */

      pf_u_sio( fid, RE, 1, &(nonuniform->dims), ierr );
      pf_u_sio( fid, RE, 1, &(nonuniform->dimd), ierr );

      if ( 
           ( nonuniform->block = 
               (PFFblock_nonuniform **) malloc( 
                   nonuniform->nblk*sizeof(PFFblock_nonuniform *) 
                                                                    ) ) == NULL
        || ( p =
               (int *) malloc( 
                   MAX(nonuniform->dims,nonuniform->dimd) * sizeof(int) 
                                                                    ) ) == NULL
          )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, fid, 
                "Error allocating memory for NONUNIFORM structure or contents");
      }

      for (ib=0; *ierr == 0 && ib < nonuniform->nblk; ++ib ) {

        if ( ( nonuniform->block[ib] =
               (PFFblock_nonuniform *) malloc( sizeof(PFFblock_nonuniform) )
                                                           ) == NULL )  {
          *ierr = 2;
          pf_wr_err ( module, *ierr, fid, 
               "Error allocating memory for NONUNIFORM structure or contents");
        }
        else if ( *ierr == 0 )      {

          block = nonuniform->block[ib];
          block->spare   = NULL;
          block->nx      = NULL;
          block->glabel  = NULL;
          block->dlabel  = NULL;
          block->blabel  = NULL;
          block->x       = NULL;
          block->data    = NULL;
          block->idata   = NULL;
          block->alloc_method = 0;

          if ( 
              ( block->nx = 
                (long *) malloc( nonuniform->dims*sizeof(long) )) == NULL
           || ( block->x = 
                (float **) malloc( nonuniform->dims*sizeof(float *)) ) == NULL 
           || ( block->glabel = 
                (char **) malloc( nonuniform->dims*sizeof(char *)) ) == NULL 
           || ( block->dlabel = 
                (char **) malloc( nonuniform->dimd*sizeof(char *)) ) == NULL 
           || ( block->data = 
                (float **) malloc( nonuniform->dimd*sizeof(float *)) ) == NULL 
             ) {
            *ierr = 2;
            pf_wr_err ( module, *ierr, fid, 
                "Error allocating memory for NONUNIFORM structure or contents");
          }
          else   {

            /* read in grid sizes */

            for(i=0,nv=1; i<nonuniform->dims; nv *= block->nx[i++] )    {
              if ( (nonuniform->head)->ds_version == 1 )  {
                /* NGD Dataset Version 1 Processing */
                pf_u_sio( fid, RE, 3, PFF_work_buf, ierr );
                pf_u_i2l( PFF_work_buf, &ltmp, ierr);
                block->nx[i] = (int) ltmp;
              }
              else {
                /* NGD Dataset Original Version Processing */
                pf_u_sio( fid, RE, 1, &tlen, ierr );
                block->nx[i] = tlen;
              }
            }

            /* read in spare words */

            block->spare = pf_re_intarray( fid, &tnv, ierr );
            block->nspare = tnv;

            /* read in grid labels */

            for( i=0,pl=block->glabel; i < nonuniform->dims; ++i,++pl )  {
              *pl = NULL;
              pf_u_string_io ( fid, RE, 0, pl, ierr );
            }

            /* read in data labels */

            for( i=0,pl=block->dlabel; i < nonuniform->dimd; ++i,++pl )  {
              *pl = NULL;
              pf_u_string_io ( fid, RE, 0, pl, ierr );
            }

            /* read in grid arrays & offsets; find max. offset */

            block->x[0] = pf_re_fltarray ( fid, keep, &tnv, p, ierr );
            if ( tnv != block->nx[0] ) { temp = 1; *ierr = 3; }
            for ( i=1, block->goff10=p[0]; i < nonuniform->dims; ++i)  {
              block->x[i] = pf_re_fltarray ( fid, keep, &tnv, p+i, ierr );
              block->goff10 = MAX (p[i],block->goff10);
              if ( tnv != block->nx[i] ) { temp = 1; *ierr = 3; }
            }

            /* scale all grid arrays to max. offset */

            for ( i=0; *ierr == 0  &&  i < nonuniform->dims; ++i)
              if ( p[i] != block->goff10 )  {
                pow10 = (float) pow ( 10.0 , (double) (p[i] - block->goff10) );
                for ( j=0; j < block->nx[i]; ++j )
                  block->x[i][j] = block->x[i][j] * pow10;
              }

            /* read in component arrays & offsets; find max. offset */

            block->data[0] = pf_re_fltarray ( fid, keep, &tnv, p, ierr );
            if ( tnv != nv ) { temp = 1; *ierr = 3; }
            for ( i=1, block->foff10=p[0]; i < nonuniform->dimd; ++i)  {
              block->data[i] = pf_re_fltarray ( fid, keep, &tnv, p+i, ierr );
              if ( tnv != nv ) { temp = 1; *ierr = 3; }
              block->foff10 = MAX (p[i],block->foff10);
            }

            /* scale all component arrays to max. offset */

            for ( i=0; *ierr == 0  &&  i < nonuniform->dimd; ++i)
              if ( p[i] != block->foff10 )  {
                pow10 = (float) pow ( 10.0 , (double) (p[i] - block->foff10) );
                for ( j=0; j < nv; ++j )
                  block->data[i][j] = block->data[i][j] * pow10;
              }

          }    /* end of single-block allocation */

        }   /* end of successful block structure allocation block */

      }   /* end of loop over blocks */

      if ( temp == 1 )   {
        *ierr = 3;
        pf_wr_err ( module, *ierr, fid, 
        "Internal inconsistancy detected while reading NONUNIFORM structure" );
      }

      if ( *ierr != 0 ) 
        pf_free_nonuniform ( ( PFFds_any* ) nonuniform, &temp );

    }   /* End of block over successfully allocated NONUNIFORM structure */

  }   /* End of block over legal dataset types */


  CHKFREE (p);
  return ( PFFds_any* ) nonuniform;
}
