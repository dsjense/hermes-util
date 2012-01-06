/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_re_uniform.c
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

PFFds_any *pf_re_uniform ( PFFfid   *fid ,int keep, int *ierr );

PFFds_any *pf_re_uniform ( PFFfid   *fid ,int keep, int *ierr )

#else

PFFds_any   *pf_re_uniform    ();

PFFds_any *pf_re_uniform ( fid, keep, ierr )

PFFfid   *fid; 
int       keep;
int      *ierr;

#endif

/* 
------------------------------------------------------------------------

        - This routine is a READ routine that reads a UNIFORM dataset 
          type dataset from a PFF file.
        - This operation is ONLY ALLOWED in READ mode !!!
        - Several dataset types map to this structure depending upon the 
          dimensionality of the grid (NDIMS) and the data (NDIMD).  The 
          following convention is used:

              Dataset type        Ndims          Ndimd
              ------------        -----          -----
                 PFTUF1             1              1
                 PFTUF3             3              1
                 PFTUV3             3              3
               etc.

        - Dataset Format:
            <HEADER>       PFTUF1, PFTUF3, etc.
            <INT>          NBLKS  (# of data blocks)
            LOOP nb=1,NBLKS
              LOOP i=1,NDIMS
                <LONG>         NX(1:i)
              ENDLOOP
              <INT>x5        Reserved for application (ISPARE)
              LOOP i=1,NDIMS
                <FLOAT>       X0 for ith spatial dimension
                <FLOAT>       DX for ith spatial dimension
              ENDLOOP
              LOOP i=1,NDIMS
                <STRING>       XLABEL  for ith spatial dimension
              ENDLOOP
              <STRING>       BLABEL
              LOOP i=1,NDIMD
                <FARRAY>       FARRAY( 1 : NX(1)*NX(2)*...*NX(NDIMS) )
              ENDLOOP
            ENDLOOP

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
                   =  2,  Error allocating memory for UNIFORM structure or 
                          contents
                   =  3,  Internal inconsistancy detected while reading 
                          UNIFORM structure
*/
{
  static char           *module    = "PF_RE_UNIFORM";
  register int           i,*psp;
  register long          j;
  int                   *p = NULL;
  int                   *ptmp;
  int                    itype, ib;
  char                 **pl;
  long                   nv, tnv, offset;
  int                    temp = 0;
  float                  pow10;
  PFFhead               *head =  NULL; 
  PFFds_uniform         *uniform  =  NULL;
  PFFblock_uniform      *block  =  NULL;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  head = pf_re_header ( fid, &offset, ierr );
  if( *ierr != 0 ) return NULL;

  i = 0;
  itype = DFAULT;
  while ( PFF_legal_uniform[i]  != DFAULT )  {
    if ( head->rawtype == PFF_legal_uniform[i] )
      itype = i;
    i++;
  }

  if ( itype == DFAULT )  {
    *ierr=1;
    pf_wr_err ( module, *ierr, fid, "Incorrect dataset type");
  }

  else    {

    /* get memory for UNIFORM structure */

    if ( (uniform = 
              (PFFds_uniform *) malloc( sizeof(PFFds_uniform) ))
                         == NULL )  {
      *ierr = 2;
      pf_wr_err ( module, *ierr, fid, 
                "Error allocating memory for UNIFORM structure or contents");
    }
    else  {

      uniform->type = head->rawtype;
      uniform->head = head;
      head = NULL;
      uniform->block   = NULL;

      /* extract dimensionality from map arrays */

      uniform->dims = PFF_ds_dims[uniform->type];
      uniform->dimd = PFF_ds_dimd[uniform->type];

      /* read in # of grid blocks */

      pf_u_sio( fid, RE, 1, &(uniform->nblk), ierr );

      if ( 
           ( uniform->block = 
               (PFFblock_uniform **) malloc( 
                   uniform->nblk*sizeof(PFFblock_uniform *) 
                                                                    ) ) == NULL
        || ( p =
               (int *) malloc( 
                   MAX(2*uniform->dims,uniform->dimd) * sizeof(int) 
                                                                    ) ) == NULL
          )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, fid, 
                "Error allocating memory for UNIFORM structure or contents");
      }

      for (ib=0; *ierr == 0 && ib < uniform->nblk; ++ib ) {

        if ( ( uniform->block[ib] =
               (PFFblock_uniform *) malloc( sizeof(PFFblock_uniform) )
                                                           ) == NULL )  {
          *ierr = 2;
          pf_wr_err ( module, *ierr, fid, 
               "Error allocating memory for UNIFORM structure or contents");
        }
        else if ( *ierr == 0 )      {

          block = uniform->block[ib];
          block->spare   = NULL;
          block->nx      = NULL;
          block->glabel  = NULL;
          block->dlabel  = NULL;
          block->blabel  = NULL;
          block->x0      = NULL;
          block->dx      = NULL;
          block->data    = NULL;
          block->alloc_method = 0;

          if ( 
              ( block->nx = 
                (long *) malloc( uniform->dims*sizeof(long) )) == NULL
           || ( block->x0 = 
                (float *) malloc( uniform->dims*sizeof(float)) ) == NULL 
           || ( block->dx = 
                (float *) malloc( uniform->dims*sizeof(float)) ) == NULL 
           || ( block->glabel = 
                (char **) malloc( uniform->dims*sizeof(char *)) ) == NULL 
           || ( block->data = 
                (float **) malloc( uniform->dimd*sizeof(float *)) ) == NULL 
             ) {
            *ierr = 2;
            pf_wr_err ( module, *ierr, fid, 
                "Error allocating memory for UNIFORM structure or contents");
          }
          else   {

            /* read in grid sizes */

            for(i=0,nv=1; i<uniform->dims; nv *= block->nx[i++] )    {
              pf_u_sio( fid, RE, 3,PFF_work_buf, ierr );
              pf_u_i2l( PFF_work_buf, &(block->nx[i]), ierr);
            }

            /* read in spare words */

            pf_u_sio( fid, RE, UNIF_MAXSPARE, PFF_work_buf, ierr );
            for (i=0;i<UNIF_MAXSPARE;i++)
              if ( PFF_work_buf[i] == DFAULT ) break;
            block->nspare = i;
            if ( i > 0 )   {
              psp = (int *) malloc(i*sizeof(int));
              if ( psp != NULL ) {
                block->spare = psp;
                for (j=0;j<i;j++,psp++)
                  *psp = PFF_work_buf[j];
              }
            }

            /* read in grid initial values and deltas */

            ptmp = p;
            for ( i=0; i < uniform->dims; ++i)  {
              pf_u_sio( fid, RE, 6,PFF_work_buf, ierr );
              pf_u_i2f( keep, PFF_work_buf, &(block->x0[i]), ptmp++, ierr);
              pf_u_i2f( keep, PFF_work_buf+3, &(block->dx[i]), ptmp++, ierr);
            }

            /* find maximum power-of-10 offset */
            
            block->goff10 = p[0];
            for ( i=1; i < 2*uniform->dims; ++i)
              block->goff10 = MAX ( p[i], block->goff10 );

            /* scale all grid arrays to max. offset */

            for ( i=0,j=0; *ierr == 0  &&  i < uniform->dims; ++i,++j)  {
              if ( p[j] != block->goff10 )  {
                pow10 = (float) pow ( 10.0 , (double) (p[j] - block->goff10) );
                block->x0[i] = block->x0[i] * pow10;
              }
              if ( p[++j] != block->goff10 )  {
                pow10 = (float) pow ( 10.0 , (double) (p[j] - block->goff10) );
                block->dx[i] = block->dx[i] * pow10;
              }
            }

            /* read in grid labels */

            for( i=0,pl=block->glabel; i < uniform->dims; ++i,++pl )  {
              *pl = NULL;
              pf_u_string_io ( fid, RE, 0, pl, ierr );
            }

            /* read in block label */

            pf_u_string_io ( fid, RE, 0, &(block->blabel), ierr );

            /* read in component arrays & offsets; find max. offset */

            block->data[0] = pf_re_fltarray ( fid, keep, &tnv, p, ierr );
            if ( tnv != nv ) { temp = 1; *ierr = 3; }
            for ( i=1, block->foff10=p[0]; i < uniform->dimd; ++i)  {
              block->data[i] = pf_re_fltarray ( fid, keep, &tnv, p+i, ierr );
              if ( tnv != nv ) { temp = 1; *ierr = 3; }
              block->foff10 = MAX (p[i],block->foff10);
            }

            /* scale all component arrays to max. offset */

            for ( i=0; *ierr == 0  &&  i < uniform->dimd; ++i)
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
        "Internal inconsistancy detected while reading UNIFORM structure" );
      }

      if ( *ierr != 0 ) 
        pf_free_uniform ( ( PFFds_any* ) uniform, &temp );

    }   /* End of block over successfully allocated UNIFORM structure */

  }   /* End of block over legal dataset types */


  CHKFREE (p);
  return ( PFFds_any* ) uniform;
}
