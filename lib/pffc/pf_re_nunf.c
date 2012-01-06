/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_re_nonuniform.c
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

PFFds_any *pf_re_nonuniform ( PFFfid   *fid ,int keep, int *ierr );

PFFds_any *pf_re_nonuniform ( PFFfid   *fid ,int keep, int *ierr )

#else

PFFds_any   *pf_re_nonuniform    ();

PFFds_any *pf_re_nonuniform ( fid, keep, ierr )

PFFfid   *fid; 
int       keep;
int      *ierr;

#endif

/* 
------------------------------------------------------------------------

        - This routine is a READ routine that reads a NONUNIFORM dataset 
          type dataset from a PFF file.
        - This operation is ONLY ALLOWED in READ mode !!!
        - Several dataset types map to this structure depending upon the 
          dimensionality of the grid (NDIMS), the dimensionality of the data 
          (NDIMD), and the type of the data (float or integer).  The 
          following convention is used:

              Dataset type        Ndims          Ndimd     Data Type
              ------------        -----          -----     ---------
                 PFTNF1             1              1         float
                 PFTNG3             3              0         float
                 PFTNF3             3              1         float
                 PFTNV3             3              3         float
                 PFTNI3             3              1        integer
               etc.

        - Dataset Format:
            <HEADER>       PFTNF3, PFTNV3, etc.
            <INT>          NBLKS  (# of data blocks)
            LOOP nb=1,NBLKS
              LOOP i=1,NDIMS
                <LONG>         NX(1:i)
              ENDLOOP
              <INT>x5        Reserved for application (ISPARE)
              LOOP i=1,NDIMS
                <FARRAY>       X(1:NX) for ith spatial dimension
              ENDLOOP
              LOOP i=1,NDIMS
                <STRING>       XLABEL  for ith spatial dimension
              ENDLOOP
              <STRING>       BLABEL
              LOOP i=1,NDIMD
                IF ( TYPE = PFTNI3 ) THEN
                  <IARRAY>       IARRAY( 1 : NX(1)*NX(2)*...*NX(NDIMS) )
                ELSE
                  <FARRAY>       FARRAY( 1 : NX(1)*NX(2)*...*NX(NDIMS) )
                ENDIF
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
                   =  2,  Error allocating memory for NONUNIFORM structure or 
                          contents
                   =  3,  Internal inconsistancy detected while reading 
                          NONUNIFORM structure
*/
{
  static char           *module    = "PF_RE_NONUNIFORM";
  register int           i,*psp;
  register long          j;
  int                   *p = NULL;
  int                    itype, ib, tmpdimd;
  char                 **pl;
  long                   ltmp, nv, tnv, offset;
  int                    temp = 0;
  float                  pow10;
  void                 **pdatatmp;
  PFFhead               *head =  NULL; 
  PFFds_nonuniform      *nonuniform  =  NULL;
  PFFblock_nonuniform   *block  =  NULL;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  head = pf_re_header ( fid, &offset, ierr );
  if( *ierr != 0 ) return NULL;

  i = 0;
  itype = DFAULT;
  while ( PFF_legal_nonuniform[i]  != DFAULT )  {
    if ( head->rawtype == PFF_legal_nonuniform[i] )
      itype = i;
    i++;
  }

  if ( itype == DFAULT )  {
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

      /* extract dimensionality from map arrays */

      nonuniform->dims = PFF_ds_dims[nonuniform->type];
      nonuniform->dimd = PFF_ds_dimd[nonuniform->type];

      /* read in # of grid blocks */

      pf_u_sio( fid, RE, 1, &(nonuniform->nblk), ierr );

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

          tmpdimd = MAX ( nonuniform->dimd, 1 );
          if ( 
              ( block->nx = 
                (long *) malloc( nonuniform->dims*sizeof(long) )) == NULL
           || ( block->x = 
                (float **) malloc( nonuniform->dims*sizeof(float *)) ) == NULL 
           || ( block->glabel = 
                (char **) malloc( nonuniform->dims*sizeof(char *)) ) == NULL 
           || ( pdatatmp = 
                (void **) malloc( tmpdimd*sizeof(float *)) ) == NULL 
             ) {
            *ierr = 2;
            pf_wr_err ( module, *ierr, fid, 
                "Error allocating memory for NONUNIFORM structure or contents");
          }
          else   {

            /* read in grid sizes */

            for(i=0,nv=1; i<nonuniform->dims; nv *= block->nx[i++] )    {
              pf_u_sio( fid, RE, 3,PFF_work_buf, ierr );
              pf_u_i2l( PFF_work_buf, &(block->nx[i]), ierr);
            }

            /* read in spare words */

            if ( PFF_fixed_spare_length[nonuniform->type] )   {
              pf_u_sio( fid, RE, NONUNIF_MAXSPARE, PFF_work_buf, ierr );
              for (i=0;i<NONUNIF_MAXSPARE;i++)   {
                if ( PFF_work_buf[i] == DFAULT ) break;
              }
              ltmp = i;
              if ( ltmp > 0 ) {
                psp = (int *) malloc(ltmp*sizeof(int));
                if ( psp != NULL ) {
                  block->spare = psp;
                  for (j=0;j<ltmp;j++,psp++)
                    *psp = PFF_work_buf[j];
                }
              }
            }
            else    {
              block->spare = pf_re_intarray ( fid, &ltmp, ierr );
            }

            block->nspare = ltmp;

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

            /* read in grid labels */

            for( i=0,pl=block->glabel; i < nonuniform->dims; ++i,++pl )  {
              *pl = NULL;
              pf_u_string_io ( fid, RE, 0, pl, ierr );
            }

            /* read in block label */

            pf_u_string_io ( fid, RE, 0, &(block->blabel), ierr );

            /* read in component arrays & offsets; find max. offset */

            if ( nonuniform->dimd > 0 )  {
              if ( nonuniform->type == PFTNI3 )  {
                block->idata = (int **) pdatatmp;
                for ( i=0; i < nonuniform->dimd; ++i)  {
                  block->idata[i] = pf_re_intarray ( fid, &tnv, ierr );
                  if ( tnv != nv ) { temp = 1; *ierr = 3; }
                }
              }
              else   {
                block->data = (float **) pdatatmp;
                block->data[0] = pf_re_fltarray ( fid, keep, &tnv, p, ierr );
                if ( tnv != nv ) { temp = 1; *ierr = 3; }
                for ( i=1, block->foff10=p[0]; i < nonuniform->dimd; ++i)  {
                  block->data[i] = pf_re_fltarray ( fid, keep, &tnv, p+i, ierr );
                  if ( tnv != nv ) { temp = 1; *ierr = 3; }
                  block->foff10 = MAX (p[i],block->foff10);
                }

                /* scale all component arrays to max. offset */

                for ( i=0; *ierr == 0  &&  i < nonuniform->dimd; ++i)   {
                  if ( p[i] != block->foff10 )  {
                    pow10 = (float) pow ( 10.0 , (double) (p[i] - block->foff10) );
                    for ( j=0; j < nv; ++j )
                      block->data[i][j] = block->data[i][j] * pow10;
                  }
                }
              }
            }
            else   {
              block->data = (float **) pdatatmp;
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
