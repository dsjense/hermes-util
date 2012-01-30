/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_wr_vrtx.c
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
#include "pff.h"
#include "pffmp.h"
#include "ds_structs.h"
#include "workspace.h"
#include  "bld_defs.h"
#include  "dir_defs.h"
#include   "wr_defs.h"
#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_wr_vertex ( PFFfid   *fid ,  PFFds_any *ds, int *ierr );

void pf_wr_vertex ( PFFfid   *fid ,  PFFds_any *ds, int *ierr )

#else

void pf_wr_vertex ();
void pf_wr_vertex ( fid, ds, ierr )

PFFfid   *fid; 
PFFds_any *ds;
int      *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine is a WRITE routine that writes a IFL (Integer/Float 
         List) dataset to a PFF file.
       - Floating data is divided into two groups:
           1)  Float List -- each value is encoded independently as a 
               <FLOAT> at the full precision of this data type.
           2)  Float Array -- the entire array is encoded as an 
               <FARRAY>.  This uses less space but has dynamic range 
               limitations for data with multi-order-of-magnitude 
               variations.
       - An integer flag is used to indicate if the float array is 
         empty.  (flag = 0 means empty)
       - This operation is ONLY ALLOWED in WRITE or Read/Write modes !!!
       - Dataset Format:
           <HEADER>       PFTVTX
           <INT>          M                 (vertex dimensionality)
           <INT>          N                 (attribute dimensionality)
           <LONG>         NV                (# of verticies)
           <INT>x5        ISPARE            (Reserved for application) 
           <STRING>xM     VLABEL            (vertex coordinate labels)
           <STRING>xN     ALABEL            (attribute labels)
#ifdef VERTEX_VERSION_ORIGINAL
           IF (M.GT.0)
             <FARRAY>       VERT(1:M,1:NV)  (mD vertex list)
           ENDIF
#else
           LOOP i=1,M
             <FARRAY>       VERT(i,1:NV)    (ith component of mD vertex list)
           ENDLOOP
#endif
           LOOP i=1,N
             <FARRAY>       Ai(1:NV)        (ith attribute list)
           ENDLOOP

------------------------------------------------------------------------

    Input:
      fid     -  pointer to PFF file structure
      ds      -  general dataset structure to be written to file [needs to
                 be cast to (PFFds_vertex *) before using]
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,        Normal return
                   =  1,        Incorrect dataset type
                   =  2,        Error allocating workspace memory
                   otherwise,   Error from called PFF utility routine
*/
{
  static char    *module    = "PF_WR_VERTEX";
  static char    *null_string = "";
  static float    zero = 0.0;
  char           *p;
  long            offset, lds, len_all, tmplong;
#ifdef VERTEX_VERSION_ORIGINAL
  long            size;
#else
  int             k, tmperr = 0;
#endif
  float          *fwork = NULL;
  PFFds_vertex   *vertex;
  PFFds_dir      *dsdir;
  int             i, j, doff;
  int             prec_x = FP_REDU;
  int             prec_a = FP_REDU;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( ds->type != PFTVTX || (ds->head)->rawtype != PFTVTX )  {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Incorrect dataset type");
  }

  /* Write header */
  (ds->head)->length = DFAULT;
#ifndef VERTEX_VERSION_ORIGINAL
  (ds->head)->ds_version = 1;   /*  Now writing Version 1 of this dataset !!! */
#endif
  pf_wr_header ( fid, ds->head, &offset, ierr );
  if( *ierr != 0 ) return;

/* --------------------------------------------------------------------- */

  /* Set precision of ordinate and data float arrays */
  if ( fid->fp_precision != FP_REDU )    prec_x = FP_FULL;
  if ( fid->fp_precision == FP_ALLFULL ) prec_a = FP_FULL;

  vertex = (PFFds_vertex *) ds;

  /*  Write out M, N, NV, and spare words */
  PFF_work_buf[0] = vertex->dims;
  PFF_work_buf[1] = vertex->dimd;
  len_all = vertex->nv;
  PFFMP_Allreduce_1(fid, len_all, tmplong, MPI_LONG, MPI_SUM);
  pf_u_l2i( len_all, PFF_work_buf+2, ierr);
  j = 0;  i = 5;
  if ( vertex->spare != NULL )  {
    while ( j < MIN(vertex->nspare,VTX_MAXSPARE) )
         PFF_work_buf[i++] = vertex->spare[j++];
  }
  while ( j++ < VTX_MAXSPARE )  PFF_work_buf[i++] = DFAULT; 

  pf_u_sio( fid, WR, i, PFF_work_buf, ierr );

  /* Write out vertex labels */
  for ( j=0; j<vertex->dims; j++ )   {
    if ( vertex->vlabel != NULL ) p = vertex->vlabel[j];
    else                          p = null_string;
    pf_u_string_io ( fid, WR, 0, &p, ierr );
  }

  /* Write out data attribute labels */
  for ( j=0; j<vertex->dimd; j++ )   {
    if ( vertex->dlabel != NULL ) p = vertex->dlabel[j];
    else                          p = null_string;
    pf_u_string_io ( fid, WR, 0, &p, ierr );
  }

  if ( PFFMP_procs > 1 && fid->mp_on ) fid->mp_mode = PFFMP_GLOBAL;

  /* write the vertex array only if M is positive */
  if ( vertex->dims > 0 )   {
#ifdef VERTEX_VERSION_ORIGINAL
    /* original version */
    size = vertex->dims*vertex->nv;
    pf_wr_fltarray ( fid, prec_x, size, vertex->vert, vertex->voff10, ierr );
#else    
    /* Version 1 */
    if ( vertex->nv > 0 &&
         (fwork = (float *) malloc( vertex->nv*sizeof(float)) ) == NULL  ) {
      tmperr = 2;
    }
    if ( fid->mp_mode == PFFMP_GLOBAL ) {
      PFFMP_Allreduce_1(fid, *ierr, tmperr, MPI_INT, MPI_MAX);
    }
    else *ierr = tmperr;

    if ( *ierr != 0 && PFFMP_rank == 0 )
      pf_wr_err ( module, *ierr, fid, "Error allocating workspace memory");

    for ( i=0; i < vertex->dims; ++i) {
      if ( fwork ) {
        for ( j=0,k=i; j<vertex->nv; j++,k+=vertex->dims )  {
          fwork[j] = vertex->vert[k];
        }
        pf_wr_fltarray ( fid, prec_x, vertex->nv, fwork, vertex->voff10, ierr );
      }
      else pf_wr_fltarray ( fid, prec_x, 0, &zero,  vertex->voff10, ierr );
    }
    free(fwork);
    fwork = NULL;
#endif
  }

  for ( i=0; i < vertex->dimd; ++i) {
    if ( vertex->aoff10 != NULL )  doff = vertex->aoff10[i];
    else                           doff = 0;
    if ( vertex->data != 0 ) fwork = vertex->data[i];

    pf_wr_fltarray ( fid, prec_a, vertex->nv, fwork, doff, ierr );
  }

  if ( PFFMP_procs > 1 && fid->mp_on ) fid->mp_mode = PFFMP_CURRENT;

/* --------------------------------------------------------------------- */

  /* write the dataset length back to the header */
  pf_wr_lds ( fid, offset, &lds, ierr );
  PFFMP_Allreduce_1(fid, lds, tmplong, MPI_LONG, MPI_SUM);

  /* build a directory dataset -- don't destroy the VERTEX's header */
  (ds->head)->length = lds;
  dsdir = pf_bld_dir ( ds->head, offset, TRUE, ierr );

  /* put directory info into memory-resident directory structure -- 
     okay to discard dsdir structure since no longer needed */
  pf_dir_put ( fid, dsdir, FALSE, ierr );

  return;
}
