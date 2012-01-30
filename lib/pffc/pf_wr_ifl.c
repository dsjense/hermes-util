/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_wr_ifl.c
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
#include "ds_structs.h"
#include "workspace.h"
#include  "bld_defs.h"
#include  "dir_defs.h"
#include   "wr_defs.h"
#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_wr_ifl ( PFFfid   *fid ,  PFFds_any *ds, int *ierr );

void pf_wr_ifl ( PFFfid   *fid ,  PFFds_any *ds, int *ierr )

#else

void pf_wr_ifl ();
void pf_wr_ifl ( fid, ds, ierr )

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
           <HEADER>       PFTIFL
           <INT>          FLTFLG            (Float flag)
           <LONG>         NFL               (length of Float List)
           <IARRAY>       IARRAY(1:NI)      (integer array)
           LOOP i=1,NFL
             <FLOAT>        FLIST(i)        (ith value in Float List)
           ENDLOOP
           IF (FLTFLG.NE.0)
             <FARRAY>       FARRAY(1:NFA)   (floating array)
           ENDIF

------------------------------------------------------------------------

    Input:
      fid     -  pointer to PFF file structure
      ds      -  general dataset structure to be written to file [needs to
                 be cast to (PFFds_ifl *) before using]
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,        Normal return
                   =  1,        Incorrect dataset type
                   otherwise,   Error from called PFF utility routine
*/
{
  static char    *module    = "PF_WR_IFL";
  long            offset, lds;
  PFFds_ifl      *ifl;
  PFFds_dir      *dsdir;
  int             loc;
  int             prec = FP_REDU;
  long            i, j, top;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( ds->type != PFTIFL || (ds->head)->rawtype != PFTIFL )  {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Incorrect dataset type");
  }

  /* Write header */
  (ds->head)->length = DFAULT;
  pf_wr_header ( fid, ds->head, &offset, ierr );
  if( *ierr != 0 ) return;

/* --------------------------------------------------------------------- */

  ifl = (PFFds_ifl *) ds;

  /* Write Float Flag and Float List length */
  if ( ifl->nf == 0 ) PFF_work_buf[0] = 0;
  else                PFF_work_buf[0] = DFAULT;

  pf_u_l2i( ifl->nflt, PFF_work_buf+1, ierr);
  pf_u_sio( fid, WR, 4, PFF_work_buf, ierr );

  /* write the integer array */
  pf_wr_intarray ( fid, ifl->ni, ifl->iarr, ierr );

  /* write the Float List */
  for ( j=0; j<ifl->nflt; j=top )   {
    top = MIN(ifl->nflt, j + PFF_RECLEN/3 );
    for ( loc=0,i=j; i<top; i++,loc+=3 )  {
      if ( ifl->floff10 == NULL ) 
           pf_u_f2i( ifl->flist[i],               0, PFF_work_buf+loc, ierr );
      else pf_u_f2i( ifl->flist[i], ifl->floff10[i], PFF_work_buf+loc, ierr );
    }
    pf_u_sio ( fid, WR, loc, PFF_work_buf, ierr );
  }

  /* write the float array */
  if ( ifl->nf > 0 ) {
    if ( fid->fp_precision == FP_ALLFULL ) prec = FP_FULL;
    pf_wr_fltarray ( fid, prec, ifl->nf, ifl->farr, ifl->faoff10, ierr );
  }

/* --------------------------------------------------------------------- */

  /* write the dataset length back to the header */
  pf_wr_lds ( fid, offset, &lds, ierr );

  /* build a directory dataset -- don't destroy the IFL's header */
  (ds->head)->length = lds;
  dsdir = pf_bld_dir ( ds->head, offset, TRUE, ierr );

  /* put directory info into memory-resident directory structure -- 
     okay to discard dsdir structure since no longer needed */
  pf_dir_put ( fid, dsdir, FALSE, ierr );

  return;
}
