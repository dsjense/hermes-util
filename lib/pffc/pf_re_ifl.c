/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_re_ifl.c
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
#include "ds_structs.h"
#include "workspace.h"

#include "free_defs.h"
#include   "re_defs.h"
#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

PFFds_any *pf_re_ifl ( PFFfid   *fid ,int keep, int *ierr );

PFFds_any *pf_re_ifl ( PFFfid   *fid ,int keep, int *ierr )

#else

PFFds_any   *pf_re_ifl    ();

PFFds_any *pf_re_ifl ( fid, keep, ierr )

PFFfid   *fid; 
int       keep;
int      *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine is a READ routine that reads a IFL (Integer/Float 
         List) dataset from a PFF file.
       - Floating data is divided into two groups:
           1)  Float List -- each value is encoded independently as a 
               <FLOAT> at the full precision of this data type.
           2)  Float Array -- the entire array is encoded as an 
               <FARRAY>.  This uses less space but has dynamic range 
               limitations for data with multi-order-of-magnitude 
               variations.
       - An integer flag is used to indicate if the float array is 
         empty.  (flag = 0 means empty)
       - This operation is ONLY ALLOWED in READ mode !!!
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
      keep    -  flag indicating whether or not to keep a non-zero value 
                 for floating data in the case of underflow
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  Normal return
                   = -1,  EOF reached  (not really an error)
                   =  1,  Incorrect dataset type
                   =  2,  Error allocating memory for IFL structure or 
                          contents
*/
{
  static char    *module    = "PF_RE_IFL";
  register long   i;
  long            offset;
  int             temp;
  PFFhead        *head =  NULL; 
  PFFds_ifl      *ifl  =  NULL;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  head = pf_re_header ( fid, &offset, ierr );
  if( *ierr != 0 ) return NULL;

  if ( head->rawtype != PFTIFL )  {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Incorrect dataset type");
  }

  else    {

    /* get memory for IFL structure */

    if ( (ifl = (PFFds_ifl *) malloc( sizeof(PFFds_ifl) )) == NULL )  {
      *ierr = 2;
      pf_wr_err ( module, *ierr, fid, 
                  "Error allocating memory for IFL structure or contents");
    }
    else  {

      ifl->type = PFTIFL;
      ifl->head = head;
      head = NULL;
      ifl->iarr    = NULL;
      ifl->farr    = NULL;
      ifl->flist   = NULL;
      ifl->floff10 = NULL;

      /* read in float flag  and float-list length */

      pf_u_sio( fid, RE, 1, &temp, ierr );
      pf_u_sio( fid, RE, 3, PFF_work_buf, ierr );
      pf_u_i2l( PFF_work_buf, &(ifl->nflt), ierr);

      /* read in integer array */

      ifl->iarr = pf_re_intarray ( fid, &(ifl->ni), ierr);

      if (ifl->nflt > 0 )   {
        if ( (ifl->flist = (float *) malloc( ifl->nflt*sizeof(float) )) 
                                                                == NULL )  {
          *ierr = 2;
          pf_wr_err ( module, *ierr, fid, 
                    "Error allocating memory for IFL structure or contents");
        }
        if ( (ifl->floff10 = (int *) malloc( ifl->nflt*sizeof(int) )) 
                                                                == NULL )  {
          *ierr = 2;
          pf_wr_err ( module, *ierr, fid, 
                    "Error allocating memory for IFL structure or contents");
        }
        if ( *ierr == 0 )  {
          for ( i=0; i<ifl->nflt; ++i )    {
            pf_u_sio( fid, RE, 3, PFF_work_buf, ierr );
            pf_u_i2f( keep, PFF_work_buf, ifl->flist+i, ifl->floff10+i, ierr );
          }
        }
      }
      else   {
        ifl->flist   = NULL;
        ifl->floff10 = NULL;
      }

      if (temp != 0 )   {
        ifl->farr = pf_re_fltarray ( fid, keep, &(ifl->nf), &(ifl->faoff10), 
                                                                         ierr);
      }
      else    {
        ifl->farr    = NULL;
        ifl->nf      = 0;
        ifl->faoff10 = 0;
      }

      if ( *ierr != 0 ) 
        pf_free_ifl ( ( PFFds_any* ) ifl, &temp );
    }
  }

  CHKFREE (head);
  return ( PFFds_any* ) ifl;
}
