/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_bld_ifl.c
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
#include  "bld_defs.h"

/*  Declare function */

#ifdef __STDC__

PFFds_ifl *pf_bld_ifl ( int app, char *title, char *type, long ni, long nfl, 
                        long nf, int *iarr, float *flist, int *floff, 
                        float *farr, int faoff, int new_strs, int new_arrs, 
                        int *ierr );

PFFds_ifl *pf_bld_ifl ( int app, char *title, char *type, long ni, long nfl, 
                        long nf, int *iarr, float *flist, int *floff, 
                        float *farr, int faoff, int new_strs, int new_arrs, 
                        int *ierr )

#else

PFFds_ifl *pf_bld_ifl ();

PFFds_ifl *pf_bld_ifl ( app, title, type, ni, nfl, nf, iarr, flist, 
                        floff, farr, faoff, new_strs, new_arrs, ierr )

int app, faoff, new_strs, new_arrs;
char *title, *type;
long ni, nfl, nf;
int *iarr, *floff;
float *flist, *farr;
int *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine is a BLD routine that builds a PFF IFL dataset 
         structure from supplied data.

------------------------------------------------------------------------

    Input:
      app      -  application dataset type
      title    -  dataset title string
      type     -  dataset type string
      ni       -  length of integer array
      nfl      -  length of float list
      nf       -  length of float array
      iarr     -  integer array
      flist    -  float list
      floff    -  power-of-ten offset for each element in float list
                  (if floff=NULL, zero is used for all list elements)
      farr     -  float array
      faoff    -  power-of-ten offset for entire float array
      new_strs -  flag indicating that new title and type strings must be 
                  created because the supplied strings must be preserved
      new_arrs -  flag indicating that new arrays (iarr, flist, floff, farr)
                  must be created because the supplied arrays must be 
                  preserved
                  WARNING: if new_strs or new_arrs are FALSE, it is critical 
                  that the strings and/or arrays not be later freed by the 
                  calling program.  Immediately setting them to NULL will 
                  help prevent this.
      ierr     -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  Normal return
                   =  2,  Error allocating memory for IFL structure
*/
{
  static char    *module    = "PF_BLD_IFL";
  PFFds_ifl      *ifl      =  NULL;
  register long   i;
  int            *iptr;
  float          *fptr;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  /* get memory for IFL structure */

  if ( (ifl = (PFFds_ifl *) malloc( sizeof(PFFds_ifl) )) == NULL )  {
    *ierr = 2;
    pf_wr_err ( module, *ierr, NULL, 
                "Error allocating memory for IFL structure");
    return NULL;
  }

  ifl->type    = PFTIFL;
  ifl->head = pf_bld_header ( PFTIFL, app, title, type, DFAULT, DFAULT,  
                          new_strs, ierr );
  if( *ierr != 0 )   {
    CHKFREE(ifl);
    return NULL;
  }

  ifl->ni   = ni;
  ifl->nf   = nf;
  ifl->nflt = nfl;
  ifl->faoff10 = faoff;

  if ( new_arrs )   {
    if ( ni > 0 )   {
      if ( ( iptr = (int *) malloc ( ni*sizeof(int) ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for IFL structure");
        return NULL;
      }
      for( i=0; i<ni; i++ )  iptr[i] = iarr[i];
      ifl->iarr = iptr;
    }
    else  ifl->iarr = NULL;

    if ( nf > 0 )   {
      if ( ( fptr = (float *) malloc ( nf*sizeof(float) ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for IFL structure");
        return NULL;
      }
      for( i=0; i<nf; i++ )  fptr[i] = farr[i];
      ifl->farr = fptr;
    }
    else  ifl->farr = NULL;

    if ( nfl > 0 )   {
      if ( ( fptr = (float *) malloc ( nfl*sizeof(float) ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for IFL structure");
        return NULL;
      }
      for( i=0; i<nfl; i++ )  fptr[i] = flist[i];
      ifl->flist = fptr;

      if ( floff != NULL )  {
        if ( ( iptr = (int *) malloc ( nfl*sizeof(int) ) )  ==  NULL )  {
          *ierr = 2;
          pf_wr_err ( module, *ierr, NULL, 
                      "Error allocating memory for IFL structure");
          return NULL;
        }
        for( i=0; i<nfl; i++ )  iptr[i] = floff[i];
        ifl->floff10 = iptr;
      }
      else  ifl->floff10 = NULL;
    }
    else  ifl->flist = NULL;
  }
  else    {
    ifl->iarr    = iarr;
    ifl->farr    = farr;
    ifl->flist   = flist;
    ifl->floff10 = floff;
  }

  return ifl;
}
