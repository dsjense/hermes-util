/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_bld_vrtx.c
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
#include "bld_defs.h"

/*  Declare function */

#ifdef __STDC__

PFFds_vertex *pf_bld_vertex ( int app, char *title, char *type, int dims,
                              int dimd, long nv, int nspare, int alloc_method, 
                              int *spare, char **vlabel, char **dlabel, 
                              float *vert, int voff, float **data, int *doff, 
                              int new_strs, int new_labs, int new_arrs, 
                              int *ierr );

PFFds_vertex *pf_bld_vertex ( int app, char *title, char *type, int dims,
                              int dimd, long nv, int nspare, int alloc_method, 
                              int *spare, char **vlabel, char **dlabel, 
                              float *vert, int voff, float **data, int *doff, 
                              int new_strs, int new_labs, int new_arrs, 
                              int *ierr )

#else

PFFds_vertex *pf_bld_vertex ();

PFFds_vertex *pf_bld_vertex ( app, title, type, dims, dimd, nv, nspare, 
                              alloc_method, spare, vlabel, dlabel, vert, voff, 
                              data, doff, new_strs, new_labs, new_arrs, ierr )

int    app, dims, dimd, nspare, alloc_method, voff;
int    new_strs, new_labs, new_arrs;
long   nv;
char  *title, *type;
int   *spare, *doff;
char **vlabel, **dlabel;
float *vert;
float **data;
int *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine is a BLD routine that builds a PFF VERTEX dataset 
         structure from supplied data.

------------------------------------------------------------------------

    Input:
      app      -  application dataset type
      title    -  dataset title string
      type     -  dataset type string
      dims     -  dimensionality of grid
      dimd     -  dimensionality of data
      nv       -  number of verticies
      nspare   -  number of spare integers (0-5)
      alloc_method -  method of allocation for data array (used only if 
                      new_arrs is FALSE):
                        0 - data & data[0..dimd] allocated separately
                        1 - data & data[0] allocated separately
                        2 - data only allocated
      spare    -  array of spare integer
      vlabel   -  array of strings labeling each grid dimension
      dlabel   -  array of strings labeling each data dimension
      vert     -  pointer to array of vertices
      voff     -  power-of-10 offset for vertex array
      data     -  pointer to array of data array pointers for each data 
                  dimension
      doff     -  array of power-of-10 offsets for each data array
      new_strs -  flag indicating that new title and type strings must be 
                  created because the supplied strings must be preserved
      new_labs -  flag indicating that new labels (vlabel, dlabel) must be 
                  created because the supplied labels must be preserved
      new_arrs -  flag indicating that new arrays (spare, vert, data)
                  must be created because the supplied arrays must be 
                  preserved
                  WARNING: if new_strs, new_labs, or new_arrs are FALSE, it 
                  is critical that the strings and/or arrays not be later 
                  freed by the calling program.  Immediately setting them to 
                  NULL will help prevent this.
      ierr     -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  Normal return
                   =  2,  Error allocating memory for VERTEX structure
*/
{
  static char    *module    = "PF_BLD_VERTEX";
  PFFds_vertex   *vertex      =  NULL;
  int             j;
  long            i, tnv;
  char           *p;
  char          **pp;
  int            *iptr;
  float          *fptr, *f;
  float         **ff;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  /* get memory for VERTEX structure */

  if ( (vertex = (PFFds_vertex *) malloc( sizeof(PFFds_vertex) )) == NULL )  {
    *ierr = 2;
    pf_wr_err ( module, *ierr, NULL, 
                "Error allocating memory for VERTEX structure");
    return NULL;
  }

  vertex->type    = PFTVTX;
  vertex->head = pf_bld_header ( PFTVTX, app, title, type, DFAULT, DFAULT,  
                          new_strs, ierr );
  if( *ierr != 0 )   {
    CHKFREE(vertex);
    return NULL;
  }

  vertex->dims = dims;
  vertex->dimd = dimd;
  vertex->nv   = nv;
  vertex->nspare = nspare;
  vertex->voff10 = voff;

  if ( new_labs )   {
    if ( dims > 0 )  {
      if ( ( pp = (char **) malloc ( dims*sizeof(char *) ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for VERTEX structure");
        return NULL;
      }
      p = NULL;
      for( j=0; j<dims; j++ )  {
        if ( vlabel != NULL )  {
          if ( ( p = (char *) malloc ( strlen(vlabel[j])+1 ) )  ==  NULL )  {
            *ierr = 2;
            pf_wr_err ( module, *ierr, NULL, 
                        "Error allocating memory for VERTEX structure");
            return NULL;
          }
          strcpy ( p, vlabel[j] );
        }
        pp[j] = p;
      }
      vertex->vlabel = pp;
    }
    else  vertex->vlabel = NULL;

    if ( dimd > 0 )  {
      if ( ( pp = (char **) malloc ( dimd*sizeof(char *) ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for VERTEX structure");
        return NULL;
      }
      p = NULL;
      for( j=0; j<dimd; j++ )  {
        if ( dlabel != NULL )  {
          if ( ( p = (char *) malloc ( strlen(dlabel[j])+1 ) )  ==  NULL )  {
            *ierr = 2;
            pf_wr_err ( module, *ierr, NULL, 
                        "Error allocating memory for VERTEX structure");
            return NULL;
          }
          strcpy ( p, dlabel[j] );
        }
        pp[j] = p;
      }
      vertex->dlabel = pp;
    }
    else  vertex->dlabel = NULL;
  }
  else    {
    vertex->vlabel = vlabel;
    vertex->dlabel = dlabel;
  }

  if ( new_arrs )   {
    if ( nspare > 0 )   {
      if ( ( iptr = (int *) malloc ( nspare*sizeof(int) ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for VERTEX structure");
        return NULL;
      }
      for( j=0; j<nspare; j++ )  iptr[j] = spare[j];
      vertex->spare = iptr;
    }
    else  vertex->spare = NULL;

    tnv = dims*nv;
    if ( tnv > 0 )   {
      if ( ( fptr = (float *) malloc ( tnv*sizeof(float) ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for VERTEX structure");
        return NULL;
      }
      for( i=0; i<tnv; i++ )  fptr[i] = vert[i];
      vertex->vert = fptr;
    }
    else  vertex->vert = NULL;

    vertex->alloc_method = 2;
    if ( dimd*nv > 0 )   {
      if ( ( ff = (float **) malloc ( dimd*sizeof(float *) +
                                      dimd*nv*sizeof(float)  ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for VERTEX structure");
        return NULL;
      }
      fptr = (float *) (ff + dimd);
      for( j=0; j<dimd; j++ )    {
        ff[j] = fptr;
        for( f=data[j],i=0; i<nv; f++,i++,fptr++ )  *fptr = *f;
      }
      vertex->data = ff;

      if ( doff != NULL )  {
        if ( ( iptr = (int *) malloc ( dimd*sizeof(int) ) )  ==  NULL )  {
          *ierr = 2;
          pf_wr_err ( module, *ierr, NULL, 
                      "Error allocating memory for VERTEX structure");
          return NULL;
        }
        for( j=0; j<dimd; j++ )  iptr[j] = doff[j];
        vertex->aoff10 = iptr;
      }
      else  vertex->aoff10 = NULL;
    }
    else  {
      vertex->data = NULL;
      vertex->aoff10 = NULL;
    }
  }
  else    {
    vertex->alloc_method = alloc_method;
    vertex->spare  = spare;
    vertex->vert   = vert;
    vertex->data   = data;
    vertex->aoff10 = doff;
  }

  return vertex;
}
