/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_fill_ublock.c
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

/* Temporary define */
#define PFTUGD    (999)

#include <stdlib.h>
#include <string.h>
#include "pff.h"
#include "bld_defs.h"
#include "fill_defs.h"
#include "free_defs.h"

/*  Declare function */

#ifdef __STDC__

void pf_fill_ublock ( PFFds_uniform *uniform, int iblk, int nspare, 
                      int alloc_method, int *spare, long *nx, float *x0, 
                      float *dx, int goff10, float **data, int foff10, 
                      char **glabel, char **dlabel, char *blabel, 
                      int new_labs, int new_arrs, int *ierr );

void pf_fill_ublock ( PFFds_uniform *uniform, int iblk, int nspare, 
                      int alloc_method, int *spare, long *nx, float *x0, 
                      float *dx, int goff10, float **data, int foff10, 
                      char **glabel, char **dlabel, char *blabel, 
                      int new_labs, int new_arrs, int *ierr )

#else

void pf_fill_ublock ();

void pf_fill_ublock ( uniform, iblk, nspare, alloc_method, spare, nx, x0, dx, 
                      goff10, data, foff10, glabel, dlabel, blabel, new_labs, 
                      new_arrs, ierr )

PFFds_uniform *uniform;
int            iblk, nspare, alloc_method, goff10, foff10;
int            new_labs, new_arrs;
int           *spare;
long          *nx;
float         *x0, *dx;
float        **data;
char         **glabel, **dlabel;
char          *blabel;
int           *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine is a BLD routine that builds a PFF UNIFORM dataset 
         structure from supplied data.
       - Although it allocates the BLOCK structures needed for the dataset,
         it does not actually populate those BLOCKs with data.  That can be
         done, for example, by using the function pf_fill_block.
       - If any of the supplied pointers to data (spare, nx, x, data, glabel,
         dlabel, blabel) are NULL, their corresponding values in the block 
         being filled REMAIN UNCHANGED.

------------------------------------------------------------------------

    Input:
      uniform  -  dataset structure for which a block is to be filled
      iblk     -  block number (0 to uniform->nblk-1)
      nspare   -  number of spare integers (0-5)
      alloc_method -  method of allocation for data and grid arrays (used 
                      only if new_arrs is FALSE):
                        0 - data & data[0..dimd] allocated separately
                        1 - data & data[0] allocated separately
                        2 - data only allocated
                        DFAULT - Don't free data at all
      spare    -  array of spare integers
      nx       -  number of grid points in each grid dimension
      x0       -  starting grid location for each grid dimension
      dx       -  grid spacing for each grid dimension
      goff10   -  power-of-10 offset for grid arrays
      data     -  pointer to array of data array pointers for each data 
                  dimension
      goff10   -  power-of-10 offset for data arrays
      glabel   -  array of strings labeling each grid dimension
      dlabel   -  array of strings labeling each data dimension
      blabel   -  string label for this block
      new_labs -  flag indicating that new labels (vlabel, dlabel) must be 
                  created because the supplied labels must be preserved
      new_arrs -  flag indicating that new arrays (spare, vert, data)
                  must be created because the supplied arrays must be 
                  preserved
                  WARNING: if new_labs or new_arrs are FALSE, it is critical 
                  that the strings and/or arrays not be later freed by the 
                  calling program.  Immediately setting them to NULL will 
                  help prevent this.
      ierr     -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  Normal return
                   =  1,  Incorrect dataset type
                   =  2,  Error allocating memory for BLOCK contents
                   =  3,  No block allocated
*/
{
  static char          *module      = "PF_FILL_UBLOCK";
  PFFblock_uniform     *block       =  NULL;
  int             j, dims, dimd;
  long            i, tnv;
  char           *p;
  char          **pp;
  int            *iptr;
  long           *lptr;
  float          *fptr, *f;
  float         **ff;
  int             UNF_type = DFAULT;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  /* is this a legal UNIFORM dataset raw type ? */
  if ( uniform->type != PFTUGD )   {
    for ( j=0; PFF_legal_uniform[j] != DFAULT; j++ )  {
      /* if it does, is it also single block */
      if ( uniform->type == PFF_legal_uniform[j] )  {
        UNF_type = uniform->type;
      }
    }
  }  else  UNF_type = PFTUGD;
  if ( UNF_type == DFAULT )  {
    *ierr = 1;
    pf_wr_err ( module, *ierr, NULL, "Incorrect dataset type");
  }
  if( *ierr != 0 ) return;

  if ( uniform->block == NULL || 
       (block = uniform->block[iblk]) == NULL )  {
    *ierr = 3;
    pf_wr_err ( module, *ierr, NULL, "No block allocated");
 }
 if( *ierr != 0 ) return;

  block->nspare = nspare;
  block->goff10 = goff10;
  block->foff10 = foff10;

  dims = uniform->dims;
  dimd = uniform->dimd;

  if ( new_labs )   {
    if ( glabel != NULL && dims > 0 )  {
      if ( ( pp = (char **) malloc ( dims*sizeof(char *) ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for BLOCK structure");
        return;
      }
      for( j=0; j<dims; j++ )  {
        if ( ( p = (char *) malloc ( strlen(glabel[j])+1 ) )  ==  NULL )  {
          *ierr = 2;
          pf_wr_err ( module, *ierr, NULL, 
                      "Error allocating memory for BLOCK structure");
          return;
        }
        strcpy ( p, glabel[j] );
        pp[j] = p;
      }
      block->glabel = pp;
    }

    if ( dlabel != NULL && dimd > 0 )  {
      if ( ( pp = (char **) malloc ( dimd*sizeof(char *) ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for BLOCK structure");
        return;
      }
      for( j=0; j<dimd; j++ )  {
        if ( ( p = (char *) malloc ( strlen(dlabel[j])+1 ) )  ==  NULL )  {
          *ierr = 2;
          pf_wr_err ( module, *ierr, NULL, 
                      "Error allocating memory for BLOCK structure");
          return;
        }
        strcpy ( p, dlabel[j] );
        pp[j] = p;
      }
      block->dlabel = pp;
    }

    if ( blabel != NULL && dimd > 0 )  {
      if ( ( p = (char *) malloc ( strlen(blabel)+1 ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for BLOCK structure");
        return;
      }
      strcpy ( p, blabel );
      block->blabel = p;
    }
  }
  else    {
    if ( glabel != NULL ) block->glabel = glabel;
    if ( dlabel != NULL ) block->dlabel = dlabel;
    if ( blabel != NULL ) block->blabel = blabel;
  }

  if ( new_arrs )   {
    if ( spare != NULL && nspare > 0 )   {
      if ( ( iptr = (int *) malloc ( nspare*sizeof(int) ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for BLOCK structure");
        return;
      }
      for( j=0; j<nspare; j++ )  iptr[j] = spare[j];
      block->spare = iptr;
    }

    if ( nx != NULL && dims > 0 )   {
      if ( ( lptr = (long *) malloc ( dims*sizeof(long) ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for BLOCK structure");
        return;
      }
      for( j=0; j<dims; j++ )  lptr[j] = nx[j];
      block->nx = lptr;
    }

    if ( x0 != NULL && dims > 0 )   {
      if ( ( fptr = (float *) malloc ( dims*sizeof(float) ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for BLOCK structure");
        return;
      }
      for( j=0; j<dims; j++ )  fptr[j] = x0[j];
      block->x0 = fptr;
    }

    if ( dx != NULL && dims > 0 )   {
      if ( ( fptr = (float *) malloc ( dims*sizeof(float) ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for BLOCK structure");
        return;
      }
      for( j=0; j<dims; j++ )  fptr[j] = dx[j];
      block->dx = fptr;
    }

    if ( dimd > 0 && data != NULL )   {
      block->alloc_method = 2;
      for( tnv=1,j=0; j<dims; j++ ) tnv *= nx[j];
      if ( ( ff = (float **) malloc ( dimd*sizeof(float *) +
                                      dimd*tnv*sizeof(float) ) )  ==  NULL )  {
        *ierr = 2;
        pf_wr_err ( module, *ierr, NULL, 
                    "Error allocating memory for BLOCK structure");
        return;
      }
      fptr = (float *) (ff + dimd);
      for( j=0; j<dimd; j++ )    {
        ff[j] = fptr;
        for( f=data[j],i=0; i<tnv; f++,i++,fptr++ )  *fptr = *f;
      }
      block->data = ff;
    }
  }
  else    {
    block->alloc_method = alloc_method;
    if ( spare != NULL && nspare > 0 ) block->spare  = spare;
    if ( nx    != NULL && dims   > 0 ) block->nx     = nx;
    if ( x0    != NULL && dims   > 0 ) block->x0     = x0;
    if ( dx    != NULL && dims   > 0 ) block->dx     = dx;
    if ( data  != NULL && dimd   > 0 ) block->data   = data;
  }

  return;
}
