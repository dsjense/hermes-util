/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_bld_header.c
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
#include  "bld_defs.h"

/*  Declare function */

#ifdef __STDC__

PFFhead *pf_bld_header ( int raw, int app, char *title, char *type, 
                         long len, int ver, int new_strs, int *ierr );

PFFhead *pf_bld_header ( int raw, int app, char *title, char *type, 
                         long len, int ver, int new_strs, int *ierr )

#else

PFFhead *pf_bld_header ();

PFFhead *pf_bld_header ( raw, app, title, type, len, ver, new_strs, ierr )

int raw, app, ver, new_strs;
char *title, *type;
long len;
int *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine is a BLD routine that builds a PFF header
         structure from supplied data.

------------------------------------------------------------------------

    Input:
      raw      -  raw dataset type
      app      -  application dataset type
      title    -  dataset title string
      type     -  dataset type string
      len      -  dataset length
      ver      -  dataset version number
      new_strs -  flag indicating that a new title and type strings must be 
                  created because the supplied strings must be preserved
                  WARNING: if FALSE, it is critical that the strings not be 
                  later freed by the calling program.  Immediately setting 
                  them to NULL will help prevent this.
      ierr     -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  Normal return
                   =  2,  Error allocating memory for header structure
*/
{
  static char    *module    = "PF_BLD_HEADER";
  PFFhead        *head      =  NULL;
  char *p;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  /* get memory for header structure */

  if ( (head = (PFFhead *) malloc( sizeof(PFFhead) )) == NULL )  {
    *ierr = 2;
    pf_wr_err ( module, *ierr, NULL, 
                "Error allocating memory for header structure");
    return NULL;
  }
  else  {
    head->rawtype    = raw;
    head->apptype    = app;
    head->length     = len;
    head->ds_version = ver;
    head->nwords_rfu = 0;
    head->rfu        = NULL;

    char blank[] = "";
    if ( new_strs )   {
      if ( title == NULL )  title = blank;
      if ( ( p = (char *) malloc ( strlen(title) + 1 ) )  ==  NULL )  {
        *ierr = 3;
        pf_wr_err ( module, *ierr, NULL, "Error allocating header strings");
        return NULL;
      }
      strcpy ( p, title );
      head->title = p;

      if ( type == NULL )  type = blank;
      if ( ( p = (char *) malloc ( strlen(type)+ 1 ) )  ==  NULL )  {
        *ierr = 3;
        pf_wr_err ( module, *ierr, NULL, "Error allocating header strings");
        return NULL;
      }
      strcpy ( p, type );
      head->type_name = p;
    }
    else    {
      head->title     = title;
      head->type_name = type;
    }
  }

  return head;
}
