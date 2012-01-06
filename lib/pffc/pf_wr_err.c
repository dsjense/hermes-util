/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_wr_err.c
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

/*  Declare function */

#ifdef __STDC__

void pf_wr_err ( char *module, int ierr, PFFfid *fid,  char *message );

void pf_wr_err ( char *module, int ierr, PFFfid *fid,  char *message )

#else

void       pf_wr_err       ();

void pf_wr_err ( module, ierr, fid, message )

PFFfid  *fid;
char    *module, *message;
int      ierr;

#endif

/* writes error message to STDOUT:

    Input:
      module  - name of module in which error occurred
      ierr    - Error #
      fid     - pointer to PFF file structure, if applicable
      message - error message

    Output:
      NONE
*/
{
  char      percent = '%';

  fprintf( stderr, "%cPFF-%s-%3.3d:  %s\n", percent, module, ierr, message );

  if  ( fid  != NULL ) 
    fprintf( stderr, "*** File = %s , Mode = %d\n", fid->name , fid->mode );
}
