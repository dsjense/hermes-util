/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_list_files.c
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

#include <string.h>
#include "pff.h"
#include "filestack.h"
#include  "get_defs.h"

#define MAX_FILELEN         (64)
#define EXTRA_HEADLINE      (30)
#define EXTRA_FILELINE      (28)

/*  Declare function */

#ifdef __STDC__

void pf_list_files ( int width, int low, int high, int nptr,
                     int *ptr_fid, char **ptr_string,  int *ierr );

void pf_list_files ( int width, int low, int high, int nptr,
                     int *ptr_fid, char **ptr_string,  int *ierr )

#else

void       pf_list_files      ();

void pf_list_files ( width, low, high, nptr, ptr_fid, ptr_string, ierr )

int      width, low, high, nptr;
int     *ptr_fid;
char   **ptr_string;
int     *ierr;

#endif

/* 
       This routine is a FILE routine that prints some portion 
       of PFF's file structure to STDOUT.

    Input:  
      ptr_string  -  array of 2-character strings containing the pointer 
                    text for the user-defined pointers
      high        -  upper limit on file entry for list
      low         -  lower limit on file entry for list
      nptr        -  # of user-defined file pointers
      ptr_fid     -  array of file #s associated with user-defined pointers
      width       -  maximum width of listing in characters
      ierr        -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   = 0,  Normal return
*/
{
  /* static char        *module    = "PF_LIST_FILES"; */
  register int        i;
  int                 actwid, nblh, nblb, menmax, menoff;
  int                 cnt, curcnt, maxcnt;
  char                dash[MAX_FILELEN+1];
  char                blank[MAX_FILELEN+1];
  char               *prefix;
  char                tptr[3];
  static char        *mode_label[3] = { "RE" , "WR" , "RW" };
  PFFfid             *fid;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  low = MAX(1,low);
  if (PFF.top == NULL )
    maxcnt = 0;
  else
    maxcnt = (PFF.top)->count;

  if ( high > maxcnt  || high < 1 )
    high = maxcnt;

  if ( PFF.current == NULL )
    curcnt = 0;
  else
    curcnt = (PFF.current)->count;

  for ( i=0; i<MAX_FILELEN; ++i )   {
    dash[i] = '-';
    blank[i] = ' ';
  }
  dash[MAX_FILELEN] = '\0';
  blank[MAX_FILELEN] = '\0';

  actwid = MIN( width, 
                MAX ( MAX_FILELEN + EXTRA_FILELINE, EXTRA_HEADLINE ) );
  nblh   = MAX( 0, actwid - EXTRA_HEADLINE)/2;
  menmax = MIN( MAX_FILELEN, actwid - EXTRA_FILELINE);
  nblb   = MAX( 0, actwid - menmax - EXTRA_FILELINE);
  menoff = MAX_FILELEN - menmax;
  prefix = blank + MAX_FILELEN - nblb;

  printf("\n");
  printf("%sDIRECTORY of Active PFF Files\n", blank+MAX_FILELEN-nblh);
  printf("%s+-------+-%s-+----+---------+\n",prefix,dash+menoff);
  printf("%s|     # | File Name%s| St | Entries |\n",prefix,blank+menoff+8);
  printf("%s+-------+-%s-+----+---------+\n",prefix,dash+menoff);

  fid = pf_get_fid ( low, ierr);

  while (  ( fid != NULL )  &&  ( (cnt = fid->count) <= high ) )  {

    strncpy(tptr,"  ",2);
    if ( nptr < 1 )  {
      if ( cnt == curcnt )  {
        strncpy(tptr,"->",2);
      }
    }
    else 
      for ( i=0; i<nptr; ++i )  {
        if ( ptr_fid[i] == cnt )  {
          strncpy(tptr,ptr_string[i],2);
          break;
        }
      }

    printf("%s| %2.2s%3d | %-*.*s | %2.2s |  %5d  |\n",
                prefix, tptr, cnt, menmax, menmax, fid->name, 
                mode_label[fid->mode], (fid->dirtop)->count );

    fid = fid->up;
  }

  printf("%s+-------+-%s-+----+---------+\n",prefix,dash+menoff);

  return;
}
