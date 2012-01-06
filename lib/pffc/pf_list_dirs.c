/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_list_dirs.c
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
#include  "get_defs.h"

#define MAX_TITLELEN        (64)
#define EXTRA_FILELINE      (29)
#define EXTRA_DIRLINE       (32)

/*  Declare function */

#ifdef __STDC__

void pf_list_dirs ( PFFfid *fid, int width, int low, int high, int nptr,
                        int *ptr_dir, char **ptr_string,  int *ierr );

void pf_list_dirs ( PFFfid *fid, int width, int low, int high, int nptr,
                        int *ptr_dir, char **ptr_string,  int *ierr )

#else

void       pf_list_dirs      ();

void pf_list_dirs ( fid, width, low, high, nptr, ptr_dir, ptr_string, ierr )

PFFfid  *fid;
int      width, low, high, nptr;
int     *ptr_dir;
char   **ptr_string;
int     *ierr;

#endif

/* 
       This routine is a DIRECTORY routine that prints some portion 
       of a PFF file's directory structure to STDOUT.

    Input:  
      ptr_string  -  array of 2-character strings containing the pointer 
                    text for the user-defined pointers
      fid         -  pointer to PFF file structure
      high        -  upper limit on directory entry for list
      low         -  lower limit on directory entry for list
      nptr        -  # of user-defined directory pointers
      ptr_dir     -  array of directory #s associated with user-defined pointers
      width       -  maximum width of listing in characters
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   = 0,  Normal return
                   = 1,  Illegal File ID (FID)
*/
{
  static char        *module    = "PF_LIST_DIRS";
  register int        i;
  int                 lfil, actwid, nblh, nblb, menmax, menoff;
  int                 cnt, curcnt, maxcnt;
  int                 enddat, ptrflg;
  static char         eodstr[] = "END-OF-DATA";
  char                dash[MAX_TITLELEN+1];
  char                blank[MAX_TITLELEN+1];
  char               *prefix;
  char                tptr[3];
  PFFdir  *dir;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( ( fid == NULL ) || ( fid->stream == NULL ) ) {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Illegal File ID (FID)" );
    return;
  }

  enddat = FALSE;
  low = MAX(1,low);
  if (fid->dirtop == NULL )
    maxcnt = 0;
  else
    maxcnt = (fid->dirtop)->count;

  if ( high > maxcnt  || high < 1 )   {
    high = maxcnt;
    enddat = TRUE;
  }
  if (fid->directory == NULL )
    curcnt = 0;
  else
    curcnt = (fid->directory)->count;

  for ( i=0; i<MAX_TITLELEN; ++i )   {
    dash[i] = '-';
    blank[i] = ' ';
  }
  dash[MAX_TITLELEN] = '\0';
  blank[MAX_TITLELEN] = '\0';

  lfil   = MAX( 1, strlen(fid->name) );
  actwid = MIN( width, 
                MAX( MAX_TITLELEN + EXTRA_DIRLINE , lfil + EXTRA_FILELINE ) );
  nblh   = MAX( 0, actwid - lfil - EXTRA_FILELINE)/2;
  menmax = MIN( MAX_TITLELEN, actwid - EXTRA_DIRLINE);
  nblb   = MAX( 0, actwid - menmax - EXTRA_DIRLINE);
  menoff = MAX_TITLELEN - menmax;
  prefix = blank + MAX_TITLELEN - nblb;

  printf("\n");
  printf("%sDIRECTORY LISTING of File:  %s\n",
                                        blank+MAX_TITLELEN-nblh,fid->name);

  printf("%s+-------+------------------+-%s-+\n",prefix,dash+menoff);
  printf("%s|     # | Data Type        | Title%s|\n",prefix,blank+menoff+4);
  printf("%s+-------+------------------+-%s-+\n",prefix,dash+menoff);

  dir = pf_get_direntry ( fid, low, ierr);

  while (  ( dir != NULL )  &&  ( (cnt = dir->count) <= high ) )  {

    ptrflg = FALSE;
    if ( nptr < 1 )  {
      if ( cnt == curcnt )  {
        strncpy(tptr,"->",2);
        ptrflg = TRUE;
      }
    }
    else { 
      for ( i=0; i<nptr; ++i )  {
        if ( ptr_dir[i] == cnt )  {
          strncpy(tptr,ptr_string[i],2);
          ptrflg = TRUE;
          break;
        }
      }
    }

    if ( ptrflg ) {
      if ( cnt < 10000 )
        printf("%s|%2.2s%4d | %-16.16s | %-*.*s |\n",
              prefix,tptr,cnt,dir->type_name,menmax,menmax,dir->title);
      else
        printf("%s|%2.2s%4.4d | %-16.16s | %-*.*s |\n",
              prefix,tptr,cnt % 10000,dir->type_name,menmax,menmax,dir->title);
    }
    else
      printf("%s| %5d | %-16.16s | %-*.*s |\n",
                      prefix,cnt,dir->type_name,menmax,menmax,dir->title);

    dir = dir->up;
  }

  if ( enddat )   {

    cnt = maxcnt + 1;

    ptrflg = FALSE;
    if ( nptr < 1 ) {
      if ( cnt == curcnt )  {
        strncpy(tptr,"->",2);
        ptrflg = TRUE;
      }
    }
    else {
      for ( i=0; i<nptr; ++i ) {
        if ( ptr_dir[i] == cnt )  {
          strncpy(tptr,ptr_string[i],2);
          ptrflg = TRUE;
          break;
        }
      }
    }

    if ( ptrflg ) {
      if ( cnt < 10000 )
        printf("%s|%2.2s%4d | %-16.16s | %-*.*s |\n",
              prefix,tptr,cnt,eodstr,menmax,menmax,blank);
      else
        printf("%s|%2.2s%4.4d | %-16.16s | %-*.*s |\n",
              prefix,tptr,cnt % 10000,eodstr,menmax,menmax,blank);
    }
    else
      printf("%s| %5d | %-16.16s | %-*.*s |\n",
              prefix,cnt,eodstr,menmax,menmax,blank);
    
  }

  printf("%s+-------+------------------+-%s-+\n",prefix,dash+menoff);

  return;
}
