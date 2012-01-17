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

void pf_list_dirs ( FILE *out, PFFfid *fid, int width, int low, int *phigh,
                    int nptr, int *ptr_dir, char **ptr_string,  int *ierr );

void pf_list_dirs ( FILE *out, PFFfid *fid, int width, int low, int *phigh,
                    int nptr, int *ptr_dir, char **ptr_string,  int *ierr )

#else

void       pf_list_dirs      ();

void pf_list_dirs ( out, fid, width, low, phigh, nptr, ptr_dir, ptr_string,
                    ierr )

FILE    *out;
PFFfid  *fid;
int      width, low, *phigh, nptr;
int     *ptr_dir;
char   **ptr_string;
int     *ierr;

#endif

/* 
       This routine is a DIRECTORY routine that prints some portion 
       of a PFF file's directory structure to a C file stream.

    Input:
      out         -  A C file stream to which the list is written 
      fid         -  pointer to PFF file structure
      phigh       -  if low >= 0: directory entry upper limit (pointer to)
                        low <  0: array of directory entries to be printed
      low         -  if low >= 0: lower limit on directory entry for list
                        low <  0: minus of # of entries in the directory
                                  entry list 
      nptr        -  # of user-defined directory pointers
      ptr_dir     -  array of directory #s associated with user-defined pointers
      width       -  maximum width of listing in characters
      ptr_string  -  array of 2-character strings containing the pointer 
                     text for the user-defined pointers
      ierr        -  If not zero, return with no operation

    Output:
      ierr        -  error flag:
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
  PFFdir             *dir;
  int                 high = 0;
  int                 nmap = 0;
  int                 imap = 0;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return;

  if ( ( fid == NULL ) || ( fid->stream == NULL ) ) {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Illegal File ID (FID)" );
    return;
  }

  enddat = FALSE;
  if ( low < 0 ) nmap = -low;
  else high = *phigh;

  low = MAX(1,low);
  if (fid->dirtop == NULL )
    maxcnt = 0;
  else
    maxcnt = (fid->dirtop)->count;

  if ( nmap == 0 ) {
    if ( high > maxcnt  || high < 1 )   {
      high = maxcnt;
      enddat = TRUE;
    }
  }
  else low = phigh[0];

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

  fprintf(out,"\n");
  fprintf(out,"%sDIRECTORY LISTING of File:  %s\n",
                                        blank+MAX_TITLELEN-nblh,fid->name);

  fprintf(out,"%s+-------+------------------+-%s-+\n",prefix,dash+menoff);
  fprintf(out,"%s|     # | Data Type        | Title%s|\n",prefix,blank+menoff+4);
  fprintf(out,"%s+-------+------------------+-%s-+\n",prefix,dash+menoff);

  dir = pf_get_direntry ( fid, low, ierr);
  if ( dir != NULL ) cnt = dir->count;
  while (  dir != NULL )  {

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
        fprintf(out,"%s|%2.2s%4d | %-16.16s | %-*.*s |\n",
                prefix,tptr,cnt,dir->type_name,menmax,menmax,dir->title);
      else
        fprintf(out,"%s|%2.2s%4.4d | %-16.16s | %-*.*s |\n",
                prefix,tptr,cnt % 10000,dir->type_name,menmax,menmax,dir->title);
    }
    else
      fprintf(out,"%s| %5d | %-16.16s | %-*.*s |\n",
              prefix,cnt,dir->type_name,menmax,menmax,dir->title);

    if ( nmap == 0 ) {
      dir = dir->up;
      if ( dir && (cnt = dir->count) > high ) dir = NULL;
    }
    else {
      ++imap;
      if ( imap >= nmap ) dir = NULL;
      else dir = pf_get_direntry ( fid, phigh[imap], ierr);
      if ( dir ) cnt = dir->count;
    }
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
        fprintf(out,"%s|%2.2s%4d | %-16.16s | %-*.*s |\n",
                prefix,tptr,cnt,eodstr,menmax,menmax,blank);
      else
        fprintf(out,"%s|%2.2s%4.4d | %-16.16s | %-*.*s |\n",
                prefix,tptr,cnt % 10000,eodstr,menmax,menmax,blank);
    }
    else
      fprintf(out,"%s| %5d | %-16.16s | %-*.*s |\n",
              prefix,cnt,eodstr,menmax,menmax,blank);
    
  }

  fprintf(out,"%s+-------+------------------+-%s-+\n",prefix,dash+menoff);

  return;
}
