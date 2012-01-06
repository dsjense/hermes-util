/*
-------------------------------------------------------------------------------
    PFF test program:  treadc.c
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
      C_Groups main standalone
*/
#include "pff.h"
#include "u_defs.h"

#include <stdio.h>
#include <string.h>

#define IBUFSIZ  1000
#define ICOUNT  PFF_RECLEN
#define FCOUNT (PFF_RECLEN/2)

#ifdef __STDC__

int main ( int argc, char *argv[])

#else

int main (argc, argv)
int argc;
char *argv[];

#endif

{
  int  i, j, cnt, ioff, i4strt, cnt0, ierr = 0, irec = 0;
  char inbuf[IBUFSIZ], *fname;
  int iarr[ICOUNT], i4[FCOUNT];
  int *xarr, *xtmp;
  float ftmp, farr[FCOUNT], fbase[FCOUNT];
  FILE *file;
  PFFfid *fid = NULL;
  
  if ( argc < 2 ) {
    printf("File to read: ");
    if ( fgets(inbuf,IBUFSIZ,stdin) == NULL ) {
      perror("treadc");
      return 1;
    }
    i = strlen(inbuf) - 1;
    if( inbuf[i] == '\n' ) inbuf[i] = '\0';
    fname = inbuf;
  }
  else  fname = argv[1];

  if ( (file = fopen(fname,"rb")) == NULL ) {
    perror("treadc");
    return 1;
  }

  /* Start TEST of 4-byte integer MD code */
  xarr = (int *) i4;


  i4strt = 1 << 15;
  for (j=0; j<2; j++) {
    irec++;
    pf_u_i2io ( file, NULL, RE, ICOUNT, iarr, &ierr);
    if ( ierr ) {
      fprintf(stderr,"pf_u_i2io error: %d\n",ierr);
      return 1;
    }
    pf_u_i4 (RE, FCOUNT, iarr, i4, &ierr);
    if ( ierr ) {
      fprintf(stderr,"pf_u_i4 error: %d\n",ierr);
      return 1;
    }
    ioff = i4strt - FCOUNT/2 + 1;
    for(i=0;i<FCOUNT;i++)  {
      if ( i4[i] != i+ioff ) {
        printf("ERROR -- %6d %6d %15d %15d  %10.8x %10.8x\n",
               i,j,i+ioff,i4[i],i+ioff,i4[i]);
      }
      else if ( i == 0 ) {
        printf("%8d %7d%15d%15d%10.8x%10.8x\n",
               irec,j,i+ioff,i4[i],i+ioff,i4[i]);
      }
    }
    i4strt *= 2;
  }
  /* End TEST of 4-byte integer MD code */

  /* Start TEST of FULL-PRECISION MD code */
  xarr = (int *) farr;
  xtmp = (int *) &ftmp;

  cnt = 0;
  for (j=0; j<2; j++) {
    cnt0 = cnt;
    for(i=0;i<FCOUNT;i++) fbase[i] = 1.0f/(++cnt0);
    irec++;
    pf_u_i2io ( file, NULL, RE, ICOUNT, iarr, &ierr);
    if ( ierr ) {
      fprintf(stderr,"pf_u_i2io error: %d\n",ierr);
      return 1;
    }
    pf_u_f4 (RE, FCOUNT, iarr, farr, &ierr);
    if ( ierr ) {
      fprintf(stderr,"pf_u_f4 error: %d\n",ierr);
      return 1;
    }
    for(i=0;i<FCOUNT;i++)  {
      ++cnt;
      if ( fbase[i] != farr[i] ) {
        ftmp = fbase[i];
        printf("ERROR -- %6d %18.10e %18.10e  %10.8x %10.8x\n",
               cnt,(double)ftmp,(double)farr[i],*xtmp,xarr[i]);
      }
      else if ( i == 0 ) {
        ftmp = fbase[i];
        printf("%8d %7d%15.8e%15.8e%10.8x%10.8x\n",
               irec,cnt,(double)ftmp,(double)farr[i],*xtmp,xarr[i]);
      }
    }
  }
  /* End TEST of FULL-PRECISION MD code */

  cnt = 0;
  while (1) {
    irec++;
    pf_u_i2io ( file, NULL, RE, ICOUNT, iarr, &ierr);
    if ( ierr ) {
      fprintf(stderr,"pf_u_i2io error: %d  irec = %d\n",ierr,irec);
      break;
    }
    cnt0 = cnt + 1;
    for(i=0;i<ICOUNT;i++)  {
      if (iarr[i] != ++cnt) {
        printf("ERROR -- %6d %6d %6d %6d %6d %10.4x %10.4x\n",
               irec, cnt % ICOUNT, cnt/ICOUNT, cnt, iarr[i], cnt, iarr[i]);
      }
    }
    printf("%8d %7d %7d\n",irec,cnt0,iarr[0]);
  }

  if ( fclose(file) ) {
    perror("treadc");
    return 1;
  }

  return 0;
}
