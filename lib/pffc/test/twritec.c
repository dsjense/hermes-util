/*
-------------------------------------------------------------------------------
    PFF test program:  twritec.c
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
  int  i, j, cnt, ioff, i4strt, ierr = 0, irec = 0;
  char inbuf[IBUFSIZ], *fname;
  int iarr[ICOUNT];
  int i4[FCOUNT];
  float farr[FCOUNT];
  FILE *file;
  PFFfid *fid = NULL;
  
  if ( argc < 2 ) {
    printf("File to write: ");
    if ( fgets(inbuf,IBUFSIZ,stdin) == NULL ) {
      perror("twritec");
      return 1;
    }
    i = strlen(inbuf) - 1;
    if( inbuf[i] == '\n' ) inbuf[i] = '\0';
    fname = inbuf;
  }
  else  fname = argv[1];

  if ( (file = fopen(fname,"wb")) == NULL ) {
    perror("twritec");
    return 1;
  }

  /* Start TEST of 4-byte integer MD code */
  i4strt = 1 << 15;
  for (j=0; j<2; j++) {
    irec++;
    ioff = i4strt - FCOUNT/2 + 1;
    for(i=0;i<FCOUNT;i++)  i4[i] = i + ioff;
    pf_u_i4 (WR, FCOUNT, iarr, i4, &ierr);
    if ( ierr ) {
      fprintf(stderr,"pf_u_i4 error: %d\n",ierr);
      return 1;
    }
    pf_u_i2io ( file, NULL, WR, ICOUNT, iarr, &ierr);
    if ( ierr ) {
      fprintf(stderr,"pf_u_i2io error: %d\n",ierr);
      return 1;
    }
    i4strt *= 2;
  }
  /* End TEST of 4-byte integer MD code */

  /* Start TEST of FULL-PRECISION MD code */
  cnt = 0;
  for (j=0; j<2; j++) {
    irec++;
    for(i=0;i<FCOUNT;i++)  farr[i] = 1.0f/(++cnt);
    pf_u_f4 (WR, FCOUNT, iarr, farr, &ierr);
    if ( ierr ) {
      fprintf(stderr,"pf_u_f4 error: %d\n",ierr);
      return 1;
    }
    pf_u_i2io ( file, NULL, WR, ICOUNT, iarr, &ierr);
    if ( ierr ) {
      fprintf(stderr,"pf_u_i2io error: %d\n",ierr);
      return 1;
    }
  }
  /* End TEST of FULL-PRECISION MD code */

  cnt = 0;
  for (j=0; j<10; j++) {
    for(i=0;i<ICOUNT;i++)  iarr[i] = ++cnt;
    irec++;
    pf_u_i2io ( file, NULL, WR, ICOUNT, iarr, &ierr);
    if ( ierr ) {
      fprintf(stderr,"pf_u_i2io error: %d\n",ierr);
      return 1;
    }
  }

  if ( fclose(file) ) {
    perror("twritec");
    return 1;
  }

  return 0;
}
