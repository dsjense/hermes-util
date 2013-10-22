/*
-------------------------------------------------------------------------------
    PFF test program:  tdoublec.c
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
#include <float.h>
#include <math.h>
#include <stdio.h>

#ifndef TRUE
#define TRUE  (1)
#define FALSE (0)
#endif

#ifndef ABS
#define ABS(A)    ((A) <  0  ? (-(A)) : (A))
#endif

#ifndef MIN
#define MIN(A,B)  ((A) > (B) ? (B) : (A))
#endif

#ifndef MAX
#define MAX(A,B)  ((A) > (B) ? (A) : (B))
#endif

#ifdef __STDC__
void pf_u_d2i ( double dval, int *ival, int *ierr );
void pf_u_i2d ( int *ival, double *dval, int *ierr );
#else
void       pf_u_d2i       ();
void       pf_u_i2d       ();
#endif

int main()
{
  int     i, ierr;
  int     ival[5];
  double  dpi, dpower, dval, dval2;

  dpi = 4.0*atan(1.0);

  for (i = 0; i < 3; ++i) {
    ierr = 0;
    dpower = pow( (double) (2*i+1), (double) (i+1) );
    dval   = pow(dpi,dpower);

    pf_u_d2i (dval, ival,  &ierr);
    if (ierr == 0) {
      printf("%1d %6d %6d %6d %6d %6d\n",i,ival[0],ival[1],ival[2],ival[3],ival[4]);
      pf_u_i2d (ival, &dval2, &ierr);
      if (ierr == 0) {
        printf("%1d %21.12e\n  %21.12e %14.3e\n",i,dval,dval2,1.0-dval2/dval);
      } else {
        printf("%1d  error in pf_u_i2d: ierr = %d\n",i,ierr);
      }
    } else {
      printf("%1d  error in pf_u_d2i: ierr = %d\n",i,ierr);
    }

    dval = -1.0/dval;
    ierr = 0;

    pf_u_d2i (dval, ival,  &ierr);
    if (ierr == 0) {
      printf("%1d %6d %6d %6d %6d %6d\n",i,ival[0],ival[1],ival[2],ival[3],ival[4]);
      pf_u_i2d (ival, &dval2, &ierr);
      if (ierr == 0) {
        printf("%1d %21.12e\n  %21.12e %14.3e\n",i,dval,dval2,1.0-dval2/dval);
      } else {
        printf("%1d  error in pf_u_i2d: ierr = %d\n",i,ierr);
      }
    } else {
      printf("%1d  error in pf_u_d2i: ierr = %d\n",i,ierr);
    }
  }

}
