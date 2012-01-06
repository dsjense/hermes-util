/*
-------------------------------------------------------------------------------
    PFF test program:  tfloatc.c
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
void pf_u_i2f ( int keep, int *ival, float *xval, int *off10, int *ierr );
#else
void       pf_u_i2f       ();
#endif

int main()
{
  int     ierr  = 0;
  int     errcnt = 0;
  int     ival[3];
  int     jval[3];
  int     kval[3];
  int     l,i,loop,ioff;
  float   xmult, fvalb, fvals, fvalm, prec;
  int try[] = { FLT_MAX_EXP-1, FLT_MIN_EXP };
  static char *label[] = { "Maximum", "Minimum" };

  ival[0] = 0;
  ival[1] = 0;
  jval[0] = 32767;
  jval[1] = 32767;
  kval[0] = 10922;
  kval[1] = 21845;

  printf("Current Macro Values:\n");
  printf("  MAX_EXP: %d\n",FLT_MAX_EXP-1);
  printf("  MIN_EXP: %d\n",FLT_MIN_EXP);
  printf("  FLT_MIN: %e\n",FLT_MIN );

  for(l=0; l<2; ++l)  {

    ival[2] = (try[l]+8192)*2;
    jval[2] = ival[2];
    kval[2] = ival[2];

    pf_u_i2f( FALSE, ival, &fvals, &ioff, &ierr);
    pf_u_i2f( FALSE, jval, &fvalb, &ioff, &ierr);
    pf_u_i2f( FALSE, kval, &fvalm, &ioff, &ierr);

    loop = ABS(try[l]);
    if ( try[l] > 0 )
      xmult = 0.5;
    else
      xmult = 2.0;

    for(i=0,prec=fvalm ; i<loop ; i++,prec*=xmult ) ;

    prec = prec*1.50;

    if ( fabs(1.0f - prec) > FLT_EPSILON ) {
      printf("%s exponent (%d) provides marginal precision: %10.7f\n",
             label[l], try[l], prec);
      ++errcnt;
    }

    if ( l == 0 ) {
      if ( fvalb > FLT_MAX ) {
        printf("Decoded value too large: FLT_MAX=%10.7e, decoded max value=%10.7e\n",
               FLT_MAX, fvalb);
        ++errcnt;
      }
    }
    else {
      if ( fvals < FLT_MIN ) {
        printf("Decoded value too small: FLT_MIN=%10.7e, decoded min value=%10.7e\n",
               FLT_MIN, fvals);
        ++errcnt;
      }
    }
  }
  if ( 1.0f/FLT_MIN > FLT_MAX ) {
    printf("FLT_MIN (%e), when reciprocated, is greater than FLT_MAX (%e)\n",
           FLT_MIN, FLT_MAX);
      ++errcnt;
  }
  if ( errcnt ) printf("WARNING: %d potential errors encountered\n",errcnt);
  else printf("All tests passed.\n");
}
