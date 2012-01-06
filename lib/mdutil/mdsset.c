/*
-------------------------------------------------------------------------------
    Machine-dependent utilities:  mdsset.c
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
    
    C_Groups @(#)
-------------------------------------------------------------------------------

    FORTRAN signal handling

*/
#if defined(ABSOFTsys) || defined(WIN32sys) || \
    (defined(PGNsys) && defined(SUNMOSos))
# define NO_SIGNALS
#endif

#include <stdio.h>
#include <signal.h>

/* function name mangling for F77 linkage */
#include "mdf77mangle.h"
#if !defined(HU_F77_MANGLING_L00)
#define  mdsset HU_F77_FUNC(  mdsset,  MDSSET )
#define  mdsinq HU_F77_FUNC(  mdsinq,  MDSINQ )
#define  mdsign HU_F77_FUNC(  mdsign,  MDSIGN )
#define  mdsdfl HU_F77_FUNC(  mdsdfl,  MDSDFL )
#endif

#if !defined(NO_SIGNALS)
static int signals[] = { SIGINT , SIGTERM , SIGUSR1 , SIGUSR2 };
static int sig_cnt[] = { 0 , 0 , 0 , 0 };

# define   NSIGS    ( sizeof sig_cnt / sizeof sig_cnt[0] )
#endif

#ifdef __STDC__

int   mdsset( void );
int   mdsinq( int *psig );
int   mdsign( int *psig );
int   mdsdfl( int *psig );
void  CatchIt( int sig );

#else

int   mdsset();
int   mdsinq();
int   mdsign();
int   mdsdfl();
void  CatchIt();

#endif

#ifdef __STDC__

int   mdsset(void)

#else

int   mdsset()

#endif

{
#if defined(NO_SIGNALS)
  return 1;
#else
  int i,ie;

  for ( i=0 ; i<NSIGS ; i++ ) {
    signal(signals[i],CatchIt); 
  }
  return 0;
#endif
}

#ifdef __STDC__

int   mdsinq( int *psig )

#else

int   mdsinq( psig )
int  *psig;

#endif

{
#if defined(NO_SIGNALS)
  return -1;
#else
  int rval, sig;

  sig = *psig - 1;

  if ( sig < NSIGS )  {
    rval = sig_cnt[sig];
    sig_cnt[sig] = 0;
  }
  else   rval = -1;

  return rval;
#endif
}

#ifdef __STDC__

int   mdsdfl( int *psig )

#else

int   mdsdfl( psig )
int  *psig;

#endif

{
#if defined(NO_SIGNALS)
  return 1;
#else
  int sig;

  sig = *psig - 1;

  if ( sig < NSIGS )  {
    if ( signal(signals[sig],SIG_DFL) != SIG_ERR )  return 0;
    else  return 1;
  }

  return 2;
#endif
}

#ifdef __STDC__

int   mdsign( int *psig )

#else

int   mdsign( psig )
int  *psig;

#endif

{
#if defined(NO_SIGNALS)
  return 1;
#else
  int sig;

  sig = *psig - 1;

  if ( sig < NSIGS )  {
    if ( signal(signals[sig],SIG_IGN) != SIG_ERR )  return 0;
    else  return 1;
  }

  return 2;
#endif
}
    
#ifdef __STDC__

void  CatchIt( int sig )

#else

void  CatchIt( sig )
int  sig;

#endif

{
#if !defined(NO_SIGNALS)
  int i;

  for ( i=0 ; i<NSIGS ; i++ )  {
    if ( sig == signals[i] ) break;
  }
  if ( i < NSIGS )  {
    sig_cnt[i]++;
    signal(sig,CatchIt);
    return;
  }
  fprintf(stderr,"CatchIt: unknown signal received - %d\n",sig);
  fflush(stdout);
  fflush(stderr);
  exit(1);
#endif
}
