/*
-------------------------------------------------------------------------------
    PFF test program:  pffpeek.c
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
 C_Groups pffpeek main
*/

#include "pff.h"
#include "filestack.h"
#include "ds_structs.h"

#include "free_defs.h"
#include "get_defs.h"
#include "list_defs.h"
#include "re_defs.h"
#include "set_defs.h"
#include "u_defs.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef WIN32sys
# include "winrtl.h"
#else
# include <unistd.h>
#endif

#define    SKIP       (3)
#define    RLEN      (30)

// need prototype for copyright function
void dump_copyright(FILE *);

int xd_print ( int order, int dimd, float **data, char *prestring, 
               int **idata, char *istring, long *max, long *ticks );
int xd_print_r ( int order, int dimd, float **data, char *prestring, 
                 int **idata, char *istring, long *max, long *ticks, 
                 long offset );

static void usage(const char *cmd, int status)
{
  FILE *o = stdout;
  if ( status ) o = stderr;

  fprintf(o,"Usage: %s [-w page_width] [pff_file ...]\n",cmd);

  exit(status);
}

int main(int argc, char *argv[])
{
  int                  zero = 0;
  register int         ib,i,j;
  float               *pf;
  char                *cmd;
  char                 action[32];
  char                 fname[80];
  int                  ierr = 0;
  int                  nds, nvskip, xskip, lenact;
  int                  quit, low, high, width=80, entry, c;
  long                *skip, lskip;
  int                  vals[RLEN];
  long                 loc, loctel;
  int                  count = 0;
  PFFdir              *dir  =  NULL;
  PFFfid              *fid = NULL, *last_fid = NULL;
  PFFds_ifl           *ifl = NULL;
  PFFds_vertex        *vertex = NULL;
  PFFds_uniform       *uni = NULL;
  PFFblock_uniform    *ublock = NULL;
  PFFds_nonuniform    *nonuni = NULL;
  PFFblock_nonuniform *block = NULL;
  PFFds_any           *ds = NULL;
  PFFhead             *head = NULL;

  if ( (cmd=strrchr(argv[0],'/')) == NULL ) cmd = argv[0];
  else ++cmd;

  /* check for valid options */
  while ( (c=getopt(argc, argv, "hw:")) != -1 ) {
    switch (c) {
    case 'h':
      usage(cmd,0);
      break;
    case 'w':
      i = atoi(optarg);
      if ( i > 0 ) width = i;
      break;
 
    case '?':  /* invalid option */
      ierr = 1;
      break;

    default:  /* getopt should never return something that goes here !! */
      ierr = 1;
      fprintf(stderr,"?? getopt returned unexpected character code: 0%o\n",c);
    }    
  }

  /* shift away the option arguments */
  argv += optind;
  argc -= optind;

  if ( ierr ) usage(cmd,1);

  printf("                  Hermes Utilities -- PFFPEEK\n");
  printf(
   " *********************************************************************\n");
  dump_copyright(stdout);
  printf(
   " *********************************************************************\n");

  quit = FALSE;

  for(i=0; i<argc; ++i) {
    fid = pf_u_open ( argv[i], RE, &nds, &ierr );
    if ( ierr ) { ierr = 0; fid = last_fid; }
    else {
      last_fid = fid;
      printf("File %s opened with %d datasets\n",argv[i],nds);
    }
  }

  while  ( ierr == 0  &&  !quit )  {

    printf("Action ? (Open,Close,Dir,Files,Read,Select,Quit):  ");
    if ( scanf("%s",action) == EOF ) break;
    lenact = strlen(action);
    switch ( *action )   {
      case 'o':  case 'O':
        do   {
          printf("Enter file name:  ");
          scanf("%s",fname);
          ierr = 0;
          fid = pf_u_open ( fname, RE, &nds, &ierr );
        }  while  ( ierr != 0 );
        printf("File %s opened with %d datasets\n",fname,nds);
        break;
      case 'c':  case 'C':
        printf("Enter file entry # (EOF to abort):  ");
        if ( scanf("%d",&entry) == EOF ) break;
        if ( fid == NULL ) 
          printf("NO FILES OPEN !!!\n");
        else  {
          fid = pf_get_fid ( entry, &ierr );
          pf_u_close ( fid, &ierr);
          fid = PFF.current;
        }
        break;
      case 'd':  case 'D':
        if ( fid == NULL ) 
          printf("NO FILES OPEN !!!\n");
        else
          pf_list_dirs ( stdout, fid, width, 1, &zero, 0, NULL, NULL, &ierr);
        break;
      case 'f':  case 'F':
          pf_list_files ( width, 1, 0, 0, 0, 0, &ierr);
        break;
      case 'r':  case 'R':
        printf("Enter dataset entry # (EOF to abort):  ");
        if ( scanf("%d",&entry) == EOF ) break;
        if ( fid == NULL ) 
          printf("NO FILES OPEN !!!\n");
        else   {
          lskip = SKIP;
          if ( lenact > 1 ) {
            i = strcspn(action,"0123456789");
            if ( i < lenact ) {
              j = strtol(action+i, NULL, 10);
              if ( j > 0 ) lskip = j;
            }
          }
          pf_set_dsp ( fid, entry, &ierr);
          ds = pf_re_ds ( fid, TRUE, &ierr );
          if ( ierr != 0) { 
            if ( ierr == -1  && count == 0 )  {
              printf("Encountered 1st end-of-file\n");
              ++count;
              ierr = 0;
            }
            else
              printf("R_ds error %d\n",ierr);

          }
          else    {

            head = ds->head;
            printf("Types = %d  %d\n", ds->type, head->rawtype );
            printf("Typename = %s\n", head->type_name );
            printf("Title    = %s\n", head->title );
            printf("Raw = %d  App = %d  Ver = %d Nrfu = %d\n",
                    head->rawtype, head->apptype, head->ds_version, 
                    head->nwords_rfu );
            if ( head->nwords_rfu > 0 ) {
              for(i=0;i<head->nwords_rfu; i++ )   {
                printf("rfu[%d] = %d\n",i,head->rfu[i]);
              }
            }

            switch ( ds->type )   {

              case PFTIFL:

                ifl = (PFFds_ifl*) ds;

                printf("ni,nf,nflt = %ld %ld %ld\n",ifl->ni,ifl->nf,ifl->nflt);

                for ( i=0; i<ifl->ni; ++i)
                  printf("iarr[%5d] = %d\n",i,ifl->iarr[i] );

                printf("farray power-of-ten offset = %d\n",ifl->faoff10 );
                for ( i=0; i<ifl->nf; ++i)
                  printf("farr[%5d] = %e\n",i,ifl->farr[i] );

                for ( i=0; i<ifl->nflt; ++i)
                  printf("flist[%5d] = %e    offset = %d\n",
                                            i,ifl->flist[i],ifl->floff10[i] );

              break;

              case PFTVTX:

                vertex = (PFFds_vertex*) ds;

                printf("dims,dimd,nspare,nv = %d %d %d %ld\n",
                        vertex->dims,vertex->dimd,vertex->nspare, vertex->nv);

                nvskip = MAX ( 1 , vertex->nv/10 );

                for ( i=0; i<vertex->nspare; ++i)
                  printf("spare[%d] = %d\n",i,vertex->spare[i] );

                for ( i=0; i<vertex->dims; ++i)
                  printf("vlabel[%d] = %s\n",i,vertex->vlabel[i] );

                for (i=0; i < vertex->dimd; ++i  )
                  printf("d_offset[%d] dlabel[%d] = %4d %s\n",
                          i,i,vertex->aoff10[i],vertex->dlabel[i] );

                printf("vertex power-of-ten offset = %d\n",vertex->voff10 );
                if ( vertex->dims > 0 )  {
                  for ( i=0,pf=vertex->vert; i<vertex->nv; i+=nvskip )  {
                    printf("[%5d] =",i);
                    for(j=0;j<vertex->dims;++j)
                      printf(" %9.2e",vertex->vert[vertex->dims*i+j]);
                    for(j=0;j<vertex->dimd;++j)
                      printf(" %9.2e",vertex->data[j][i]);
                    printf("\n");
                  }
                }

              break;

              case PFTUF1:
              case PFTUF3:

                uni = (PFFds_uniform*) ds;

                printf("dims,dimd,nblk = %d %d %d \n",
                              uni->dims,uni->dimd,uni->nblk );
                skip = (long*) malloc ( uni->dims*sizeof(int) );

                for ( ib=0 ; ib < uni->nblk ; ++ib )   {

                  ublock = uni->block[ib];

                  for ( i=0; i<ublock->nspare; ++i)
                    printf("spare[%d] = %d\n",i,ublock->spare[i] );

                  printf("nx =");
                  for ( i=0 ; i < uni->dims ; ++i )   {
                    skip[i] = (lskip);
                    printf(" %ld",ublock->nx[i]);
                  }
                  printf("\n");

                  printf("blabel[%d] = %s\n",ib,ublock->blabel );

                  printf("grid power-of-ten offset = %d\n",ublock->goff10 );
                  for ( i=0; i<uni->dims; ++i)  {
                    printf("glabel[%d] = %s\n",i,ublock->glabel[i] );
                    printf("x0[%d]     = %e\n",i,ublock->x0[i]);
                    printf("dx[%d]     = %e\n",i,ublock->dx[i]);
                  }

                  printf("data power-of-ten offset = %d\n",ublock->foff10 );

                  i = xd_print ( uni->dims, uni->dimd, ublock->data, "data",
                                 NULL, "", ublock->nx, skip );
                  printf("print count = %d\n", i );

                }
                free(skip);

              break;

              case PFTNF3:
              case PFTNV3:
              case PFTNG3:
              case PFTNI3:
              case PFTNGD:

                nonuni = (PFFds_nonuniform*) ds;

                printf("dims,dimd,nblk = %d %d %d \n",
                              nonuni->dims,nonuni->dimd,nonuni->nblk );
                skip = (long*) malloc ( nonuni->dims*sizeof(int) );

                for ( ib=0 ; ib < nonuni->nblk ; ++ib )   {

                  block = nonuni->block[ib];

                  for ( i=0; i<block->nspare; ++i)
                    printf("spare[%d] = %d\n",i,block->spare[i] );

                  printf("nx =");
                  for ( i=0 ; i < nonuni->dims ; ++i )   {
                    skip[i] = (lskip);
                    printf(" %ld",block->nx[i]);
                  }
                  printf("\n");

                  if ( block->blabel != NULL )
                    printf("blabel[%d] = %s\n",ib,block->blabel );

                  printf("grid power-of-ten offset = %d\n",block->goff10 );
                  for ( i=0; i<nonuni->dims; ++i)  {
                    if ( block->glabel != NULL && block->glabel[i] != NULL )  {
                      printf("glabel[%d] = %s\n",i,block->glabel[i] );
                    }
                    xskip = MAX ( 1 , block->nx[i]/15 );
                    for ( j=0; j<block->nx[i]; j+=xskip )  {
                      printf("x%d[%5d] = %e\n",i,j,block->x[i][j]);
                    }
                    if (j-xskip < block->nx[i]-1) {
                      j = block->nx[i] - 1;
                      printf("x%d[%5d] = %e\n",i,j,block->x[i][j]);
                    }
                  }

                  if ( block->dlabel != NULL )  {
                    for ( i=0; i<nonuni->dimd; ++i)  {
                      if ( block->dlabel[i] != NULL )  {
                        printf("dlabel[%d] = %s\n",i,block->dlabel[i] );
                      }
                    }
                  }

                  if ( block->data && nonuni->dimd > 0 ) 
                      printf("data power-of-ten offset = %d\n",block->foff10 );

                  i = xd_print (nonuni->dims, nonuni->dimd, block->data,"data",
                                block->idata, "idata", block->nx, skip );
                  printf("print count = %d\n", i );

                }
                free(skip);

              break;

              default:

                printf("Program TA does not know how to process datatype %d !\n",
                        ds->type);

            }

            pf_free_ds(ds, &ierr);
          }
        }

        break;
      case 's':  case 'S':
        printf("Enter file entry # (EOF to abort):  ");
        if ( scanf("%d",&entry) == EOF ) break;
        if ( fid == NULL ) 
          printf("NO FILES OPEN !!!\n");
        else  {
          fid = pf_get_fid ( entry, &ierr );
          PFF.current = fid;
        }
        break;
      case 'q':  case 'Q':
        quit = TRUE;
    }
  }
  return MAX(0,ierr);
}
#ifdef __STDC__
int xd_print ( int order, int dimd, float **data, char *prestring, 
               int **idata, char *istring, long *max, long *ticks )
#else
int xd_print ( order, dimd, data, prestring, idata, istring, max, ticks )
int          order, dimd;
int        **idata;
float      **data;
char        *prestring, *istring;
long        *max;
long        *ticks;
#endif
{
  if ( dimd == 0 || (data == NULL && idata == NULL) ) return 0;
  return  xd_print_r ( order, dimd, data, prestring, idata, istring, 
                       max, ticks, 0 );
}

#ifdef __STDC__
int xd_print_r ( int order, int dimd, float **data, char *prestring, 
                 int **idata, char *istring, long *max, long *ticks, 
                 long offset )
#else
int xd_print_r ( order, dimd, data, prestring, idata, istring, max, 
                 ticks, offset )
int          order, dimd;
int        **idata;
float      **data;
char        *prestring, *istring;
long        *max;
long        *ticks;
long         offset;
#endif
{
  long       i,j,tickm1,skp,strt;
  int        count = 0;
  char      *newstring, *dstring;

  --order;

  tickm1 = MAX ( 0 , MIN(max[order],ticks[order]) - 1 );
  if ( tickm1 == 0 )
    skp = max[order];
  else
    skp = (max[order] - 1)/tickm1;

  skp = MAX(skp,1);
  strt = (max[order] - 1 - skp*tickm1)/2;

  if ( data != NULL ) dstring = prestring;
  else                dstring = istring;

  if ( order == 0 )    {
    for ( i=strt; i<max[order]; i+=skp )   {
      printf("%s[%3ld] =",dstring,i);
      for ( j=0; j<dimd; j++)
        if ( data != NULL ) printf(" %9.2e",data[j][i+offset]);
        else                printf(" %9d"  ,idata[j][i+offset]);
      printf("\n");
      ++count;
    }
  }
  else                 {
    newstring = (char *) malloc ( strlen(dstring) + 6 );
    for ( i=strt; i<max[order]; i+=skp )   {
      sprintf(newstring,"%s[%3ld]",dstring,i);
      count += xd_print_r ( order, dimd, data, newstring, idata, newstring,
                            max, ticks, (offset+i)*max[order-1] );
    }
    free(newstring);
  }

  return count;
}
