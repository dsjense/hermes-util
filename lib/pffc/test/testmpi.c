/*
-------------------------------------------------------------------------------
    PFF test program:  testmpi.c
-------------------------------------------------------------------------------
     $Id: testmpi.c,v 1.1.1.1 2012/01/06 19:48:16 mfpasik Exp $
     
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
     C_Groups main standalone mpi
*/
#ifdef USE_MPI
# include "mpi.h"
#endif
#include "bld_defs.h"
#include "set_defs.h"
#include "u_defs.h"
#include "wr_defs.h"

#include <libgen.h>  /* for basename() */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
/*
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
*/

#ifdef USE_MPI
# define MP_RETURN(V)  MPI_Finalize(); return (V);
#else
# define MP_RETURN(V)  return (V);
#endif  

int main(int argc, char *argv[])
{
  int i,j, nx, ny, ld,lx, nds, my_box;
  float dx, dy, xs, ys;
  int ierr = 0;
  int me = 0;
  char precstr[] = "Reduced";
  int mode = WR_MP;
  int nprocs = 1;
  int nboxes = 1;
  int shft   = 0;
  long len = 0;
  int spare = 1;
  int zero = 0;
  PFFfid *fid;
  PFFds_vertex *vertex;
  char *vlab[3];
  char xlab[] = "Xi";
  char ylab[] = "Xj";
  char zlab[] = "Xk";
  char dlab[] = "Data";

  char *dlabs[1];
  float *data[1];

  int ic[] = {1,1,2,2};
  int jc[] = {2,3,2,3};

  float x[30];
  float dat[10];

  float del = 1.0f;

  int arg_off = 0, use_full = 0, use_reduced = 0, errflg = 0;
  char copt;
  char *opts;
  char *cmd;


#ifdef USE_MPI
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD,&nprocs);
  MPI_Comm_rank(MPI_COMM_WORLD,&me);
#endif

  cmd = basename(argv[0]);
  while ( argc > arg_off+1 && argv[arg_off+1][0] == '-' ) {
    opts = argv[arg_off+1];
    for(i=1; i< strlen(opts); ++i) {
      copt = argv[arg_off+1][i];
      switch (copt) {
        case 'f':
          use_full=1;
          break;
        case 'r':
          use_reduced=1;
          break;
        case 'a':
          mode=RW_MP;
          break;
        default:
          if (me==0) fprintf(stderr,"%s: Unknown command option \'%c\'\n",
                             cmd,copt);
          ++errflg  ;
      }
    }
    ++arg_off;
  }
  if ( use_full + use_reduced > 1) {
    if (me==0) fprintf(stderr,"%s: \'-f\' and \'-r\' cannot both be used.\n",
                       cmd);
    ++errflg;
  }
  if ( errflg > 0 )  {
    if (me==0) fprintf(stderr,"Usage: %s [ -f | -r ] [ nblks [shift] ]\n",cmd);
    MP_RETURN(errflg);
  }

  nboxes = nprocs;
  if ( argc > arg_off+1 ) nboxes = atoi(argv[arg_off+1]);
  if ( argc > arg_off+2 ) shft = atoi(argv[arg_off+2]);

  
  if ( use_full == 1 ) {
    pf_set_fp_precision(NULL, FP_FULL, &ierr);
    strcpy(precstr,"Full");
  }

  fid = pf_u_open ("pars.pff", mode, &nds, &ierr );
  if (ierr != 0) {
    printf("Open error: %d %d\n",mode,ierr);
    MP_RETURN(1);
  }
  if ( mode == RW_MP ) {
    if (me==0) printf("In append mode with %d datasets\n",nds);
    pf_set_dsp(fid,nds+1,&ierr);
    if (ierr != 0) {
      printf("Error setting dataset pointer in append mode: %d %d %d\n",
             me, nds, ierr);
      MP_RETURN(1);
    }
    if (me==0) printf("New loc: %ld\n",pf_u_tell(fid,&ierr));
  }

  my_box = (me + shft) % nprocs;
  if ( my_box < nboxes ) {
    nx = ic[my_box];
    ny = jc[my_box];
    dx = del/nx;
    dy = del/ny;
    xs = del*(my_box/2) + 0.5*dx;
    ys = del*(my_box%2) + 0.5*dy;
    lx = ld = 0;
    for (j=0;j<ny;++j) {
      for (i=0;i<nx;++i) {
        x[lx++] = xs + i*dx;
        x[lx++] = ys + j*dy;
        x[lx++] = 0.0;
        dat[ld++] = xs + i*dx + ys + j*dy;
      }
    }
    len = ld;
    printf("Proc %d: my_box = %d -- length: %ld -- Precision: %s\n",
           me,my_box,len,precstr);
  }
  else printf("Proc %d: No Box -- length: 0\n",me);
  vlab[0] = xlab;
  vlab[1] = ylab;
  vlab[2] = zlab;
  dlabs[0] = dlab;
  data[0]  = dat;
  /*
void pf_wr_vertex ( PFFfid   *fid ,  PFFds_any *ds, int *ierr );
  */
  vertex =pf_bld_vertex ( 21, "pars", "vertex data", 3, 1, len, 1, 0, &spare,
                          vlab, dlabs, x, 0, data, &zero, 1, 1, 1, &ierr );
    
  pf_wr_vertex ( fid , (PFFds_any *) vertex, &ierr );
  pf_u_close (fid, &ierr );
  
  MP_RETURN(0);
}
