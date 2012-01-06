/*
 * $Id$ 
 * 
 * Copyright (2008) Sandia Corporation. Under the terms of
 * Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
 * Government retains certain rights in this software.
 * 
 * Hermes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 * 
 * Hermes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General
 * Public License along with Hermes.  If not, see
 * <http://www.gnu.org/licenses/>.
 * 
 * C_Groups pff2xy main standalone
*/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "pff.h"
#include "u_defs.h"
#include "ds_structs.h"
#include "re_defs.h"
#include "free_defs.h"
#include "set_defs.h"

int main(int argc, char **argv)
{
  PFFfid            *fid;
  PFFds_any         *ds;
  PFFds_uniform     *uds;
  PFFblock_uniform  *b;
  PFFhead           *header;

  int     num_datasets;
  int     error_flag;
  char    *filename;
  char    *title;
  float   x0, dx;
  int     num_points;
  float   *data;
  int     n;
  int     i;

  if (argc != 2) {
    fprintf(stderr, "usage: pff2xy filename.pff\n");
    exit(1);
  }

  filename = malloc(sizeof(char)*(strlen(argv[1])+1));
  assert(filename != 0);
  strcpy(filename, argv[1]);

  error_flag = 0;
  fid = pf_u_open(filename, RE, &num_datasets, &error_flag);
  if (error_flag != 0) {
    fprintf(stderr, "pf_u_open error flag: %d\n", error_flag);
    exit(1);
  }

  for (n=0; n<num_datasets; n++) {
    pf_set_dsp(fid, n+1, &error_flag);
    ds = pf_re_uniform(fid, 1, &error_flag);
    if (error_flag == 0) {
      assert(ds->type == PFTUF1);
      uds = (PFFds_uniform *) ds;

      header = uds->head;
      title = header->title;

      assert(uds->nblk == 1);
      b = uds->block[0];

      assert(uds->dims == 1);
      x0 = b->x0[0];
      dx = b->dx[0];
      num_points = b->nx[0];
      assert(b->goff10==0 && b->foff10==0);
      assert(uds->dimd == 1);
      data = b->data[0];
      printf("%d\n", num_points);
      printf("%s\n", title);
      for(i=0;i<num_points;i++)
        printf("%15.7e %15.7e\n", x0+i*dx, data[i]);

      pf_free_ds(ds, &error_flag);

    } else {
      fprintf(stderr, "Dataset %d: pf_re_uniform error flag = %d\n",
              n+1,error_flag);
      error_flag = 0;
    }
  }
  pf_u_close(fid, &error_flag);

  free(filename);

  return 0;
}
