/*
-------------------------------------------------------------------------------
    PFF I/O utility:  init_data.h
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

#ifndef INIT_DATA_H

#define INIT_DATA_H

#include "pff.h"
#include "pffmp.h"
#include "filestack.h"
#include "typenames.h"
#include "ds_structs.h"
#include "re_defs.h"
#include "wr_defs.h"
#include "free_defs.h"
#include "workspace.h"

struct s_PFF  PFF              = { NULL , NULL };
int           PFF_fp_precision = FP_REDU;

int      PFFMP_procs =  0;
int      PFFMP_rank  = -1;
MPI_Comm PFFMP_comm;

int    PFF_work_buf[PFF_RECLEN];

char *PFF_type_names[] = { "Directory"       ,   /* DIR */
                           "Uniform 3D Float",   /* UF3 */ 
                           "Uniform 1D Float",   /* UF1 */ 
                           "Non-Unif. 3D FLT",   /* NF3 */ 
                           "Non-Unif. 3D VEC",   /* NV3 */ 
                           "Vertex List"     ,   /* VTX */ 
                           "Int/Float List  ",   /* IFL */ 
                           "Non-Unif. Grid  ",   /* NGD */
                           "Non-Unif. 3D GRD",   /* NG3 */ 
                           "Non-Unif. 3D INT",   /* NI3 */ 
                           "Unknown"
                         };

int      PFF_legal_uniform[]    = { PFTUF1 , PFTUF3 , DFAULT };
int      PFF_legal_nonuniform[] = { PFTNF3 , PFTNV3 , PFTNG3 , PFTNI3 , DFAULT };

int      PFF_ds_dims[] = { DFAULT ,   /* DIR */
                                3 ,   /* UF3 */
                                1 ,   /* UF1 */
                                3 ,   /* NF3 */
                                3 ,   /* NV3 */
                           DFAULT ,   /* VTX */
                           DFAULT ,   /* IFL */
                           DFAULT ,   /* NGD */
                                3 ,   /* NG3 */
                                3     /* NI3 */
                         };

int      PFF_ds_dimd[] = { DFAULT ,   /* DIR */
                                1 ,   /* UF3 */
                                1 ,   /* UF1 */
                                1 ,   /* NF3 */
                                3 ,   /* NV3 */
                           DFAULT ,   /* VTX */
                           DFAULT ,   /* IFL */
                           DFAULT ,   /* NGD */
                                0 ,   /* NG3 */
                                1     /* NI3 */
                         };

int PFF_fixed_spare_length[] = { TRUE  ,   /* DIR */
                                 TRUE  ,   /* UF3 */
                                 TRUE  ,   /* UF1 */
                                 TRUE  ,   /* NF3 */
                                 TRUE  ,   /* NV3 */
                                 TRUE  ,   /* VTX */
                                 TRUE  ,   /* IFL */
                                 FALSE ,   /* NGD */
                                 FALSE ,   /* NG3 */
                                 FALSE     /* NI3 */
                               };

PFFfree   free_functs[] = { pf_free_dir        ,   /* DIR */
                            pf_free_uniform    ,   /* UF3 */
                            pf_free_uniform    ,   /* UF1 */
                            pf_free_nonuniform ,   /* NF3 */
                            pf_free_nonuniform ,   /* NV3 */
                            pf_free_vertex     ,   /* VTX */
                            pf_free_ifl        ,   /* IFL */
                            pf_free_nonuniform ,   /* NGD */
                            pf_free_nonuniform ,   /* NG3 */
                            pf_free_nonuniform     /* NI3 */
                          };

PFFread   read_functs[] = { pf_re_dir          ,   /* DIR */
                            pf_re_uniform      ,   /* UF3 */
                            pf_re_uniform      ,   /* UF1 */
                            pf_re_nonuniform   ,   /* NF3 */
                            pf_re_nonuniform   ,   /* NV3 */
                            pf_re_vertex       ,   /* VTX */
                            pf_re_ifl          ,   /* IFL */
                            pf_re_ngd          ,   /* NGD */
                            pf_re_nonuniform   ,   /* NG3 */
                            pf_re_nonuniform       /* NI3 */
                          };

PFFwrite write_functs[] = { pf_wr_dir          ,   /* DIR */
                            pf_wr_uniform      ,   /* UF3 */
                            pf_wr_uniform      ,   /* UF1 */
                            pf_wr_nonuniform   ,   /* NF3 */
                            pf_wr_nonuniform   ,   /* NV3 */
                            pf_wr_vertex       ,   /* VTX */
                            pf_wr_ifl          ,   /* IFL */
                            pf_wr_ngd          ,   /* NGD */
                            pf_wr_nonuniform   ,   /* NG3 */
                            pf_wr_nonuniform       /* NI3 */
                          };

#endif
