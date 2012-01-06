/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_re_vertex.c
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

#include <stdlib.h>
#include "pff.h"
#include "ds_structs.h"
#include "workspace.h"

#include "free_defs.h"
#include   "re_defs.h"
#include    "u_defs.h"

/*  Declare function */

#ifdef __STDC__

PFFds_any *pf_re_vertex ( PFFfid   *fid ,int keep, int *ierr );

PFFds_any *pf_re_vertex ( PFFfid   *fid ,int keep, int *ierr )

#else

PFFds_any   *pf_re_vertex    ();

PFFds_any *pf_re_vertex ( fid, keep, ierr )

PFFfid   *fid; 
int       keep;
int      *ierr;

#endif

/* 
------------------------------------------------------------------------

       - This routine is a READ routine that reads a VTX (Vertex List) 
         dataset from a PFF file.
       - Verticies have dimensionality "m"
       - Each vertex has "n" attributes
       - This operation is ONLY ALLOWED in READ mode !!!
       - Dataset Format:
           <HEADER>       PFTVTX
           <INT>          M                 (vertex dimensionality)
           <INT>          N                 (attribute dimensionality)
           <LONG>         NV                (# of verticies)
           <INT>x5        ISPARE            (Reserved for application) 
           <STRING>xM     VLABEL            (vertex coordinate labels)
           <STRING>xN     ALABEL            (attribute labels)
#ifdef VERTEX_VERSION_ORIGINAL
           IF (M.GT.0)
             <FARRAY>       VERT(1:M,1:NV)  (mD vertex list)
           ENDIF
#else
           LOOP i=1,M
             <FARRAY>       VERT(i,1:NV)    (ith component of mD vertex list)
           ENDLOOP
#endif
           LOOP i=1,N
             <FARRAY>       Ai(1:NV)        (ith attribute list)
           ENDLOOP

------------------------------------------------------------------------

    Input:
      fid     -  pointer to PFF file structure
      keep    -  flag indicating whether or not to keep a non-zero value 
                 for floating data in the case of underflow
      ierr    -  If not zero, return with no operation

    Output:
      ierr    -  error flag:
                   =  0,  Normal return
                   = -1,  EOF reached  (not really an error)
                   =  1,  Incorrect dataset type
                   =  2,  Error allocating memory for VERTEX structure or 
                          contents
                   =  3,  Internal inconsistancy detected while reading vertex
                          structure
                   =  4,  Error allocating workspace memory
*/
{
  static char    *module    = "PF_RE_VERTEX";
  register int    i,j,k,*p;
  char          **pl;
  long            tnv, offset;
  int             temp;
  float          *fwork = NULL;
  PFFhead        *head =  NULL; 
  PFFds_vertex   *vertex  =  NULL;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return NULL;

  head = pf_re_header ( fid, &offset, ierr );
  if( *ierr != 0 ) return NULL;

  if ( head->rawtype != PFTVTX )  {
    *ierr = 1;
    pf_wr_err ( module, *ierr, fid, "Incorrect dataset type");
  }

  else    {

    /* get memory for VERTEX structure */

    if ( (vertex = (PFFds_vertex *) malloc( sizeof(PFFds_vertex) )) == NULL )  {
      *ierr = 2;
      pf_wr_err ( module, *ierr, fid, 
                  "Error allocating memory for VERTEX structure or contents");
    }
    else  {

      vertex->type = PFTVTX;
      vertex->head = head;
      head = NULL;
      vertex->spare   = NULL;
      vertex->vlabel  = NULL;
      vertex->dlabel  = NULL;
      vertex->vert    = NULL;
      vertex->data    = NULL;
      vertex->aoff10  = NULL;

      vertex->alloc_method = 0;

      /* read in dimensionality and # of verticies */

      pf_u_sio( fid, RE, 5, PFF_work_buf, ierr );
      vertex->dims = PFF_work_buf[0];
      vertex->dimd = PFF_work_buf[1];
      pf_u_i2l( PFF_work_buf+2, &(vertex->nv), ierr);

      /* read in spare words */

      pf_u_sio( fid, RE, VTX_MAXSPARE, PFF_work_buf, ierr );
      for (i=0;i<VTX_MAXSPARE;i++)
        if ( PFF_work_buf[i] == DFAULT ) break;
      vertex->nspare = i;
      if ( i > 0 )   {
        p = (int *) malloc(i*sizeof(int));
        if ( p != NULL ) {
          vertex->spare = p;
          for (j=0;j<i;j++,p++)
            *p = PFF_work_buf[j];
        }
      }

      if (vertex->dims > 0 )   {
        if ( (vertex->vlabel = (char **) malloc( vertex->dims*sizeof(char *) )) 
                                                                == NULL )  {
          *ierr = 2;
          pf_wr_err ( module, *ierr, fid, 
                    "Error allocating memory for VERTEX structure or contents");
        }

        for(i=0,pl=vertex->vlabel; i < vertex->dims; ++i,++pl )  {
          *pl = NULL;
          pf_u_string_io ( fid, RE, 0, pl, ierr );
        }
      }

      if (vertex->dimd > 0 )   {
        if ( (vertex->dlabel = (char **) malloc( vertex->dimd*sizeof(char *) )) 
                                                                == NULL )  {
          *ierr = 2;
          pf_wr_err ( module, *ierr, fid, 
                    "Error allocating memory for VERTEX structure or contents");
        }

        for(i=0,pl=vertex->dlabel; i < vertex->dimd; ++i,++pl )  {
          *pl = NULL;
          pf_u_string_io ( fid, RE, 0, pl, ierr );
        }
      }

      if (vertex->dims > 0 )   {
        if ( (vertex->head)->ds_version == 1 )  {
          /* Vertex Dataset Version 1 Processing */
          if ( (fwork = 
                (float *) malloc( vertex->nv*sizeof(float)) ) == NULL) {
            *ierr = 4;
            pf_wr_err ( module, *ierr, fid, 
                        "Error allocating workspace memory");
          }
          else   {
            if ( (vertex->vert = (float *) 
              malloc( vertex->dims*vertex->nv*sizeof(float)) ) == NULL ) {
              *ierr = 2;
              pf_wr_err ( module, *ierr, fid, 
                    "Error allocating memory for VERTEX structure or contents");
            }
            for ( i=0; i < vertex->dims && *ierr == 0; ++i) {
              fwork = pf_re_fltarray ( fid, keep, &tnv, &(vertex->voff10), 
                                       ierr );
              if ( tnv != vertex->nv )   {
                *ierr = 3;
                pf_wr_err ( module, *ierr, fid, 
             "Internal inconsistancy detected while reading VERTEX structure" );
              }
              else   {
                for ( j=0,k=i; j<vertex->nv; j++,k+=vertex->dims )  {
                  vertex->vert[k] = fwork[j];
                }
              }
            }
            free(fwork);
            fwork = NULL;
          }
          
        }
        else   {
          /* Vertex Dataset Original Version Processing */
          vertex->vert = pf_re_fltarray ( fid, keep, &tnv, 
            &(vertex->voff10), ierr );
          if ( tnv != vertex->dims*vertex->nv )   {
            *ierr = 3;
            pf_wr_err ( module, *ierr, fid, 
              "Internal inconsistancy detected while reading VERTEX structure" );
          }
        }
      }

      if ( (*ierr == 0) && (vertex->dimd > 0) )   {
        
        if ( (vertex->data = (float **) malloc( vertex->dimd*sizeof(float *) )) 
                                                                == NULL )  {
          *ierr = 2;
          pf_wr_err ( module, *ierr, fid, 
                    "Error allocating memory for VERTEX structure or contents");
        }
        if ( (vertex->aoff10 = (int *) malloc( vertex->dimd*sizeof(int) )) 
                                                                == NULL )  {
          *ierr = 2;
          pf_wr_err ( module, *ierr, fid, 
                    "Error allocating memory for VERTEX structure or contents");
        }

        for ( i=0,p=vertex->aoff10; i < vertex->dimd; ++i, ++p) {
          vertex->data[i] = pf_re_fltarray ( fid, keep, &tnv, p, ierr );
          if ( tnv != vertex->nv )   {
            *ierr = 3;
            pf_wr_err ( module, *ierr, fid, 
            "Internal inconsistancy detected while reading VERTEX structure" );
          }
        }
      }

      if ( *ierr != 0 ) 
        pf_free_vertex ( ( PFFds_any* ) vertex, &temp );
    }
  }

  CHKFREE (head);
  return ( PFFds_any* ) vertex;
}
