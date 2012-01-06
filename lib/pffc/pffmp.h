/*
-------------------------------------------------------------------------------
    PFF I/O utility:   pffmp.h
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

#ifndef PFFMP_H
#define PFFMP_H

#if defined(USE_MPI)
# include "mpi.h"
# define PFFMP_Allreduce_1(FID,VAR,TEMP,TYPE,OP) \
         if ( PFFMP_procs > 1 && (FID)->mp_on ) { \
           TEMP = (VAR); \
           MPI_Allreduce(&(TEMP), &(VAR), 1, TYPE, OP, PFFMP_comm); \
         }
# define PFFMP_Allreduce(FID,P_IN,P_OUT,CNT,TYPE,OP) \
         if ( PFFMP_procs > 1  && (FID)->mp_on ) { \
           MPI_Allreduce(P_IN, P_OUT, CNT, TYPE, OP, PFFMP_comm); \
         }
# define PFFMP_Allgather_1(FID,VAR,ARRAY,TYPE) \
         if ( PFFMP_procs > 1  && (FID)->mp_on ) { \
           MPI_Allgather(&(VAR), 1, TYPE, ARRAY, 1, TYPE, PFFMP_comm); \
         }
# define PFFMP_Recv(P_MSG,CNT,SRC,TYPE,TAG,STAT) \
         MPI_Recv(P_MSG, CNT, TYPE, SRC, TAG, PFFMP_comm, &STAT)
# define PFFMP_Send(P_MSG,CNT,DEST,TYPE,TAG) \
         MPI_Send(P_MSG, CNT, TYPE, DEST, TAG, PFFMP_comm)
#else
typedef int MPI_Comm;
typedef int MPI_Status;
# if defined(__STDC__) || defined(__cplusplus)
enum { MPI_COMM_WORLD=0, MPI_INT=0, MPI_LONG=0, MPI_FLOAT=0, MPI_MAX=0,
       MPI_MIN=0, MPI_SUM=0 };
# else
#  define MPI_COMM_WORLD 0
#  define MPI_INT        0
#  define MPI_LONG       0
#  define MPI_FLOAT      0
#  define MPI_MAX        0
#  define MPI_MIN        0
#  define MPI_SUM        0
# endif
# define PFFMP_Allreduce(FID,P_IN,P_OUT,CNT,TYPE,OP,COMM)
# define PFFMP_Allreduce_1(FID,P_VAR,TEMP,TYPE,OP)
# define PFFMP_Allgather_1(FID,VAR,ARRAY,TYPE) ( (VAR) = *(ARRAY) )
# define PFFMP_Recv(P_MSG,CNT,RCV_RANK,TYPE,TAG,STAT)
# define PFFMP_Send(P_MSG,CNT,DEST,TYPE,TAG)
#endif

#if defined(__STDC__) || defined(__cplusplus)
  enum { PFFMP_CURRENT, PFFMP_GLOBAL };
#else
# define PFFMP_CURRENT (0)
# define PFFMP_GLOBAL  (1)
#endif

/*  Global PFF I/O variables  */

extern int       PFFMP_procs;
extern int       PFFMP_rank;
extern MPI_Comm  PFFMP_comm;

#endif
