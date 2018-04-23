/*
-------------------------------------------------------------------------------
    PFF I/O utility:   pff.h
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

#ifndef PFF_H
#define PFF_H

#if defined(USING_PFF_NAMESPACE)
/* don't want stdio in our local namespace, so temporarily get out of it */
}
#endif

#include <stdio.h>

#if defined(USING_PFF_NAMESPACE)
/* now get back in the namespace */
namespace PFF {
#endif

#ifndef TRUE
# if defined(__STDC__) || defined(__cplusplus)
  enum { FALSE, TRUE };
# else
#  define TRUE  (1)
#  define FALSE (0)
# endif
#endif

#ifndef SEEK_SET
# if defined(__STDC__) || defined(__cplusplus)
  enum { SEEK_SET, SEEK_CUR, SEEK_END };
# else
#  define SEEK_SET  (0)
#  define SEEK_CUR  (1)
#  define SEEK_END  (2)
# endif
#endif

#if defined(__STDC__) || defined(__cplusplus)
  enum { RE, WR, RW, WR_MP, RW_MP };
#else
# define RE     (0)
# define WR     (1)
# define RW     (2)
# define WR_MP  (3)
# define RW_MP  (4)
#endif

#if defined(__STDC__) || defined(__cplusplus)
  enum { DFRAME=-1, EOFFLG=-2, DFAULT=-3, FFRAME=-4, NULFLG=-5, FP_FULL=-6,
         FP_ALLFULL=FP_FULL, FP_ORDFULL=-7, FP_REDU=-3, INTP_2=-3, INTP_4=-6 };
#else
# define DFRAME  (-1)
# define EOFFLG  (-2)
# define DFAULT  (-3)
# define FFRAME  (-4)
# define NULFLG  (-5)
# define FP_FULL (-6)
# define FP_ALLFULL (-6)
# define FP_ORDFULL (-7)
# define FP_REDU DFAULT
# define INTP_2  DFAULT
# define INTP_4  FP_FULL
#endif

#if defined(__STDC__) || defined(__cplusplus)
enum PFF_data_types { PFTDIR, PFTUF3, PFTUF1, PFTNF3, PFTNV3, PFTVTX, PFTIFL,
                      PFTNGD, PFTNG3, PFTNI3 };
#else
# define PFTDIR (0)
# define PFTUF3 (1)
# define PFTUF1 (2)
# define PFTNF3 (3)
# define PFTNV3 (4)
# define PFTVTX (5)
# define PFTIFL (6)
# define PFTNGD (7)
# define PFTNG3 (8)
# define PFTNI3 (9)
#endif

#if defined(__STDC__) || defined(__cplusplus)
enum { PFF_RECLEN=2048, PFF_HEADERLEN=16, PFF_MAXRFU=10, MAXPFFINTS=2048, 
       PFFDIRLISTSIZE=20, PFFFILELISTSIZE=7 };
#else
# define PFF_RECLEN      2048
# define PFF_HEADERLEN   16
# define PFF_MAXRFU      10
# define MAXPFFINTS      2048
# define PFFDIRLISTSIZE  20
# define PFFFILELISTSIZE 7
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

#define CHKFREE(A)  if ( (A) != NULL ) free (A)

typedef struct s_PFFfid    PFFfid;
typedef struct s_PFFdir    PFFdir;
typedef struct s_PFFhead   PFFhead;
typedef struct s_PFFfreeID PFFfreeID;

struct s_PFFfreeID {
  int index;
  PFFfreeID *next;
};

struct s_PFF         {       /* Master pointers to PFF file linked list */
 
  PFFfid     *top;          /* Pointer to top of PFF file stack */
  PFFfid     *current;      /* Pointer to current PFF file */
  PFFfreeID  *free_stk;     /* Stack of reusable file counts */
  int        open_cnt;      /* Number of currently open files */
};

struct s_PFFfid     {       /* Element for PFF file linked list */

  PFFfid    *up;            /* Pointer toward top of stack */
  PFFfid    *down;          /* Pointer toward bottom of stack */
  char       *name;         /* Pointer to string containing name of file */
  FILE       *stream;       /* Pointer to file stream */
  int         mode;         /* Mode indicator */
  int         mp_on;        /* indicates if file opened for MP access */
  int         mp_current;   /* indicates file is active for this processor */
  int         mp_mode;      /* indicates if write operation is for current
                               processor or all processors */
  int         fp_precision; /* Indicator for floating point precision */
  int         extend_flag;  /* Indicator that file has been extended */
  long        position;     /* current file position (in 2-byte words) */
  long        last_word;    /* last word of data written to file */
  int         count;        /* file counter (relative to bottom of list) */
  PFFdir     *directory;    /* Pointer to current directory */
  PFFdir     *dirtop;       /* Pointer to top (last) of directory linked list */
  PFFdir     *dirbottom;    /* Pointer to bottom (first) of directory linked 
                                                                         list */
};

struct s_PFFdir     {       /* Element for PFF directory linked list */

  int         rawtype;      /* Integer raw dataset type indicator */
  int         apptype;      /* Integer application dataset type indicator */
  char       *title;        /* Pointer to string containing title */
  char       *type_name;    /* Pointer to string type descriptor */
  long        length;       /* Length of dataset (in 2-byte units) */
  long        offset;       /* Offset in file (in 2-byte units) */
  int         count;        /* directory element counter */
  PFFdir     *up;           /* Pointer toward top of stack */
  PFFdir     *down;         /* Pointer toward bottom of stack */
};

struct s_PFFhead     {      /* Structure to contain all PFF header information
                               for one dataset */

  int         rawtype;      /* Integer raw dataset type indicator */
  int         apptype;      /* Integer application dataset type indicator */
  char       *title;        /* Pointer to string containing title */
  char       *type_name;    /* Pointer to string type descriptor */
  long        length;       /* Length of dataset (in 2-byte units) */
  int         ds_version;   /* version of dataset format */
  int         nwords_rfu;   /* # of non-default reserved words */
  int        *rfu;          /* array of non-default reserved words */
};

#ifdef __cplusplus
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif

#if defined(__STDC__) || defined(__cplusplus)
  EXTERN void pf_wr_err ( char *module, int ierr, PFFfid *fid,  char *message );
#else
  extern void pf_wr_err ();
#endif

#endif
