/* **********************************************************************
    PFF directory block manager software
    C_Groups @(#) dynamic
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
    
   ******************************************************************* */
/*
 This code is only needed for dynamic !!!
*/
#ifndef STATIC_MEM

# include <stdlib.h>

/* function name mangling for F77 linkage */
# include "mdf77mangle.h"
# if !defined(HU_F77_MANGLING_L00)
#  define dirblk_init     HU_F77_FUNC_( dirblk_init    ,  DIRBLK_INIT     )
#  define get_dirblk      HU_F77_FUNC_( get_dirblk     ,  GET_DIRBLK      )
#  define assign_dirblk   HU_F77_FUNC_( assign_dirblk  ,  ASSIGN_DIRBLK   )
#  define free_dirblk     HU_F77_FUNC_( free_dirblk    ,  FREE_DIRBLK     )
# endif

# define LIST_INCR  200
# define FILE_INCR   20

static int ***db_by_file = 0;
static int *max_by_file = 0;
static int mxfils = 0;
static int db_size = 0;

int dirblk_init( int *pmxfils, int *pdb_size )
{
  int i;

  mxfils = *pmxfils;
  db_size = *pdb_size;

  if ( (db_by_file = (int ***) calloc(mxfils,sizeof(int **))) == NULL ||
       (max_by_file = (int *) calloc(mxfils,sizeof(int))) == NULL ) {
    return 1;
  }

  return 0;
}

/*
   Return value:
     0 - Successful completion
     1 - Block is already assigned
     2 - Unable to allocate memory
 */
int assign_dirblk(const int *blk, const int *fid)
{
  int b, f, i, old_size;
  int *db;

  b = *blk;
  f = *fid - 1;

  old_size = max_by_file[f];
  while ( max_by_file[f] <= b ) max_by_file[f] += FILE_INCR;

  if ( max_by_file[f] > old_size ) {
    if ( ( db_by_file[f] = 
           (int **) realloc(db_by_file[f], sizeof(int *)*max_by_file[f])) ==
         NULL ) return 2;
    for(i=old_size; i<max_by_file[f]; ++i) db_by_file[f][i] = NULL;
  }

  if ( db_by_file[f][b] == NULL ) {
    if ( ( db = (int *) calloc(db_size,sizeof(int))) == NULL ) return 2;
    db_by_file[f][b] = db;
  }
  else return 1;

  return 0;
}

/*
   Return value:
     0 - Successful completion
     1 - Block is not assigned
 */
int get_dirblk(const int *blk, const int *fid, int **dblk)
{
  int b, f;

  b = *blk;
  f = *fid - 1;

  if ( max_by_file == NULL || max_by_file[f] <= b ||
       db_by_file[f][b] == NULL ) return 1;

  *dblk = db_by_file[f][b];
  
  return 0;
}

/*
   Return value:
     0 - Successful completion
     1 - Block is not assigned
*/
int free_dirblk(const int *blk, const int *fid)
{
  int b, f, handle;

  b = *blk;
  f = *fid - 1;

  if ( db_by_file[f][b] == NULL ) return 1;

  free(db_by_file[f][b]);
  db_by_file[f][b] = NULL;

  return 0;
}
#else
/* dummy code to keep compilers from complaining */
static char junk = '\0';
#endif
