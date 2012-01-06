/*
-----------------------------------------------------------------------------
    Linked list management routines:  namell.c
-----------------------------------------------------------------------------
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
-----------------------------------------------------------------------------
*/

#include <stdlib.h>
#include <string.h>

/* function name mangling for F77 linkage */
#include "mdf77mangle.h"
#if !defined(HU_F77_MANGLING_L00)
#define  name_init   HU_F77_FUNC_WITH_UNDERSCORES(  name_init  ,  NAME_INIT   )
#define  add_name    HU_F77_FUNC_WITH_UNDERSCORES(  add_name   ,  ADD_NAME    )
#define  get_name    HU_F77_FUNC_WITH_UNDERSCORES(  get_name   ,  GET_NAME    )
#define  remove_name HU_F77_FUNC_WITH_UNDERSCORES(  remove_name,  REMOVE_NAME )
#define  grow_name   HU_F77_FUNC_WITH_UNDERSCORES(  grow_name  ,  GROW_NAME   )
#define  clear_name  HU_F77_FUNC_WITH_UNDERSCORES(  clear_name ,  CLEAR_NAME  )
#define  check_name  HU_F77_FUNC_WITH_UNDERSCORES(  check_name ,  CHECK_NAME  )
#endif

#define     UP     -3
#define     DOWN   -2
#define     FIRST  -1
#define     LAST    0

#define     INT     0
#define     FLT    -1

#define     MAX_BINS  100

#define BNDWRDS  ( sizeof(void *)/sizeof(int) )

typedef  struct  allo_str    Allo;
typedef  struct  bin_str     Bin;

struct  allo_str  {
  Allo   *next;
  Allo   *prev;
  long    index, type;
  size_t  loc, len;
  char   *name;
};

struct  bin_str  {
  Allo   *first;
  Allo   *last ;
  Allo   *current;
  long    guard;
};

#ifdef __STDC__
int name_init(int *bin, int *guard );
int add_name(int *bin, size_t *loc, size_t *len, int *type, char *name );
int get_name(int *bin, int *index, size_t *loc, size_t *len, int *type,
             char *name );
int remove_name(int *bin );
int grow_name(int *bin, size_t *len );
int clear_name(int *bin );
int check_name(int *bin, int *cnt, size_t *next);
#else
int name_init();
int add_name();
int get_name();
int remove_name();
int grow_name();
int clear_name();
int check_name(bin, cnt, next);
#endif


Bin   Bin_Info[MAX_BINS];

#ifdef __STDC__
int name_init(int *bin, int *guard )
#else
int name_init(bin, guard )
int *bin;
int *guard;
#endif
{
  Bin  *pbin;

  if ( *bin > MAX_BINS || *bin < 1 ) return 1;

  pbin = Bin_Info + *bin - 1;

  pbin->first   = NULL;
  pbin->last    = NULL;
  pbin->current = NULL;
  pbin->guard   = *guard;

  return 0;
}

#ifdef __STDC__
int add_name(int *bin, size_t *loc, size_t *len, int *type, char *name )
#else
int add_name(bin, loc, len, type, name )
int *bin;
size_t *loc;
size_t *len;
int *type;
char *name;
#endif
{
  Bin  *pbin;
  Allo *al;
  char *pc;

  if ( *bin > MAX_BINS || *bin < 1 ) return 1;

  pbin = Bin_Info + *bin - 1;

  if ( (al = (Allo *) malloc(sizeof(Allo))) == NULL ) return 2;
  if ( (pc = (char *) malloc(sizeof(char)*(strlen(name)+1))) == NULL )  {
    free(al);
    return 3;
  }
/*
  if ((al = (Allo *)malloc(sizeof(Allo) + 
                           sizeof(char)*(strlen(name)+1))) == NULL ) return 2;
  pc = (char *) (al + 1);
*/

  al->prev = pbin->last;
  if ( pbin->last == NULL ) {
    pbin->first = al;
    al->index   = 1;
  }
  else  {
    pbin->last->next = al;
    al->index = pbin->last->index + 1;
  }
  al->next = NULL;
  al->loc  = *loc;
  al->len  = *len;
  al->type = *type;

  strcpy(pc,name);
  al->name = pc;

  pbin->last    = al;
  pbin->current = al;

  return 0;
}

#ifdef __STDC__
int get_name(int *bin, int *index, size_t *loc, size_t *len, int *type,
             char *name )
#else
int get_name(bin, index, loc, len, type, name )
int *bin;
int *index;
size_t *loc;
size_t *len;
int *type;
char *name;
#endif
{
  Bin  *pbin;
  Allo *al;

  if ( *bin > MAX_BINS || *bin < 1 ) return 1;

  pbin = Bin_Info + *bin - 1;

  if      ( *index == LAST )        pbin->current = pbin->last;
  else if ( *index == FIRST )       pbin->current = pbin->first;
  else if ( *index == UP )          pbin->current = pbin->current->prev;
  else if ( *index == DOWN )        pbin->current = pbin->current->next;
  else if ( pbin->current == NULL ) pbin->current = pbin->last;
  else if ( *index < 1 || *index > pbin->last->index ) return 2;
  else  {
    while ( pbin->current != NULL && pbin->current->index > *index )
              pbin->current = pbin->current->prev;
    while ( pbin->current != NULL && pbin->current->index < *index )
              pbin->current = pbin->current->next;
  }
  if ( (al = pbin->current) == NULL ) return 2;

  *loc  = al->loc;
  *len  = al->len;
  *type = al->type;
  strcpy(name,al->name);

  return 0;
}

#ifdef __STDC__
int remove_name(int *bin )
#else
int remove_name(bin )
int *bin;
#endif
{
  Bin  *pbin;
  Allo *al;

  if ( *bin > MAX_BINS || *bin < 1 ) return 1;

  pbin = Bin_Info + *bin - 1;

  if ( (al = pbin->last) == NULL ) return 2;

  free(al->name);
  pbin->last    = al->prev;
  pbin->current = al->prev;
  free(al);
  if ( pbin->last != NULL ) pbin->last->next = NULL;
  else pbin->first = NULL;

  return 0;
}

#ifdef __STDC__
int grow_name(int *bin, size_t *len )
#else
int grow_name(bin, len )
int *bin;
size_t *len;
#endif
{
  Bin  *pbin;

  if ( *bin > MAX_BINS || *bin < 1 ) return 1;

  pbin = Bin_Info + *bin - 1;

  pbin->last->len = *len;

  return 0;
}

#ifdef __STDC__
int clear_name(int *bin )
#else
int clear_name(bin )
int *bin;
#endif
{
  int   ival;
  Bin  *pbin;

  if ( *bin > MAX_BINS || *bin < 1 ) return 1;

  pbin = Bin_Info + *bin - 1;

  while ( (ival = remove_name(bin)) == 0 );

  if ( ival != 2 ) return ival;

  return 0;
}

#ifdef __STDC__
int check_name(int *bin, int *cnt, size_t *next)
#else
int check_name(bin, cnt, next )
int *bin, *cnt
size_t *next;
#endif
{
  Bin  *pbin;
  Allo *al;
  size_t loc = BNDWRDS;
  long lst_indx = 0;
  int rval = 0;
  int ng = 0;

  if ( *bin > MAX_BINS || *bin < 1 ) return 1;

  pbin = Bin_Info + *bin - 1;

  if ( (al = pbin->first) == NULL ) return 0;

  ng = (pbin->guard - 1)/BNDWRDS + 1;
  while ( al != NULL ) {
    lst_indx = al->index;
    if ( al->loc != loc ) return lst_indx;
    loc += al->len + BNDWRDS*( (*cnt != lst_indx) ? (ng + 2) : 1 );
    al = al->next;
  }
  if ( *cnt != lst_indx ) rval -= 1;
  if ( *next != loc ) rval -= 2;

  return 0;
}
