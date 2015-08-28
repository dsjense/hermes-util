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
/*! \file namell.c
 *  \brief File containing tools for maintaining linked lists of memory bin
 *         allocation data.
 *
 *  \addtogroup PrivateInterface
 *    \{
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

#if defined(__STDC__) || defined(__cplusplus)
/*! \brief Enumeration of possible locations on the linked list
 *    \li \b UP, previous link relative to current link
 *    \li \b DOWN, next ink relative to current link
 *    \li \b FIRST, first link on list
 *    \li \b LAST, last link on list
 *  \note These MUST be consistent with similar parameters defined in stak.inc
 */
  enum { UP=-3, DOWN, FIRST, LAST };
/*! \brief maximum # of memory bins that can be allocated
 *  \note This MUST be consistent with parameter \b NBINS defined in stak.inc
 */
  enum { MAX_BINS=100 };
/*! \brief number of integer words required to store boundary lengths bounding
 *         array data locations
 */
  enum { BNDWRDS=sizeof(void *)/sizeof(int) };
#else
  #define     UP     -3
  #define     DOWN   -2
  #define     FIRST  -1
  #define     LAST    0
  #define     MAX_BINS  100
  #define BNDWRDS  ( sizeof(void *)/sizeof(int) )
#endif

/*
#define     INT     0
#define     FLT    -1
*/

/*! \brief Alias for struct allo_str */
typedef  struct  allo_str    Allo;
/*! \brief Alias for struct bin_str */
typedef  struct  bin_str     Bin;

/*! \brief structure containing data related to an single allocation in a
 *         memory bin
 */
struct  allo_str  {
  /*! pointer to data for next allocation */
  Allo   *next;
  /*! pointer to data for previous allocation */
  Allo   *prev;
  /*! index of this allocation (1 indicates first allocation for bin) */
  long    index;
  /*! variable type (see enumeration of types in stkenum.inc) */
  long    type;
  /*! allocation's offset (in integer words) in the memory bin */
  size_t  loc;
  /*! allocation's length (in integer words) */
  size_t  len;
  /*! Supplied name associated with allocation */
  char   *name;
};

/*! \brief structure containing data related to the linked list associated with
 *         a memory bin
 */
struct  bin_str  {
  /*! pointer to the data associated with the first allocation for this bin */
  Allo   *first;
  /*! pointer to the data associated with the last allocation for this bin */
  Allo   *last ;
  /*! pointer to the data associated with the current allocation for this bin */
  Allo   *current;
  /*! # of guard words used for this memory bin */
  long    guard;
};

#ifdef __STDC__
/*! \brief Initializes the linked list structure for the specified bin.
 *
 *  \param[in] bin   A pointer to the bin number to be initialized
 *                   (1-\b MAX_BINS).
 *  \param[in] guard A pointer to the number of guard words used for this bin
 *                   (in units of integer words).
 *  \returns
 *   \li  0, Successful completion
 *   \li  1, Bin out of range
 *  \note All function arguments are pointers so that this function can be
 *        called directly from Fortran.
 */
int name_init(int *bin, int *guard );
/*! \brief Adds a new allocation data block the linked list for the specified
 *         bin.
 *
 *  \param[in] bin   A pointer to the bin number of the allocation
 *                   (1-\b MAX_BINS).
 *  \param[in] loc   A pointer to the integer-word offset of the allocation.
 *  \param[in] len   A pointer to the integer-word length of the allocation.
 *  \param[in] type  A pointer to the allocation's type (see enumeration of
 *                   types in stkenum.inc).
 *  \param[in] name  String containing the name assigned to the allocation.
 *  \returns
 *   \li  0, Successful completion
 *   \li  1, Bin out of range
 *   \li  2, Error allocating memory for allocation data block
 *   \li  3, Error allocating memory for allocation name string

 *  \note All function arguments are pointers so that this function can be
 *        called directly from Fortran.
 */
int add_name(int *bin, size_t *loc, size_t *len, int *type, char *name );
/*! \brief Retrieves the information from the specified allocation data block in
 *         the linked list for the specified bin.
 *
 *  \param[in]  bin   A pointer to the bin number of the requested allocation
 *                    (1-\b MAX_BINS).
 *  \param[in]  index A pointer to the index of the requested allocation
 *                    (1 is first).
 *  \param[out] loc   A pointer to the integer-word offset of the allocation.
 *  \param[out] len   A pointer to the integer-word length of the allocation.
 *  \param[out] type  A pointer to the allocation's type (see enumeration of
 *                    types in stkenum.inc).
 *  \param[out] name  String returning the name assigned to the allocation.
 *  \returns
 *   \li  0, Successful completion
 *   \li  1, Bin out of range
 *   \li  2, Index out of range

 *  \note All function arguments are pointers so that this function can be
 *        called directly from Fortran.
 */
int get_name(int *bin, int *index, size_t *loc, size_t *len, int *type,
             char *name );
/*! \brief Removes the LAST allocation data block in the linked list for the
 *         specified bin.
 *
 *  \param[in]  bin   A pointer to the bin number of the requested removal
 *                    (1-\b MAX_BINS).
 *  \returns
 *   \li  0, Successful completion
 *   \li  1, Bin out of range
 *   \li  2, Bin is empty (no allocation data blocks)

 *  \note The function argument is a pointer so that this function can be
 *        called directly from Fortran.
 */
int remove_name(int *bin );
/*! \brief Changes the length attribute of the LAST allocation data block in
 *         the linked list for the specified bin.
 *
 *  \param[in] bin   A pointer to the bin number to be initialized
 *                   (1-\b MAX_BINS).
 *  \param[in] len   A pointer to the new integer-word length of the allocation.
 *  \returns
 *   \li  0, Successful completion
 *   \li  1, Bin out of range
 *   \li  2, Bin is empty (no allocation data blocks)
 *  \note All function arguments are pointers so that this function can be
 *        called directly from Fortran.
 */
int grow_name(int *bin, size_t *len );
/*! \brief Removes ALL allocation data blocks in the linked list for the
 *         specified bin.
 *
 *  \param[in]  bin   A pointer to the bin number that is to be cleared
 *                    (1-\b MAX_BINS).
 *  \returns
 *   \li  0, Successful completion
 *   \li  1, Bin out of range
 *  \note The function argument is a pointer so that this function can be
 *        called directly from Fortran.
 */
int clear_name(int *bin );
/*! \brief Using the bin's guard word count and each allocation's offset and
 *         length, checks for consistency of memory layout.
 *
 *  \param[in]  bin   A pointer to the bin number that is to be checked
 *                    (1-\b MAX_BINS).
 *  \param[in]  cnt   A pointer to the number of allocations that have been
 *                    made for the bin
 *  \param[in]  next  A pointer to the integer-word offset to the first unused
 *                    work in the bin
 *  \returns
 *   \li  0, Successful completion
 *   \li <0, Negative of the index of the allocation whose offset is
 *           inconsistent with the lengths of its and all previous allocations
 *   \li  1, Index of last allocation data block does not match the number of
 *           allocations (supplied by \e cnt)
 *   \li  2, Offset to next available word of of bin is inconsistent with the
 *           lengths of all previous allocations (supplied by \e next)
 *   \li  3, Both error conditions 1 and 2 occurred
 *   \li  4, Bin out of range
 *  \note All function arguments are pointers so that this function can be
 *        called directly from Fortran.
 */
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

/*! \brief Array information blocks for each memory bin */
static Bin   Bin_Info[MAX_BINS];

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

  if ( pbin->last == NULL ) return 2;

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

  if ( *bin > MAX_BINS || *bin < 1 ) return 4;

  pbin = Bin_Info + *bin - 1;

  if ( (al = pbin->first) == NULL ) return 0;

  ng = (pbin->guard - 1)/BNDWRDS + 1;
  while ( al != NULL ) {
    lst_indx = al->index;
    if ( al->loc != loc ) return -lst_indx;
    loc += al->len + BNDWRDS*( (*cnt != lst_indx) ? (ng + 2) : 1 );
    al = al->next;
  }
  if ( *cnt != lst_indx ) rval += 1;
  if ( *next != loc ) rval += 2;

  return rval;
}
/*! \} */
