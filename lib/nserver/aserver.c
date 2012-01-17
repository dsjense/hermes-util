/* **********************************************************************
    C Name Server software
    D. Seidel
    3/2/99
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* function name mangling for F77 linkage */
#include "mdf77mangle.h"
#if !defined(HU_F77_MANGLING_L00)
#define  ns_defarray     HU_F77_FUNC_( ns_defarray    , NS_DEFARRAY     )
#define  ns_freearray    HU_F77_FUNC_( ns_freearray   , NS_FREEARRAY    )
#define  ns_putarrval    HU_F77_FUNC_( ns_putarrval   , NS_PUTARRVAL    )
#define  ns_getarrval    HU_F77_FUNC_( ns_getarrval   , NS_GETARRVAL    )
#define  ns_getarrlims   HU_F77_FUNC_( ns_getarrlims  , NS_GETARRLIMS   )
#define  nsu_debug_array HU_F77_FUNC_( nsu_debug_array, NSU_DEBUG_ARRAY )
#endif

#ifndef DEBUG
# define INCR_SIZE  200
#endif

/* Container for storing integer array */
struct s_Array
{
  int lo, hi;
  int *array;
};

typedef struct s_Array Array;

/* file scope variables */
static int     NSA_count    = 0;
static int     NSA_nfree    = 0;
static int    *NSA_freelist = NULL;
static Array  *NSA_arrays  = NULL;

/* prototypes */
int ns_defarray  ( int *lo, int *hi, int *init_val );
int ns_freearray ( int *handle );
int ns_putarrval ( int *handle, int *index, int *val );
int ns_getarrval ( int *handle, int *index, int *val );
int ns_getarrlims ( int *handle, int *lo, int *hi );
void nsu_debug_array (int *cnt, int *free);

/*  ns_defarray allocates storage of an integer array w/ min & max indices
    of "lo" and "hi", respectively. Note that all elements in the array are 
      initialized to the supplied value "init_val".

      lo       --  minimum index of array (read only)
      hi       --  maximum index of array (read only)
      init_val --  initial value for all array elements

      Return value:   > 0 -- array's handle for later retrieval
                      0   -- memory allocation error
                     -1   -- illegal values for lo and hi
*/
int ns_defarray  ( int *lo, int *hi, int *init_val )
{
  int  *p;
  int   i, j, loc, len;

  len = *hi - *lo + 1;
  if ( len <= 0 ) return -1;

  if ( NSA_nfree == 0 )  {
    NSA_count += INCR_SIZE;
    if ( (NSA_arrays = 
          (Array *) realloc(NSA_arrays,NSA_count*sizeof(Array))) == NULL ||
         (NSA_freelist = 
          (int *) realloc(NSA_freelist,NSA_count*sizeof(int))) == NULL )  {
      perror("ns_defarray");
      return 0;
    }
    for (i=0,j=NSA_count; i<INCR_SIZE; i++)  {
      NSA_freelist[i] = --j;
      NSA_arrays[j].array  = NULL;
    }
    NSA_nfree = INCR_SIZE;
  }

  if ( (p = (int *) malloc(len*sizeof(int))) == NULL )   {
    perror("ns_defarray");
    return 0;
  }

  loc = NSA_freelist[--NSA_nfree];
  for(i=0;i<len;i++) p[i] = *init_val;
  NSA_arrays[loc].array = p;
  NSA_arrays[loc].lo = *lo;
  NSA_arrays[loc].hi = *hi;
  return loc+1;
}

/*  ns_freearray frees the array referenced by handle and returns a status
      code. Note that upon successful completion, the handle is set to zero.
      Also note that if the supplied value of handle is zero, nothing is done
      and zero is returned.

      handle --  supplied string handle (read/write)


      Return value:    0   -- successful completion
                      -1   -- illegal handle value
                      -2   -- handle is not active
*/
int ns_freearray ( int *handle )
{
  int loc;

  if ( *handle == 0 ) return 0;
  loc = (*handle) - 1;
  if ( loc < 0  ||  loc >= NSA_count )  return -1;
  if ( NSA_arrays[loc].array == NULL ) return -2;

  free(NSA_arrays[loc].array);
  NSA_arrays[loc].array = NULL;
  NSA_freelist[NSA_nfree++] = loc;
  *handle = 0;
  return 0;
}

/*  ns_putarrval stores an integer value in the array referenced by the
    supplied handle at index.

      handle --  supplied string handle (read only)
      index  --  array index (read only)
      val    --  value to be stored (write only)

      Return value:    0   -- normal completion
                      -1   -- an invalid handle supplied
                      -2   -- illegal index
*/
int ns_putarrval ( int *handle, int *index, int *val )
{
  int aloc, iloc;
  Array *a;

  aloc = (*handle) - 1;
  if ( aloc < 0 || aloc >= NSA_count || 
       NSA_arrays[aloc].array == NULL )  return -1;
  a = &NSA_arrays[aloc];
  iloc = (*index) - a->lo;
  if ( iloc < 0 || *index > a->hi )  return -2;

  a->array[iloc] = *val;
  return 0;
}

/*  ns_getarrval retrieves an integer value from the array referenced by the
    supplied handle at index.

      handle --  supplied string handle (read only)
      index  --  array index (read only)
      val    --  returned value (write only)

      Return value:    0   -- normal completion
                      -1   -- an invalid handle supplied
                      -2   -- illegal index
*/
int ns_getarrval ( int *handle, int *index, int *val )
{
  int aloc, iloc;
  Array *a;

  aloc = (*handle) - 1;
  if ( aloc < 0 || aloc >= NSA_count || 
       NSA_arrays[aloc].array == NULL )  return -1;
  a = &NSA_arrays[aloc];
  iloc = (*index) - a->lo;
  if ( iloc < 0 || *index > a->hi )  return -2;

  *val = a->array[iloc];
  return 0;
}

/*  ns_getarrlims retrieves the minimum index and maximum index of the integer
    array referenced by the supplied handle at index.

      handle --  supplied string handle (read only)
      lo     --  lower array index (write only)
      hi     --  upper array index (write only)

      Return value:    0   -- normal completion
                      -1   -- an invalid or inactive handle supplied
*/
int ns_getarrlims ( int *handle, int *lo, int *hi )
{
  int aloc;

  aloc = (*handle) - 1;
  if ( aloc < 0 || aloc >= NSA_count || 
       NSA_arrays[aloc].array == NULL )  return -1;

  *lo = NSA_arrays[aloc].lo;
  *hi = NSA_arrays[aloc].hi;

  return 0;
}

/*  nsu_debug_array is a utility function used by NS_debug to obtain
    statistics regarding the server's allocated arrays.

      cnt  --  number of allocated arrays (write only)
      free --  number of allocated arrays that are currently unused
               (write only)
*/
void nsu_debug_array (int *cnt, int *free)
{
  *cnt = NSA_count;
  *free = NSA_nfree;
}
