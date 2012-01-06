/*
-----------------------------------------------------------------------------
    Machine-dependent memory management routines:  mdmem.c
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

/* function name mangling for F77 linkage */
#include "mdf77mangle.h"
#if !defined(HU_F77_MANGLING_L00)
#define  mdgmem HU_F77_FUNC(  mdgmem,  MDGMEM )
#define  mdgmem_st HU_F77_FUNC_(  mdgmem_st,  MDGMEM_ST )
#define  mdrmem HU_F77_FUNC(  mdrmem,  MDRMEM )
#define  mdgrowmem HU_F77_FUNC(  mdgrowmem,  MDGROWMEM )
#define  mdgrowmem_st HU_F77_FUNC_(  mdgrowmem_st,  MDGROWMEM_ST )
#endif

#ifdef __STDC__
void* mdgmem(int *size);
int   mdrmem(void **ptr);
void* mdgrowmem(void **ptr, int *size);
#else
void* mdgmem();
int   mdrmem();
void* mdgrowmem();
#endif

/*
#ifdef __STDC__
void* mdgrowmem(void **ptr, int *size)
#else
void* mdgrowmem(ptr,size)
void **ptr;
int *size;
#endif
{
  void *nptr;

  printf("mdgrowmem: %p  %d\n",*ptr,*size);
  nptr = realloc(*ptr, sizeof(int)*(*size));
  printf("mdgrowmem: done  %p\n",nptr);
  return nptr;
}

#ifdef __STDC__
void* mdgmem(int *size)
#else
void* mdgmem(size)
int *size;
#endif
{
  void *ptr;

  ptr = malloc(sizeof(int)*(*size));
  printf("mdgmem: %p  %d %d\n",ptr,*size,sizeof(int)*(*size));
  return ptr;
}
*/

#ifdef __STDC__
void* mdgmem(int *size)
#else
void* mdgmem(size)
int *size;
#endif
{
  return malloc(sizeof(int)*(*size));
}

#ifdef __STDC__
void* mdgmem_st(size_t *size)
#else
void* mdgmem_st(size)
size_t *size;
#endif
{
  return malloc(sizeof(int)*(*size));
}

#ifdef __STDC__
void* mdgrowmem(void **ptr, int *size)
#else
void* mdgrowmem(ptr,size)
void **ptr;
int *size;
#endif
{
  return realloc(*ptr, sizeof(int)*(*size));
}

#ifdef __STDC__
void* mdgrowmem_st(void **ptr, size_t *size)
#else
void* mdgrowmem_st(ptr,size)
void **ptr;
size_t *size;
#endif
{
  return realloc(*ptr, sizeof(int)*(*size));
}

#ifdef __STDC__
int mdrmem(void **ptr)
#else
int mdrmem(ptr)
void **ptr;
#endif
{
  if ( *ptr != NULL ) free(*ptr);
  *ptr = NULL;
  return 0;
}
