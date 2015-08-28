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
/*! \file mdmem.c
 *  \brief File containing underlying memory allocation functions used by the
 *         STAK library.
 *
 *  \addtogroup PrivateInterface
 *    \{
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
/*! \brief Function to allocate space for an integer array of specified length.
 *
 *  This function is essentially a wrapper for \e malloc (stdlib.h), with units
 *  of words rather than bytes.
 *  \param[in] size A pointer to the size of the requested allocation (in units
 *                  of integer words).
 *  \return A \e void* pointer to the requested memory. If called from Fortran,
 *          the returned type is defined by the Hermes macro \b HU_PTR_TYPE.
 *  \note All function arguments are pointers so that this function can be
 *        called directly from Fortran.
 */
void* mdgmem(int *size);
/*! \brief function to release allocated memory.
 *
 *  \param[in] ptr A \e pointer to the pointer to be released. If called
 *                  from Fortran, the Fortran type is defined by the Hermes
 *                   macro \b HU_PTR_TYPE.
 *  \param[in] ptr A \e pointer to the pointer to be released.
 * 
 *  \note
 *    \li Function argument is a \e pointer to the pointer so that this
 *        function can be called directly from Fortran.
 *    \li If the pointer is zero, no action is taken
 *  \warning
 *    The supplied pointer \b MUST have originally been allocated by a call to
 *    \e malloc,  \e calloc, or  \e realloc (stdlib.h). This is the case for
 *    any pointer returned by functions in the STAK library, \e except \ref
 *    stkpad).
 */
int   mdrmem(void **ptr);
/*! \brief Function to reallocate space for an integer array with an new length.
 *
 *  This function is essentially a wrapper for \e realloc (stdlib.h), with units
 *  of words rather than bytes.
 *  \param[in] ptr  A \e pointer to the pointer to be reallocated. If called
 *                  from Fortran, the Fortran type is defined by the Hermes
 *                   macro \b HU_PTR_TYPE.
 *  \param[in] size A pointer to the new size requested (in units
 *                  of integer words).
 *  \return A \e void* pointer to the requested memory (in general
 *          this will be different from the suppled value of \e ptr). If called
 *          from Fortran, the returned type is defined by the Hermes macro
 *          \b HU_PTR_TYPE.
 *  \note All function arguments are pointers so that this function can be
 *        called directly from Fortran.
 *  \warning
 *    The supplied pointer \b MUST have originally been allocated by a call to
 *    \e malloc,  \e calloc, or  \e realloc (stdlib.h). This is the case for
 *    any pointer returned by functions in the STAK library, \e except \ref
 *    stkpad).
 */
void* mdgrowmem(void **ptr, int *size);
/*! \brief Function to allocate space for an integer array of specified length.
 *
 *  This function is essentially a wrapper for \e malloc (stdlib.h), with units
 *  of words rather than bytes. 
 *  \param[in] size A pointer to the size of the requested allocation (in units
 *                  of integer words).
 *  \return A \e void* pointer to the requested memory. If called from Fortran,
 *          the returned type is defined by the Hermes macro \b HU_PTR_TYPE.
 *  \note \li All function arguments are pointers so that this function can be
 *            called directly from Fortran.
 *        \li This is identical to \ref mdgmem, \e except the supplied \e size
 *            argument is type <TT>size_t</TT>. If called from Fortran, this
 *            would be the type defined by the Hermes macro \b HU_SIZE_T.
 */
void* mdgmem_st(size_t *size);
/*! \brief Function to reallocate space for an integer array with an new length.
 *
 *  This function is essentially a wrapper for \e realloc (stdlib.h), with units
 *  of words rather than bytes.
 *  \param[in] ptr  A \e pointer to the pointer to be reallocated. If called
 *                  from Fortran, the Fortran type is defined by the Hermes
 *                   macro \b HU_PTR_TYPE.
 *  \param[in] size A pointer to the new size requested (in units
 *                  of integer words).
 *  \return A \e void* pointer to the requested memory (in general
 *          this will be different from the suppled value of \e ptr). If called
 *          from Fortran, the returned type is defined by the Hermes macro
 *          \b HU_PTR_TYPE.
 *  \note \li All function arguments are pointers so that this function can be
 *            called directly from Fortran.
 *        \li This is identical to \ref mdgrowmem, \e except the supplied \e
 *            size argument is type <TT>size_t</TT>. If called from Fortran,
 *            this would be the type defined by the Hermes macro \b HU_SIZE_T.
 *  \warning
 *    The supplied pointer \b MUST have originally been allocated by a call to
 *    \e malloc,  \e calloc, or  \e realloc (stdlib.h). This is the case for
 *    any pointer returned by functions in the STAK library, \e except \ref
 *    stkpad).
 */
void* mdgrowmem_st(void **ptr, size_t *size);
#else
void* mdgmem();
int   mdrmem();
void* mdgrowmem();
void* mdgmem_st();
void* mdgrowmem_st();
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
/*! \} */
