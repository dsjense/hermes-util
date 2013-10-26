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

/*! \file nserver.c
 *  \brief File containing the underlying C implementation of the Name Server.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "nserver_c.h"

/* function name mangling for F77 linkage */
#include "mdf77mangle.h"
#if !defined(HU_F77_MANGLING_L00)
/* \cond NEVER */
# define nsu_putname     HU_F77_FUNC_( nsu_putname    ,  NSU_PUTNAME     )
# define ns_freename     HU_F77_FUNC_( ns_freename    ,  NS_FREENAME     )
# define nsu_getname     HU_F77_FUNC_( nsu_getname    ,  NSU_GETNAME     )
# define nsu_find        HU_F77_FUNC_( nsu_find       ,  NSU_FIND        )
# define nsu_find_match  HU_F77_FUNC_( nsu_find_match ,  NSU_FIND_MATCH  )
# define nsu_findorput   HU_F77_FUNC_( nsu_findorput  ,  NSU_FINDORPUT   )
# define ns_locate       HU_F77_FUNC_( ns_locate      ,  NS_LOCATE       )
# define ns_locate_match HU_F77_FUNC_( ns_locate_match,  NS_LOCATE_MATCH )
# define nsu_debug       HU_F77_FUNC_( nsu_debug      ,  NSU_DEBUG       )
/* \endcond */
#endif

#ifndef DEBUG
/*! \brief Increment size for allocating handles for the free list */
# define INCR_SIZE  200
#endif

/* file scope variables */
/*! \brief Number of allocated string handles */
static int    NSU_count    = 0;
/*! \brief Number of free allocated string handles */
static int    NSU_nfree    = 0;
/*! \brief Array of free string handles */
static int   *NSU_freelist = NULL;
/*! \brief Array of strings associated with each handle */
static char **NSU_strings  = NULL;

/* prototypes */
int nsu_putname ( const int *nblen, const char *name );
int ns_freename ( int *handle );
int nsu_getname ( const int *handle, const int *maxlen, char *name );
int nsu_findorput ( const int *nblen, const char *name, const int *hlist,
                    const int *nlist );
int nsu_find ( const int *nblen, const char *name, const int *hlist,
               const int *nlist );
int ns_locate ( const int *handle, const int *hlist, const int *nlist );
int nsu_find_match ( const int *nblen, const char *name, const int *hlist,
                     const int *nlist );
int ns_locate_match ( const int *handle, const int *hlist, const int *nlist );
void nsu_debug (int *cnt, int *free, int *hmax);
int UTIL_get_free_handle_loc();

/*! \addtogroup PublicInterface Fortran-callable Application (Public) Interface
 *
 * \brief Although not absolutely guaranteed, it is the intent that
 *        the functions in the NSERVER library's application layer
 *        will not change in terms of their functionality or interface
 *        specification. Any changes in future versions can be
 *        expected be upward compatible with earlier versions.
 *
 *  The application interface is implemented primarily in
 *  Fortran-callable C. However, those functions which pass Fortran
 *  character variables, or which perform I/O, are implemented with an
 *  intermediate Fortran wrapper function or subroutine.
 *
 *  These wrapper functions fall into two categories:
 *   \li <b>Conversion of Fortran character variables to C \a char * strings</b>
 *       \n For handling Fortran character variables, the wrapper
 *          determines then length of the character variable, and
 *          passes that length and a pointer to the character string
 *          to the underlying C function. For an output character
 *          variable, the length passed is its full length (including
 *          trailing blanks), and for an input character variable,
 *          the length passed is its non-blank length.
 *   \li <b>Proper handling of I/O</b>
 *       \n Since some Fortran compilers (e.g., gfortran) do not use the same
 *          buffer for stdout as C, synchronization of output from a Fortran
 *          application calling a NSERVER function writing to stdout can 
 *          produce unexpected results if NSERVER's output is performed from
 *          C rather than from Fortran. Consequently, such functions must
 *          contain a layer of Fortran to avoid this problem.

 *  All of the application-level functions which are implemented in C
 *  return the C type \a int, and further, all arguments of these
 *  functions are pointers to \a int (\a int *). A Fortran application
 *  calling these functions should pass Fortran \a INTEGER variables
 *  as arguments. These functions return, and should be declared as,
 *  type \a INTEGER.
 *
 *  \{
 */

/*! \brief ns_freename frees the string referenced by handle and returns a
 *         status code.
 *
 *  \param[in,out] handle  supplied string handle
 *
 *  \return \li  0, successful completion
 *          \li  -1, illegal handle value
 *          \li  -2, handle is not active
 *
 *  \note Upon successful completion, the handle is set to zero.
 *  \note If the supplied value of handle is zero, nothing is done
 *        and zero is returned.
 *  \note ns_freename is a member of NSERVER's application (public) interface
 */
int ns_freename ( int *handle )
{
  int loc;

  if ( *handle == 0 ) return 0;
  loc = (*handle) - 1;
  if ( loc < 0  ||  loc >= NSU_count )  return -1;
  if ( NSU_strings[loc] == NULL ) return -2;

  free(NSU_strings[loc]);
  NSU_strings[loc] = NULL;
  NSU_freelist[NSU_nfree++] = loc;
  *handle = 0;
  return 0;
}

/*! \brief ns_locate attempts to find a string (referenced by a handle) among 
 *         the strings referenced by handles in the supplied handle list.
 *
 *  If a matching string is found, the index of the associated handle in the
 *  list is returned. (Note that this index starts at 1).  If the string is
 *  not found, zero is returned. If one of the supplied handles is not valid,
 *  a negative integer is returned.
 *
 *  \param[in] handle  supplied string handle
 *  \param[in] hlist   supplied list of string handle
 *  \param[in] nlist   length of handle list
 *
 *  \return \li  0,   no match found
 *          \li  > 0, list index of handle matching string
 *          \li  < 0, one of the supplied handles is invalid
 *  \note ns_locate is a member of NSERVER's application (public) interface
 */
int ns_locate ( const int *handle, const int *hlist, const int *nlist )
{
  int i, loc, locl, n;

  loc = (*handle) - 1;
  n = *nlist;

  if ( loc < 0 || loc >= NSU_count || 
       NSU_strings[loc] == NULL ) return -(n + 1);
  for ( i=0; i<n; i++ )   {
    locl = hlist[i] - 1;
    if ( loc == locl )  break;
    if ( locl < 0 || locl >= NSU_count || 
         NSU_strings[locl] == NULL ) return -(i + 1);
    if ( strcmp(NSU_strings[loc],NSU_strings[locl]) == 0 )  break;
  }
  if ( i < n )  return i+1;

  return 0;  /* not found */
}

/*! \brief ns_locate_match attempts to uniquely match a string (referenced by
 *         a handle) to the leading characters of the strings referenced by
 *         handles in the supplied handle list.
 *
 *  If a unique match is not found, but one of the strings in the list exactly
 *  matches the string, it is considered the unique match. If a unique match
 *  is found, the index of the associated handle in the list is returned.
 *  (Note that this index starts at 1). If the string is not matched, zero is
 *  returned. If the match is not unique, -1 is returned. If one of the
 *  supplied handles is not valid, -2 is returned.
 *
 *  \param[in] handle  supplied string handle
 *  \param[in] hlist   supplied list of string handle
 *  \param[in] nlist   length of handle list
 *
 *  \return \li    0, no match found
 *          \li  > 0, list index of handle matching string
 *          \li   -1, the match is not unique (multiple matches)
 *          \li   -2, one of the supplied handles is invalid
 *  \note ns_locate_match is a member of NSERVER's application (public)
 *        interface
 */
int ns_locate_match ( const int *handle, const int *hlist, const int *nlist )
{
  int loc, tlen;

  loc = (*handle) - 1;
  tlen = strlen(NSU_strings[loc]);

  if ( loc < 0 || loc >= NSU_count || 
       NSU_strings[loc] == NULL ) return -2;

  return nsu_find_match(&tlen, NSU_strings[loc], hlist, nlist);
}

/*!  \} */ /* END of PublicInterface group */


/*! \addtogroup CPublicInterface C/C++ Application (Public) Interface
 *
 * \brief Although not absolutely guaranteed, it is the intent that
 *        the functions in the NSERVER library's application layer
 *        will not change in terms of their functionality or interface
 *        specification. Any changes in future versions can be
 *        expected be upward compatible with earlier versions.
 *
 *  The functions in this group are designed to allow C and C++ code
 *  to use the features of the NSERVER library. In particular, this
 *  provides an easy way to indirectly pass strings, via NSERVER handles,
 *  between Fortran and C/C++ code.
 *
 *  \{
 */

/*! \brief nsc_putname provides a means of storing strings directly from C code.
 *
 *  \param[in]  name   pointer to supplied string
 *
 *  \return \li   >  0, string's handle for later retrieval
 *          \li   <= 0, memory allocation error
 */
int nsc_putname ( const char *name )
{
  char *p;
  int   loc, slen;

  slen = strlen(name);
  if ( (p = (char *) malloc(slen+1)) == NULL )   {
    perror("nsc_putname");
    return -1;
  }
  strcpy(p,name);

  if ( (loc = UTIL_get_free_handle_loc()) < 0) return 0;
  NSU_strings[loc] = p;
  return loc+1;
}

/*! \brief nsc_getname provides access from C code to the string referenced
 *         by the supplied handle.
 *
 *  It returns a pointer the string, or NULL on error.
 *
 *  \param[in]  handle   supplied string handle
 *
 *  \return \li  Pointer to the string associated with provided handle
 *          \li  NULL, on error
 */
char *nsc_getname(int handle)
{
  int loc;

  loc = handle - 1;
  if ( loc < 0 || loc >= NSU_count || NSU_strings[loc] == NULL ) return NULL;

  return NSU_strings[loc];
}

/*!  \} */ /* END of CPublicInterface group */


/*! \addtogroup PrivateInterface Documentation of Non-Public, Utility Functions
 * \brief The utility functions used in the NSERVER's Non-Public layer
 *        are used by the Fortran wrapper functions and their use by
 *        application code should be avoided, since neither their
 *        functionality, calling interface, or even their continued
 *        existence in future versions of the library are guaranteed.
 *  \{
 */

/*! \brief nsu_putname stores the supplied string and returns a handle to
 *         that string
 *
 *  \warning  This is a utility function used by NS_putname and is not intended
 *            to be a public interface!
 *
 *  \param[in] nblen  pointer to non-blank length of supplied string "name"
 *  \param[in] name   supplied string
 *
 *  \return \li   > 0, string's handle for later retrieval
 *          \li     0, memory allocation error
 */
int nsu_putname ( const int *nblen, const char *name )
{
  char *p;
  int   loc, slen;

  slen = *nblen;
  if ( (p = (char *) malloc(slen+1)) == NULL )   {
    perror("nsu_putname");
    return -1;
  }
  strncpy(p,name,slen);
  p[slen] = '\0';

  if ( (loc = UTIL_get_free_handle_loc()) < 0) return 0;
  NSU_strings[loc] = p;
  return loc+1;
}

/*! \brief nsu_getname accesses the string referenced by handle and returns
 *         the length of the string or a status code on error
 *
 *  \warning  This is a utility function used by NS_getname and is not intended
 *            to be a public interface!
 *
 *  \param[in]  handle   supplied string handle
 *  \param[in]  maxlen   maximum length of returned string
 *  \param[out] name     returned string
 *
 *  \return \li  >= 0, length of string returned
 *          \li    -1, illegal handle value
 *          \li    -2, handle is not active
 */
int nsu_getname ( const int *handle, const int *maxlen, char *name )
{
  int i, loc, ml, slen, clen;

  loc = (*handle) - 1;
  ml = *maxlen;

  if ( loc < 0  ||  loc >= NSU_count )  return -1;
  if ( NSU_strings[loc] == NULL ) return -2;

  slen = strlen(NSU_strings[loc]);
  clen = slen < ml ? slen : ml;
  strncpy(name,NSU_strings[loc],clen);
  for(i=clen; i<ml; ++i) name[i] = ' ';

  return slen;
}

/*! \brief nsu_find attempts to find a supplied string among the strings
 *         referenced by handles in the supplied handle list.
 *
 *  If a matching string is found, the index of the associated handle in
 *  the list is returned. (Note that this index starts at 1).  If the
 *  string is not found, zero is returned.
 *
 *  \warning  This is a utility function used by NS_find and is not intended
 *            to be a public interface!
 *
 *  \param[in] nblen  pointer to non-blank length of supplied string "name"
 *  \param[in] name   supplied string
 *  \param[in] hlist  supplied list of string handles
 *  \param[in] nlist  length of handle list
 *
 *  \return \li  > 0, list index of handle matching string
 *          \li    0, no match found
 *          \li   -1, an invalid handle supplied
 */
int nsu_find ( const int *nblen, const char *name, const int *hlist,
               const int *nlist )
{
  int i, n, loc, slen;

  n = *nlist;
  slen = *nblen;

  for ( i=0; i<n; i++ )   {
    loc = hlist[i] - 1;
    if ( loc < 0 || loc >= NSU_count || NSU_strings[loc] == NULL )  return -1;
    if ( strlen(NSU_strings[loc]) == slen &&
         strncmp(name,NSU_strings[loc],slen) == 0 )  break;
  }
  if ( i < n )  return i+1;

  return 0;
}

/*! \brief nsu_findorput attempts to find a supplied string among the strings 
 *         referenced by handles in the supplied handle list.
 *
 *  If a matching string is found, the index of the associated handle in the
 *  list is returned. (Note that this index starts at 1). If the string is
 *  not found, the string is stored and the negative of the assigned handle
 *  is returned.
 *
 *  \warning  This is a utility function used by NS_findorput and is not
 *            intended to be a public interface!
 *
 *  \param[in] nblen  pointer to non-blank length of supplied string "name"
 *  \param[in] name   supplied string
 *  \param[in] hlist  supplied list of string handle
 *  \param[in] nlist  length of handle list
 *
 *  \return \li  > nlist, no match found and error encountered while 
 *                        attempting to add the string to the list or
 *                        an invalid handle was supplied
 *          \li      > 0, list index of handle matching string
 *          \li      < 0, negative of handle assigned to the string if
 *                        no match is found
 */
int nsu_findorput ( const int *nblen, const char *name, const int *hlist,
                    const int *nlist )
{
  int i, n, loc, slen;

  n = *nlist;
  slen = *nblen;

  for ( i=0; i<n; i++ )   {
    loc = hlist[i] - 1;
    if ( loc < 0 || loc >= NSU_count || NSU_strings[loc] == NULL )  return n+2;
    if ( strlen(NSU_strings[loc]) == slen &&
         strncmp(name,NSU_strings[loc],slen) == 0 )  break;
  }
  if ( i < n )  return i+1;

  i = nsu_putname(nblen,name);
  if ( i == 0 ) return n+1;

  return -i;
}

/*! \brief nsu_find_match attempts to uniquely match a supplied string to the
 *         leading characters of the strings referenced by handles in the
 *         supplied handle list.
 *
 *  If a unique match is not found, but one of the strings in the list exactly
 *  matches the string, it is considered the unique match. If a unique match
 *  is found, the index of the associated handle in the list is returned.
 *  (Note that this index starts at 1). If the string is not matched, zero
 *  is returned. If the match is not unique, -1 is returned. If one of the
 *  supplied handles is not valid, -2 is returned.
 *
 *  \warning  This is a utility function used by NS_find_match and is not
 *            intended to be a public interface!
 *
 *  \param[in] nblen  pointer to non-blank length of supplied string "name"
 *  \param[in] name   supplied string
 *  \param[in] hlist  supplied list of string handle
 *  \param[in] nlist  length of handle list
 *
 *  \return \li    0, no match found
 *          \li  > 0, list index of handle matching string
 *          \li   -1, the match is not unique (multiple matches)
 *          \li   -2, one of the supplied handles is invalid
 */
int nsu_find_match ( const int *nblen, const char *name, const int *hlist,
                     const int *nlist )
{
  int exact, i, last, mcnt, n, loc, tlen;

  n = *nlist;
  tlen = *nblen;

  mcnt = 0;
  last = 0;
  exact = 0;
  for ( i=0; i<n; i++ )   {
    loc = hlist[i] - 1;
    if ( loc < 0 || loc >= NSU_count || 
         NSU_strings[loc] == NULL ) return -2;
    if ( strncmp(name,NSU_strings[loc],tlen) == 0 ) {
      ++mcnt;
      last = i + 1;
      if ( strlen(NSU_strings[loc]) == tlen ) {
        if ( exact == 0 ) exact = last;
        else return -1;  /* multiple exact matches found */
      }
    }
  }
  if ( mcnt == 0 ) return 0;       /* not found */
  if ( mcnt == 1 ) return last;    /* unique match found */
  if ( exact > 0 ) return exact;   /* exact match found */

  return -1;  /* multiple non-exact matches found */
}

/*! \brief nsu_debug obtains statistics regarding the server's allocated
 *          strings.
 *
 *  \warning  This is a utility function used by NS_debug and is not intended
 *            to be a public interface!
 *
 *  \param[out] cnt   number of allocated strings
 *  \param[out] free  number of allocated strings that are currently unused
 *  \param[out] hmax  largest handle index currently in use
 */
void nsu_debug (int *cnt, int *free, int *hmax)
{
  int i;

  *cnt = NSU_count;
  *free = NSU_nfree;
  *hmax = -1;

  for(i=0;i<NSU_count;i++)  if (NSU_strings[i]) *hmax = i;
  ++(*hmax);
}

/* local utility functions */
/*! \brief UTIL_get_free_handle_loc retrieves the an unused location in the
 *         NSERVER string list. Note that the returned array index is on less
 *         than the handle used to access the string.
 *
 *  This function performs the following tasks in its operation:
 *   \li Allocates or reallocates the string array if it is fully used.
 *   \li Removes the returned index from the stack of free locations
 *
 *  \warning  This is a utility function used by nsu_putname and nsc_putname.
 *            It is not intended to be a public interface!
 *
 *  \return \li   <  0, memory allocation error
 *          \li   >= 0, the index of the string in the NSU_strings array (one
 *                      less than the string handle
 */
int UTIL_get_free_handle_loc()
{
  int   i, j;

  if ( NSU_nfree == 0 )  {
    NSU_count += INCR_SIZE;
    if ( (NSU_strings = 
          (char **) realloc(NSU_strings,NSU_count*sizeof(char *))) == NULL ||
         (NSU_freelist = 
          (int *) realloc(NSU_freelist,NSU_count*sizeof(int))) == NULL )  {
      perror("UTIL_get_free_handle_loc");
      return -1;
    }
    for (i=0,j=NSU_count; i<INCR_SIZE; i++)  {
      NSU_freelist[i] = --j;
      NSU_strings[j]  = NULL;
    }
    NSU_nfree = INCR_SIZE;
  }
  return NSU_freelist[--NSU_nfree];
}

/*!  \} */ /* END of PrivateInterface group */
