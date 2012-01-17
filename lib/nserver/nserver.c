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
# define nsu_putname     HU_F77_FUNC_( nsu_putname    ,  NSU_PUTNAME     )
# define ns_freename     HU_F77_FUNC_( ns_freename    ,  NS_FREENAME     )
# define nsu_getname     HU_F77_FUNC_( nsu_getname    ,  NSU_GETNAME     )
# define nsu_find        HU_F77_FUNC_( nsu_find       ,  NSU_FIND        )
# define nsu_find_match  HU_F77_FUNC_( nsu_find_match ,  NSU_FIND_MATCH  )
# define nsu_findorput   HU_F77_FUNC_( nsu_findorput  ,  NSU_FINDORPUT   )
# define ns_locate       HU_F77_FUNC_( ns_locate      ,  NS_LOCATE       )
# define ns_locate_match HU_F77_FUNC_( ns_locate_match,  NS_LOCATE_MATCH )
# define nsu_debug       HU_F77_FUNC_( nsu_debug      ,  NSU_DEBUG       )
#endif

#ifndef DEBUG
# define INCR_SIZE  200
#endif

/* file scope variables */
static int    NSU_count    = 0;
static int    NSU_nfree    = 0;
static int   *NSU_freelist = NULL;
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

/*  nsu_putname stores the supplied string and returns a handle to that string

      nblen  --  points to integer non-blank length of supplied string "name"
                 (read only)
      name   --  supplied string (read only)

      Return value:   > 0 -- string's handle for later retrieval
                      0   -- memory allocation error
*/
int nsu_putname ( const int *nblen, const char *name )
{
  char *p;
  int   i, j, loc, slen;

  if ( NSU_nfree == 0 )  {
    NSU_count += INCR_SIZE;
    if ( (NSU_strings = 
          (char **) realloc(NSU_strings,NSU_count*sizeof(char *))) == NULL ||
         (NSU_freelist = 
          (int *) realloc(NSU_freelist,NSU_count*sizeof(int))) == NULL )  {
      perror("nsu_putname");
      return 0;
    }
    for (i=0,j=NSU_count; i<INCR_SIZE; i++)  {
      NSU_freelist[i] = --j;
      NSU_strings[j]  = NULL;
    }
    NSU_nfree = INCR_SIZE;
  }

  slen = *nblen;
  if ( (p = (char *) malloc(slen+1)) == NULL )   {
    perror("nsu_putname");
    return -1;
  }
  strncpy(p,name,slen);
  p[slen] = '\0';

  loc = NSU_freelist[--NSU_nfree];
  NSU_strings[loc] = p;
  return loc+1;
}

/*  ns_freename frees the string referenced by handle and returns a status code
      Note that upon successful completion, the handle is set to zero.
      Also note that if the supplied value of handle is zero, nothing is done
      and zero is returned.

      handle --  supplied string handle (read/write)


      Return value:    0   -- successful completion
                      -1   -- illegal handle value
                      -2   -- handle is not active
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

/*  nsu_getname accesses the string referenced by handle and returns the length
      of the string or a status code on error

      handle --  supplied string handle (read only)
      maxlen --  maximum length of returned string (read only)
      name   --  returned string (write only)

      Return value:  >=  0  -- length of string returned
                        -1  -- illegal handle value
                        -2  -- handle is not active
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

/*  nsu_find attempts to find a supplied string among the strings referenced 
      by handles in the supplied handle list.  If a matching string is 
      found, the index of the associated handle in the list is returned.
      (Note that this index starts at 1).  If the string is not found, zero  
      is returned.

      nblen  --  points to integer non-blank length of supplied string "name"
                 (read only)
      name   --  supplied string (read only)
      hlist  --  supplied list of string handle (read only)
      nlist  --  length of handle list (read only)

      Return value:  > 0 -- list index of handle matching string
                     0   -- no match found
                     -1  -- an invalid handle supplied
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

/*  nsu_findorput attempts to find a supplied string among the strings 
      referenced by handles in the supplied handle list.  If a matching string
      is found, the index of the associated handle in the list is returned.
      (Note that this index starts at 1).  If the string is not found, the 
      string is stored and the negative of the assigned handle is returned.

      nblen  --  points to integer non-blank length of supplied string "name"
                 (read only)
      name   --  supplied string (read only)
      hlist  --  supplied list of string handle (read only)
      nlist  --  length of handle list (read only)

      Return value:  > nlist -- no match found and error encountered while 
                                attempting to add the string to the list or
                                an invalid handle was supplied
                     > 0     -- list index of handle matching string
                     < 0     -- negative of handle assigned to the string if
                                no match is found
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

/*  ns_locate attempts to find a string (referenced by a handle) among 
      the strings referenced by handles in the supplied handle list.  If a 
      matching string is found, the index of the associated handle in the list
      is returned. (Note that this index starts at 1).  If the string is not 
      found, zero is returned. If one of the supplied handles is not valid,a
      negative integer is returned.

      handle --  supplied string handle (read only)
      hlist  --  supplied list of string handle (read only)
      nlist  --  length of handle list (read only)

      Return value:  0     -- no match found
                     > 0   -- list index of handle matching string
                     < 0   -- one of the supplied handles is invalid
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

/*  nsu_find_match attempts to uniquely match a supplied string to the
      leading characters of the strings referenced by handles in the
      supplied handle list. If a unique match is not found, but one
      of the strings in the list exactly matches the string, it is
      considered the unique match. If a unique match is found, the index
      of the associated handle in the list is returned. (Note that this
      index starts at 1). If the string is not matched, zero is returned.
      If the match is not unique, -1 is returned. If one of the supplied
      handles is not valid, -2 is returned.

      nblen  --  points to integer non-blank length of supplied string "name"
                 (read only)
      name   --  supplied string (read only)
      hlist  --  supplied list of string handle (read only)
      nlist  --  length of handle list (read only)

      Return value:  0     -- no match found
                     > 0   -- list index of handle matching string
                     -1    -- the match is not unique (multiple matches)
                     -2    -- one of the supplied handles is invalid
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

/*  ns_locate_match attempts to uniquely match a string (referenced by a  
      handle) to the leading characters of the strings referenced by
      handles in the supplied handle list. If a unique match is not
      found, but one of the strings in the list exactly matches
      the string, it is considered the unique match. If a unique match
      is found, the index of the associated handle in the list is
      returned. (Note that this index starts at 1). If the string is
      not matched, zero is returned. If the match is not unique, -1 is
      returned. If one of the supplied handles is not valid, -2 is
      returned.

      handle --  supplied string handle (read only)
      hlist  --  supplied list of string handle (read only)
      nlist  --  length of handle list (read only)

      Return value:  0     -- no match found
                     > 0   -- list index of handle matching string
                     -1    -- the match is not unique (multiple matches)
                     -2    -- one of the supplied handles is invalid
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

/*  nsu_debug is a utility function used by NS_debug to obtain statistics
    regarding the server's allocated strings.

      cnt  --  number of allocated strings (write only)
      free --  number of allocated strings that are currently unused
               (write only)
      hmax --  largest handle index currently in use (write only)
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
