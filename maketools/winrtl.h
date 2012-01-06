/*  C_Groups hermes winrtl
 *  $Id$
 *  
 *  Copyright (2008) Sandia Corporation. Under the terms of
 *  Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
 *  Government retains certain rights in this software.
 *  
 *  Hermes is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as
 *  published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *  
 *  Hermes is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *  
 *  You should have received a copy of the GNU Lesser General
 *  Public License along with Hermes.  If not, see
 *  <http://www.gnu.org/licenses/>.
 *  
 */

#ifndef winrtl_h
#define winrtl_h 1

#ifdef __cplusplus
# include <cstdio>
# define EXTERN extern "C"
#else
# include <stdio.h>
# define EXTERN extern
#endif

typedef FILE DIR;

#ifndef WIN32sys
# define optarg Woptarg
# define optind Woptind
# define opterr Wopterr
# define optopt Woptopt

# define getopt Wgetopt
# define opendir Wopendir
# define closedir Wclosedir
# define readdir Wreaddir
#endif

struct dirent
{
  char *d_name;
};

EXTERN char *optarg;
EXTERN int optind, opterr, optopt;

EXTERN int getopt(int argc, char * const argv[], const char *optstring);
EXTERN DIR* opendir(const char *dirname);
EXTERN int closedir(DIR *dirp);
EXTERN struct dirent *readdir(DIR *dirp);

#endif
