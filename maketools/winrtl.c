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

#include "winrtl.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LINESIZ  256

/* External Variables */
char *optarg = 0;
int optind = 1, opterr = 1, optopt = 0;

/* File-local variables */
static char *nextchar = 0;
static int done = 0;

static DIR* dfile = 0;
static char line[LINESIZ];

static char dest[] = " > OPENDIR.TMP";  /* 14 characters */
char* outfile = dest + 3;
#ifdef WIN32sys
static const char path_delim = '\\';
static const char dcmd[] = "dir /b ";   /* 7 characters */
# define EXTRA 22    /*  14 + 7 + 1 */
#else
static const char path_delim = '/';
static const char dcmd[] = "/bin/ls ";  /* 8 characters */
# define EXTRA 23    /*  14 + 8 + 1 */
#endif

struct dirent the_dirent;

static void winrtl_perr(const char *arg0, const char *message)
{
  const char *module = strrchr(arg0,path_delim);
  if ( module == 0 ) module =  arg0;
  else ++module;

  fprintf(stderr,"%s: %s\n",module,message);

}

EXTERN int getopt(int argc, char * const argv[], const char *optstring)
{
  char c;
  char *flag = 0;
  
  if ( done ) return -1;

  if ( optind >= argc ) return (done = -1);

  if ( optind == 0 ) ++optind;

  if ( nextchar == 0 ) {
    if ( argv[optind][0] == '-' ) {
      optopt = argv[optind][1];
      if ( optopt == '\0' ) return (done = -1);
      if ( optopt == '-' && argv[optind][2] == '\0' ) {
        ++optind;
        return  (done = -1);
      }
      nextchar = argv[optind] + 1;
    }
    else {
      return  (done = -1);
    }
  }
  else optopt = *nextchar;

  /* nextchar != 0 */
  flag = strchr(optstring,optopt);
  if ( flag ) {
    if ( flag[1] == ':' ) {  /* comes with an argument */
      /* optarg is in next argument */
      if ( *(++nextchar) == '\0' ) {
        if ( ++optind < argc ) optarg = argv[optind];
        else {
          c = (optstring[0] == ':') ? ':' : '?';
          if ( opterr && c == '?' ) {
            sprintf(line, "option requires an argument -- %c", optopt);
            winrtl_perr(argv[0],line);
          }
          done = 1;
          return c;
        }
      }
      /* optarg is in remainder of this argument */
      else optarg = nextchar;
      ++optind;
      nextchar = 0;
    }
    else {  /* comes without an argument */
      if ( *(++nextchar) == '\0' ) {  /* last character in this argument */
        ++optind;
        nextchar = 0;
      }
    }
  }
  else {
    if ( opterr ) {
      sprintf(line, "invalid option -- %c", optopt);
      winrtl_perr(argv[0],line);
    }
    if ( *(++nextchar) == '\0' ) {  /* last character in this argument */
      ++optind;
      nextchar = 0;
    }
    return '?'; 
  }

  return optopt;
}


EXTERN DIR* opendir(const char *dirname)
{
  size_t len;
  int err;
  char *cmd;

  len = strlen(dirname);
  cmd = (char *) malloc(len + EXTRA);
  strcpy(cmd,dcmd);
  strcat(cmd,dirname);
  strcat(cmd,dest);

  err = system(cmd);
  if ( err ) return 0;
  free(cmd);

  dfile =fopen(outfile,"r");

  the_dirent.d_name = line;

  return dfile;
}
   
EXTERN struct dirent *readdir(DIR *dirp)
{
  size_t len;

  if ( ! dfile || dirp != dfile ) return 0;
  do {
    if ( ! fgets(line, LINESIZ, dirp) ) return 0;
    len = strlen(line) - 1;
    line[len] = '\0';
  } while ( strcmp(line,outfile) == 0 );

  return &the_dirent;
}

EXTERN int closedir(DIR *dirp)
{
  int err;

  if ( ! dfile || dirp != dfile ) return -1;
  dfile = 0;
  err = fclose(dirp);
  if ( err ) return err;
  else return remove(outfile);
}
