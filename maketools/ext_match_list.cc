//  C_Groups hermes
//  $Id$
//  
//  Copyright (2008) Sandia Corporation. Under the terms of
//  Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
//  Government retains certain rights in this software.
//  
//  Hermes is free software: you can redistribute it and/or modify
//  it under the terms of the GNU Lesser General Public License as
//  published by the Free Software Foundation, either version 3 of
//  the License, or (at your option) any later version.
//  
//  Hermes is distributed in the hope that it will be useful, but
//  WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU Lesser General Public License for more details.
//  
//  You should have received a copy of the GNU Lesser General
//  Public License along with Hermes.  If not, see
//  <http://www.gnu.org/licenses/>.
//  

#include "ext_match_list.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
// #include <fnmatch.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32sys
# define STAT_STRUCT _stat
# define STAT_FUNCT _stat
# define S_ISREG(mode)	 ( (mode) & _S_IFREG )
# include "winrtl.h"
#else
# define STAT_STRUCT stat
# define STAT_FUNCT stat
# include <dirent.h>
# include <unistd.h>
#endif

using std::string;
using std::list;
using std::strlen;
using std::strcmp;

void Ext_Match_List::AddExtension(const string &ext)
{
  mat_exts.push_back(ext);
}

int Ext_Match_List::BldFileList(list<string> &filelist)
{
  int n = 0;
#ifdef USE_SCANDIR
  struct dirent **namelist;

  int cnt = scandir(".", &namelist, 0, alphasort);
  if (cnt < 0)
    perror("scandir");
  else {
    while(cnt--) {
# define DP (namelist[cnt])
# if 0
    } }  // for auto-tabbing
# endif
#else
  DIR *dirp = opendir(".");
  if ( !dirp ) {
    perror("couldn't open '.'");
    return -1;
  }
  else {
    struct dirent *dp = readdir(dirp);
    while ( dp ) {
# define DP dp
#endif
      list<string>::iterator iter;
      bool match = false;
      // don't match anything but regular files or symbolic links
      // don't match files whose names start with '.'
      struct STAT_STRUCT buf;
      STAT_FUNCT(DP->d_name, &buf);
      bool type_ok = S_ISREG(buf.st_mode);
      if ( type_ok && DP->d_name[0] != '.' ) {
        // if no extension constraints supplied match everything else
        if ( mat_exts.empty() ) match = true;
        for(iter=mat_exts.begin(); iter != mat_exts.end(); iter++) {
          if ( fextmatch((*iter).c_str(),DP->d_name) ) {
            match = true;
            break;
          }
        }
      }
      if ( match ) {
        filelist.push_back(DP->d_name);
        ++n;
      }
#undef DP
#ifdef USE_SCANDIR
      free(namelist[cnt]);
    }
    free(namelist);
#else
# if 0
    {  // for auto-tabbing
# endif
      dp = readdir(dirp);
    }
#endif
  }
  closedir(dirp);
  return n;
}  

int Ext_Match_List::fextmatch(const char *ext, const char *string)
{
  int ldelt = strlen(string) - strlen(ext) - 1;
  
  if ( ldelt < 0 ) return 0;
  
  const char *p = string + ldelt;
  if ( *(p++) != '.' ) return 0;

  if ( strcmp(p,ext) ) return 0;

  return 1;
}
