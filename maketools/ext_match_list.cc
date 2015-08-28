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

#include <iostream>
#include <sstream>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32sys
# define STAT_STRUCT _stat
# define STAT_FUNCT _stat
# define S_ISREG(mode)	 ( (mode) & _S_IFREG )
# include "winrtl.h"
static const char file_sep = '\\';
#else
# define STAT_STRUCT stat
# define STAT_FUNCT stat
# include <dirent.h>
# include <unistd.h>
# include <fnmatch.h>
static const char file_sep = '/';
#endif

using std::string;
using std::map;
using std::list;
using std::strlen;
using std::strcmp;
using std::ostringstream;
using std::cout;
using std::endl;

Ext_Match_List::Ext_Match_List()
{
  search_dirs.push_back(".");
}

void Ext_Match_List::AddExtension(const string &ext)
{
  mat_exts.push_back(ext);
}

void Ext_Match_List::AddExcludePattern(const std::string &pat)
{
  exclude_pattern.push_back(pat);
}


void Ext_Match_List::AddSearchDir(const std::string &dir)
{
  search_dirs.push_back(dir);
}

void Ext_Match_List::printit()
{
  list<string>::iterator iter;
  cout << "SearchDirs:";
  for(iter=search_dirs.begin(); iter != search_dirs.end(); iter++) {
    cout << " " << (*iter);
  }
  cout << "\nExtensions:";
  for(iter=mat_exts.begin(); iter != mat_exts.end(); iter++) {
    cout << " " << (*iter);
  }
  cout << "\nExcludePatterns:";
  for(iter=exclude_pattern.begin(); iter != exclude_pattern.end(); iter++) {
    cout << " " << (*iter);
  }
  cout << endl;
}

int Ext_Match_List::BldFileList(list<string> &filelist) {
  map<string,string> filemap;
  int n = BldFileMap(filemap);

  map<string,string>::iterator pos;
  for(pos=filemap.begin(); pos != filemap.end(); pos++) {
      string fulname = pos->first;
      if (pos->second != "") fulname = pos->second + file_sep + pos->first;
    filelist.push_back(fulname);
  }
  return n;
}
int Ext_Match_List::BldFileMap(map<string,string> &filemap)
{
  int n = 0;
  list<string>::iterator d_iter;
  for(d_iter=search_dirs.begin(); d_iter != search_dirs.end(); d_iter++) {
    const char *dname = (*d_iter).c_str();
    string dirsave  = dname;
    if (dirsave == ".") dirsave = "";
#ifdef USE_SCANDIR
    struct dirent **namelist;

    int cnt = scandir(dname, &namelist, 0, alphasort);
    if (cnt < 0)
      perror("scandir");
    else {
      while(cnt--) {
# define DP (namelist[cnt])
# if 0
      } }  // for auto-tabbing
# endif
#else
    DIR *dirp = opendir(dname);
    if ( !dirp ) {
      ostringstream tmp;
      tmp << "couldn't open " << dname;
      perror(tmp.str().c_str());
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
        if (DP->d_name[0] != '.' ) {
          struct STAT_STRUCT buf;
          STAT_FUNCT(DP->d_name, &buf);
          bool type_ok = S_ISREG(buf.st_mode);
          if ( type_ok ) {
            // if no extension constraints supplied match everything else
            if ( mat_exts.empty() ) match = true;
            for(iter=mat_exts.begin(); iter != mat_exts.end(); iter++) {
              if ( fextmatch((*iter).c_str(),DP->d_name) ) {
                match = true;
                break;
              }
            }
          }
        }
        if ( match ) {
          ++n;
          if(filemap.find(DP->d_name) == filemap.end()) {
            filemap[DP->d_name] = dirsave;
          }
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
    }
    closedir(dirp);
#endif
  }
  return n;
}  

int Ext_Match_List::fextmatch(const char *ext, const char *fname)
{
  int ldelt = strlen(fname) - strlen(ext) - 1;
  
  if ( ldelt < 0 ) return 0;
  
  const char *p = fname + ldelt;
  if ( *(p++) != '.' ) return 0;

  if ( strcmp(p,ext) ) return 0;

#ifndef WIN32sys
  list<string>::iterator iter;
  for(iter=exclude_pattern.begin(); iter != exclude_pattern.end(); iter++) {
    
    if (fnmatch((*iter).c_str(),fname,0)==0) return 0;
  }
#endif

  return 1;
}
