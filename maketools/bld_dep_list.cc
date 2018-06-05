//  C_Groups
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

#include "l_group.h"
#include "ext_match_list.h"
#include "file_compare.h"
#include <sys/types.h>
#include <sys/stat.h>

#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#ifdef WIN32sys
# define STAT_STRUCT _stat
# define STAT_FUNCT _stat
# define S_ISREG(mode)	 ( (mode) & _S_IFREG )
# include "winrtl.h"
static const char file_sep = '\\';
#else
# define STAT_STRUCT stat
# define STAT_FUNCT stat
# include <unistd.h>
static const char file_sep = '/';
#endif

#ifdef WIN32sys
static const char path_delim = '\\';
#else
static const char path_delim = '/';
#endif

// using std::cout;
// using std::endl;

using std::string;
using std::list;
using std::strlen;
using std::strncmp;

extern "C" char *copy(const char *str);
extern "C" char	*base_name(const char *file);

#include "mkdep.h"

#include "bld_dep_list_usage.h"

int main(int argc, char *argv[])
{
  string tmpstr;
  int c;
  L_Group lgrp;
  int error = 0;
  Ext_Match_List src_matcher;
  string src_exts = "";
  string lgrp_flgs = "";
  string LIBname = "";
  bool eflg = false;
  bool partial = false;
  bool winflg = false;
  string mode = "w";
  string blank = " ";
  string undrscr = "_";
  string md_string = "# Include Dependencies";
  list<string> MDargs;
  string err_file = "";
  string prefix = "";
  string psuf = "";
  bool prefix_override = false;
  string extradep = "";
  bool extradep_override = false;
  bool list_src_dep = false;
  string vPath = "";

  char *cmd = argv[0] + strlen(argv[0]);
  for(--cmd; cmd > argv[0]; --cmd)
    if ( *cmd == path_delim ) { ++cmd; break; }

#ifdef WIN32sys
  const char optList[] = "hdm:n:N:k:s:v:D:I:V:L:lEWap:X:Pe:S:";
#else
  const char optList[] = "hdm:n:N:k:s:v:x:D:I:V:L:lEWap:X:Pe:S:";
#endif
  while ( (c=getopt(argc, argv, optList)) != -1 ) {
    switch (c) {
    case 'h':
      bld_dep_list_usage(std::cout,0);
      std::exit(0);
      break;
    case 'd':
      list_src_dep = true;
      break;
    case 'm':
      lgrp.AddMatch(optarg);
      lgrp_flgs += string(" -m") + string(optarg);
      break;
    case 'n': case 'N':
      if (c == 'N') lgrp.AddNomatch(optarg,true);
      else lgrp.AddNomatch(optarg);
      lgrp_flgs += string(" -") + string(1,c) + string(optarg);
      break;
    case 'k':
      lgrp.SetKeyString(optarg);
      break;
    case 's':
      src_matcher.AddExtension(optarg);
      src_exts += blank + string(optarg);
      ;
      break;
    case 'v':
      {
        char *vlist  = new char[strlen(optarg)+1];
        std::strcpy(vlist,optarg);
        char *tok = strtok(vlist," :");
        while (tok) {
          //std::cout << "VPATH token: " << tok << std::endl;
          char *pc = tok + (strlen(tok)-1);
          while ( *pc == file_sep ) *pc-- = '\0';
          src_matcher.AddSearchDir(tok);
          if(!vPath.empty()) vPath += ':';
          vPath += tok;
          tok = strtok(NULL," :");
        }
        delete [] vlist;
      }
      break;
    case 'x':
      src_matcher.AddExcludePattern(optarg);
      break;
    case 'D':
      MDargs.push_back(string("-D") + optarg);
      break;
    case 'I':
      MDargs.push_back(string("-I") + optarg);
      break;
    case 'V':
      MDargs.push_back(string("-V") + optarg);
      break;
    case 'L':
      if ( !LIBname.empty() || winflg || eflg ) error=1;
      else LIBname = optarg;
      break;
    case 'l':
      if ( !LIBname.empty() || winflg || eflg ) error=1;
      else LIBname = "LIBRARY";
      break;
    case 'E':
      if ( !LIBname.empty() ) error=1;
      else {
        eflg = true;
      }
      break;
    case 'p':
      {
        prefix_override = true;
        tmpstr = optarg;
        string::size_type idx = tmpstr.find(':');
        if ( idx == string::npos ) prefix = tmpstr;
        else {
          prefix = tmpstr.substr(0,idx);
          psuf = tmpstr.substr(idx+1);
        }
      }
      break;
    case 'X':
      extradep_override = true;
      extradep = optarg;
      break;
    case 'W':
      if ( !LIBname.empty() ) error=1;
      else {
        winflg = true;
      }
      break;
    case 'a':
      mode = "a";
      break;
    case 'P':
      partial = true;
      break;
    case 'e':
      err_file = optarg;
      break;
    case 'S':
      md_string = optarg;
      break;
 
    case '?':
      error = 1;
      break;

    default:
      error = 1;
      fprintf (stderr,"?? getopt returned character code 0%o ??\n", c);
    }    
  }

  // cout <<  "lgrp_flgs: " << lgrp_flgs << endl;
  // cout <<  "src_exts: " << src_exts << endl;
  // cout <<  "LIBname: " << LIBname << endl;
  // cout <<  "eflg: " << eflg << endl;
  // cout <<  "winflg: " << winflg << endl;
  // cout <<  "mode: " << mode << endl;
  // cout <<  "Error file: " << err_file << endl;

  argv += optind;
  argc -= optind;

  // for(int i=0; i<argc; ++i) printf("ARG%d: %s\n",i,argv[i]);

  if ( error || argc > 2 || src_exts.empty() ) {
    bld_dep_list_usage(std::cerr,1);
    std::exit(1);
  }

  if ( !err_file.empty() ) {
#ifdef WIN32sys
    if ( err_file == "0" ) err_file = "nul";
#else
    if ( err_file == "0" ) err_file = "/dev/null";
#endif
    if ( freopen(err_file.c_str(), mode.c_str(), stderr) == NULL )
      fprintf(stderr,"Error redirecting stderr\n");
  }

  string product = "";
  string ofilename = "dep_list";
  if ( argc ) {
    tmpstr = argv[0];
    if ( tmpstr != "-" ) {
      product = tmpstr;
      ofilename = product + undrscr + ofilename;
    }
  }
  string platform = "";
  if ( argc == 2 ) {
    platform = argv[1];
    if ( isalnum(argv[1][0]) ) {
      ofilename += undrscr;
    }
    ofilename += platform;
  }
  // cout <<  "product: " << product << endl;
  // cout <<  "platform: " << platform << endl;
  // cout <<  "ofilename: " << ofilename << endl;
  //src_matcher.printit();
  //std::cout << "vPath: " << vPath << std::endl;


  string bldfile = ofilename;
  bool need_cmp = false;
  bool is_file = false;
  struct STAT_STRUCT buf;
  if ( ! STAT_FUNCT(ofilename.c_str(), &buf) ) {
    is_file = need_cmp = S_ISREG(buf.st_mode);
    if ( is_file ) bldfile += ".try";
    // cout <<  "Found file: " << is_file << " " << bldfile << endl;
  }
  if ( is_file && mode == "a" ) {
    mode = "r+";  // read file looking for md_string
    bool is_try = false;
    if ( ! STAT_FUNCT(bldfile.c_str(), &buf) ) 
      is_try = S_ISREG(buf.st_mode);
    if ( ! is_try ) {
      bldfile = ofilename;
      need_cmp = false;
    }
    // cout <<  "Found try: " << is_try << " " << bldfile << endl;
  }
  need_cmp = need_cmp && !partial;
  // cout <<  "bldfile: " << need_cmp << " " << bldfile << endl;
    
  FILE *wfile = fopen(bldfile.c_str(), mode.c_str());

  bool found = false;
  if ( mode == "w" ) {
    if ( !product.empty() )
      fprintf(wfile,"# Product:            %s\n",product.c_str());
    if ( !platform.empty() )
      fprintf(wfile,"# Platform:           %s\n",platform.c_str());
  }
  else {
    mode = "a"; // Reset to "a" for later tests
    char buf[1024];
    int len = md_string.length();
    const char *mds = md_string.c_str();
    while ( !found && fgets(buf, 1024, wfile) ) {
      if (*buf == '#' && strncmp(mds, buf, len) == 0) found = true;
    }
    fseek(wfile,0,SEEK_END);
  }
  fprintf(wfile,"\n# Source extensions: %s\n",src_exts.c_str());

  list<string> slist;
  int nsrc = src_matcher.BldFileList(slist);
  
  list<string>::iterator iter;

  if ( !lgrp_flgs.empty() ) {
    fprintf(wfile,"# lgrp flags:        %s\n",lgrp_flgs.c_str());
    for(iter=slist.begin(); iter != slist.end(); ) {
      if ( ! lgrp.CheckFile(*iter) ) {
        iter = slist.erase(iter);
        --nsrc;
      }
      else ++iter;
    }
  }
  if ( !found ) fprintf(wfile,"\n%s\n",md_string.c_str());

  if ( eflg && winflg ) {
    fprintf(wfile,"\n");
    for(iter=slist.begin(); iter != slist.end(); iter++) {
      char *base = base_name((*iter).c_str());
      fprintf(wfile, "$(OUTDIR)\\%s.exe : $(INTDIR)\\%s.obj\n", base, base);
      std::free(base);
    }
    fclose(wfile);
  }
  else {
    MDargs.push_back(string("-f") + bldfile);
    MDargs.push_back(string("-s") + md_string);
    MDargs.push_back("-Y");
    MDargs.push_back("-nobackup");
    if ( mode == "a" ) MDargs.push_back("-a");

    string suffix = ".o";
    if ( winflg ) {
      if ( !prefix_override ) prefix = "$(INTDIR)\\";
      suffix += "bj";
      // if ( !extradep_override ) extradep = "$(INTDIR)";
    }
    else if ( eflg ) {
      suffix = "";
    }
    else if ( !LIBname.empty() ) {
      if ( !prefix_override ) prefix = "$(" + LIBname + ")(";
      suffix += ")";
    }
    if (list_src_dep || winflg) MDargs.push_back("-S");
    if ( !psuf.empty() ) suffix = psuf;
    suffix += blank;
    if ( !prefix.empty() ) MDargs.push_back("-p" + prefix);
    if ( !suffix.empty() ) MDargs.push_back("-o" + suffix);
    if ( !extradep.empty() ) MDargs.push_back("-X" + extradep);
    if ( !vPath.empty() ) {
      MDargs.push_back("-vpath");
      MDargs.push_back(vPath);
    }
    fclose(wfile);

    int nargs = nsrc + MDargs.size() + 2;

    char **hmkdargs = new char*[nargs];

    int arg = 0;
    hmkdargs[arg++] = copy(cmd);
 
    for(iter=MDargs.begin(); iter != MDargs.end(); iter++) {
      hmkdargs[arg++] = copy((*iter).c_str());
    }
    for(iter=slist.begin(); iter != slist.end(); iter++) {
      hmkdargs[arg++] = copy((*iter).c_str());
    }
    hmkdargs[arg] = 0;

    // printf("Nargs check: %d %d\n",arg,nargs);
    //for(int i=0;i<=arg;++i) printf("%s ",hmkdargs[i]);
    //printf("\n");
  
    /* int stat = */ mkdep(arg,hmkdargs);
    // printf("mkdep status: %d\n",stat);
  }

  if ( need_cmp ) {
    File_Compare *cmp = new File_Compare(ofilename,bldfile);
    bool the_same = cmp->TheSame();
    delete cmp;
    //if ( the_same ) remove(ofilename.c_str());
    //else {
    if ( ! the_same ) {
      string bakfile = ofilename + ".bak";
      rename(ofilename.c_str(),bakfile.c_str());
      //rename(bldfile.c_str(),ofilename.c_str());
    }
    rename(bldfile.c_str(),ofilename.c_str());
  }

  return 0;
}
