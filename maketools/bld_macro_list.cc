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
#include "line_dumper.h"
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

// using std::cout;
// using std::endl;
using std::strlen;
using std::string;
using std::list;
using std::map;

#include "bld_macro_list_usage.h"

int main(int argc, char *argv[])
{
  string tmpstr;
  int c;
  L_Group lgrp;
  int error = 0;
  Ext_Match_List inc_matcher, src_matcher;
  string src_exts = "";
  string inc_exts = "";
  string lgrp_flgs = "";
  string SRCname = "";
  string INCname = "INCS";
  string OBJname = "OBJS";
  string LIBname = "";
  string DELname = "";
  bool eflg = false;
  bool partial = false;
  bool winflg = false;
  string mode = "w";
  string blank = " ";
  string undrscr = "_";
  string prefix = "";
  bool prefix_override = false;

  char *cmd = argv[0];

#ifdef WIN32sys
  const char optList[] = "hm:n:N:k:s:i:S:I:O:L:v:lEWap:PD:d";
#else
  const char optList[] = "hm:n:N:k:s:i:S:I:O:L:v:x:lEWap:PD:d";
#endif
  while ( (c=getopt(argc, argv, optList)) != -1 ) {
    switch (c) {
    case 'h':
      bld_macro_list_usage(std::cout,0);
      std::exit(0);
      break;
    case 'm':
      lgrp.AddMatch(optarg);
      lgrp_flgs += string(" -m") + string(optarg);
      break;
    case 'n': case 'N':
      if (c == 'N') lgrp.AddNomatch(optarg,true);
      else lgrp.AddNomatch(optarg);
      lgrp_flgs += string(" -n") + string(optarg);
      break;
    case 'k':
      lgrp.SetKeyString(optarg);
      break;
    case 's':
      src_matcher.AddExtension(optarg);
      src_exts += blank + string(optarg);
      ;
      break;
    case 'i':
      inc_matcher.AddExtension(optarg);
      inc_exts += blank + string(optarg);
      break;
    case 'S':
      SRCname = optarg;
      break;
    case 'I':
      INCname = optarg;
      break;
    case 'O':
      OBJname = optarg;
      break;
    case 'L':
      if ( !LIBname.empty() || winflg || eflg ) error=1;
      else LIBname = optarg;
      break;
    case 'x':
      src_matcher.AddExcludePattern(optarg);
      inc_matcher.AddExcludePattern(optarg);
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
          //std::cout << "VPATH token: " << tok << std::endl;
          src_matcher.AddSearchDir(tok);
          inc_matcher.AddSearchDir(tok);
          tok = strtok(NULL," :");
        }
        delete [] vlist;
      }
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
      prefix_override = true;
      prefix = optarg;
      break;
    case 'W':
      if ( !LIBname.empty() ) error=1;
      else {
        winflg = true;
      }
      break;
    case 'D':
      if ( !DELname.empty() ) error=1;
      else DELname = optarg;
      break;
    case 'd':
      if ( !DELname.empty() ) error=1;
      else DELname = "@";
      break;
    case 'a':
      mode = "a";
      break;
    case 'P':
      partial = true;
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
  // cout <<  "inc_exts: " << inc_exts << endl;
  // cout <<  "SRCname: " << SRCname << endl;
  // cout <<  "INCname: " << INCname << endl;
  // cout <<  "OBJname: " << OBJname << endl;
  // cout <<  "LIBname: " << LIBname << endl;
  // cout <<  "DELname: " << DELname << endl;
  // cout <<  "eflg: " << eflg << endl;
  // cout <<  "winflg: " << winflg << endl;
  // cout <<  "mode: " << mode << endl;
  //src_matcher.printit();
  //lgrp.printit();

  argv += optind;
  argc -= optind;

  // for(int i=0; i<argc; ++i) printf("ARG%d: %s\n",i,argv[i]);

  if ( error || argc > 2 || src_exts.empty() )  {
    bld_macro_list_usage(std::cerr,1);
    std::exit(1);
  }

  string product = "";
  string ofilename = "macro_list";
  if ( DELname == "@" ) DELname = "dep_list";
  if ( argc ) {
    tmpstr = argv[0];
    if ( tmpstr != "-" ) {
      product = tmpstr;
      ofilename = product + undrscr + ofilename;
      if ( !DELname.empty() ) DELname = product + undrscr + DELname;
    }
  }
  string platform = "";
  if ( argc == 2 ) {
    platform = argv[1];
    if ( isalnum(argv[1][0]) ) {
      ofilename += undrscr;
      if ( !DELname.empty() ) DELname +=undrscr;
    }
    ofilename += platform;
    if ( !DELname.empty() ) DELname += platform;
  }
  // cout <<  "product: " << product << endl;
  // cout <<  "platform: " << platform << endl;
  // cout <<  "ofilename: " << ofilename << endl;
  // cout <<  "DELname: " << DELname << endl;

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

  if ( mode != "a" ) {
    if ( !product.empty() )
      fprintf(wfile,"# Product:            %s\n",product.c_str());
    if ( !platform.empty() )
      fprintf(wfile,"# Platform:           %s\n",platform.c_str());
    fprintf(wfile,"\n");
  }
  fprintf(wfile,"# Source extensions: %s\n",src_exts.c_str());
  if ( !inc_exts.empty() )
    fprintf(wfile,"# Include extensions:%s\n",inc_exts.c_str());

  list<string> slist;
  map<string,string> smap;
  int n = src_matcher.BldFileMap(smap);
  
  list<string> ilist;
  map<string,string> imap;
  if ( !inc_exts.empty() ) n = inc_matcher.BldFileMap(imap);
  list<string>::iterator iter;
  map<string,string>::iterator pos;

  if ( !lgrp_flgs.empty() ) {
    fprintf(wfile,"# lgrp flags:        %s\n",lgrp_flgs.c_str());
    for(pos=smap.begin(); pos != smap.end(); ) {
      string fulname = pos->first;
      if (pos->second != "") fulname = pos->second + file_sep + pos->first;
      if ( ! lgrp.CheckFile(fulname) ) smap.erase(pos++);
      else ++pos;
    }
    if ( !inc_exts.empty() ) {
      for(pos=imap.begin(); pos != imap.end(); ) {
        string fulname = pos->first;
        if (pos->second != "") fulname = pos->second + file_sep + pos->first;
        if ( ! lgrp.CheckFile(fulname) ) imap.erase(pos++);
        else ++pos;
      }
    }
  }
  for(pos=smap.begin(); pos != smap.end(); ++pos) slist.push_back(pos->first);
  for(pos=imap.begin(); pos != imap.end(); ++pos) ilist.push_back(pos->first);
  
  slist.sort();
  ilist.sort();

  Line_Dumper dumper(wfile);
  if ( !SRCname.empty() ) {
    fprintf(wfile,"\n");
    tmpstr = SRCname + string(" =");
    // printf("tmpstr: %s\n",tmpstr.c_str());
    dumper.SendPhrase(tmpstr);
    for(iter=slist.begin(); iter != slist.end(); iter++) {
      dumper.SendPhrase(blank + (*iter));
    }
    dumper.Reset(0);
  }

  if ( !inc_exts.empty() && !INCname.empty() ) {
    fprintf(wfile,"\n");
    tmpstr = INCname + string(" =");
    // printf("tmpstr: %s\n",tmpstr.c_str());
    dumper.SendPhrase(tmpstr);
    for(iter=ilist.begin(); iter != ilist.end(); iter++) {
      dumper.SendPhrase(blank + (*iter));
    }
    dumper.Reset(0);
  }

  if ( !OBJname.empty() ) {
    string suffix = ".o";
    if ( winflg ) {
      if ( eflg ) {
        if ( !prefix_override ) prefix = "$(OUTDIR)\\";
        suffix = ".exe";
      }
      else {
        if ( !prefix_override ) prefix = "$(INTDIR)\\";
        suffix += "bj";
      }
    }
    else if ( eflg ) {
      suffix = "";
    }
    else if ( !LIBname.empty() ) {
      if ( !prefix_override ) prefix = "$(" + LIBname + ")(";
      suffix += ")";
    }
    tmpstr = OBJname + string(" =");
    // printf("tmpstr: %s\n",tmpstr.c_str());
    fprintf(wfile,"\n");
    dumper.SendPhrase(tmpstr);
    for(iter=slist.begin(); iter != slist.end(); iter++) {
      string::size_type dot = (*iter).rfind(".");
      string basname = (*iter).substr(0, dot);
      string send = blank + prefix + basname + suffix;
      dumper.SendPhrase(send);
    }
    dumper.Reset(0);
  }

  if ( partial ) fprintf(wfile,"\n");

  fclose(wfile);

  if ( need_cmp ) {
    File_Compare *cmp = new File_Compare(ofilename,bldfile);
    bool the_same = cmp->TheSame();
    delete cmp;
    if ( the_same ) {
      remove(bldfile.c_str());
    }
    else {
      string bakfile = ofilename + ".bak";
      rename(ofilename.c_str(),bakfile.c_str());
      rename(bldfile.c_str(),ofilename.c_str());
      if ( !DELname.empty() ) remove(DELname.c_str());
    }
  }
  else if ( !is_file &&!DELname.empty() ) remove(DELname.c_str());

  return 0;
}
