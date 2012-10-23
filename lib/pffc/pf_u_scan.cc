/*
-------------------------------------------------------------------------------
    PFF I/O utility:  pf_u_scan.cc
-------------------------------------------------------------------------------
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
      
-------------------------------------------------------------------------------
*/

#include "pff.hh"
#include "pff_get.hh"
#include "StringMatch.h"

#include <vector>
#include <cstdlib>
#include <iostream>

using namespace PFF;

extern "C" int *pf_u_scan(PFFfid *fid, const int *dlist, const int *range,
                          const char *find, int exact, int match, int *nmap,
                          int *ierr );


int *pf_u_scan(PFFfid *fid, const int *dlist, const int *range,
               const char *find, int exact, int match, int *nmap, int *ierr )
{
  static char module[]    = "PF_U_SCAN";
  const int MAX_TITLELEN = 64;

  /* Check to see if the error flag is set already */
  if( *ierr != 0 ) return 0;

  if ( ( fid == NULL ) || ( fid->stream == NULL ) ) {
    *ierr = 1;
    char tmp[] = "Illegal File ID (FID)";
    pf_wr_err ( module, *ierr, fid, tmp );
    return 0;
  }
  int kb = std::max(1,dlist[0]);
  int ke = 0;
  if ( fid->dirtop ) {
    ke = std::min((fid->dirtop)->count,dlist[1]);
    if ( ke < 1 ) ke = (fid->dirtop)->count;
  }
  int ksk = std::max(1,dlist[2]);

  int sb = std::max(1,range[0]) - 1;
  int se = std::min(MAX_TITLELEN,range[1]);
  if ( se < 1 ) se = MAX_TITLELEN;
  int sblen = se - sb;

  int k = kb;
  StringMatch matcher(find, exact);
  bool wantMatch = match != 0;
  std::vector<int> dslist;
  while ( k <= ke ) {
    PFFdir *dir = pf_get_direntry ( fid, k, ierr);
    std::string title = dir->title;
    int slen = title.size();
    bool ismatch = false;
    if ( slen >= sb ) {
      int l = std::min(sblen, slen - sb);
      ismatch = matcher.IsMatch(title.substr(sb,l));
    }
    if ( ismatch == wantMatch ) dslist.push_back(k);
    k += ksk;
  }
  *nmap = dslist.size();

  if ( *nmap == 0 ) return 0;

  // NOTE: for "C" compatibility, use malloc. Calling program must use "C"
  //       free function to delete this returned memory address. 
  int *map = (int *) malloc(*nmap*sizeof(int));
  for(int i=0; i<(*nmap); ++i) map[i] = dslist[i];
  return map;
}
