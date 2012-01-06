// $Id$
// 
// Copyright (2008) Sandia Corporation. Under the terms of
// Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
// Government retains certain rights in this software.
// 
// Hermes is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.
// 
// Hermes is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General
// Public License along with Hermes.  If not, see
// <http://www.gnu.org/licenses/>.
// 
//  C_Groups pffdiff

/*! \file PFFfile.cc
 *  \brief Implementation of the PFF_File class.
 */

#include <algorithm>
#include <iostream>
#include <string>
#include "PFFfile.h"
#include "pff_get.hh"
#include "pff_re.hh"
#include "pff_set.hh"
#include "pff_u.hh"
#include "pff_wr.hh"

using std::cout;
using std::endl;
using std::find;
using std::string;

// Class PFF_File 

/****************************************************************************/
PFF_File::PFF_File(const std::string &fname, PFF_File_Modes mode)
  : fid(0), last_error(0), file_mode(mode)
/****************************************************************************/        
{
  int nds;               // # of datasets in the file
  int cmode = PFF::RE;  // mode flag that is known to C library from pff.h

  switch (mode)  {
  case READ:
    cmode = PFF::RE;
    break;
  case WRITE:
    cmode = PFF::WR;
    break;
  case READWRITE:
    cmode = PFF::RW;
    break;
  case WRITE_MP:
    cmode = PFF::WR_MP;
    break;
  }
  filename = fname;
  fid = PFF::pf_u_open(filename.c_str(),cmode,&nds,&last_error);
}

/****************************************************************************/
PFF_File::~PFF_File()
/****************************************************************************/
{
  PFF::pf_u_close ( fid, &last_error );
}

/****************************************************************************/
int PFF_File::Dataset_Count() const
/****************************************************************************/
{
  if ( fid ) return fid->dirtop->count;
  return 0;
}

/****************************************************************************/
int PFF_File::Current_Dataset() const
/****************************************************************************/
{
  if ( fid ) return fid->directory->count;
  return 0;
}

/****************************************************************************/
void PFF_File::Current_Dataset(int dataset)
/****************************************************************************/
{
  PFF::pf_set_dsp( fid, dataset, &last_error);
}

/****************************************************************************/
std::string PFF_File::FileName() const
/****************************************************************************/
{
  return filename;
}
  
/****************************************************************************/
int PFF_File::Find_Datasets(std::vector<int> &found, const std::string &match,
                            PFF_Match_Mode mode, bool invert, bool append)
/****************************************************************************/
{
  int nds = Dataset_Count();

  // if this is the first call that needs a list of dataset titles, we need to
  // build the list
  if ( titles.empty() ) {
    titles.reserve(nds);
    for (int ds=1; ds<=nds; ++ds) {
      PFF::PFFdir *dir = PFF::pf_get_direntry(fid, ds, &last_error);
      titles.push_back(dir->title);
    }
  }
  if ( !append ) found.clear();
  int start_size = found.size();

  for (int dsi=0; dsi<nds; ++dsi) {
    int ds = dsi + 1;
    bool s_match = false;
    switch ( mode ) {
    case EXACT_MATCH:
      if ( match == titles[dsi] ) s_match = true;
      break;
    case SUBSTRING_MATCH:
      if ( titles[dsi].find(match) != string::npos ) s_match = true;
      break;
    case LEADING_MATCH:
      if ( titles[dsi].find(match) == 0 ) s_match = true;
      break;
    }
    s_match = s_match != invert;
    if ( s_match && append ) {
      // already on list, so don't add it again
      if ( find(found.begin(), found.end(), ds) != found.end() )
        s_match = false;
    }
    if ( s_match ) found.push_back(ds);
  }
  return found.size() - start_size;
}

/****************************************************************************/
PFF::PFFds_any *PFF_File::Read_Dataset(int &type, int dataset, bool keep)
/****************************************************************************/
{

  if ( dataset ) Current_Dataset(dataset);
  else dataset = Current_Dataset();

  PFF::PFFdir *dir = PFF::pf_get_direntry(fid, dataset, &last_error);

  if ( last_error ) return 0;

  type = dir->rawtype;

  PFF::PFFds_any *pff_ds = PFF::pf_re_ds( fid, (int) keep, &last_error );
  if ( last_error || pff_ds->type != dir->rawtype ) return 0;

  return pff_ds;
}

/****************************************************************************/
 int PFF_File::Write_Dataset(PFF::PFFds_any *any_dataset)
/****************************************************************************/
{
  PFF::pf_wr_ds (fid, any_dataset, &last_error );
  return last_error;
}

/****************************************************************************/
void PFF_File::Set_File_Precision(PFF_FP_Precision setting)
/****************************************************************************/
{
  int val = PFF::FP_REDU;
  switch (setting)  {
  case REDUCED:
    val = PFF::FP_REDU;
    break;
  case FULL:
    val = PFF::FP_FULL;
    break;
  }
  PFF::pf_set_fp_precision(fid, val, &last_error);
}

/****************************************************************************/
void PFF_File::Set_PFF_Precision(PFF_FP_Precision setting)
/****************************************************************************/
{
  int val = PFF::FP_REDU;
  switch (setting)  {
  case REDUCED:
    val = PFF::FP_REDU;
    break;
  case FULL:
    val = PFF::FP_FULL;
    break;
  }
  int ierr = 0;
  PFF::pf_set_fp_precision(0, val, &ierr);
}
