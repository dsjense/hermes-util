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

/*! \file comparators.cc
 *  \brief Implementation for the Comparators base class and the test
 *         classes derived from it.
 */
 
#include <algorithm>
#include <iostream>
#include <cmath>
#include <sstream>
#include "comparators.h"
#include "metrics.h"
#include "test_environment.h"

using std::string;
using std::list;
using std::ios;
using std::ostringstream;

/*****************************************************************************/
Generic_Test::SubRangeInfo::SubRangeInfo(PFF_Dataset<Real> *d,
                                         const std::list<SubRangeInput *> &srl)
  : ndim(0), ds(d), modes(0), low(0), high(0), work(0)
/*****************************************************************************/
{
  // allocate object attributes
  ndim = ds->Spatial_Dimensions();
  work =  new int[3*ndim];
  modes = new unsigned char[ndim];
  low = new Real[ndim];
  high = new Real[ndim];
  // Initialize arrays (really only needed for modes & work)
  for(int i=0;i<ndim;++i) {
    work[i] = modes[i] = 0;
    low[i] = high[i] = 0.0;
  }
  bool err = false;
  // loop over input subrange specifications, storing info in this object
  list<SubRangeInput *>::const_iterator pos;
  for(pos=srl.begin();pos!=srl.end();++pos) {
    SubRangeInput *sub = (*pos);
    int dir = sub->dir;
    if ( dir == -1 ) {
      // if direction not supplied, ndim required to be one
      if (ndim > 1 ) err = true;
      else dir = sub->dir = 0;
    }
    if ( !err ) {
      // make sure direction is valid and hasn't already been specified
      if ( dir < ndim && !work[dir] ) work[dir] = 1;
      else err = true;
    }
    if (err) {
      throw Test_Error("Subrange specification inconsistent with dataset");
    }
    // If no errors, save values from input specification
    modes[dir] = sub->mode;
    low[dir] = sub->low;
    high[dir] = sub->high;
  }
  // Initialize entire work array to zero
  for(int i=0;i<3*ndim;++i) work[i] = 0;
}

/*****************************************************************************/
Generic_Test::SubRangeInfo::~SubRangeInfo()
/*****************************************************************************/
{
  delete [] modes;
  delete [] low;
  delete [] high;
  delete [] work;
}

/*****************************************************************************/
bool Generic_Test::SubRangeInfo::GetBlockIndicies(int block, int *&max,
                                                  int *&lo, int *&hi)
/*****************************************************************************/
{
  // map provided array pointers to locations in work buffer
  max = work; lo = work + ndim; hi = lo + ndim;
  // loop over all spatial dimensions, looking for subrange limits for block
  for(int dir=0; dir<ndim; ++dir) {
    // get grid count for this direction, and set hi,lo defaults w/o subrange
    max[dir] = hi[dir] = ds->Grid_Length(dir,block);
    lo[dir] = 0;
    Real *grid = ds->Grid(dir, block);
    int last = max[dir];
    // lower subrange bound supplied, find new low limit
    if ( modes[dir] & 1 ) {
      // subrange does not overlap block, so return false
      if ( grid[last-1] < low[dir] ) return false;
      int i; for(i=0; i<last; ++i) if ( low[dir] <= grid[i] ) break;
      lo[dir] = i;
    }
    // upper subrange bound supplied, find new low limit
    if ( modes[dir] & 2 ) {
      // subrange does not overlap block, so return false
      if ( grid[0] > high[dir] ) return false;
      int i; for(i=last-1; i>=0; --i) if ( high[dir] >= grid[i] ) break;
      hi[dir] = i + 1;
    }
  }
  // overlap found, so return true
  return true;
}

/*****************************************************************************/
Generic_Test::Generic_Test(Token_Stream *tok_stream,
                           Test_Environment* test_env,
                            Generic_Metric::Metric_Type type,
                           const string &keyword_delims,
                           const string &flag_chars)
  : token_stream(tok_stream), env(test_env), metric(0), test_type(type),
    kw_delims(keyword_delims), flag_chrs(flag_chars), use_base(true),
    use_test(true), loop_count(0), ds_number(-1), ds_name(""), base_ds(0),
    test_ds(0), test_name("??"), sr_info(0)
/*****************************************************************************/
{
  if (test_type == Generic_Metric::RANGE ) use_base = false;
}

Generic_Test::Generic_Test(Test_Environment* test_env, int ds_num,
                           Generic_Metric::Metric_Type type, Real *lims)
  : token_stream(0), env(test_env), metric(0), test_type(type), kw_delims(""),
    flag_chrs(""), use_base(true), use_test(true),
    loop_count(0), ds_number(ds_num), ds_name(""), base_ds(0), test_ds(0),
    test_name("??")
/*****************************************************************************/
{
  switch (test_type) {
    case Generic_Metric::MAXIMUM:
      metric = new Maximum_Metric(this, lims ? *lims : -1.0 );
      break;
    case Generic_Metric::MEAN:
      metric = new Mean_Metric(this, lims ? *lims : -1.0 );
      break;
    case Generic_Metric::RMS:
      metric = new RMS_Metric(this, lims ? *lims : -1.0 );
      break;
    case Generic_Metric::RANGE:
      if ( lims ) metric = new Range_Metric(this, lims[0], lims[1]);
      else        metric = new Range_Metric(this, 0.0, 0.0);
      use_base = false;
      break;
    default: ;
  }
}

/*****************************************************************************/
Generic_Test::~Generic_Test()
/*****************************************************************************/
{
  delete base_ds;
  delete test_ds;
  delete metric;
  while ( !sublist.empty() ) {
    delete sublist.back();
    sublist.pop_back();
  }
  delete sr_info;
}

/*****************************************************************************/
void Generic_Test::Initialize() throw(Test_Error)
/*****************************************************************************/
{
  bool both = true;
  int dsnb = ds_number;
  int dsnt = ds_number;
  string titleb = ds_name;
  string titlet = ds_name;

  if ( ds_number < 1 && ds_name.empty() ) 
    throw Test_Error("PFF dataset title or number not supplied");

  if (use_base) {
    if ( !env->Is_File(Test_Environment::BASE_FILE) )
      throw Test_Error("PFF base file needed but not open");
    if ( dsnb < 1 ) {
      dsnb = env->Get_Dataset_Number(Test_Environment::BASE_FILE, ds_name);
      if ( dsnb < 1 )
        throw Test_Error("Unable to find dataset in base file", ds_name);
      ds_number = dsnb;
    }
    base_ds = env->Get_Dataset(Test_Environment::BASE_FILE, dsnb);
    if ( base_ds == 0 )
      throw Test_Error("Unable to open dataset in base file");
    ds_type = base_ds->Raw_Type();
    titleb = base_ds->Title();
    if ( ds_name.empty() ) ds_name = titleb;
    else if ( ds_name != titleb )
      throw Test_Error("Dataset number/title mismatch in base file", ds_name);
  } else both = false;

  if (use_test) {
    if ( !env->Is_File(Test_Environment::TEST_FILE) )
      throw Test_Error("PFF test file needed but not open");
    if ( dsnt < 1 ) {
      dsnt = env->Get_Dataset_Number(Test_Environment::TEST_FILE, ds_name);
      if ( dsnt < 1 )
        throw Test_Error("Unable to find dataset in test file", ds_name);
      if ( !use_base ) ds_number = dsnt;
    }
    test_ds = env->Get_Dataset(Test_Environment::TEST_FILE, dsnt);
    if ( test_ds == 0 )
      throw Test_Error("Unable to open dataset in test file");
    ds_type = test_ds->Raw_Type();
    titlet = test_ds->Title();
    if ( ds_name.empty() ) {
      if ( !use_base ) ds_name = titlet;
    }
    else if ( ds_name != titlet )
      throw Test_Error("Dataset number/title mismatch in test file", ds_name);
  } else both = false;

  if ( both ) {
    string msg("");
    if ( dsnt != ds_number ) msg += "Base & Test file indicies do not match";

    if ( titlet != ds_name ) {
      if ( !msg.empty() ) msg += "\n     ";
      msg += "Base & Test file names do not match: ";
      msg += ds_name;
      msg += ", ";
      msg += titlet;
    }

    if ( base_ds->Raw_Type() != test_ds->Raw_Type() ) {
      if ( !msg.empty() ) msg += "\n     ";
      msg += "Type mismatch between base and test datasets";
    }
    if ( !msg.empty() ) throw Test_Error(msg);
  }

  if ( !sublist.empty() ) {
    PFF_Dataset<Real> *ds = base_ds ? base_ds : test_ds;
    sr_info = new SubRangeInfo(ds, sublist);
  }
}

/*****************************************************************************/
void Generic_Test::Check_Shape() const throw(Test_Error)
/*****************************************************************************/
{
  if ( !use_base || !use_test ) return;

  if ( base_ds->Block_Count() != test_ds->Block_Count() )
    throw Test_Error("Block-count mismatch between base and test datasets");
  if ( base_ds->Spatial_Dimensions() != test_ds->Spatial_Dimensions() )
    throw Test_Error("Attribute dimension mismatch between base and test"
                     " datasets");
  if ( base_ds->Attribute_Dimensions() != test_ds->Attribute_Dimensions() )
    throw Test_Error("Spatial dimension mismatch between base and test"
                     " datasets");

  for(int b=0; b<base_ds->Block_Count(); ++b) {
    for(int n=0; n<base_ds->Spatial_Dimensions(); ++n) {
      if ( base_ds->Grid_Length(n,b) != test_ds->Grid_Length(n,b) )
        throw Test_Error("Shape mismatch between base and test datasets");
    }
  }
}

/*****************************************************************************/
bool Generic_Test::Analyze_Data()
/*****************************************************************************/
{
  PFF_Dataset<Real> *ds = base_ds ? base_ds : test_ds;
  int zero = 0;
  int *max = 0;
  int *low = &zero; // point to zero for default with no subrange
  int *high = 0;

  // If no subrange specified, can treat data as 1D array
  int ndim = 1;
  // otherwise, we need to recursively handle each dimension
  if ( sr_info ) ndim = ds->Spatial_Dimensions();

  int nattr = ds->Attribute_Dimensions();

  loop_count = 0;
  for(int b=0; b<ds->Block_Count(); ++b) {
    int nmax = ds->Data_Length(b);
    bool overlap = true; // with no subrange, overlap always true
    if ( sr_info ) overlap = sr_info->GetBlockIndicies(b, max, low, high);
    else  high = max = &nmax;
      
    if ( overlap ) {
      for(int a=0; a<nattr; ++a) {
        Real* rbase = 0;
        if ( use_base ) rbase = base_ds->R_Data(a, b);
        Real* rtest = 0;
        if ( use_test ) rtest = test_ds->R_Data(a, b);
        loop_count += Apply_Recursive(ndim, rbase, rtest, max, low, high);

        delete [] rbase;
        delete [] rtest;
      }
    }
  }
  const char *c_extra = 0;

  // add extra output if subrange is specified
  string extra;
  if ( sr_info ) {
    extra = "[";
    ostringstream tmp;
    tmp.setf(ios::showpoint);
    tmp.setf(ios::scientific,ios::floatfield);
    tmp.precision(3);
    for(int s=0;s<ndim;++s) {
      if ( s ) extra += ", ";
      if ( sr_info->modes[s] & 1 ) {
        tmp << sr_info->low[s] << ":";
        extra += tmp.str();
        tmp.str("");
      }
      else extra += "*:";
      if ( sr_info->modes[s] & 2 )  {
        tmp << sr_info->high[s];
        extra += tmp.str();
        tmp.str("");
      }
      else extra += ("*");
    }
    extra += "] count=";
    tmp << loop_count/nattr;
    extra += tmp.str();
    c_extra = extra.c_str();
  }
  return metric->Analyze(loop_count, c_extra);
}

/*****************************************************************************/
int Generic_Test::Apply_Recursive(int order, const Real *rbase,
                                  const Real *rtest, const int *max,
                                  const int *low, const int *high, int offset)
/*****************************************************************************/
{
  --order;

  int count = 0;
  if ( order == 0 )    {
    // if we've drilled down to 1st dimension, we can process the data over
    // the range of 1st dimension for current values of all higher dimensions
    Real base_val = 0.0;
    Real test_val = 0.0;
    count = high[0] - low[0];
    int lo = low[0] + offset;
    int hi = lo + count;
    for (int i=lo; i<hi; ++i) {
      if ( use_base ) base_val = rbase[i];
      if ( use_test ) test_val = rtest[i];
      metric->Apply(base_val, test_val);
    }
  }
  else {
    // if not yet to 1st dimension, need to drill down to next lower dimension
    // with the appropriate offset into linear array for this and all higher
    // dimensions
    for (int i=low[order]; i<high[order]; ++i) {
      count += Apply_Recursive(order, rbase, rtest, max, low, high,
                               (offset+i)*max[order-1]);
    }
  }
  return count;
}

/*****************************************************************************/
bool Generic_Test::Run_Test() // returns loop pass count
/*****************************************************************************/
{
  Initialize();
  Check_Shape();
  return Analyze_Data();
}

/*****************************************************************************/
Generic_Test::Keyword_Status Generic_Test::Parse_Keyword(Token &keyword)
/*****************************************************************************/
{
  Token tok;
  Token key;
  Keyword_Status rval = IS_OTHER;

  while (1) {
    tok = token_stream->Lookahead();
    if ( tok.Type() == TK_STRING &&
         flag_chrs.find(tok.As_String()) != string::npos ) {
      token_stream->Pop();
      tok = token_stream->Lookahead();
      if ( tok.Type() != TK_IDENTIFIER )
        token_stream->Parse_Error("Illegal flag syntax",
                                  tok.Force_As_String());

      rval = IS_FLAG;
      key = token_stream->Pop();
    }
    else if (tok.Type() == TK_IDENTIFIER ) {
      key = tok;
      token_stream->Pop();
      tok = token_stream->Pop();
      if ( tok.Type() == TK_STRING &&
           kw_delims.find(tok.As_String()) != string::npos ) {
        rval = IS_KEYWORD;
      }
      else { // otherwise, put both tokens back on the stack and return
        token_stream->Push_Back(tok);
        token_stream->Push_Back(key);
        rval = IS_OTHER;
      }
    }
    else if (tok.Type() == TK_EXIT ) {
      rval = IS_OTHER;
    }
    else token_stream->Parse_Error("Invalid keyword/flag syntax",
                                   tok.Force_As_String());

    if ( rval == IS_OTHER ) break;

    // now scan & process keywords that Generic_Test knows about
    if ( key == "DS*_INDEX" ) {
      if ( rval == IS_FLAG )
        token_stream->Parse_Error("Unknown option flag", key.As_String());
      else                   ds_number = token_stream->Parse_Integer();
    }
    else if ( key == "TI*TLE" ) {
      if ( rval == IS_FLAG )
        token_stream->Parse_Error("Unknown option flag", key.As_String());
      else                   ds_name = token_stream->Parse_String();
    }
    else if ( key == "SUB*RANGE" ) {
      // syntax is:  SUBRANGE : [dir] [LOW=value] [HIGH=val]
      //    dir is optional for 1D datasets
      //    Either HIGH or LOW must be provided, or both
      if ( rval == IS_FLAG )
        token_stream->Parse_Error("Unknown option flag", key.As_String());
      else {
        SubRangeInput *sub = new SubRangeInput;
        sublist.push_back(sub);
        tok = token_stream->Lookahead();
        if ( tok.Type() == TK_INTEGER ) {
          sub->dir = token_stream->Parse_Integer() - 1;
          tok = token_stream->Lookahead();
        }
        bool done = false;
        bool err = false;
        while (tok.Type() == TK_IDENTIFIER && !done ) {
          key = tok;
          token_stream->Pop();
          tok = token_stream->Pop();
          if ( tok.Type() == TK_STRING &&
               kw_delims.find(tok.As_String()) != string::npos ) {
            rval = IS_KEYWORD;
          }
          else if (tok.Type() == TK_EXIT ) {
            rval = IS_OTHER;
            break;
          }
          else done = true;

          if ( !done && (key == "LOW" || key == "HI*GH") ) {
            Real val = token_stream->Parse_Real();
            if ( key == "LOW" ) {
              if ( sub->mode & 1 ) err = true;
              else {
                sub->mode += 1;
                sub->low = val;
              }
            }
            else {
              if ( sub->mode & 2 ) err = true;
              else {
                sub->mode += 2;
                sub->high = val;
              }
            }
          }
          else done = true;

          if ( done && sub->mode == 0 ) err = true;
          if (err) token_stream->Parse_Error("Subrange option needs HIGH and/"
                                      "or LOW specifier", key.As_String());
          if (done) {
            token_stream->Push_Back(tok);
            token_stream->Push_Back(key);
          }
          tok = token_stream->Lookahead();
        }
      }
    }
    else break; // If we don't know about this keyword, pass back to caller

    if ( rval == IS_OTHER ) break;
  }
  keyword = key;
  return rval;
}

/*****************************************************************************/
void Generic_Test::Print_Result_Header(bool *success) const
/*****************************************************************************/
{
  if ( success ) std::cout << (*success ? "pass: " : "FAIL: ");

  string name = ds_name;
  string::size_type idx1 = name.find("      ");
  if (idx1 != string::npos ) {
    string::size_type idx2 = name.find_first_not_of(" ",idx1);
    if ( idx2 != string::npos ) name.replace(idx1, idx2-idx1, " ... ");
    else name.erase(idx1);
  }

  if ( ds_number > 0 )
    std::cout << test_name << ", \"" << name << "\"(#" << ds_number << ")";
  else
    std::cout << test_name << ", \"" << name << "\"(?" << "?)";
}

/*****************************************************************************/
void Generic_Test::Parse_Metric_Keywords()
/*****************************************************************************/
{
  Token key;

  Real limit = -1.0;
  Real min_limit = 0.0;
  Real max_limit = 0.0;
  while(1) {
    Keyword_Status stat = Parse_Keyword(key);
    if ( stat == IS_OTHER ) break; // Must be next command
 
    // now scan & process metric-related keywords
    bool matched = false;
    switch (test_type) {
      case Generic_Metric::RANGE:
        if ( key == "LIM*ITS" ) {
          if ( stat == IS_FLAG ) {
            min_limit = 0.0;
            max_limit = 1.0;
          }
          else {
            Real lim2 = 0.0;
            Real lim1 = token_stream->Parse_Real();
            Token next = token_stream->Lookahead();
            if ( next.Type() == TK_REAL || next.Type() == TK_INTEGER )
              lim2 = token_stream->Parse_Real();
            min_limit = std::min(lim1, lim2);
            max_limit = std::max(lim1, lim2);
          }
          matched = true;
        }
        break;
      default:
        if ( key == "LIM*IT" ) {
          if ( stat == IS_FLAG ) limit = 1.0;
          else                   limit = token_stream->Parse_Real();
          matched = true;
        }
    }
    if (!matched) 
      token_stream->Parse_Error("Unknown keyword: ", key.As_String());
  }
  switch (test_type) {
    case Generic_Metric::MAXIMUM:
      metric = new Maximum_Metric(this, limit);              break;
    case Generic_Metric::MEAN:
      metric = new Mean_Metric(this, limit);                 break;
    case Generic_Metric::RMS:
      metric = new RMS_Metric(this, limit);                  break;
    case Generic_Metric::RANGE:
      metric = new Range_Metric(this, min_limit, max_limit); break;
    default: ;
  }
}

/*****************************************************************************/
Grid_Test::Grid_Test(Token_Stream *tok_stream, Test_Environment* test_env,
                     Generic_Metric::Metric_Type type,
                     const string &keyword_delims, const string &flag_chars)
  : Generic_Test(tok_stream, test_env, type, keyword_delims, flag_chars),
    block(0), direction(0)
/*****************************************************************************/
{
  Set_Test_Name();
  Parse_Metric_Keywords();
}

Grid_Test::Grid_Test(Test_Environment* test_env, int ds_num,
                     Generic_Metric::Metric_Type type, Real *lims, int dir,
                     int blk)
  : Generic_Test(test_env, ds_num, type, lims), block(blk), direction(dir)
/*****************************************************************************/
{
  Set_Test_Name();
}

/*****************************************************************************/
Grid_Test::~Grid_Test()
/*****************************************************************************/
{
}

/*****************************************************************************/
void Grid_Test::Set_Test_Name()
/*****************************************************************************/
{
  switch (test_type) {
    case Generic_Metric::MAXIMUM:   test_name ="MAX_DEVIATION/GRID";   break;
    case Generic_Metric::MEAN:      test_name ="MEAN_DEVIATION/GRID";  break;
    case Generic_Metric::RMS:       test_name ="RMS_DEVIATION/GRID";   break;
    case Generic_Metric::RANGE:     test_name ="RANGE_ALLOWED/GRID";   break;
    default: ;
  }
}

/*****************************************************************************/
void Grid_Test::Initialize() throw(Test_Error)
/*****************************************************************************/
{
  Generic_Test::Initialize();

  // Only allow certain types for now
  switch (ds_type) {
  case PFF_Dataset<Real>::UF3:
  case PFF_Dataset<Real>::UF1:
  case PFF_Dataset<Real>::NF3:
  case PFF_Dataset<Real>::NV3:
  case PFF_Dataset<Real>::NI3:
  case PFF_Dataset<Real>::NG3:
    break;
  case PFF_Dataset<Real>::NGD:
    {
      PFF_Dataset<Real> *ds = base_ds ? base_ds : test_ds;
      if ( ds->Spatial_Dimensions() == 0 ) 
        throw Test_Error("0-D NGD datasets are not supported");
    }
    break;
  default:
    throw Test_Error("Dataset type is not supported");
  }
}

/*****************************************************************************/
bool Grid_Test::Analyze_Data()
/*****************************************************************************/
{
  Real base_val = 0.0;
  Real test_val = 0.0;
  PFF_Dataset<Real> *ds = base_ds ? base_ds : test_ds;

  int dstart = 0;
  int dend = ds->Spatial_Dimensions();
  int bstart = 0;
  int bend = ds->Block_Count();
  bool success = true;

  if ( direction ) { dstart = direction - 1; dend = direction; }
  if ( block ) { bstart = block - 1; bend = block; }

  for(int s=dstart; s<dend; ++s) {
    loop_count = 0;
    for(int b=bstart; b<bend; ++b) {
      int *max = 0;
      int *low = 0;
      int *high = 0;
      bool overlap = true;
      int lo = 0;
      int hi;
      if ( sr_info ) {
        overlap = sr_info->GetBlockIndicies(b, max, low, high);
        lo = low[s];
        hi = high[s];
      }
      else hi = ds->Grid_Length(s,b);

      if ( overlap ) {
        Real* rbase = 0;
        if ( use_base ) rbase = base_ds->Grid(s, b);
        Real* rtest = 0;
        if ( use_test ) rtest = test_ds->Grid(s, b);
        for(int n=lo; n<hi; ++n) {
          if ( use_base ) base_val = rbase[n];
          if ( use_test ) test_val = rtest[n];
          metric->Apply(base_val, test_val);
        }
        loop_count += (hi - lo);
        delete [] rbase;
        delete [] rtest;
      }
    }
    // add extra output for the dimension of the test
    char cs[] = "1";
    cs[0] = '1' + s;
    string extra = "DIM " + string(cs);
    // add extra output if subrange is specified
    if ( sr_info ) {
      ostringstream tmp;
      tmp.setf(ios::showpoint);
      tmp.setf(ios::scientific,ios::floatfield);
      tmp.precision(3);
      if ( sr_info->modes[s] & 1 ) {
        tmp << " [" << sr_info->low[s] << ":";
        extra += tmp.str();
        tmp.str("");
      }
      else extra += " [*:";
      if ( sr_info->modes[s] & 2 )  {
        tmp << sr_info->high[s] << "]";
        extra += tmp.str();
        tmp.str("");
      }
      else extra += ("*]");
      tmp << " count=" << loop_count;
      extra += tmp.str();
    }
    bool okay = metric->Analyze(loop_count, extra.c_str());
    if ( !okay ) success = false;
  }
  return success;
}

/*****************************************************************************/
Generic_Test::Keyword_Status Grid_Test::Parse_Keyword(Token &keyword)
{
/*****************************************************************************/
  Token tok;
  Token key;
  Keyword_Status rval = IS_OTHER;

  while (1) {
    rval = Generic_Test::Parse_Keyword(key);
    if ( rval == IS_OTHER ) break; // Must be next command
 
    // now scan & process keywords that Grid_Test knows about
    if ( key == "BL*OCK" ) {
      if ( rval == IS_FLAG )
        token_stream->Parse_Error("Unknown option flag", key.As_String());
      else                   block = token_stream->Parse_Integer();
    }
    else if ( key == "DIR*ECTION" ) {
      if ( rval == IS_FLAG )
        token_stream->Parse_Error("Unknown option flag", key.As_String());
      else                   direction = token_stream->Parse_Integer();
    }
    else break; // If we don't know about this keyword, pass back to caller
  }
  keyword = key;
  return rval;
}

/*****************************************************************************/
Attr_Test::Attr_Test(Token_Stream *tok_stream, Test_Environment* test_env,
                     Generic_Metric::Metric_Type type,
                     const string &keyword_delims, const string &flag_chars)
  : Generic_Test(tok_stream, test_env, type, keyword_delims, flag_chars),
    block(0), direction(0)
/*****************************************************************************/
{
  Set_Test_Name();
  Parse_Metric_Keywords();
}

Attr_Test::Attr_Test(Test_Environment* test_env, int ds_num,
                     Generic_Metric::Metric_Type type, Real *lims, int dir,
                     int blk)
  : Generic_Test(test_env, ds_num, type, lims), block(blk), direction(dir)
/*****************************************************************************/
{
  Set_Test_Name();
}

/*****************************************************************************/
Attr_Test::~Attr_Test()
/*****************************************************************************/
{
}

/*****************************************************************************/
void Attr_Test::Set_Test_Name()
/*****************************************************************************/
{
  switch (test_type) {
    case Generic_Metric::MAXIMUM:   test_name ="MAX_DEVIATION";   break;
    case Generic_Metric::MEAN:      test_name ="MEAN_DEVIATION";  break;
    case Generic_Metric::RMS:       test_name ="RMS_DEVIATION";   break;
    case Generic_Metric::RANGE:     test_name ="RANGE_ALLOWED";   break;
    default: ;
  }
}

/*****************************************************************************/
void Attr_Test::Initialize() throw(Test_Error)
/*****************************************************************************/
{
  Generic_Test::Initialize();

  // Only allow certain types for now
  switch (ds_type) {
  case PFF_Dataset<Real>::UF3:
  case PFF_Dataset<Real>::UF1:
  case PFF_Dataset<Real>::NF3:
  case PFF_Dataset<Real>::NV3:
    break;
  case PFF_Dataset<Real>::NGD:
    {
      PFF_Dataset<Real> *ds = base_ds ? base_ds : test_ds;
      if ( ds->Spatial_Dimensions() == 0 || ds->Attribute_Dimensions() == 0 ) 
        throw Test_Error("0-D NGD datasets are not supported");
    }
    break;
  default:
    throw Test_Error("Dataset type is not supported");
  }
}

/*****************************************************************************/
bool Attr_Test::Analyze_Data()
/*****************************************************************************/
{
  return Generic_Test::Analyze_Data();
}

/*****************************************************************************/
Generic_Test::Keyword_Status Attr_Test::Parse_Keyword(Token &keyword)
{
/*****************************************************************************/
  Token tok;
  Token key;
  Keyword_Status rval = IS_OTHER;

  while (1) {
    rval = Generic_Test::Parse_Keyword(key);
    if ( rval == IS_OTHER ) break; // Must be next command
 
    // now scan & process keywords that Attr_Test knows about
    // currently, there are no such keywords, so just break

    break; // If we don't know about this keyword, pass back to caller
  }
  keyword = key;
  return rval;
}
