//  C_Groups
//  $Id$
//  
//  Copyright (2014) David Seidel.
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

#include "pff_fstack.hh"
#include "PFFfile.h"
#include "PFFdataset.h"
#include "pff_ds.hh"
#include "pff_get.hh"
#include "pff_fill.hh"
#include "pff_list.hh"
#include "pff_set.hh"
#include "pff_wr.hh"
#include "pff_u.hh"
#include "StringMatch.h"
#include <Python.h>
#include <algorithm>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cassert>
#include <termios.h>
#include <unistd.h>

#include "pff_ext.h"

using std::string;
using std::cout;
using std::endl;

typedef std::map<int,PFF_File *> FileMap;
typedef PFF_Dataset<float> PFF_DS;
typedef std::map<int,PFF_DS *> DSMap;

struct FileData {
  ~FileData();
  FileMap fm;
};
FileData::~FileData()
{
  FileMap::iterator pos = fm.begin();
  int cnt = 0;
  for(  ; pos != fm.end(); ++pos) {
    delete pos->second;
    ++cnt;
  }
  fm.clear();
  if (cnt) {
    string plural = "";
    if (cnt > 1) plural = "s";
    cout << cnt << " open PFF file" << plural << " closed." << endl;
  }
 }

static FileData fdata;
FileMap &filemap = fdata.fm;

static std::vector<int> freeDShandles;
static DSMap dsmap;
static int maxHandle = 0;
static const int freeGrow = 10;

static PyObject *PFF_Error = 0;

class UorNwrite {
 public:
  char typekey;
  int curblk;
  int sdim, adim, nblk;
  PFF::PFFds_any *UorNds;
  UorNwrite() : typekey(0), curblk(0), UorNds(0) { }
  void Clear() { typekey = 0; curblk = 0; UorNds = 0; }
  bool InUse() { if (UorNds) return true; else return false; }
};
static UorNwrite UorNds;

PFF_File *FindCurrentFile()
{
  FileMap::iterator pos = filemap.begin();
  PFF_File *file = 0;
  for(  ; pos != filemap.end(); ++pos) {
    if (pos->second->Get_BaseFID() == PFF::PFF.current) {
      file = pos->second;
      break;
    }
  }
  return file;
}

int process_labels(char **lptr, int cnt, char *out)
{
  int totlen = 0;
  char *buf = out;
  for(int i=0; i<cnt; ++i) {
    int len = 0;
    if ( *lptr ) len = std::strlen(*lptr);
    if (out) {
      if ( *lptr ) std::strcpy(buf,*lptr);
      else *buf = '\0';
      buf += (len + 1);
    }
    totlen += (len + 1);
    ++lptr;
  }
  return totlen;
}

extern "C" {
  PyObject *get_type_names(PyObject *self, PyObject *args)
  {
    return Py_BuildValue( "{i:s,i:s,i:s,i:s,i:s,i:s,i:s,i:s,i:s}",
                          PFF::PFTUF3, "UF3",
                          PFF::PFTUF1, "UF1",
                          PFF::PFTNF3, "NF3",
                          PFF::PFTNV3, "NV3",
                          PFF::PFTVTX, "VTX",
                          PFF::PFTIFL, "IFL",
                          PFF::PFTNGD, "NGD",
                          PFF::PFTNG3, "NG3",
                          PFF::PFTNI3, "NI3" );
  }

  PyObject *get_ctype_sizes(PyObject *self, PyObject *args)
  {
    return Py_BuildValue( "{c:i,c:i,c:i,c:i}",
                          'i', sizeof(int),
                          'l', sizeof(long),
                          'f', sizeof(float),
                          'd', sizeof(double) );
  }

  PyObject *open_pff_file(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    char *fname = 0;
    static char rtype[] = "r";
    char *type = rtype;
    static char arg1[] = "file";
    static char arg2[] = "mode";
    static char *argnames[] = { arg1, arg2, 0 };

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"s|s", argnames, 
                                      &fname, &type) ) return 0;

    string stype(type);
    std::transform(stype.begin(), stype.end(), stype.begin(), tolower);
 
    // cout << fname << " " << stype << endl;

    PFF_File::PFF_File_Modes mode;
    if  (stype ==  "r") mode = PFF_File::READ;
    else if (stype == "w") mode = PFF_File::WRITE;
    else if (stype == "rw") mode = PFF_File::READWRITE;
    else {
      PyErr_SetString(PFF_Error, "invalid value for \"mode\" argument");
      return 0;
    }

    PFF_File *t = new PFF_File(fname, mode);
    if ( t->Status() ) {
      delete t;
      PyErr_SetString(PFF_Error, "Error opening file");
      return 0;
    }
    if ( mode == PFF_File::READWRITE) t->Current_Dataset(t->Dataset_Count()+1);

    PFF::PFFfid *fid = t->Get_BaseFID();
    filemap[fid->count] = t;
    return Py_BuildValue("i",fid->count);
  }

  PyObject *pff_file_list(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    static char arg1[] = "range";
    static char arg2[] = "width";
    static char *argnames[] = { arg1, arg2, 0 };

    int wid = 80, low = 1, hi = -1;

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"|(ii)i", argnames, 
                                      &low, &hi,  &wid) ) return 0;
    int ierr = 0;
    PFF::pf_list_files(wid, low, hi, 0, 0, 0, &ierr);
    return Py_BuildValue("i",ierr);
  }

  PyObject *pff_util_i2f(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    static char arg1[] = "ival";
    static char arg2[] = "keep";
    static char arg3[] = "offset";
    static char *argnames[] = { arg1, arg2, arg3, 0 };

    int ialen = 0, iasz = 0;
    char cia;
    char *iabuf = 0;
    int keep = 0, offset = 0;

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"(cis#)|ii", argnames, 
                                      &cia, &iasz, &iabuf ,&ialen,
                                      &keep, &offset) ) return 0;
    int ierr = 0;
    long ni = ialen/iasz;
    //cout << cia  << " " << iasz <<" "<< ialen <<" "<< ni << endl;
         
    assert(iasz == sizeof(int));

    int *ival = 0;
    if ( ni ) ival = (int *)iabuf;
#if 0
    if ( ni ) {
      int *ival = (int *)iabuf;
      for(int i = 0; i<ni; ++i) cout << i << " " << ival[i] << endl;
    }
#endif
    float xval = 0.0;
    int off10 = 0;
    int pkeep = PFF::FALSE;
    if (keep) {
      pkeep = PFF::TRUE;
      offset = 1;
    }
    PFF::pf_u_i2f(pkeep, ival, &xval, &off10, &ierr);
    if ( ierr ) {
      PyErr_SetString(PFF_Error, "Error converting int array to float");
      return 0;
    }
    if ( !offset and off10 != 0 )  {
      PyErr_SetString(PFF_Error, "Non-zero power-of-10 offset encountered");
      return 0;
    }
    if (offset) return Py_BuildValue("fi",xval,off10);
    else return Py_BuildValue("f",xval);
  }

  PyObject *pff_util_i2d(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    static char arg1[] = "ival";
    static char *argnames[] = { arg1, 0 };

    int ialen = 0, iasz = 0;
    char cia;
    char *iabuf = 0;

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"(cis#)", argnames, 
                                      &cia, &iasz, &iabuf, &ialen) ) return 0;
    int ierr = 0;
    long ni = ialen/iasz;
    //cout << cia  << " " << iasz <<" "<< ialen <<" "<< ni << endl;
         
    assert(iasz == sizeof(int));

    int *ival = 0;
    if ( ni ) ival = (int *)iabuf;
#if 0
    if ( ni ) {
      int *ival = (int *)iabuf;
      for(int i = 0; i<ni; ++i) cout << i << " " << ival[i] << endl;
    }
#endif
    double xval = 0.0;
    PFF::pf_u_i2d(ival, &xval, &ierr);
    if ( ierr ) {
      PyErr_SetString(PFF_Error, "Error converting int array to float");
      return 0;
    }
    return Py_BuildValue("d",xval);
  }

  PyObject *pff_util_i2l(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    static char arg1[] = "ival";
    static char *argnames[] = { arg1, 0 };

    int ialen = 0, iasz = 0;
    char cia;
    char *iabuf = 0;

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"(cis#)", argnames, 
                                      &cia, &iasz, &iabuf, &ialen) ) return 0;
    int ierr = 0;
    long ni = ialen/iasz;
    //cout << cia  << " " << iasz <<" "<< ialen <<" "<< ni << endl;
         
    assert(iasz == sizeof(int));

    int *ival = 0;
    if ( ni ) ival = (int *)iabuf;
#if 0
    if ( ni ) {
      int *ival = (int *)iabuf;
      for(int i = 0; i<ni; ++i) cout << i << " " << ival[i] << endl;
    }
#endif
    long lval = 0;
    PFF::pf_u_i2l(ival, &lval, &ierr);
    if ( ierr ) {
      PyErr_SetString(PFF_Error, "Error converting int array to long int");
      return 0;
    }
    return Py_BuildValue("l",lval);
  }

  PyObject *pff_util_f2i(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    static char arg1[] = "xval";
    static char arg2[] = "offset";
    static char *argnames[] = { arg1, arg2, 0 };

    float xval = 0.0;
    int offset = 0;

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"f|i", argnames, 
                                      &xval, &offset) ) return 0;
    int ierr = 0;

    int ival[3];
    PFF::pf_u_f2i(xval, offset, ival, &ierr);
    if ( ierr ) {
      PyErr_SetString(PFF_Error, "Error converting float to int array");
      return 0;
    }
    int size = sizeof(int);
    PyObject *r = Py_BuildValue("[ciis#]",
                                 'i', size, 3, (char *) ival, size*3);
    return r;
  }

  PyObject *pff_util_d2i(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    static char arg1[] = "dval";
    static char *argnames[] = { arg1, 0 };

    double dval = 0.0;

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"d", argnames, 
                                      &dval) ) return 0;
    int ierr = 0;

    int ival[5];
    PFF::pf_u_d2i(dval, ival, &ierr);
    if ( ierr ) {
      PyErr_SetString(PFF_Error, "Error converting double to int array");
      return 0;
    }
    int size = sizeof(int);
    PyObject *r = Py_BuildValue("[ciis#]",
                                 'i', size, 5, (char *) ival, size*5);
    return r;
  }

  PyObject *pff_util_l2i(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    static char arg1[] = "lval";
    static char *argnames[] = { arg1, 0 };

    long lval = 0.0;

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"l", argnames, 
                                      &lval) ) return 0;
    int ierr = 0;

    int ival[3];
    PFF::pf_u_l2i(lval, ival, &ierr);
    if ( ierr ) {
      PyErr_SetString(PFF_Error, "Error converting long int to int array");
      return 0;
    }
    int size = sizeof(int);
    PyObject *r = Py_BuildValue("[ciis#]",
                                 'i', size, 3, (char *) ival, size*3);
    return r;
  }

  PyObject *pff_fp_precision(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    static char arg1[] = "file";
    static char arg2[] = "value";
    static char *argnames[] = { arg1, arg2, 0 };

    int file = 0, value = -1;

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"|ii", argnames,
                                      &file, &value) ) return 0;
    if ( value < -1 || value > 2 ) {
      PyErr_SetString(PFF_Error, "Illegal parameter: -1 <= value <= 2");
      return 0;
    }

    int ierr = 0;
    if ( filemap.empty() && file >= 0 ) {
      PyErr_SetString(PFF_Error, "No PFF files currently open");
      return 0;
    }
    bool all = false;
    bool dflt = false;
    int nfiles = 0;
    PFF::PFFfid *fid = 0;
    if (file < 0) {
      ++nfiles;
      dflt = true;
      if (file < -1) {
        all = true;
        nfiles += filemap.size();
      }
    }
    else {
      if ( file > 0 ) fid = PFF::pf_get_fid(file, &ierr);
      else  fid = PFF::PFF.current;
      if ( fid == 0 ) {
        std::ostringstream tmp;
        tmp << "File \"" << file << "\" not open";
        PyErr_SetString(PFF_Error, tmp.str().c_str());
        return 0;
      }
      ++nfiles;
    }
    int lmap = 2*nfiles*sizeof(int);
    int *map = (int *) malloc(lmap);
    int mloc = 0;
    static int v2fp[] = {PFF::FP_REDU, PFF::FP_ALLFULL, PFF::FP_ORDFULL};
    if ( dflt ) {
      map[mloc++] = 0;
      map[mloc++] = PFF::pf_get_fp_precision(0, &ierr);
      if (value >= 0) {
        PFF::pf_set_fp_precision(0, v2fp[value], &ierr);
        if ( ierr ) {
          PyErr_SetString(PFF_Error, "Error setting pff global precision");
          return 0;
        }
      }
    }
    if ( fid ) {
      int id = file;
      if ( id == 0 ) id = fid->count;
      map[mloc++] = id;
      map[mloc++] = PFF::pf_get_fp_precision(fid, &ierr);
      if (value >= 0) {
        PFF::pf_set_fp_precision(fid, v2fp[value], &ierr);
        if ( ierr ) {
          PyErr_SetString(PFF_Error, "Error setting pff global precision");
          return 0;
        }
      }
    }
    else if (all) {
      FileMap::iterator pos = filemap.begin();
      //PFF_File *pfile = 0;
      for(  ; pos != filemap.end(); ++pos) {
        map[mloc++] = pos->first;
        fid = (pos->second)->Get_BaseFID();
        map[mloc++] = PFF::pf_get_fp_precision(fid, &ierr);
        if (value >= 0) {
          PFF::pf_set_fp_precision(fid, v2fp[value], &ierr);
          if ( ierr ) {
            PyErr_SetString(PFF_Error, "Error setting pff global precision");
            return 0;
          }
        }
      }
    }
    for(int l=1; l<mloc; l+=2) {
      if ( map[l] == PFF::FP_REDU ) map[l] = 0;
      else if ( map[l] == PFF::FP_ALLFULL ) map[l] = 1;
      else map[l] = 2;
    }
    PyObject *rval = Py_BuildValue("[ciis#]",'i',sizeof(int),mloc,map,lmap);
    CHKFREE(map);
    return rval;
  }

  PyObject *pff_ds_list(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    static char arg1[] = "file";
    static char arg2[] = "range";
    static char arg3[] = "match";
    static char arg4[] = "width";
    static char *argnames[] = { arg1, arg2, arg3, arg4, 0 };

    int file = 0, low = 1, hi = -1, wid = 80;
    char *match = 0;

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"|i(ii)si", argnames, &file,
                                      &low, &hi, &match, &wid) ) return 0;
    int ierr = 0;
    PFF::PFFfid *fid = 0;
    if ( filemap.empty() ) {
      PyErr_SetString(PFF_Error, "No PFF files currently open");
      return 0;
    }
    if ( file > 0 ) fid = PFF::pf_get_fid(file, &ierr);
    else fid = PFF::PFF.current;
    if ( fid == 0 ) {
      std::ostringstream tmp;
      tmp << "File \"" << file << "\" not open";
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
    }
    if ( std::strlen(match) > 0 ) {
      int dlist[] = { low, hi, 1 };
      int range[] = { 1, -1 };
      int exact = 0, getmatch = 1;
      int nmap = 0;
      int *map = PFF::pf_u_scan(fid, dlist, range, match, exact, getmatch,
                                &nmap, &ierr);
      if (map) {
        PFF::pf_list_dirs(stdout, fid, wid, -nmap, map, 0, 0, 0, &ierr);
        std::free(map);
      }
      else {
        cout << "No matching datasets found" << endl;
      }
    }
    else {
      PFF::pf_list_dirs(stdout, fid, wid, low, &hi, 0, 0, 0, &ierr);
    }
    return Py_BuildValue("");
  }

  PyObject *pff_get_match(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    static char arg1[] = "string";
    static char arg2[] = "file";
    static char arg3[] = "range";
    static char arg4[] = "exactcase";
    static char arg5[] = "match";
    static char arg6[] = "width";
    static char *argnames[] = { arg1, arg2, arg3, arg4, arg5, arg6, 0 };

    int file = 0, low = 1, hi = -1, wid = 80, exact = 0, getmatch = 1;
    char *strng = 0;

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"s|i(ii)iii", argnames,
                                      &strng, &file, &low, &hi, &exact,
                                      &getmatch, &wid) ) return 0;
    int ierr = 0;
    PFF::PFFfid *fid = 0;
    if ( filemap.empty() ) {
      PyErr_SetString(PFF_Error, "No PFF files currently open");
      return 0;
    }
    if ( file > 0 ) fid = PFF::pf_get_fid(file, &ierr);
    else fid = PFF::PFF.current;
    if ( fid == 0 ) {
      std::ostringstream tmp;
      tmp << "File \"" << file << "\" not open";
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
    }
    int *map = 0;
    int nmap = 0;
    if ( std::strlen(strng) == 0 ) {
      if (fid->dirtop) nmap = (fid->dirtop)->count;
      if (nmap){
        map = (int *) malloc(nmap*sizeof(int));
        for(int i=0; i<nmap; ++i) map[i] = i + 1;
      }
    }
    else {
      int dlist[] = { low, hi, 1 };
      int range[] = { 1, -1 };
      map = PFF::pf_u_scan(fid, dlist, range, strng, exact, getmatch,
                           &nmap, &ierr);
    }
    int size = sizeof(int);
    int lmap = nmap*size;
    char *pmap = (char *) map;
    if (!map) pmap = strng;
    PyObject *rval = Py_BuildValue("[ciis#]",'i',size,nmap,pmap,lmap);
    CHKFREE(map);
    return rval;
  }

  PyObject *pff_adv_dsp(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    static char arg1[] = "file";
    static char *argnames[] = { arg1, 0 };

    int id = 0;

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"|i", argnames, &id) )
      return 0;

    PFF_File *file = 0;
    if ( filemap.empty() ) {
      PyErr_SetString(PFF_Error, "No PFF files currently open");
      return 0;
    }
    if ( id > 0 ) {
      FileMap::iterator pos = filemap.begin();
      if ( (pos = filemap.find(id)) == filemap.end() ) {
        std::ostringstream tmp;
        tmp << "File \"" << id << "\" not open";
        PyErr_SetString(PFF_Error, tmp.str().c_str());
        return 0;
      }
      file = pos->second;
    }
    else file = FindCurrentFile();

    if ( file->Mode() == PFF_File::WRITE ) {
      std::ostringstream tmp;
      tmp << "Dataset pointer of Write-Only file cannot be advanced";
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
    }
    int dsi = file->Current_Dataset();
    if ( dsi &&
         ( dsi < file->Dataset_Count() || file->Mode() != PFF_File::READ ) )
      file->Current_Dataset(++dsi);

    return Py_BuildValue("i",dsi);
  }

  PyObject *close_pff_file(PyObject *self, PyObject *args)
  {
    int entry = 0;
    if ( filemap.empty() ) {
      PyErr_SetString(PFF_Error, "No PFF files currently open");
      return 0;
    }
    if ( !PyArg_ParseTuple(args,"|i", &entry) ) return 0;
    int cnt = 0;
    FileMap::iterator pos = filemap.begin();
    if ( entry > 0 ) {
      if ( (pos = filemap.find(entry)) == filemap.end() ) {
        std::ostringstream tmp;
        tmp << "File \"" << entry << "\" not open";
        PyErr_SetString(PFF_Error, tmp.str().c_str());
        return 0;
      }
      delete filemap[entry];
      filemap.erase(entry);
      ++cnt;
    }
    else {
      for(  ; pos != filemap.end(); ++pos) {
        delete pos->second;
        ++cnt;
      }
      filemap.clear();
    }
    return Py_BuildValue("i",cnt);
  }

  PyObject *set_pff_file(PyObject *self, PyObject *args)
  {
    int entry = 0;
    if ( filemap.empty() ) {
      PyErr_SetString(PFF_Error, "No PFF files currently open");
      return 0;
    }
    if ( !PyArg_ParseTuple(args,"i", &entry) ) return 0;
    FileMap::iterator pos = filemap.begin();
    if ( (pos = filemap.find(entry)) == filemap.end() ) {
      std::ostringstream tmp;
      tmp << "File \"" << entry << "\" not open";
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
    }
    PFF::PFF.current = (pos->second)->Get_BaseFID();
    return Py_BuildValue("");
  }

  PyObject *pff_getfilename(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    static char arg1[] = "id";
    static char *argnames[] = { arg1, 0 };

    int id = 0;

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"|i", argnames, 
                                      &id) ) return 0;
    
    PFF_File *file = 0;
    if ( filemap.empty() ) {
      PyErr_SetString(PFF_Error, "No PFF files currently open");
      return 0;
    }
    if ( id > 0 ) {
      FileMap::iterator pos = filemap.begin();
      if ( (pos = filemap.find(id)) == filemap.end() ) {
        std::ostringstream tmp;
        tmp << "File \"" << id << "\" not open";
        PyErr_SetString(PFF_Error, tmp.str().c_str());
        return 0;
      }
      file = pos->second;
    }
    else file = FindCurrentFile();

    if ( file == 0) {
      std::ostringstream tmp;
      tmp << "File \"" << id << "\" not open";
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
    }
    return Py_BuildValue( "s", file->FileName().c_str() );
  }

  PyObject *read_ds_header(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    static char arg1[] = "ds";
    static char arg2[] = "id";
    static char *argnames[] = { arg1, arg2, 0 };

    int id = 0, ds = 0;

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"|ii", argnames, 
                                      &ds, &id) ) return 0;
    
    PFF::PFFfid *fid = PFF::PFF.current;
    PFF_File *file = 0;
    if ( filemap.empty() ) {
      PyErr_SetString(PFF_Error, "No PFF files currently open");
      return 0;
    }
    if ( id > 0 ) {
      FileMap::iterator pos = filemap.begin();
      if ( (pos = filemap.find(id)) == filemap.end() ) {
        std::ostringstream tmp;
        tmp << "File \"" << id << "\" not open";
        PyErr_SetString(PFF_Error, tmp.str().c_str());
        return 0;
      }
      file = pos->second;
      fid = file->Get_BaseFID();
      id = fid->count;
    }
    else file = FindCurrentFile();

    if ( fid == 0 || file == 0) {
      std::ostringstream tmp;
      tmp << "File \"" << id << "\" not open";
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
    }
    if ( ds <= 0 ) ds = file->Current_Dataset();

    PFF_DS *dset = PFF_DS::Read_Dataset(file, ds);
    if ( !dset )  {
      file->Clear_Error();
      std::ostringstream tmp;
      tmp << "Dataset \"" << ds << "\" could not be accessed";
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
    }

    if ( freeDShandles.empty() ) {
      for(int i=maxHandle+freeGrow; i>maxHandle; --i)
        freeDShandles.push_back(i);
      maxHandle += freeGrow;
    }
    int hndl = freeDShandles.back(); freeDShandles.pop_back();
    dsmap[hndl] = dset;
    //cout << fid->count << " " << file->FileName() << " " << ds << " "
    //     << hndl << " " << dset->Raw_Type() << " " << dset->Title() << " " 
    //     << dset->Block_Count() << endl;

    int raw = dset->Raw_Type();
    int app = dset->Application_Type();
    int nblk = dset->Block_Count();
    int sdim = dset->Spatial_Dimensions();
    int adim = dset->Attribute_Dimensions();
    PFF::PFFhead *hdr = dset->Get_DSany()->head;
    char *tit = hdr->title;
    char *typ = hdr->type_name;
    int nrfu = hdr->nwords_rfu;
    int size = sizeof(int);
    int lrfu = nrfu*size;
   
    return Py_BuildValue("{s:i,s:i,s:i,s:i,s:i,s:i,s:s,s:s,s:[ciis#]}",
                         "handle",hndl,
                         "rawtype",raw,
                         "apptype",app,
                         "nblk",nblk,
                         "sdim",sdim,
                         "adim",adim,
                         "title",tit,
                         "typename",typ,
                         "rfu",'i',size,nrfu,(char *)hdr->rfu,lrfu);
  }

  PyObject *release_ds_handle(PyObject *self, PyObject *args)
  {
    int hndl = -1;
    if ( !PyArg_ParseTuple(args,"i", &hndl) ) return 0;

    DSMap::iterator pos = dsmap.find(hndl);
    if ( pos == dsmap.end() ) {
      std::ostringstream tmp;
      tmp << "Tried to delete an invalid dataset handle: " << hndl;
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
    }
    //cout << "DEBUG: deleting dataset for handle " << hndl << " "
    //     << pos->first <<endl;
    delete pos->second;
    dsmap.erase(pos);
    freeDShandles.push_back(hndl);
    return Py_BuildValue("");
  }

  PyObject *get_labels(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    int handle = 0;
    char ltype = 0;

    static char arg1[] = "handle";
    static char arg2[] = "labeltype";
    static char *argnames[] = { arg1, arg2, 0 };

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"ic", argnames, 
                                      &handle, &ltype) ) return 0;

    //cout << handle << " " << ltype << endl;
    PFF_DS *dset = dsmap[handle];
    PFF::PFFds_any *dsany = dset->Get_DSany();
    int rawtype = dsany->head->rawtype;
    char *buf = 0;
    int len = 0;
    switch (rawtype)  {
    case PFF::PFTUF3:
    case PFF::PFTUF1:
      {
        PFF::PFFds_uniform *dsu = (PFF::PFFds_uniform *) dsany;
        int nblk = dsu->nblk;
        int nlabs = 1;
        if ( ltype == 'S' ) nlabs = dsu->dims;
        else if ( ltype == 'A' ) nlabs = dsu->dimd;
        else if ( ltype != 'B' ) {
          std::ostringstream tmp;
          tmp << "Illegal label type: " << ltype;
          PyErr_SetString(PFF_Error, tmp.str().c_str());
          return 0;
        }
        for(int pass=0; pass<2; ++pass) {
          if ( pass == 1 ) buf = new char[len];
          len = 0;
          char *bloc = buf;
          for(int i=0; i<nblk; ++i) {
            int blen = 0;
            PFF::PFFblock_uniform *blk = dsu->block[i];
            char **labs = blk->glabel;
            if ( ltype == 'A' ) labs = blk->dlabel;
            if ( ltype == 'B' ) labs = &(blk->blabel);
            if (labs) blen = process_labels(labs, nlabs, bloc);
            else {
              blen = nlabs;
              if ( pass == 1 ) {
                for(char* j=bloc;j<bloc+nlabs; ++j) *j = '\0'; 
              }
            }
            len += blen;
            if ( pass == 1 ) bloc += blen;
          }
        }
      }
      break;
    case PFF::PFTNF3:
    case PFF::PFTNV3:
    case PFF::PFTNG3:
    case PFF::PFTNI3:
    case PFF::PFTNGD:
      {
        PFF::PFFds_nonuniform *dsn = (PFF::PFFds_nonuniform *) dsany;
        int nblk = dsn->nblk;
        int nlabs = 1;
        if ( ltype == 'S' ) nlabs = dsn->dims;
        else if ( ltype == 'A' ) nlabs = dsn->dimd;
        else if ( ltype != 'B' ) {
          std::ostringstream tmp;
          tmp << "Illegal label type: " << ltype;
          PyErr_SetString(PFF_Error, tmp.str().c_str());
          return 0;
        }
        for(int pass=0; pass<2; ++pass) {
          if ( pass == 1 ) buf = new char[len];
          len = 0;
          char *bloc = buf;
          for(int i=0; i<nblk; ++i) {
            int blen = 0;
            PFF::PFFblock_nonuniform *blk = dsn->block[i];
            char **labs = blk->glabel;
            if ( ltype == 'A' ) labs = blk->dlabel;
            if ( ltype == 'B' ) labs = &(blk->blabel);
            if (labs) blen = process_labels(labs, nlabs, bloc);
            else {
              blen = nlabs;
              if ( pass == 1 ) {
                for(char* j=bloc;j<bloc+nlabs; ++j) *j = '\0'; 
              }
            }
            len += blen;
            if ( pass == 1 ) bloc += blen;
          }
        }
      }
      break;
    case PFF::PFTVTX:
      {
        PFF::PFFds_vertex *dsv = (PFF::PFFds_vertex *) dsany;
        int nlabs = 0;
        if ( ltype == 'S' ) nlabs = dsv->dims;
        else if ( ltype == 'A' ) nlabs = dsv->dimd;
        else {
          std::ostringstream tmp;
          tmp << "Illegal label type: " << ltype;
          PyErr_SetString(PFF_Error, tmp.str().c_str());
          return 0;
        }
        for(int pass=0; pass<2; ++pass) {
          if ( pass == 1 ) buf = new char[len];
          len = 0;
          char **labs = dsv->vlabel;
          if ( ltype == 'A' ) labs = dsv->dlabel;
          if (labs) len += process_labels(labs, nlabs, buf);
          else {
            len = nlabs;
            if ( pass == 1 ) {
              for(char* j=buf;j<buf+nlabs; ++j) *j++ = '\0'; 
            }
          }
        }
      }
      break;
    case PFF::PFTIFL:
      break;
    default:
      std::ostringstream tmp;
      tmp << "Dataset raw type unknown: " << rawtype;
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
      break;
    }
    if ( !buf ) return Py_BuildValue("");

    PyObject *r = Py_BuildValue("s#", buf, len-1);
    delete [] buf;
    return r;
 
  }

  PyObject *get_num_arrays(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    int handle = 0;
    char *dtype = 0;
    int block = 0;
    int shft = 0;

    static char arg1[] = "handle";
    static char arg2[] = "datatype";
    static char arg3[] = "block";
    static char arg4[] = "shift";
    static char *argnames[] = { arg1, arg2, arg3, arg4, 0 };

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"is|ii", argnames, 
                             &handle, &dtype, &block, &shft) ) return 0;

    //cout << handle << " " << dtype << " " << block << endl;

    PFF_DS *dset = dsmap[handle];
    PFF::PFFds_any *dsany = dset->Get_DSany();
    int rawtype = dsany->head->rawtype;
    char type = 'f';
    int size = sizeof(float);
    char structtype = 0;
    int nblk = 1;
    bool is_blocktype = false;
    switch (rawtype)  {
    case PFF::PFTUF3:
    case PFF::PFTUF1:
      structtype = 'u';
      nblk = ((PFF::PFFds_uniform *) dsany)->nblk;
      is_blocktype = true;
      break;
    case PFF::PFTNF3:
    case PFF::PFTNV3:
    case PFF::PFTNG3:
    case PFF::PFTNI3:
    case PFF::PFTNGD:
      structtype = 'n';
      nblk = ((PFF::PFFds_nonuniform *) dsany)->nblk;
      is_blocktype = true;
      break;
    case PFF::PFTVTX:
      structtype = 'v';
      break;
    case PFF::PFTIFL:
      structtype = 'i';
      break;
    default:
      std::ostringstream tmp;
      tmp << "Dataset raw type unknown: " << rawtype;
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
      break;
    }
    if ( is_blocktype && (block < 0 || block >= nblk) ) {
      std::ostringstream tmp;
      tmp << "Illegal block index: " << block;
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
    }
    char *buf = 0;
    char *abuf = 0;
    float *fbuf = 0;
    int *ibuf = 0;
    long *lbuf = 0;
    int len = 0;
    bool bad_dtype = false;
    switch (structtype)  {
    case 'u':
      {
        PFF::PFFds_uniform *dsu = (PFF::PFFds_uniform *) dsany;
        PFF::PFFblock_uniform *blk = dsu->block[block];

        len = 1;
        int dims = dsu->dims;
        int dimd = dsu->dimd;
        if ( std::strcmp(dtype,"spare") == 0 ) {
          len = blk->nspare;
          ibuf = blk->spare;
        }
        else if ( std::strcmp(dtype,"nx") == 0 ) {
          len = dims;
          lbuf = blk->nx;
        }
        else if ( std::strcmp(dtype,"x") == 0 ) {
          len = 2*dims;
          abuf = new char[len*sizeof(float)];
          fbuf = (float *) abuf;
          float *fptr = fbuf;
          float *gptr = fbuf + dims;
          for(int i=0; i<dims; ++i) {
            *fptr++ = blk->x0[i];
            *gptr++ = blk->dx[i];
          }
        }
        else if ( dimd >0 && std::strcmp(dtype,"data") == 0 ) {
          if ( shft < 0 || shft >= dimd ) {
            std::ostringstream tmp;
            tmp << "Illegal shift index: " << shft;
            PyErr_SetString(PFF_Error, tmp.str().c_str());
            return 0;
          }
          len = 1;
          for(int i=0; i<dims; ++i) len *= blk->nx[i];
          fbuf = *(blk->data + shft);
        }
        else bad_dtype = true;
      }
      break;
    case 'n':
      {
        PFF::PFFds_nonuniform *dsn = (PFF::PFFds_nonuniform *) dsany;
        PFF::PFFblock_nonuniform *blk = dsn->block[block];

        len = 1;
        int dims = dsn->dims;
        int dimd = dsn->dimd;
        if ( std::strcmp(dtype,"spare") == 0 ) {
          len = blk->nspare;
          ibuf = blk->spare;
        }
        else if ( std::strcmp(dtype,"nx") == 0 ) {
          len = dims;
          lbuf = blk->nx;
        }
        else if ( std::strcmp(dtype,"x") == 0 ) {
          if ( shft < 0 || shft >= dims ) {
            std::ostringstream tmp;
            tmp << "Illegal shift index: " << shft;
            PyErr_SetString(PFF_Error, tmp.str().c_str());
            return 0;
          }
          len = blk->nx[shft];
          fbuf = *(blk->x + shft);
        }
        else if ( dimd >0 && std::strcmp(dtype,"data") == 0 ) {
          if ( shft < 0 || shft >= dimd ) {
            std::ostringstream tmp;
            tmp << "Illegal shift index: " << shft;
            PyErr_SetString(PFF_Error, tmp.str().c_str());
            return 0;
          }
          len = 1;
          for(int i=0; i<dims; ++i) len *= blk->nx[i];
          if ( blk->data ) fbuf = *(blk->data + shft);
          else ibuf = *(blk->idata + shft);
        }
        else bad_dtype = true;
      }
      break;
    case 'v':
      {
        PFF::PFFds_vertex *dsv = (PFF::PFFds_vertex *) dsany;

        len = 0;
        int dimd = dsv->dimd;
        if ( std::strcmp(dtype,"spare") == 0 ) {
          len = dsv->nspare;
          ibuf = dsv->spare;
        }
        else if ( std::strcmp(dtype,"x") == 0 ) {
          len = dsv->dims*dsv->nv;
          fbuf = dsv->vert;
        }
        else if ( dimd > 0 && std::strcmp(dtype,"data") == 0 ) {
          if ( shft < 0 || shft >= dimd ) {
            std::ostringstream tmp;
            tmp << "Illegal shift index: " << shft;
            PyErr_SetString(PFF_Error, tmp.str().c_str());
            return 0;
          }
          len = dsv->nv;
          fbuf = *(dsv->data + shft);
        }
        else bad_dtype = true;
      }
      break;
    case 'i':
      {
        PFF::PFFds_ifl *dsi = (PFF::PFFds_ifl *) dsany;

        len = 0;
        if ( std::strcmp(dtype,"iarray") == 0 ) {
          len = dsi->ni;
          ibuf = dsi->iarr;
        }
        else if ( std::strcmp(dtype,"farray") == 0 ) {
          len = dsi->nf;
          fbuf = dsi->farr;
        }
        else if ( std::strcmp(dtype,"flist") == 0 ) {
          len = dsi->nflt;
          fbuf = dsi->flist;
        }
        else bad_dtype = true;
      }
      break;
    default:
      break;
    }
    if ( bad_dtype ) {
      std::ostringstream tmp;
      tmp << "Illegal data type: " << dtype;
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
    }
    if ( len == 0 )  return Py_BuildValue("");

    if ( fbuf ) {
      buf = (char *) fbuf;
    }
    else if ( ibuf ) {
      buf = (char *) ibuf;
      size = sizeof(int);
      type = 'i';
    }
    else if ( lbuf ) {
      buf = (char *) lbuf;
      size = sizeof(long);
      type = 'i';
    }
    else return Py_BuildValue("");

    PyObject *r = Py_BuildValue("[ciis#]", type, size, len, buf, size*len);
    if ( abuf ) delete [] abuf;
    return r;
 
  }

  PyObject *pff_write_ifl(PyObject *self, PyObject *args)
  {
    int rawtype = 0;
    int apptype = 0;
    char *tname = 0;
    char *title = 0;
    int ialen = 0, falen = 0, fllen = 0, iasz = 0, fasz = 0, flsz = 0;
    char cia, cfa, cfl;
    char *iabuf = 0;
    char *fabuf = 0;
    char *flbuf = 0;
    int file = 0;

//345678901234567890123456789012
    if ( !PyArg_ParseTuple(args,"(iiss)(cis#)(cis#)(cis#)|i", 
                           &rawtype, &apptype, &tname, &title,
                           &cia, &iasz, &iabuf ,&ialen,
                           &cfa, &fasz, &fabuf ,&falen,
                           &cfl, &flsz, &flbuf ,&fllen, &file) ) return 0;
    int ierr = 0;
    PFF::PFFfid *fid = 0;
    if ( filemap.empty() ) {
      PyErr_SetString(PFF_Error, "No PFF files currently open");
      return 0;
    }
    if ( file > 0 ) fid = PFF::pf_get_fid(file, &ierr);
    else fid = PFF::PFF.current;
    if ( fid == 0 ) {
      std::ostringstream tmp;
      tmp << "File \"" << file << "\" not open";
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
    }
    long ni = ialen/iasz;
    long nf = falen/fasz;
    long nfl = fllen/flsz;
         
    cout << rawtype << " " << apptype << " \"" << tname << "\" \"" << title
         << "\" " << ni << " " << nf << " " << nfl << endl;

    assert(iasz == sizeof(int));
    assert(fasz == sizeof(float));
    assert(flsz == sizeof(float));

    int *iarray = 0;
    if ( ni ) iarray = (int *)iabuf;
    float *farray = 0;
    if ( nf ) farray = (float *)fabuf;
    float *flist = 0;
    if ( nfl ) flist = (float *)flbuf;
#if 0
    if ( ni ) {
      int *iarray = (int *)iabuf;
      for(int i = 0; i<ni; ++i) cout << i << " " << iarray[i] << endl;
    }
    if ( nf ) {
      float *farray = (float *)fabuf;
      for(int i = 0; i<nf; ++i) cout << i << " " << farray[i] << endl;
    }
    if ( ni ) {
      float *flist = (float *)flbuf;
      for(int i = 0; i<nfl; ++i) cout << i << " " << flist[i] << endl;
    }
#endif
    PFF::PFFds_ifl *ds = PFF::pf_bld_ifl(apptype, title, tname, ni, nfl, nf,
                                         iarray, flist, 0, farray, 0,
                                         PFF::TRUE, PFF::TRUE, &ierr);
    if ( ierr ) {
      PyErr_SetString(PFF_Error, "Error creating IFL dataset");
      return 0;
    }
    PFF::pf_wr_ifl(fid, (PFF::PFFds_any *)ds, &ierr);
    PFF::pf_free_ifl((PFF::PFFds_any *)ds, &ierr);
    if ( ierr ) {
      PyErr_SetString(PFF_Error, "Error writing dataset to file");
      return 0;
    }

    return Py_BuildValue("");
  }

  PyObject *pff_write_vtx(PyObject *self, PyObject *args)
  {
    int rawtype = 0;
    int apptype = 0;
    char *tname = 0;
    char *title = 0;
    int lablen = 0, ialen = 0, xalen = 0, dalen = 0, iasz = 0, xasz = 0, 
      dasz = 0;
    int sdim, adim, nvi;
    char cia, cxa, cda;
    char *labbuf = 0;
    char *iabuf = 0;
    char *xabuf = 0;
    char *dabuf = 0;
    int file = 0;

//345678901234567890123456789012
    if ( !PyArg_ParseTuple(args,"(iiss)(iii)s#(cis#)(cis#)(cis#)|i", 
                           &rawtype, &apptype, &tname, &title,
                           &sdim, &adim, &nvi, &labbuf, &lablen,
                           &cia, &iasz, &iabuf ,&ialen,
                           &cxa, &xasz, &xabuf ,&xalen,
                           &cda, &dasz, &dabuf ,&dalen, &file) ) return 0;
    int ierr = 0;
    PFF::PFFfid *fid = 0;
    if ( filemap.empty() ) {
      PyErr_SetString(PFF_Error, "No PFF files currently open");
      return 0;
    }
    if ( file > 0 ) fid = PFF::pf_get_fid(file, &ierr);
    else fid = PFF::PFF.current;
    if ( fid == 0 ) {
      std::ostringstream tmp;
      tmp << "File \"" << file << "\" not open";
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
    }
    long nv = nvi;

    if (ialen) assert(iasz == sizeof(int));
    if (sdim) assert(xasz == sizeof(float));
    if (adim) assert(dasz == sizeof(float));

    int *spare = 0;
    int nspare = ialen/iasz;
    if ( nspare ) spare = (int *)iabuf;
    float *xa = 0;
    if ( xalen ) xa = (float *)xabuf;
    float *dataa = 0;
    if ( dalen ) dataa = (float *)dabuf;
#if 0      
    cout << rawtype << " " << apptype << " \"" << tname << "\" \"" << title
         << "\" " << ialen << " " << xalen << " " << dalen << " " << lablen
         << endl;
    cout << sdim << " " << adim << " " << nv << endl; 
    char *s = labbuf;
    int nlen = sdim;
    for(int p=0; p<2; ++p) {
      for(int i=0;i<nlen; ++i) {
        cout << i << " \"" << s << "\"" << endl;
        s += (std::strlen(s) + 1);
      }
      nlen = adim;
    }
    cout << s - labbuf << endl;
    if ( spare ) {
      for(int i = 0; i<nspare; ++i) cout << i << " " << spare[i] << endl;
    }
    if ( xa ) {
      for(int i = 0; i<xalen/xasz; ++i) cout << i << " " << xa[i] << endl;
    }
    if ( dataa ) {
      for(int i = 0; i<dalen/dasz; ++i) cout << i << " " << dataa[i] << endl;
    }
#endif
    char *s = labbuf;
    char **glabs = 0;
    if ( sdim ) {
      glabs = new char *[sdim];
      for(int i=0;i<sdim; ++i) {
        glabs[i] = s;
        s += (std::strlen(s) + 1);
      }
    }
    char  **dlabs = 0;
    float **data = 0;
    float  *fptr = dataa;
    int    *doff = 0;
    if ( adim ) {
      dlabs = new char *[adim];
      data = new float *[adim];
      doff = new int[adim];
      for(int i=0;i<adim; ++i) {
        dlabs[i] = s;
        doff[i] = 0;
        data[i] = fptr;
        s += (std::strlen(s) + 1);
        fptr += nv;
      }
    }
    assert(s-labbuf == lablen);

    PFF::PFFds_vertex *ds = PFF::pf_bld_vertex(apptype, title, tname, sdim,
                                               adim, nv, nspare, 0, spare,
                                               glabs, dlabs, xa, 0, data, doff,
                                               PFF::TRUE, PFF::TRUE,
                                               PFF::TRUE, &ierr);
    delete [] glabs;
    delete [] dlabs;
    delete [] data;
    delete [] doff;

    if ( ierr ) {
      PyErr_SetString(PFF_Error, "Error creating VERTEX dataset");
      return 0;
    }
    PFF::pf_wr_vertex(fid, (PFF::PFFds_any *)ds, &ierr);
    PFF::pf_free_vertex((PFF::PFFds_any *)ds, &ierr);
    if ( ierr ) {
      PyErr_SetString(PFF_Error, "Error writing dataset to file");
      return 0;
    }
    return Py_BuildValue("");
  }

  PyObject *pff_bld_UorNds(PyObject *self, PyObject *args)
  {
    int rawtype = 0;
    int apptype = 0;
    char *tname = 0;
    char *title = 0;
    int sdim = 0, adim = 0, nblk = 0;

//345678901234567890123456789012
    if ( !PyArg_ParseTuple(args,"(iiss)(iii)", 
                           &rawtype, &apptype, &tname, &title,
                           &sdim, &adim, &nblk) ) return 0;
    int ierr = 0;
    char structtype = 0;
    switch (rawtype)  {
    case PFF::PFTUF3:
    case PFF::PFTUF1:
      structtype = 'u';
      break;
    case PFF::PFTNF3:
    case PFF::PFTNV3:
    case PFF::PFTNG3:
    case PFF::PFTNI3:
    case PFF::PFTNGD:
      structtype = 'n';
      break;
    default:
      std::ostringstream tmp;
      tmp << "Illegal dataset ";
      PyErr_SetString(PFF_Error, "Illegal raw dataset type");
      return 0;
      break;
    }
#if 0
    cout << rawtype << " " << apptype << " \"" << tname << "\" \"" << title
         << "\"" << endl;
    cout << structtype << " " << sdim << " " << adim << " " << nblk << endl;
#endif
    UorNds.Clear();
    assert(!UorNds.InUse());
    UorNds.typekey = structtype;
    UorNds.sdim = sdim;
    UorNds.adim = adim;
    UorNds.nblk = nblk;
    if ( structtype == 'u' ) {
      UorNds.UorNds = (PFF::PFFds_any *) 
        PFF::pf_bld_uniform(apptype, title, tname, sdim, adim, nblk,
                        PFF::TRUE, &ierr);
    }
    else {
      UorNds.UorNds = (PFF::PFFds_any *) 
        PFF::pf_bld_nonuniform(apptype, title, tname, rawtype, sdim, adim, nblk,
                               PFF::TRUE, &ierr);
    }
    return Py_BuildValue("");
  }

  PyObject *pff_fill_UorNds(PyObject *self, PyObject *args)
  {
    int lablen = 0, splen = 0, nxlen = 0, xalen = 0, dalen = 0, spsz = 0,
      nxsz = 0, xasz = 0,  dasz = 0;
    char csp, cnx, cxa, cda;
    char *labbuf = 0;
    char *spbuf = 0;
    char *nxbuf = 0;
    char *xabuf = 0;
    char *dabuf = 0;

//345678901234567890123456789012
    if ( !PyArg_ParseTuple(args,"s#(cis#)(cis#)(cis#)(cis#)", 
                           &labbuf, &lablen,
                           &csp, &spsz, &spbuf ,&splen,
                           &cnx, &nxsz, &nxbuf ,&nxlen,
                           &cxa, &xasz, &xabuf ,&xalen,
                           &cda, &dasz, &dabuf ,&dalen) ) return 0;

    assert(splen == 0 || spsz == sizeof(int));
    assert(nxlen == 0 || nxsz == sizeof(long));
    assert(xalen == 0 || xasz == sizeof(float));
    assert(dalen == 0 || dasz == sizeof(float));

    int ierr = 0;
    int *spare = 0;
    int nspare = splen/spsz;
    if ( nspare ) spare = (int *)spbuf;
    long *nx = 0;
    if ( nxlen ) nx = (long *)nxbuf;
    float *xa = 0;
    if ( xalen ) xa = (float *)xabuf;
    float *dataa = 0;
    int *idataa = 0;
    if ( dalen ) {
      if (cda == 'f') dataa = (float *)dabuf;
      else idataa = (int *)dabuf;
    }
#if 0
    cout << splen << " " << nxlen << " " << xalen << " " << dalen << " "
         << lablen << endl;
    char *s = labbuf;
    int nlen = UorNds.sdim;
    for(int p=0; p<3; ++p) {
      for(int i=0;i<nlen; ++i) {
        cout << i << " \"" << s << "\"" << endl;
        s += (std::strlen(s) + 1);
      }
      if (p == 0) nlen = UorNds.adim;
      else nlen = 1;  //block label
    }
    cout << s - labbuf << endl;
    if ( spare ) {
      for(int i = 0; i<nspare; ++i) cout << i << " " << spare[i] << endl;
    }
    if ( nx ) {
      for(int i = 0; i<nxlen/nxsz; ++i) cout << i << " " << nx[i] << endl;
    }
    if ( xa ) {
      int xlen = xalen/xasz;
      int skip = std::max(xlen/30,1);
      for(int i = 0; i<xlen; i+=skip) cout << i << " " << xa[i] << endl;
    }
    if ( dataa ) {
      int dlen = dalen/dasz;
      int skip = std::max(dlen/30,1);
      for(int i = 0; i<dlen; i+=skip) cout << i << " " << dataa[i] << endl;
    }
#else
    assert( UorNds.InUse() && UorNds.curblk < UorNds.nblk );

    char *s = labbuf;
    int nlen = UorNds.sdim; // grid labels first pass
    char **glabs = new char *[UorNds.sdim];
    char **dlabs = 0;
    char *blab = 0;
    if (UorNds.adim > 0 ) dlabs = new char *[UorNds.adim];
    for(int p=0; p<3; ++p) {
      for(int i=0;i<nlen; ++i) {
        if ( p == 0 ) glabs[i] = s;
        else if ( p == 1 ) dlabs[i] = s;
        else blab = s;
        s += (std::strlen(s) + 1);
      }
      if (p == 0) nlen = UorNds.adim; // attribute labels next pass (p=1)
      else if (p == 1) nlen = 1;  //block label next pass (p=2)
    }
    assert(s-labbuf == lablen);

    void **data = 0;
    int **idata = 0;
    float **fdata = 0;
    if ( dalen ) {
      int dlen1 = 1;
      for (int i=0; i<UorNds.sdim; ++i ) dlen1 *= nx[i];
      if ( dataa ) {
        fdata = new float *[UorNds.adim];
        float *floc = dataa;
        for (int i=0; i<UorNds.adim; ++i ) {
          fdata[i] = floc;
          floc += dlen1;
        }
        assert(floc-dataa == dalen/dasz);
        data = (void **)fdata;
      }
      else {
        idata = new int *[UorNds.adim];
        int *iloc = idataa;
        for (int i=0; i<UorNds.adim; ++i ) {
          idata[i] = iloc;
          iloc += dlen1;
        }
        assert(iloc-idataa == dalen/dasz);
        data = (void **)idata;
      }
    }
    if ( UorNds.typekey == 'u' ) {
      float *x0 = xa;
      float *dx = xa + UorNds.sdim;
      assert(fdata != 0);
      PFF::pf_fill_ublock((PFF::PFFds_uniform *)UorNds.UorNds, UorNds.curblk,
                          nspare, 0, spare, nx, x0, dx, 0, fdata, 0,
                          glabs, dlabs, blab, PFF::TRUE, PFF::TRUE, &ierr);
    }
    else {
      float **x = new float *[UorNds.sdim];
      float *floc = xa;
      for (int i=0; i<UorNds.sdim; ++i ) {
        x[i] = floc;
        floc += nx[i];
      }
      assert(floc-xa == xalen/xasz);
      PFF::pf_fill_nblock((PFF::PFFds_nonuniform *)UorNds.UorNds, UorNds.curblk,
                          nspare, 0, spare, nx, x, 0, data, 0,
                          glabs, dlabs, blab, PFF::TRUE, PFF::TRUE, &ierr);
      delete [] x;
    }
    if ( cda == 'f' ) delete [] fdata;
    else delete [] idata;
    delete [] dlabs;
    delete [] glabs;
#endif

    ++UorNds.curblk;
    return Py_BuildValue("");
  }

  PyObject *pff_write_UorNds(PyObject *self, PyObject *args)
  {
    int file = 0;

//345678901234567890123456789012
    if ( !PyArg_ParseTuple(args,"|i", &file) ) return 0;
    int ierr = 0;
    PFF::PFFfid *fid = 0;
    if ( filemap.empty() ) {
      PyErr_SetString(PFF_Error, "No PFF files currently open");
      return 0;
    }
    if ( file > 0 ) fid = PFF::pf_get_fid(file, &ierr);
    else fid = PFF::PFF.current;
    if ( fid == 0 ) {
      std::ostringstream tmp;
      tmp << "File \"" << file << "\" not open";
      PyErr_SetString(PFF_Error, tmp.str().c_str());
      return 0;
    }
    assert( UorNds.InUse() && UorNds.nblk == UorNds.curblk );
    PFF::pf_wr_ds(fid, UorNds.UorNds, &ierr);
    PFF::pf_free_ds(UorNds.UorNds, &ierr);
    UorNds.Clear();
    if ( ierr ) {
      PyErr_SetString(PFF_Error, "Error writing dataset to file");
      return 0;
    }
    return Py_BuildValue("");
  }

  PyObject *pff_scanlist(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    int nlist = 0, buflen = 0, exact = 0, getmatch = 1;
    char *buf = 0;
    char *find = 0;
    static char arg1[] = "nlist";
    static char arg2[] = "list";
    static char arg3[] = "findstring";
    static char arg4[] = "exactcase";   // defaults to 0
    static char arg5[] = "match";       // defaults to 1
    static char *argnames[] = { arg1, arg2, arg3, arg4, arg5, 0 };

    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"is#s|ii", argnames, &nlist,
                                      &buf, &buflen, &find, &exact,
                                      &getmatch) ) return 0;
#if 0
    cout << buflen << " " << nlist << " " << exact << " " << getmatch
         << endl;
    char *s = buf;
    for(int i=0;i<nlist; ++i) {
        cout << i << " \"" << s << "\"" << endl;
        s += (std::strlen(s) + 1);
      }
    return Py_BuildValue("");
#else
    char *s = buf;
    StringMatch matcher(find, exact);
    bool wantMatch = getmatch != 0;
    std::vector<int> list;
    list.reserve(nlist);
    for(int i=0;i<nlist; ++i) {
      bool ismatch = matcher.IsMatch(string(s));
      if ( ismatch == wantMatch ) list.push_back(i);
      s += (std::strlen(s) + 1);
    }
    //for(int i=0; i<list.size(); ++i) cout << i << " " << list[i] << endl;
    int nmat = list.size();
    int size = sizeof(int);
    int lmat = nmat*size;
    char *plist = buf;
    if ( !list.empty() ) plist = (char *)(&list[0]);
    return Py_BuildValue("[ciis#]",'i',size,nmat,plist,lmat);
#endif
  }

  PyObject *hit_any_key(PyObject *self, PyObject *args, PyObject *kwargs)
  {
    static char defprompt[] = "Hit any key: ";
    char *prompt = defprompt;
    static char arg1[] = "prompt";
    static char *argnames[] = { arg1, 0 };

//345678901234567890123456789012
    if ( !PyArg_ParseTupleAndKeywords(args,kwargs,"|s", argnames, &prompt) )
      return 0;

    struct termios old_tio, new_tio;
    unsigned char c;

    printf(prompt);
    //system("/bin/stty  -icanon min 1 time 0");
    /* get the terminal settings for stdin */
    tcgetattr(STDIN_FILENO,&old_tio);

    /* we want to keep the old setting to restore them a the end */
    new_tio=old_tio;

    /* disable canonical mode (buffered i/o) and local echo */
    new_tio.c_lflag &=(~ICANON & ~ECHO);

    /* set the new settings immediately */
    //tcsetattr(STDIN_FILENO,TCSANOW,&new_tio);
    //tcsetattr(STDIN_FILENO,TCSADRAIN,&new_tio);
    tcsetattr(STDIN_FILENO,TCSAFLUSH,&new_tio);

    c = getchar();
    //system("/bin/stty sane");
    /* restore the former settings */
    tcsetattr(STDIN_FILENO,TCSANOW,&old_tio);

    if (c != '\n') printf("\n");

    return Py_BuildValue("i",c);
  }
};

static PyMethodDef pff_extmethods[] = {
  { "get_type_names", (PyCFunction) get_type_names, METH_VARARGS, ccGetTypes },
  { "get_ctype_sizes", (PyCFunction) get_ctype_sizes, METH_VARARGS,
    ccGetCtypes },
  { "close", (PyCFunction) close_pff_file, METH_VARARGS, ccClose },
  { "setcurfile", (PyCFunction) set_pff_file, METH_VARARGS, ccSetFile },
  { "filelist", (PyCFunction) pff_file_list, METH_VARARGS | METH_KEYWORDS,
    ccShow },
  { "u_i2f", (PyCFunction) pff_util_i2f, METH_VARARGS | METH_KEYWORDS,
    ccI2f },
  { "u_i2d", (PyCFunction) pff_util_i2d, METH_VARARGS | METH_KEYWORDS,
    ccI2d },
  { "u_i2l", (PyCFunction) pff_util_i2l, METH_VARARGS | METH_KEYWORDS,
    ccI2l },
  { "u_f2i", (PyCFunction) pff_util_f2i, METH_VARARGS | METH_KEYWORDS,
    ccF2i },
  { "u_d2i", (PyCFunction) pff_util_d2i, METH_VARARGS | METH_KEYWORDS,
    ccD2i },
  { "u_l2i", (PyCFunction) pff_util_l2i, METH_VARARGS | METH_KEYWORDS,
    ccL2i },
  { "fp_precision", (PyCFunction) pff_fp_precision,
    METH_VARARGS | METH_KEYWORDS, ccFPrec },
  { "getmatch", (PyCFunction) pff_get_match, METH_VARARGS | METH_KEYWORDS,
    ccGetMatch },
  { "dslist", (PyCFunction) pff_ds_list, METH_VARARGS | METH_KEYWORDS, ccDir },
  { "open", (PyCFunction) open_pff_file, METH_VARARGS | METH_KEYWORDS, ccOpen },
  { "advance_ds_pointer", (PyCFunction) pff_adv_dsp,
    METH_VARARGS | METH_KEYWORDS, ccAdvDsp },
  { "readhdr", (PyCFunction) read_ds_header, METH_VARARGS | METH_KEYWORDS,
    ccGetHdr },
  { "getfilename", (PyCFunction) pff_getfilename, METH_VARARGS | METH_KEYWORDS,
    ccGetFName },
  { "releaseDSHandle", (PyCFunction) release_ds_handle, METH_VARARGS,
    ccRelHdl },
  { "get_labels", (PyCFunction) get_labels, METH_VARARGS | METH_KEYWORDS,
    ccGetLbls },
  { "get_num_arrays", (PyCFunction) get_num_arrays,
    METH_VARARGS | METH_KEYWORDS, ccGetNumAry },
  { "write_ifl", (PyCFunction) pff_write_ifl, METH_VARARGS, ccWrIFL },
  { "write_vtx", (PyCFunction) pff_write_vtx, METH_VARARGS, ccWrVtx },
  { "bld_multiblkds", (PyCFunction) pff_bld_UorNds, METH_VARARGS, ccBldUorN },
  { "fill_multiblkds", (PyCFunction) pff_fill_UorNds, METH_VARARGS,
    ccFillUorN },
  { "write_multiblkds", (PyCFunction) pff_write_UorNds, METH_VARARGS,
    ccWrUorN },
  { "scanlist", (PyCFunction) pff_scanlist, METH_VARARGS | METH_KEYWORDS,
    ccScanList },
  { "hak", (PyCFunction) hit_any_key, METH_VARARGS | METH_KEYWORDS, ccHAK },
  { 0, 0, 0, 0 }
};

PyMODINIT_FUNC initpff_ext(void) {
  char ename[] = "pff_ext.Error";
  PyObject *m = Py_InitModule3("pff_ext", pff_extmethods,ccModule);
  PyObject *d = PyModule_GetDict(m);
  PFF_Error = PyErr_NewException(ename, 0, 0);
  PyDict_SetItemString(d,"PFF_Error",PFF_Error);
}
