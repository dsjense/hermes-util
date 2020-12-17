# $Id$
# 
# Copyright (2014) David Seidel.
# 
# Hermes is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
# 
# Hermes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General
# Public License along with Hermes.  If not, see
# <http://www.gnu.org/licenses/>.
# 
import exceptions
import types
import math
import re as regex
import pff
import pff_ext as pex
import plots
from plots import _ovrplt
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.lines
import copy as cpy
import utils
import scipy.signal

__doc__ = \
'''Set of PFMPL functions for reading, writing, plotting, and otherwise
manipulating one-dimensional PFF datasets.'''

__all__ = [ ]

__all__.append('re')
def re(*args, **kwargs):
    '''Read waveform data from a PFF file and store in WDF arrays.

Usage:
  re( wdfa, dataset, [fileid=integer], [xydata=bool], [zero=val],
      [skip=integer]] )

Arguments:
  wdfa:    Integer index of WDF array to receive waveform or a list of integer
           WDF array numbers to receive waveforms.
  dataset: Integer index of dataset to be read, a list of integer dataset 
           indices to be read, or a string to be matched to the comments
           (titles) of all datasets in the file.
  fileid:  Index of PFF file from which to read datasets.
  xydata:  If True and the dataset contains uniform data, it will be stored
           in the WDF array as if it were non-uniform X-Y data. IF XYDATA is
           False and the dataset contains non-uniform data, it will be
           interpolated to a uniform grid and stored in the WDF array in
           uniform format.
  zero:    If the dataset contains uniform data, and the X0 = DX, an
           additional datapoint is prepended to the data, with X = 0 and
           Y = y0. If "zero" is a number, y0 = zero. If "zero" is the
           string 'DIR' or 'NEU', y0 equals 0 or Y[X0], respectively.
  skip:    If skip > 1, the data is "thinned" by using only every `skip'
           data values.

  Note: If WDFA and DATASET are both lists, they must be of equal
        length. If one is an integer and one is a list, the integer
        value is replaced by a list of consecutive integers starting
        with the original integer and whose length matches the length
        of the original list. If DATASET is a string, it is first
        replaced by a list of indices of datasets whose comments
        (titles) match the string. Depending on whether WDFA is an
        integer or list, the conventions previously described are
        applied.

Return value: Returns the number of datasets read if successful,
              or None on error'''

    DN = 'RE'
    argnames = [ 'wdfa', 'dataset' ]
    optvals = [ ]
    kwdefs = { 'fileid':0, 'xydata':None,  'zero':None, 'skip':1 }

    res = utils.process_args(args, kwargs, 'WRI', argnames, kwdefs, \
                             optvals, extra=False)

    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + re.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    wdl = utils.parsewsl(_wdflist, wdfa)
    if wdl is None:
        print DN + ": WDFA must be an integer or a list of integers"
        return None

    okay = True
    if type(dataset) is types.ListType:
        dsl = dataset
        for i in range(len(dataset)):
            if type(dataset[i]) is not types.IntType: okay = False
    elif type(dataset) is types.IntType:
        dsl = [dataset]
    elif type(dataset) is types.StringType:
        try:
            tmp = pex.getmatch(dataset,fileid)
        except pex.PFF_Error, e:
            print DN + ": Error:", e
            return None
        if tmp[2] > 0:
            dsl = pff.buf2list(tmp)
        else:
            print DN + ": No datasets matching \"" + dataset + "\" found"
            return 0
        if type(wdfa) == types.ListType and len(wdl) != len(dsl):
            print DN + ": length of WDFA (" + str(len(wdl)) + \
                  ") must equal number of matching datasets found (" + \
                  str(len(dsl)) + ")"
            return None
    else: okay = False

    if not okay:
        print DN + ": DATASET must be a string, integer, or a list of integers"
        return None

    nwd = len(wdl)
    nds = len(dsl)
    if type(wdfa) == types.ListType and type(dataset) == types.ListType and \
       nwd != nds:
        print DN + ": length of WDFA (" + str(nwd) + \
            ") must equal length of DATASET (" + str(nds) + ")"
        return None

    if nwd != nds:
        if nwd == 1:
            wdl = range(wdfa, wdfa+nds)
            nwd = len(wdl)
        else:
            l0 = dsl[0]
            dsl = range(l0,l0+nwd)
            nds = len(dsl)

    assert(nwd == nds)

    narray = 0
    for i in range(nwd):
        ds = pff.read_dataset(dsl[i],fileid)
        if ds is not None:
            tkey = ds.typekey
            if ds.sdim > 1 and ds.rawtype == pff.NF3:
                nx = ds.nx[0]
                if nx[1] == 1 and nx[2] == 1:
                    ds.nx[0] = np.asarray([nx[0]])
                    ds.x[0] = [ ds.x[0][0] ]
                    ds.data[0] = ds.data[0].reshape((nx[0],))
                    ds.glabels = np.asarray(ds.glabels[0,0]).reshape((1,1))
                    ds.dlabels = np.reshape(ds.blabels,(1,1))
                    del ds.blabels
                    ds.g_eps = np.asarray(ds.g_eps[:,0]).reshape((2,1))
                    ds.g_range = np.asarray(ds.g_range[:,0,:]).reshape((2,1,2))
                    ds.rawtype = pff.NGD
                    ds.rawname = 'NGD'
                    ds.sdim = 1
            if zero is not None and ds.rawname == "UF1":
                if abs(ds.dx[0][0]-ds.x0[0][0]) < 1.e-5*ds.dx[0][0]:
                    val = zero
                    if type(zero) is types.StringType:
                        if zero[:3].upper() == 'NEU':
                            val = ds.data[0][0]
                        elif zero[:3].upper() == 'DIR':
                            val = 0.0
                        else:
                            print 'Unknown ZERO specification -- assume 0.0'
                            val = 0.0
                        
                    zar = np.array([val],dtype=ds.data[0].dtype)
                    ds.data[0] = np.concatenate((zar,ds.data[0]))
                    ds.x0[0][0] = 0.0
                    ds.nx[0][0] += 1
                    ds.g_range[:,:,0] = 0.0
            if skip > 1:
                ds.data[0] = ds.data[0][0][::skip]
                if ds.rawname == "UF1": ds.dx[0][0] *= skip
                elif ds.rawname == "NGD": ds.x[0][0] = ds.x[0][0][::skip]
                nx = ds.data[0].size
                ds.nx[0][0] = nx
                ds.g_range[:,:,1] = (nx-1)*ds.dx[0][0]
                    
                    
            if (tkey != 'U' and tkey != 'N') or \
                    ds.nblk > 1 or ds.sdim >1 or ds.adim > 1:
                print DN + ": skipping dataset",dsl[i], \
                      "-- it does not have the proper format"
            else:
                # If not NGD, there are no data labels, only the block label
                if ds.rawtype != pff.NGD:
                    ds.dlabels = np.reshape(ds.blabels,(1,1))
                    del ds.blabels

                if xydata is not None:
                    if xydata and tkey == 'U':
                        ### need to make NUNF
                        #### generate ds.x
                        xtmp = np.empty((ds.nx[0][0]),dtype=pff.PFFnp_float)
                        xtmp[:] = np.asarray(np.linspace(ds.g_range[0,0,0], \
                                                         ds.g_range[0,0,1], \
                                                         ds.nx[0][0]), \
                                             dtype=pff.PFFnp_float)
                        ds.x = [[xtmp]]
                        del ds.x0, ds.dx
                        ds.rawname='NGD'
                        ds.rawtype=pff.NGD
                        ds.typekey='N'
                        ds = pff.NUNF_dataset(new=ds.__dict__)
                        
                    elif not xydata and tkey == 'N':
                        ds = ds.make_uniform()
                        x0tmp = np.empty((1),dtype=pff.PFFnp_float)
                        x0tmp[:] = ds.g_range[0,0,0]
                        ds.x0 = [x0tmp]
                        dxtmp = np.empty((1),dtype=pff.PFFnp_float)
                        dxtmp[:] = (ds.g_range[0,0,1] - ds.g_range[0,0,0]) / \
                                (ds.nx[0][0] - 1)
                        ds.dx = [dxtmp]
                        del ds.x
                        ds.rawname='UF1'
                        ds.rawtype=pff.UF1
                        ds.typekey='U'
                        ds = pff.UNF_dataset(new=ds.__dict__)
                    
                _wdflist[wdl[i]] = ds
                narray += 1

    return narray


__all__.append('reasc')
def reasc(*args,**kwargs):
    '''Usage:
  reasc(file,nvar, [out], [format=int], [limit=int], [skip=int], \\
        [xydata=None|bool], [sortx=bool] )

Arguments:
  file:    file name, or Python file object
  narray:  Number of dependent variables in an input line, i.e.,
           (X, Y1, Y2, ..., Ynvar) for formats 1-4,
           (Y1, Y2, ..., Ynvar) for format 5
  out:     First WDF array or list of WDF arrays to receive waveforms
  format:  format of file:
              0: only data values in the file maximum of LIMIT values
              1: Number of remaining lines in first line
              2: First NVAR lines containing data labels 
              3: First line: # of points, next NVAR lines: data labels
              4: EXCEL CSV format: First line contains NVAR data labels
              5: First line: delta-X, # of points, X0 = delta-X
           All formats support space, tab, and/or comma delimiters
  limit:   Maximum number of data-containing lines to be read
  skip:    Number of initial header lines in file to be ignored
  xydata:  If True, the data will be left on a nonuniform grid.
           If False, the data unconditionally converted to a uniform grid.
           If None, the data will be converted to a uniform grid only if its
           abscissa values are already uniformly spaced.
           If a floating point number, data will be converted to a uniform grid
           if abs{X[i] - (X[0] + i*dx)}/X[i] < xydata, for all non-zero X[i], 
           where dx is the separation between the two X values closest to zero.
  sortx:   If set, sort X values before saving.

Return value: Number of points in waveforms, or None on error.'''
   
    DN = 'REASC'
    argnames = [ 'file', 'narray', 'out' ]
    optvals = [ None ]
    kwdefs = { 'format':0, 'limit':None, 'skip':0, 'xydata':None, \
               'sortx':False }
    ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, \
                             optvals, extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + reasc.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    okay = True

    if out is not None:
        t = type(out)
        if t is types.IntType:  out = [ out ]
        elif t is not types.ListType:
            print  DN + ": OUT must be an integer or list of integers"
            okay = False
        else:
            for i in out:
                if type(i) is not types.IntType:
                    print  DN + ": OUT must be an integer or list of integers"
                    okay = False
        if okay:
            dlt = narray - len(out)
            if dlt > 0:
                nxt = out[-1] + 1
                out += range(nxt,nxt+dlt)
    else:
        out = get_empty_wdf(count=narray)
        nl = min(2,narray)
        if narray == 1: tail = " " + str(out)
        else: tail = "s " + str(out) + "-" + str(out+narray-1)
        print DN+": Waveform" +tail[:nl]+ "will be written to WDF array" + tail
        out = range(out,out+narray)

    if not okay:
        print reasc.__doc__  ;  return None

    arrays = readasc(file,narray,format=format, limit=limit, skip=skip)
    if arrays is None: return None
    x,y,labs = arrays
    if format == 5:
        dx = x[0]
        npts = len(y[:,0])
        if xydata: x = np.linspace(dx,npts*dx,npts)
        ##print x,y.shape,npts
    else:
        npts = len(x)
        
        ##print x.shape,y.shape,labs

        if sortx or not xydata:
            sind = np.argsort(x)
            if len(np.where(np.subtract(sind,range(npts)) != 0)[0]) > 0:
                ##print "needs sort"
                x = x[sind]
                y = y[sind,:]
            ##else: print "does't need sort"

    t = type(file)
    if t is types.StringType: fname = file
    else: fname = file.name
    dct = {'apptype':3, 'nblk':1, 'adim':1, 'sdim':1, 'rfu':None, \
           'typename':'Time History', 'file':fname, 'spare':[None] }
    dct['nx'] = [ np.array([npts]) ]
    dct['blabels'] = np.array([''])
    tmp = [ np.array(['']) ]
    dct['dlabels'] = np.reshape(tmp,(1,1))
    nlabs = len(labs)
    if nlabs > narray:
        tmp = np.array([labs[0]])
        labs = labs[1:]
    dct['glabels'] = np.reshape(tmp,(1,1))
    dct['g_range'] = np.empty((2,1,2),dtype=pff.PFFnp_float)
    dct['g_eps'] = np.empty((2,1),dtype=pff.PFFnp_float)
    dct['x'] = [[ x ]]
    dct['rawname'] = 'NGD'
    dct['rawtype'] = pff.NGD
        
    ## '':, '':, '':, '':, '':, '':,
    ## remaining:  ,g_range,g_eps,typekey
    ## dx,x0
    for i in range(narray):
        dct['data'] = [ y[:,i] ]
        if i < nlabs: dct['title'] = labs[i]
        else:  dct['title'] = ''
        ds = pff.NUNF_dataset(new=dct)
        ds.fill_grid_range()
        ##print ds.g_range
        ##print ds.g_eps
        mkUniform = False
        if format == 5 and not xydata:  mkUniform = True
        elif not xydata:  # None or False
            uds = ds.make_uniform(quiet=1)
            ##print uds, xydata
            if uds is None or xydata is not None:
                mkUniform = True
                if uds is not None: ds = uds
                x0tmp = np.empty((1),dtype=pff.PFFnp_float)
                x0tmp[:] = ds.g_range[0,0,0]
                dxtmp = np.empty((1),dtype=pff.PFFnp_float)
                dxtmp[:] = (ds.g_range[0,0,1] - ds.g_range[0,0,0]) / \
                    (ds.nx[0][0] - 1)
        elif type(xydata) is types.FloatType:
            if format == 5:  mkUniform = True
            else:
                x0 = x[0]
                ##ilow = max(1,np.argmin(np.abs(x)))
                ilow = np.where(x[:-1]*x[1:] <= 0.0)[0]
                if len(ilow) > 0: ilow = ilow[0] + 1
                elif x0 >= 0.0: ilow = 1
                else: ilow = npts-1
                dx = x[ilow] - x[ilow-1]
                wnz = np.where(x != 0.0)[0]
                xu = dx*np.arange(npts,dtype=pff.PFFnp_float)[wnz] + x0
                relerr = abs(x[wnz] - xu)/x[wnz]
                if relerr.max() < xydata:
                    ##print 'Uniform: ', relerr.max(), ' <', xydata
                    mkUniform = True
                    x0tmp = np.array([x0],dtype=pff.PFFnp_float)
                    dxtmp = np.array([dx],dtype=pff.PFFnp_float)
                
        if mkUniform:
            if format == 5:
                x0tmp = np.asarray(x,dtype=pff.PFFnp_float)
                dxtmp = x0tmp.copy()
                ##print x0tmp,dxtmp
            ds.x0 = [x0tmp]
            ds.dx = [dxtmp]
            del ds.x
            ds.rawname='UF1'
            ds.rawtype=pff.UF1
            del ds.typekey
            ds = pff.UNF_dataset(new=ds.__dict__)
            if format == 5: ds.fill_grid_range()
                    
        _wdflist[out[i]] = ds

    return npts

__all__.append('readasc')
def readasc(*args,**kwargs):
    '''Usage:
  readasc(file, nvar, [format=int], [limit=int], [skip=int], [quiet=bool], \\
          [dtype=numpy.type] )

Arguments:
  file:    file name, or Python file object
  nvar:    Number of dependent variables in an input line, i.e.,
           (X, Y1, Y2, ..., Ynvar) for formats 1-4,
           (Y1, Y2, ..., Ynvar) for format 5
  format:  format of file:
              0: only data values in the file maximum of LIMIT values
              1: Number of remaining lines in first line
              2: First NVAR lines containing data labels 
              3: First line: # of points, next NVAR lines: data labels
              4: EXCEL CSV format: First line contains NVAR data labels
              5: First line: delta-X, # of points, X0 = delta-X
           All formats support space, tab, and/or comma delimiters
  limit:   Maximum number of data-containing lines to be read
  skip:    Number of initial header lines in file to be ignored
  quiet:   If true, usage information will NOT be printed on error
  dtype:   NUMPY data type for returned arrays

Return value: Tuple containing X(np), Y(np,NVAR) and LABELS, 
              or None on error.
              X and Y are 1- and 2-D numpy.ndarray objects, respectively,
              and LABELS is a list of string labels. This list will be
              empty for files not containing label information. For Excel
              CVS format files, it will have NVAL+1 elements, the first
              being the label for the X data'''

    DN = 'READASC'
    argnames = [ 'file', 'nvar' ]
    optvals = [ ]
    kwdefs = { 'format':0, 'limit':None, 'skip':0, 'quiet':False, \
               'dtype':pff.PFFnp_float }
    ##, '':, '':, '':, '':, '':, '':, 


    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + readasc.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    okay = True

    t = type(file)
    need_close = False
    if t is types.StringType:
        try:
            f = open(file)
            need_close = True
        except IOError, e:
            print DN + ":",e
            okay = False
    elif t is types.FileType:
        f = file
        if f.closed or f.mode != 'r':
            print DN + ": File",f.name,"is not open for reading:"
            okay = False

    ##print file, nvar, okay, need_close
    ##for k in kwvalid: exec "print k, " + k
    nvarp1 = nvar + 1
    bad = []
    if type(nvar) is not types.IntType: bad.append('nvar')
    if limit is not None and type(limit) is not types.IntType:
        bad.append('limit')
    if type(skip) is not types.IntType: bad.append('skip')
    if type(format) is not types.IntType or format < 0 or format > 5:
        bad.append('format')
    if len(bad) > 0:
        print DN + ": The following arguments were incorrectly specified:",bad
        okay = False

    if not okay:
        if not quiet: print readasc.__doc__
        return None

    f.seek(0)
    for i in range(skip): f.readline()

    ex = regex.compile("[ ,\t\n\r]+")
    nvals = None
    labs = []
    bad = False
    lcount = 0
    iy0 = 1
    ##print lcount
    if format == 1 or format == 3:
        l = f.readline()
        try:
            nvals = float(l)
        except ValueError, e:
            print DN + ": Error reading # of datapoints\n  ", e
            return None
    if format == 2 or format == 3:
        for i in range(nvar):
            l = f.readline()
            if not l:
                bad = True  ;  break
            else:   labs.append(l.replace('\n','').replace('\r','').strip())
        lcount += nvar
    if format == 4:
        l = f.readline()
        if not l:  bad = True
        else:  labs = l.replace('\n','').replace('\r','').split(',')[:nvarp1]
        lcount += 1
    if format == 5:
        l = f.readline()
        if not l:  bad = True
        else:
            nums = ex.sub(" ",l).strip().split()
            if len(nums) != 2: bad = True
            else:
                try:
                    x = np.asfarray(nums[:1],dtype=dtype)
                    nvals = int(np.asarray(nums[1:],dtype=int)[0])
                except ValueError: bad = True
            nvarp1 = nvar
            print lcount,nums,x,nvals
        lcount += 1
        iy0 = 0
   
    if bad:
        print DN + ": EOF encountered while reading header/labels"
        return None

    ##print labs

    if nvals is None:
        nvals = 0
        sloc = f.tell()
        ##print 'nvals:',nvals, sloc
        while True:
            l = f.readline()
            if not l: break
            nvals += 1  ##  ;  print nvals,l
        f.seek(sloc)

    ##print 'nvals:',nvals,f.tell()
    if limit is not None and limit < nvals: ns = limit
    else: ns = nvals
    if format != 5: x = np.empty((ns,),dtype=dtype)
    y = np.empty((ns,nvar),dtype=dtype,order='F')

    i = 0
    if limit is None: limit = 2*nvals
    while i < limit:
        l = f.readline()
        if not l: break
        lcount += 1
        nums = ex.sub(" ",l).strip().split()
        if len(nums) < nvarp1:
            print DN + ": insufficient data in line #",lcount
            return None
        try:
            vals = np.asfarray(nums[:nvarp1],dtype=dtype)
            ##print lcount,vals
            if format != 5: x[i] = vals[0]
            y[i,:] = vals[iy0:]
        except ValueError, e:
            print DN + ": Error reading numerical data in line #",lcount
            return None
        i = i + 1

    if need_close: f.close()

    return (x, y, labs)

__all__.append('wri')
def wri(*args, **kwargs):
    '''Write waveform data to a PFF file from one or more WDF arrays.

Usage:
  wri(wdfs, [wdmax], [fileid=int], [clabel=string], [xlabel=string], \\
      [ylabel=string], [tlabel=string], [apptype=int], [precision=int], \\
      [full=bool], [reduced=bool] )

Arguments:
  wdfs:      Starting WDF index, or list of WDF indices, for storing datasets
             that are read
  wdmax:     Final WDF index. If specified, WDFS must be an integer.
  fileid:    PFF file index of file to be read from
  clabel:    If specified, will be used as the dataset comment
  xlabel:    If specified, will be used as the dataset's abscissa (x) label
  ylabel:    If specified, will be used as the dataset's ordinate (y) label
  tlabel:    If specified, will be used as the dataset's type label
  apptype:   If specified, will be used as the dataset's application type
  precision: Precision used to write dataset. Legal values are:
               pff.FP_REDUCED, pff.FP_FULL, or pff.FP_ORDFULL
  full:      If set, write precision will be pff.FP_FULL
  reduced:   If set, write precision will be pff.FP_REDUCED

Return value: Number waveforms datasets written, or None on error.'''
   
    DN = 'WRI'
    argnames = [ 'wdfs', 'wdmax' ]
    optvals = [ None ]
    kwdefs = { 'fileid':0, 'clabel':None, 'xlabel':None, 'ylabel':None, \
               'tlabel':None, 'apptype':None, 'precision':None, 'full':None, \
               'reduced':None }
    ##, '':, '':, '':, '':, '':, '':, 
    attr_map = { 'apptype':'apptype', 'clabel':'title', 'tlabel':'typename', \
                 'xlabel':'glabels', 'ylabel':'dlabels' }
    prec_map = { 'full':pff.FP_FULL, 'reduced':pff.FP_REDUCED }

    others = argnames + ['fileid','precision']


    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + wri.__doc__  ;  return None

    ##keys = res.keys()
    ##keys.sort()
    ##for k in keys:   exec "print k, res[k]"

    for k in others: exec k + " = res[k]"

    wdlist = utils.parsewsl(_wdflist, wdfs, wdmax, stringOK=True)
    if wdlist is None:  return 0
    cnt = len(wdlist)
    ##print wdlist
    if cnt == 0:
        print DN + ': Specified WDF arrays are empty'
        return 0

    okay = True
    need_cpy = False
    attr_set = []

    for k in attr_map:
        val = res[k]
        if val is not None:
            ##print "processing attribute", k, res[k]
            is_label = k[1:6] == "label"
            sval = repr(val)
            if is_label:
                if type(val) is not types.StringType:
                    okay = False
                    print  DN + ": Supplied value for", k, "must be a string"
                else:
                    if  k == 'xlabel' or k == 'ylabel':
                        ts = attr_map[k][:4]
                        scmd = ts + " = np.reshape(np.array([" + sval + \
                               "]),(1,1))"
                        ##print "scmd:",scmd
                        exec scmd
                        sval = ts
            else:
                if type(val) is not types.IntType:
                    okay = False
                    print DN + ": Supplied value for", k, "must be an integer"
            if okay:
                scmd = "ds." + attr_map[k] + " = " + sval 
                attr_set.append(scmd)
    ##print attr_set
    for k in prec_map:
        val = res[k]
        if val is not None and val is not False and val != 0:
            ##print "processing prec", k, res[k]
            exec "precision = " + repr(prec_map[k])
    ##print wdfs, wdmax, fileid, precision

    need_cpy = len(attr_set) > 0

    for i in wdlist:
        ds = _wdflist[i]
        if need_cpy or ds.rawtype != pff.NGD:
            ds = cpy.deepcopy(_wdflist[i])
            for s in attr_set:
                exec s
            # If not NGD, the only place for these strings is the block label
            if ds.rawtype != pff.NGD:  ds.blabels = np.reshape(ds.dlabels,(1,))
        try:
            ds.write(fileid,precision,ignore=True)
        except pex.PFF_Error, e:
            print DN + ":", e
            return None

    return cnt


__all__.append('whelp')
def whelp(*args, **kwargs):
    '''List information about the WDF arrays.

Usage:
  whelp( [wdfs], [wdmax], [file=bool], [full=bool] )

Arguments:
  wdfs:     Integer index of first WDF array to be listed, a list of integer
            WDF array numbers to be listed, or a string to be matched to
            WDF array comments (titles). If WDMAX is specified, WDFS must be
            an integer <= WDMAX. If WDFS is a non-positive integer, information
            for all valid WDF arrays will be printed.
  wdmax:    Integer index of last WDF array to be listed.
  file:     If True, filename will be included in output for brief mode.
  full:     If True, full, detailed WDF array information will be printed.
            If False, only a brief listing (WDF array number, comment) is given.
            If not specified, a detailed listing will be given if WDFS is an
            integer and WDMAX is not specified, or if WDFS is a list.

Return value: Returns 0 if successful, or None on error'''

    DN = 'WHELP'
    argnames = ['wdfs','wdmax']
    optvals = [ None,None ]
    kwdefs = { 'file':False, 'full':None }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + whelp.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    detailed = False
    if type(wdfs) == types.IntType and wdfs <= 0:
        wdlist = _wdflist.keys()
        wdlist.sort()
    else:
        wdlist = utils.parsewsl(_wdflist, wdfs, wdmax, stringOK=True)
        if wdlist is None:  return None

        if (type(wdfs) == types.IntType and wdmax is None) or \
           type(wdfs) == types.ListType:
            detailed = True

    if len(wdlist) == 0:
        print DN + ': Specified WDF arrays are empty'  ;  return None

    if full is not None:
        if full == 0: detailed = False
        else:         detailed = True

    if detailed:
        for i in wdlist:
            ds = _wdflist[i]
            print "Values for wdf array number" , i
            print " Comment:", ds.title
            print "   File:", ds.file
            print " X-label:", ds.glabels[0][0]
            print " Y-label:", ds.dlabels[0][0]
            if ds.typekey == 'U':
                print " Start Time: ",ds.x0[0][0],", Delta Time: ",ds.dx[0][0]
            else:
                print " Start Time: ", ds.g_range[0][0][0], \
                      ", Non-Uniform X-Axis"
            print " Number of Points:", ds.nx[0][0]
            print " Range of X-values: ", ds.g_range[0][0][0], \
                  "  ", ds.g_range[0][0][1]
            data =  ds.data[0]
            print " Range of Y-values: ", data.min(), "  ", data.max()
            print " -----------------"
    else:
        print "WDF Arrays with Valid Data are Listed:"
        print "Array   Comment",
        if file: print ": File"
        else: print ""
        jd = max(int(math.log10(max(wdlist)))+1,3)
        fmt="%"+str(jd)+"d"
        for i in range(8-jd): fmt += " "
        fmt += "%s%s"
        for i in wdlist:
            fstr = ''
            if file: fstr = ": " + _wdflist[i].file
            print fmt % ( i, _wdflist[i].title, fstr )

    return 0


__all__.append('delwdf')
def delwdf(*args,**kwargs):
    '''Delete one or more WDF arrays.

Usage:
  delwdf( [wdfs], [wdmax], [quiet=bool] )

Arguments:
  wdfs:  Integer index of first WDF array to be deleted, a list of integer
         WDF array indices to be deleted, or a string to be matched to
         WDF array comments (titles). If WDMAX is specified, WDFS must be
         an integer <= WDMAX. If WDFS is a non-positive integer, all valid
         WDF arrays will be deleted. If WDFS is a string, all WDF arrays whose
         comments match the string will be deleted.
  wdmax: Integer index of last WDF array to be deleted.
  quiet: If True, no message confirming deletion will be printed.

Return value: If successful, returns the number of WDF arrays deleted,
              or None on error'''

    DN = 'DELWDF'
    argnames = ['wdfs','wdmax']
    optvals = [ None,None ]
    kwdefs = { 'quiet':False }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + delwdf.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    if type(wdfs) == types.IntType and wdfs <= 0:
        dlen = len(_wdflist)  ;  _wdflist.clear()
    else:
        dlist = utils.parsewsl(_wdflist,wdfs,wdmax,stringOK=True)
        if dlist is None:  return None
        dlen = len(dlist)
        for w in dlist:  del _wdflist[w]

    if not quiet:  print dlen, 'Waveform Arrays have been Cleared'

    return dlen
    

__all__.append('get_empty_wdf')
def get_empty_wdf(*args,**kwargs):
    '''Finds one or more consecutive empty (unused) WDF arrays.

Usage:
  get_empty_wdf( [first], [last=int], [count=int] )

Arguments:
  first: Integer indicating the array index to begin searching for empty
         WDF arrays. If non-positive, 1 will be used.
  last:  Integer index of last array location to be searched. (Default: None)
  count: Number of consecutive array locations that to be empty. (Default: 1)

Return value: If successful, returns the index of the first empty WDF array,
              or None if a consecutive block of empty arrays could not be
              found (this can occur only if LAST is specified).'''

    DN = 'GET_EMPTY_WDF'
    argnames = ['first']
    optvals = [ 1 ]
    kwdefs = { 'last':None, 'count':1 }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + get_empty_wdf.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    first = max(first,1)
    return utils.get_empty_wsl(_wdflist,first,last,count)


__all__.append('w2i')
def w2i(*args,**kwargs):
    '''Returns the dataset object corresponding to a WDF array.

Usage:
  w2i( [wdf], [copy=bool] )

Arguments:
  wdf:   Integer index of the WDF array.
  copy:  If True, return a copy of the WDF array, rather than a reference to
         it. (Default: True)

Return value: If successful, returns the dataset object corresponding to the
              requested WDF array, or None on error'''

    DN = 'W2I'
    argnames = ['wdf']
    optvals = [ ]
    kwdefs = { 'copy':True }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + w2i.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    if not _wdflist.has_key(wdf):
        print DN + ':',wdf, "does not have valid data"
        return None

    if copy:
        return  cpy.deepcopy(_wdflist[wdf])
    else:
        return _wdflist[wdf]


__all__.append('getMean')
def getMean(*args,**kwargs):
    '''\
Returns mean value over the range of a WDF array, and optionally, the 
standard deviation. The range over which the mean is taken can be limited
using the `window' keyword.

Usage:
  getMean(wdf, [window=bool|list|'zoom'], [stddev=bool] )

Arguments:
  wdf:    Integer index of the WDF array.
  window: If a 2-element list, it describes the range of the abcissa coordinate
          over which the mean is evaluated. If not a string, and evaluates as
          True, a secondary window will display a plot of the WDF array, and the
          cursor can be used to select the beginning and ending abscissa values
          that define the range over which the mean is taken. Finally, if
          `window' is a string whose first character is 'z', an opportunity to
          zoom in on a specific region of the plot is provided before the cursor
          is used to select the abscissa range.
  stddev: If True, the standard deviation over the specified abscissa range will
          also be returned.

Return value: If successful, the mean is returned, or if `stddev' is True, a
              2-element tuple containing the mean and standard deviation is
              returned. On error, None is returned'''

    DN = 'getMean'
    argnames = ['wdf']
    optvals = [ ]
    kwdefs = { 'window':False, 'stddev':False }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + getMean.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    if not _wdflist.has_key(wdf):
        print DN + ':',wdf, "does not have valid data"  ;  return None

    ds = _wdflist[wdf]
    bad = False
    zoom = False
    if window is not False:
        try:
            w = np.asarray(window)
            s = w.shape
            if len(s) == 1 and s[0] == 2:
                t = str(w.dtype)
                if t[:3] != 'int' and t[:5] != 'float': bad = True
                window = True
            else:
                w = None
                try:
                    if type(window) is types.StringType:
                        if len(window) > 0 and window.upper()[0]== 'Z':
                            zoom,window = True,True
                        else: bad = True
                    elif window: window = True
                    else: window = False
                except ValueError:
                    bad = True
        except:
            bad = True

        if bad:
            print DN + ': Invalid WINDOW keyword value'  ;  return None

        if window:
            if w is None:
                w = getWdfWind(wdf,zoom=zoom)
                print 'Selected X limits: ',(w.min(),w.max())

            xmin,xmax = w.min(),w.max()
            if ds.typekey == 'U':
                x0 = ds.x0[0][0] ; dxi = 1.0/ds.dx[0][0] 
                i1 = int((xmin - x0)*dxi) + 1
                i2 = int((xmax - x0)*dxi) + 1
                if i2 == i1: i2 += 1
            else:
                x = ds.x[0][0]
                i1 = np.where(x > xmin)[0][0]
                i2 = np.where(x > xmax)[0][0]
            d = ds.data[0][i1:i2]
    else:
        d = ds.data[0]

    if not stddev: return d.mean()

    return d.mean(), d.std()

__all__.append('getX')
def getX(*args,**kwargs):
    '''Returns an array containing the abscissa (X) values of a WDF array.

Usage:
  getX( [wdf] )

Arguments:
  wdf:   Integer index of the WDF array.

Return value: If successful, returns array of abscissa values of the WDF array,
              or None on error.'''

    DN = 'getX'
    res = utils.process_args(args, kwargs, DN, ['wdf'], {}, [], extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + getX.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    if not _wdflist.has_key(wdf):
        print DN + ':',wdf, "does not have valid data"  ;  return None

    return _wdflist[wdf].getx()

__all__.append('getY')
def getY(*args,**kwargs):
    '''Returns an array containing the ordinate (Y) values of a WDF array.

Usage:
  getY( [wdf] )

Arguments:
  wdf:   Integer index of the WDF array.

Return value: If successful, returns array of ordinate values of the WDF array,
              or None on error.'''

    DN = 'getY'
    res = utils.process_args(args, kwargs, DN, ['wdf'], {}, [], extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + getY.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    if not _wdflist.has_key(wdf):
        print DN + ':',wdf, "does not have valid data"  ;  return None

    #print wdf,type(_wdflist[wdf].data[0])
    #print _wdflist[wdf].data[0].shape, _wdflist[wdf].data[0].flags
    return _wdflist[wdf].data[0].copy(order='F')

__all__.append('getYVal')
def getYVal(*args, **kwargs):
    '''Returns the ordinate (Y) value of a WDF array corresponding to a
supplied abscissa (X) value.

Usage:
  getYVal( wdf, xval )

Arguments:
  wdf:   Integer index of the WDF array.
  xval:  Real abscissa value.

Return value: If successful, returns ordinate value corresponding to XVAL,
              or None on error.'''

    DN = 'getYVal'
    argnames = [ 'wdf', 'xval' ]
    res = utils.process_args(args, kwargs, DN, argnames, {}, [], extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + getYVal.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    if not _wdflist.has_key(wdf):
        print DN + ':',wdf, "does not have valid data"
        return None

    ds = _wdflist[wdf]
    f1, indx = ds.find_blk_intercept(xval)
    ##print indx,f1
    if indx < 0:
        print DN + ':',xval, 'is outside dataset range'
        return None
    else:
        data = ds.data[0]
        return (1.0 - f1)*data[indx] + f1*data[indx+1]

__all__.append('getXRange')
def getXRange(*args,**kwargs):
    '''Returns the range of abscissa (X) values of a WDF array.

Usage:
  getXRange( [wdf] )

Arguments:
  wdf:   Integer index of the WDF array.

Return value: If successful, returns the tuple (XMIN,XMAX) for the WDF array,
              or None on error.'''

    DN = 'getXRange'
    res = utils.process_args(args, kwargs, DN, ['wdf'], {}, [], extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + getXRange.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    if not _wdflist.has_key(wdf):
        print DN + ':',wdf, "does not have valid data"  ;  return None

    return tuple(_wdflist[wdf].g_range[0,0])

__all__.append('getYRange')
def getYRange(*args,**kwargs):
    '''Returns the range of ordinate (Y) values of a WDF array.

Usage:
  getYRange( [wdf] )

Arguments:
  wdf:   Integer index of the WDF array.

Return value: If successful, returns the tuple (YMIN,YMAY) for the WDF array,
              or None on error.'''

    DN = 'getYRange'
    res = utils.process_args(args, kwargs, DN, ['wdf'], {}, [], extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + getYRange.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    if not _wdflist.has_key(wdf):
        print DN + ':',wdf, "does not have valid data"  ;  return None

    data = _wdflist[wdf].data[0]
    return ( data.min(), data.max() )

__all__.append('getNp')
def getNp(*args,**kwargs):
    '''Returns the number of x-y pairs in a WDF array.

Usage:
  getNp( [wdf] )

Arguments:
  wdf:   Integer index of the WDF array.

Return value: If successful, returns the number of x-y pairs in the WDF array,
              or None on error.'''

    DN = 'getNp'
    res = utils.process_args(args, kwargs, DN, ['wdf'], {}, [], extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + getNp.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    if not _wdflist.has_key(wdf):
        print DN + ':',wdf, "does not have valid data"  ;  return None

    return _wdflist[wdf].nx[0][0]

__all__.append('getLabel')
def getLabel(*args,**kwargs):
    '''Returns a label of a WDF array corresponding to a supplied type KEY.

Usage:
  getLabel( wdf, key )

Arguments:
  wdf:   Integer index of the WDF array.
  key:   A string containing any of the following characters, which indicate:
            'C' : Dataset comment label
            'X' : Abscissa (X) axis label
            'Y' : Ordinate (Y) axis label
            'T' : Dataset type label
         Other characters in the string are ignored, as are repeated characters.
         The test for these characters is NOT case-sensitive.
         If KEY is None, KEY is set to 'CXYT'.
         If not supplied, KEY defaults to 'C'.

Return value: If more than one label is requested, a tuple of labels,
              whose order matches the order that their character codes
              were encountered in KEY, is returned.  If only one label
              is requested, that label is returned.
              On error, None is returned.'''

    all = 'CXYT'
    DN = 'getLabel'
    argnames = [ 'wdf', 'key' ]
    optvals = [ 'C' ]
    res = utils.process_args(args, kwargs, DN, argnames, {}, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + getLabel.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    if not _wdflist.has_key(wdf):
        print  DN + ":",wdf, "does not have valid data"
        return None

    ds = _wdflist[wdf]
    tup = ()
    if key is None: tkey = all
    elif type(key) is types.StringType:  tkey = key.upper()
    else:
        print DN + ": Parameter \"key\" must be a string"  ;  return None

    for c in tkey:
        if c == 'C': tup += (ds.title, )
        elif c == 'X': tup += (str(ds.glabels[0][0]), )
        elif c == 'Y': tup += (str(ds.dlabels[0][0]), )
        elif c == 'T': tup += (ds.typename, )

    if len(tup) == 1:  return tup[0]
    else:  return tup


_plodefs = { 'overlay':True, 'nogrid':False, 'xrange':None, 'yrange':None, \
             'title':None, 'xlabel':None, 'ylabel':None, 'wait':False, \
             'legend':0, 'color':None, 'line':None, 'lw':None, 'charsize':None }
_plopts_cur = {}
_pdchmx = 0
for i in _plodefs.keys():
    _pdchmx = max(_pdchmx, len(i))
    _plopts_cur[i] = _plodefs[i]

__all__.append('plo')
def plo(*args, **kwargs):
    '''Plot plot one or more waveform (WDF) arrays.

Usage:
  plo([wdf], [count], [xrange=list], [yrange=list], [title=str], \\
      [xlabel=str], [ylabel=str], [overlay=bool],  [nogrid=bool], \\
      [wait=bool],  [legend=value],  [color=value],  [line=value], \\
      [lw=int], [charsize=int], [left=bool], [right=bool], [histogram=value], \\
      [unset=bool], [setdefault=bool], [showdefault=bool] )

Arguments:
  wdf:          Integer index, or list of integer indices, of WDF arrays
                to be plotted.Optional ONLY if SETDEFAULT, SHOWDEFAULT, or
                UNSET have been specified
  count:        Number of consecutively indexed WDF arrays to be plotted,
                starting with WDF. If COUNT is specified, WDF must be an
                integer.
  xrange*:      Horizontal (x) range of plot
  yrange*:      Vertical (y) range of plot
  xlabel*:      Horizontal (x) axis label for output plot
  ylabel*:      Vertical (y) axis label for output plot
  title*:       Title for plot
  overlay*:     If True, multiple WDF arrays will be plotted on a single
                axes. If False, each requested WDF array will be plotted
                in its own axes. (DEFAULT:True)
  nogrid*:      If True, plots will be plotted on the same axes as the last
                plot command, if possible. (DEFAULT:False)
  wait*:        If True, user will need to hit the "ENTER" key between plots.
                (DEFAULT:False)
  legend*:      Location of plot legend. If None, no legend is used. Other
                legal values are the legal values of the LOC keyword of the the
                mathplotlib.axes.Axes.legend method. (DEFAULT:0 or 'best')
  color*:       An integer, string, or list of integers and/or strings. Strings
                are interpreted as names of matplotlib colors. Integers
                are mapped (modulo 8) to 'k' (black), 'r' (red), 'g' (green),
                'b' (blue), 'c' (cyan), 'm' (magenta), 'y' (yellow), and
                'orange'.
  line*:        An integer, string, or list of integers and/or strings. Strings
                are interpreted as names of matplotlib line types. Integers
                are mapped (modulo 4) to '-' (solid), ':' (dotted), '--'
                (dashed), and '-.' (dash_dot).
  lw*:          Line weight, in pixels
  charsize*:    Character size, in points
  left:         If True, requested plots will have only a left axis.
  right:        If True, and the previous plot command set the LEFT keyword,
                then the requested plots will be scaled to an independent right
                axis.
  histogram:    A list containing one element for each WDF array to be
                plotted. If it has fewer elements, it is extended by
                successively appending the last element until it has
                enough elements. If not a list, its value will be used
                for every array to be plotted. If the element is a
                Python FloatType variable, or it otherwise evaluates
                to `True', the array will be plotted as a histogram,
                with each supplied abscissa value assumed to be the
                midpoint of a histogram bin. With this assumption, the
                array of bin bounds is uniquely determined if the
                lowest bin boundary (X0) is also known. If an element
                of HISTOGRAM is a floating point value, that value is
                used for the lowest bin boundary for the corresponding
                array. If not, the dataset's spare word array is
                searched for the `X0' key, and if it exists, it is
                used to decode the value of the lowest bin boundary.
                If X0 is not available from either of these sources,
                or the supplied value does not produce a consistent,
                monotonic array of bin boundaries, the internal bin
                boundaries are assumed to be midway between the
                supplied abscissa values, and the outer boundaries are
                obtained by mirror symmetry about the supplied
                abscissa's extrema.
  setdefault:   If set, provided option values will be made default
  showtdefault: If set, current default option values will be printed
  unset:        If set, default option values will be reset to original values

Note: The default values of the keywords denoted with '*' above, can be modified
      using the SETDEFAULT keyword.

Return value: If error encountered, returns None. Otherwise, 0 is returned'''

    DN = 'PLO'
    argnames = [ 'wdf', 'count' ]
    optvals = [ None, None ]
    kwdefs = { 'unset':False, 'showdefault':False, 'setdefault':False, \
               'left':False, 'right':False, 'use_fig':None, 'histogram':None,
               'xlog':False, 'ylog':False }
               ##, '':, '':, '':, '':, '':, '':, 
    kwdefs.update(_plopts_cur) ## append settable option defaults
    ##print kwdefs

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]  ;  nr = [ 0,None ]
        print nl[res] + plo.__doc__  ;  return nr[res]

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    if unset:
        for k in _plodefs.keys():
            _plopts_cur[k] = _plodefs[k]
        return 0
    if setdefault:
        for k in keys:
            if _plopts_cur.has_key(k): exec "_plopts_cur[k] = " + k
        if not showdefault:  return 0
    if showdefault:
        blnk = ''
        for i in range(_pdchmx):  blnk += ' '
        print 'Default parameters for the PLO command are:'
        dkeys = _plopts_cur.keys()  ; dkeys.sort()
        for i in dkeys:
            l = len(i)
            tmp = "   " + i + blnk[l:] + " = "
            print tmp + str(_plopts_cur[i])
        return 0

    if wdf is None:
        print DN + ": WDF array index must be specified"  ;  return None

    plist = utils.parsewsl(_wdflist,wdf,count=count,stringOK=True)
    if plist is None:
        print DN + ": Error parsing WDF and COUNT"  ;  return None
    nplts = len(plist)
    if nplts == 0:
        print DN + ": No specified WDF arrays contain data"  ;  return None

    ##print plist
    okay = True
    if xrange is not None:
        typ = type(xrange)
        if (typ is types.ListType or typ is types.TupleType) and \
           len(xrange) == 2:
            xmin, xmax = xrange
        else:
            print DN + ": XRANGE must be a 2-element list or tuple"
            okay = False
    else: xmin = None
    if yrange is not None:
        typ = type(yrange)
        if (typ is types.ListType or typ is types.TupleType) and \
           len(yrange) == 2:
            ymin, ymax = yrange
        else:
            print DN + ": YRANGE must be a 2-element list or tuple"
            okay = False
    else: ymin = None

    leftright = None
    if left or right:
        if left and right:
            print DN + \
              ": LEFT and RIGHT cannot both be specified for the same plot"
            okay = False
        if nogrid:
            print DN + ": NOGRID must be false if LEFT or RIGHT is specified"
            okay = False
        if not overlay and nplts > 1:
            print DN + ": OVERLAY must be true if LEFT or RIGHT is specified"
            okay = False
        if okay:
            if left: leftright = 'L'
            else: leftright = 'R'

    typ = type(histogram)
    if typ is not types.ListType and typ is not types.TupleType:
        histogram = [histogram]
    nhist = len(histogram)
    add = histogram[-1]
    for i in range(nhist,nplts):   histogram.append(add)
    xhist = []
    for i,w in enumerate(plist):
        x0 = None
        if type(histogram[i]) is types.FloatType:
            x0 = histogram[i]
            histogram[i] = True
        if histogram[i]:
            xh = getX(w)
            if x0 is None:
                sx0 = w2i(w).findSpare('X0',3)
                if sx0 is not None: x0 = pff.i2f(sx0)
            xf = utils.XfFromXh(xh,x0)
        else: xf = None
        xhist.append(xf)

    if not okay:   return None

    f = plt.gcf()
    if not nogrid:
        f,ax = plots.adv_frame(leftright, use_fig=use_fig)
        if ax is None:  return None
        xr = xrange  ;  yr = yrange
        if (not right and xrange is None) or yrange is None:
            tlist = plist
            if not overlay: tlist = plist[0:1]
            for i,w in enumerate(tlist):
                if not right and xrange is None:
                    if histogram[i]: mnx = (xhist[i][0], xhist[i][-1])
                    else: mnx = getXRange(w)
                    if xmin is None:
                        xmin, xmax = mnx
                    else:
                        xmin = min(xmin,mnx[0])
                        xmax = max(xmax,mnx[1])
                if yrange is None:
                    mnx = getYRange(w)
                    if ymin is None:
                        ymin, ymax = mnx
                    else:
                        ymin = min(ymin,mnx[0])
                        ymax = max(ymax,mnx[1])

            if not right and xrange is None:
                if xlog: xr = utils.log_bounds(getX(w))[0:2]
                else: xr = utils.nice_bounds(xmin,xmax,log=xlog)[0:2]
            if yrange is None:
                if ylog: yr = utils.log_bounds(getY(w))[0:2]
                else: yr = utils.nice_bounds(ymin,ymax,log=ylog)[0:2]
        if left:  _left_xr = xr
        else: _left_xr = None
        if right: xr = _left_xr
        ax.set_xlim(xr)
        ax.set_ylim(yr)
        #print 'LOG:',xlog,ylog
        if (xlog): ax.set_xscale('log')
        if (ylog): ax.set_yscale('log')
        #print ax.xaxis.get_smart_bounds(),ax.yaxis.get_smart_bounds()
        w = plist[0]
        if title == ".": ax.set_title(getLabel(w))
        elif title is not None: ax.set_title(title)
        if not right:
            if xlabel is None: ax.set_xlabel(getLabel(w,'X'))
            else: ax.set_xlabel(xlabel)
        if ylabel is None: ax.set_ylabel(getLabel(w,'Y'))
        else: ax.set_ylabel(ylabel)
        plots.set_frame_props(ax,lw,charsize,right)
        ovoff = 0
        _ovrplt.set(0,0,nplts,xrange, yrange,ax)
    else:
        if _ovrplt.x == 0:
            xrange = _ovrplt.xr
            yrange = _ovrplt.yr
            ax = _ovrplt.ax
            ovoff = _ovrplt.znorm
            _ovrplt.znorm += nplts
        else:
            print DN + ": NOGRID option invalid if last plot wasn't by PLO"
            return None

    ##print color, type(color)
    cmap = plots.bld_map(color)
    ##print line, type(line)
    lmap = plots.bld_map(line,'line')
    reset = False
    iplt = 0
    lst_a = -1
    if right: lst_a = -2
    for a in f.axes[lst_a:]:
        iplt += len(a.lines)
    pextra = {}  ;  lprops = {}
    if lw is not None:  pextra['lw'] = lw
    if charsize is not None:  lprops['size'] = charsize
    for i in range(nplts):
        w = plist[i]
        if reset:
            handles, labels = ax.get_legend_handles_labels()
            if legend is not None:
                ax.legend(handles,labels,loc=legend,frameon=False, prop=lprops)
            f.canvas.draw()
            if wait:
                if type(wait) is types.StringType:
                    hak(wait)
                else: hak()
            f,ax = plots.adv_frame()
            if xrange is None:
                if xlog: xr = utils.log_bounds(getX(w))[0:2]
                else:
                    if histogram[i]: xmin,xmax = (xhist[i][0], xhist[i][-1])
                    else: xmin,xmax = getXRange(w)
                    xr = utils.nice_bounds(xmin,xmax,log=xlog)[0:2]
            if yrange is None:
                if ylog: yr = utils.log_bounds(getY(w))[0:2]
                else:
                    ymin,ymax = getYRange(w)
                    yr = utils.nice_bounds(ymin,ymax,log=ylog)[0:2]
            ax.set_xlim(xr)
            ax.set_ylim(yr)
            if (xlog): ax.set_xscale('log')
            if (ylog): ax.set_yscale('log')
            if title == ".": ax.set_title(getLabel(w))
            if xlabel is None: ax.set_xlabel(getLabel(w,'X'))
            if ylabel is None: ax.set_ylabel(getLabel(w,'Y'))
            plots.set_frame_props(ax,lw,charsize,right)
        ls = plots.get_property(iplt,lmap)
        c = plots.get_property(iplt,cmap)
        if histogram[i]:
            xf = xhist[i]
            yh = getY(w)
            npt = 2*yh.size
            yy = np.empty((npt,),dtype=np.single)
            for i in range(npt):  yy[i] = yh[i/2]
            ##yy = np.fromiter((yh[i/2] for i in range(npt)),dtype=np.single)
            xx = yy.copy()
            xx[0],xx[-1] = (xf[0],xf[-1])
            for i in range(2,npt):  xx[i-1] = xf[i/2]
            ##xx[1:-1] = np.fromiter((xf[i/2] for i in range(2,npt)),
            ##                       dtype=np.single)
        else:   xx, yy = ( getX(w), getY(w) )
        ax.plot(xx,yy,label=getLabel(w),c=c,ls=ls,**pextra)
        reset = not overlay
        iplt += 1

    if legend is not None:
        if right:
            handles = []  ;  labels = []
            for a in f.axes:
                h,l = a.get_legend_handles_labels()
                handles += h  ;  labels += l
                axtmp = -2
        else:
            axtmp = -1
            handles,labels = ax.get_legend_handles_labels()

        f.axes[axtmp].legend(handles,labels,loc=legend,frameon=False,
                             prop=lprops)
    f.canvas.draw()
    if wait and overlay:
        if type(wait) is types.StringType:   hak(wait)
        else: hak()
 
    return 0

__all__.append('xplo')
def xplo(*args, **kwargs):
    '''Plot a WDF array versus a second WDF array at the same times..

Usage:
  xplo(wdx, wdy, [xrange=list], [yrange=list], [title=str], [xlabel=str], \\
      [ylabel=str], [nogrid=bool], [wait=bool], [color=value], [line=value], \\
      [lw=int], [charsize=int], [out=int] )

Arguments:
  wdx:          Integer index of WDF array for X axis
  wdy:          Integer index of WDF array for Y axis
  xrange*:      Horizontal (x) range of plot
  yrange*:      Vertical (y) range of plot
  xlabel*:      Horizontal (x) axis label for output plot
  ylabel*:      Vertical (y) axis label for output plot
  title*:       Title for plot
  nogrid*:      If True, plots will be plotted on the same axes as the last
                plot command, if possible. (DEFAULT:False)
  wait*:        If True, user will need to hit the "ENTER" key between plots.
                (DEFAULT:False)
  color*:       An integer, string, or list of integers and/or strings. Strings
                are interpreted as names of matplotlib colors. Integers
                are mapped (modulo 8) to 'k' (black), 'r' (red), 'g' (green),
                'b' (blue), 'c' (cyan), 'm' (magenta), 'y' (yellow), and
                'orange'.
  line*:        An integer, string, or list of integers and/or strings. Strings
                are interpreted as names of matplotlib line types. Integers
                are mapped (modulo 4) to '-' (solid), ':' (dotted), '--'
                (dashed), and '-.' (dash_dot).
  lw*:          Line weight, in pixels
  charsize*:    Character size, in points
  out:          If supplied, the resulting x-y dataset obtained by combining
                the two wdf arrays will be saved as wdf array `out'

Note: The default values of the keywords denoted with '*' above, can be modified
      using the SETDEFAULT keyword with the PLO command.

Return value: If error encountered, returns None. Otherwise, 0 is returned'''

    DN = 'XPLO'
    argnames = [ 'wdx', 'wdy' ]
    optvals = [ ]
    kwdefs = { 'out':None }
    needed = argnames[:]  ;  needed.extend(kwdefs.keys())
    ##print id(argnames),id(needed)
    plopts_toss = [ 'overlay', 'legend' ]
               ##, '':, '':, '':, '':, '':, '':, 
    kwdefs.update(_plopts_cur) ## append settable option defaults
    for toss in plopts_toss:
        del kwdefs[toss]
    ##print kwdefs,args,len(args)

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]  ;  nr = [ 0,None ]
        print nl[res] + xplo.__doc__  ;  return nr[res]

    for k in needed:
        exec k + " = res[k]"
        del res[k]
    ##print wdx,wdy,out
    ##print res

    for k in res.keys():
        exec k + " = res[k]"
   ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    xcom, m1, m2 = get_common_time(wdx,wdy)
    ##print wdx,wdy,xcom,m1,m2
    dsx = w2i(wdx,copy=False)
    dsy = w2i(wdy,copy=False)
    xa = dsx.data[0]
    ya = dsy.data[0]
    if xcom is not None:
        nxc = len(xcom)
        newtype = 'U'
        if not m2:
            ya = _fit2grid(dsy.getx(),ya,xcom)
        if not m1:
            xa = _fit2grid(dsx.getx(),xa,xcom)

    if out is None:
        out = get_empty_wdf(10000)
        delout = True
    else: delout = False
    if xlabel is None:   xlabel = getLabel(wdx,'Y')
    if ylabel is None:   ylabel = getLabel(wdy,'Y')
    if title is None:   title = ylabel + ' vs. ' + xlabel
    ##print repr(xlabel),repr(ylabel),repr(title)
    i2w(xa,ya,out,clab=title,xlab=xlabel,ylab=ylabel)
    plo(out,**res)

    if delout: del _wdflist[out]

__all__.append('xfr')
def xfr(*args, **kwargs):
    '''Copies the one or more WDF arrays to other WDF arrays.

Usage:
  xfr( wdf1, wdf2, [count], [quiet=bool] )

Arguments:
  wdf1:  Integer index, or list of integer indices, defining the source/s
         for the copy operation.
  wdf2:  Integer index, or list of integer indices, defining the destination/s
         for the copy operation.
  count: The number of consecutive WDF arrays, starting at WDF1, to be copied
         to destination arrays starting at WDF2. If COUNT is specified, both
         WDF1 and WDF2 must be integers.
  quiet: If True, warning messages regarding empty (unused) source arrays are
         suppressed.

Note: If WDF1 is a list, and WDF2 is an integer, WDF2 will be changed to a list
      of equal length, starting at WDF2. If WDF1 and WDF2 are both lists, and
      WDF2 is shorter than WDF1, WDF2 will be extended to the same length by
      adding consecutive integer elements starting with the last element of the
      original WDF2 list.

Return value: If successful, returns the number of WDF arrays copied,
              or None on error.'''

    DN = 'XFR'
    argnames = [ 'wdf1', 'wdf2', 'count' ]
    optvals = [ None ]
    kwdefs = { 'quiet':False }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]  ;  nr = [ 0,None ]
        print nl[res] + xfr.__doc__  ;  return nr[res]

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    len1 = None
    t1 = type(wdf1)  ;  t2 = type(wdf2)
    if t2 is types.IntType or t2 is types.ListType:
        len1 = count
        if count is not None:
            if t1 is types.IntType and t2 is types.IntType and \
               type(count) is types.IntType:
                wdf1 = range(wdf1,count+wdf1)
                wdf2 = range(wdf2,count+wdf2)
            else:   len1 = None
        else:
            len1 = 1
            if t1 is types.ListType:
                len1 = len(wdf1)
                if t2 is types.IntType:
                    wdf2 = range(wdf2,len1+wdf2)
                else:
                    len2 = len(wdf2)
                    if len2 < len1:
                        st = wdf2[len2-1] + 1
                        delt = len1 - len2
                        wdf2 += range(st,st+delt)
            elif t1 is types.IntType:
                wdf1 = [wdf1]
                if t2 is types.IntType:
                    wdf2 = [wdf2]

    if len1 is None:
        print "Invalid syntax"  ;  return None

    clist = utils.parsewsl(_wdflist,wdf1,valid=True)
    if clist is None:
        print DN + ": Error parsing WDF1"  ;  return None
    elif len(clist) == 0:
        print "No specified WDF arrays contain data"  ;  return 0

    cnt = 0
    notvalid = ''
    for i in range(len1):
        w = wdf1[i]
        try:
            indx = clist.index(w)
        except ValueError:
            indx = None
        if indx is not None:
            cnt += 1
            w2 = wdf2[i]
            _wdflist[w2] = cpy.deepcopy(_wdflist[w])
        elif not quiet:
            notvalid += " " + str(w)

    if notvalid:
        print "xfr: No data in some WDF arrays:" + notvalid

    return cnt

__all__.append('cha')
def cha(*args,**kwargs):
    '''Changes attributes of one or more WDF arrays.

Usage:
  cha( (wdfs, [wdmax], [file=str], [clabel=str], [tlabel=str], [xlabel=str], \\
       [ylabel=str], [scale=real], [tzero=real], [mdt=real], [append=bool] )

Arguments:
  wdfs:   Starting index, or list of indices, of WDF arrays to be changed.
  wdmax:  Final WDF index. If specified, WDFS must be an integer.
  file:   New filename associated with dataset.
  clabel: New dataset comment (see APPEND).
  tlabel: New dataset typename (see APPEND).
  xlabel: New abscissa (x) label (see APPEND).
  ylabel: New ordinate (y) label (see APPEND).
  scale:  Ordinate values (y) will be multiplied by SCALE.
  tzero:  Change the initial time of the first data point to TZERO.
  mdt:    Abscissa (x) values will be multiplied by MDT.
  append: If True, any supplied labels (CLABEL, TLABEL, XLABEL, and/or YLABEL)
          will be appended to the corresponding original labels of the dataset.
          Otherwise, any supplied labels will replace the original labels.

Return value: If successful, returns the number of WDF arrays changed,
              or None on error'''

    DN = 'CHA'
    argnames = [ 'wdfs', 'wdmax' ]
    optvals = [ None ]
    kwdefs = { 'file':None, 'clabel':None, 'tlabel':None, 'xlabel':None, \
               'ylabel':None, 'scale':None, 'tzero':None, 'mdt':None, \
               'append':False }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]  ;  nr = [ 0,None ]
        print nl[res] + cha.__doc__  ;  return None

    append = res['append']  ;  del res['append']
    for k in argnames:
        exec k + " = res[k]"  ;  del res[k]
    for k in res.keys():
        if res[k] is None: del res[k]

    keys = res.keys()
    okay = True
    clist = utils.parsewsl(_wdflist,wdfs,wdmax,stringOK=True)
    if clist is None:
        print DN + ": Error parsing WDF and WDMAX"  ;  okay = False
    if len(res) == 0:
        print DN + ": At least one change must be specified"  ;  okay = False
    for k in keys:
        if k == "scale" or k == "tzero" or k == "mdt":
            num, t = utils.is_number(res[k])
            if not num:
                print DN + ":",k.upper(),"must be a number"  ;  okay = False
        else:
            if type(res[k]) is not types.StringType:
                print DN + ":",k.upper(),"must be a string"  ;  okay = False
    if not okay:
        print "\n" + cha.__doc__  ;  return None

    if len(clist) == 0:
        print DN + ": No specified WDF arrays contain data"  ;  return 0

    for wd in clist:
        ds = _wdflist[wd]
        for k in keys:
            if k == 'scale':
                 ds.data[0] *= res[k]
            elif k == 'file':
               ds.file = res[k]
            elif k == 'tzero':
                if ds.typekey == 'U':
                    tsh = ds.x0[0][0] - res[k]
                    ds.x0[0][0] = res[k]
                else:
                    x = ds.x[0][0]
                    tsh = x[0] - res[k]
                    x -= tsh
                ds.g_range -= tsh
            elif k == 'mdt':
                val = res[k]
                if ds.typekey == 'U':
                    ds.x0[0][0] *= val
                    ds.dx[0][0] *= val
                else:
                    ds.x[0][0] *= val
                ds.g_range *= val
                ds.g_eps *= val
            else:
                ##print k, res[k]
                if append:
                    if k == 'clabel':   ds.title += res[k]
                    elif k == 'clabel': ds.typename += res[k]
                    elif k == 'xlabel':
                        s = ds.glabels[0][0] + res[k]
                        ds.glabels = np.reshape(np.array([s]), (1,1))
                    elif k == 'ylabel':
                        s = ds.dlabels[0][0] + res[k]
                        ds.dlabels = np.reshape(np.array([s]), (1,1))
                else:
                    if k == 'clabel':   ds.title = res[k]
                    elif k == 'tlabel': ds.typename = res[k]
                    else:
                      s = res[k]
                      if k == 'xlabel':
                        ds.glabels = np.reshape(np.array([s]), (1,1))
                      elif k == 'ylabel':
                        ds.dlabels = np.reshape(np.array([s]), (1,1))

    return len(clist)


__all__.append('i2w')
def i2w(*args,**kwargs):
    '''Convert X and Y arrays to a waveform dataset and/or WDF array. In a
different form, it can be used to copy a waveform dataset into a WDF array.

Usage:
  i2w(x, y, [wdf], [clabel=str], [tlabel=str], [file=str], [xlabel=str], \\
      [ylabel=str], [spare=iarray], [copy=True|False] )
    or
  i2w(ds, wdf, [clabel=str], [tlabel=str], [file=str], [xlabel=str], \\
      [ylabel=str], [spare=iarray], [copy=True|False])

Arguments:
  x:      list or numpy array containing abscissa (x) values.
  y:      list or numpy array containing ordinate (y) values.
  wdf:    if supplied, the produced dataset is placed in this WDF index.
  ds:     PFF dataset to be placed in a WDF array.
  clabel: Comment label for output dataset.
  tlabel: Type label for output dataset.
  xlabel: Abscissa (x) axis label for output dataset.
  ylabel: Ordinate (y) axis label for output dataset.
  spare:  Integer spare word array
  file:   Filename associated with output dataset.
  copy:   Case 1: X and Y are specified --
             If COPY is specified, the PFF dataset object produced will be
             returned. If COPY is True, a copy will be returned, if False,
             a reference to the WDF array's dataset will be returned. If
             WDF is not specified, the dataset is returned unconditionally.
          Case 2: DS is specified --
             If COPY is True, the WDF array is set to a copy of DS, otherwise
             it is set to a reference to DS.

Note: If X as only 2 elements and Y has more than 2 elements, X is interpreted
      as the ordered pair (X0,DX), which is used to build a uniform-grid 
      dataset.

Return value: If X, Y, and COPY are all specified, the PFF dataset object
              produced is returned. Otherwise, None is returned'''

    DN = 'I2W'
    argnames = [ 'a1', 'a2', 'a3' ]
    optvals = [ None ]
    kwdefs = { 'file':None, 'clabel':None, 'tlabel':None, 'xlabel':None, \
               'ylabel':None, 'spare':None, 'copy':None }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]  ;  nr = [ 0,None ]
        print nl[res] + i2w.__doc__  ;  return None

    rcpy = res['copy']  ;  del res['copy']
    tspare = res['spare']  ;  del res['spare']
    for k in argnames:
        exec k + " = res[k]"  ;  del res[k]
    for k in res.keys():
        if res[k] is None: del res[k]

    ds = None
    okay = True
    if _is_wdfds(a1):
        ds = a1
        wdf = a2
        if a3 is not None:
            print DN + \
                ": If DS specified, only 2 positional arguments are allowed"
            okay = False
    else:
        x = a1  ;  y = a2  ;  wdf = a3  ;  name = 'X'
        for a in [x,y]:
            t = type(a)  ;  tokay = True
            if t is types.ListType or t is np.ndarray:
                if len(a) < 2:
                    print  DN + ":", name, "must have at least two elements"
                    okay = False
                if t is types.ListType:
                    for val in a:
                        num, dum = utils.is_number(val)
                        if not num:
                            tokay = False ; break
                elif t is np.ndarray:
                    if a.ndim != 1: tokay = False
                    else:
                        dt = str(a.dtype)[0].upper()
                        if dt != 'F' and dt != 'I': tokay = False
            else: tokay = False
            if not tokay:
                print DN + ":", name, "must be a list of 1D-ndarray of numbers"
                okay = False
            name = 'Y'
    if wdf is not None and type(wdf) is not types.IntType:
        print DN + ": WDF must be an integer"  ;  okay = None

    if tspare is None:
        spare = None
    else:
        try:
            spare = np.asarray(tspare,dtype=np.intc)
        except ValueError:
            print DN + \
              ": Spare array must be a 1D NUMPY.NDARRAY object of integer type"
            okay = False

    keys = res.keys()
    ##if res.has_key('ylabel'):
    ##    print 'ylabel',res['ylabel'],repr(type(res['ylabel']))
    for k in keys:
        # Right now, all settable keywords are strings
        if type(res[k]) is not types.StringType and \
           type(res[k]) is not np.string_:
            print DN + ":",k.upper(),"must be a string"  ;  okay = False
    if not okay:
        print "\n" + i2w.__doc__  ;  return None

    ##print ds,wdf,rcpy
    ##if ds is None:   print x,y
    ##print res

    if ds is not None:
        if rcpy:   _wdflist[wdf] = cpy.deepcopy(ds)
        else:  _wdflist[wdf] = ds
        if len(res): cha(wdf,**res)
        return None
         
    yy = y  ;  nx = len(x)  ;  ny = len(y)
    if type(y) == types.ListType or y.dtype is not pff.PFFnp_float:
        yy = np.array(y,dtype=pff.PFFnp_float)
    dict = {'data': [yy], 'apptype':3, 'adim':1, 'sdim':1, 'spare':[spare], \
            'nblk':1, 'nx':[np.array([ny],dtype=pff.PFFnp_long)], \
            'blabels':np.array(['']), 'rfu':None}

    if nx == 2 and ny > 2:
        typekey = 'U'
        dict['x0'] = [np.array([x[0]],dtype=pff.PFFnp_float)]
        dict['dx'] = [np.array([x[1]],dtype=pff.PFFnp_float)]
        dict['rawtype'] = pff.UF1
        dict['rawname'] = 'UF1'
        gr = [x[0], x[0] + (ny-1)*x[1]]
    elif nx == ny:
        typekey = 'N'
        xx = x
        if type(x) == types.ListType or x.dtype is not pff.PFFnp_float:
            xx = np.array(x,dtype=pff.PFFnp_float)
        dict['x'] = [[xx]]
        dict['rawtype'] = pff.NGD
        dict['rawname'] = 'NGD'
        gr = [xx.min(), xx.max()]
    else:
        print DN + ": X and Y arrays must have the same length"
        return None

    eps = pff.GridEpsilonFactor*(gr[1] - gr[0])
    dict['g_range'] = np.array([gr,gr],dtype=pff.PFFnp_float).reshape((2,1,2))
    dict['g_eps'] = np.array([eps,eps],dtype=pff.PFFnp_float).reshape((2,1))
    for k in keys:
        if k == 'clabel':
            dict['title'] = res[k]
        elif k == 'tlabel':
            dict['typename'] = res[k]
        elif k == 'file':
            dict['file'] = res[k]
        elif k == 'xlabel':
            dict['glabels'] = np.array([res[k]]).reshape(1,1)
        elif k == 'ylabel':
            dict['dlabels'] = np.array([res[k]]).reshape(1,1)

    if not dict.has_key('title'): dict['title'] = ''
    if not dict.has_key('typename'): dict['typename'] = ''
    if not dict.has_key('file'): dict['file'] = ''
    if not dict.has_key('glabels'):
        dict['glabels'] = np.array(['']).reshape(1,1)
    if not dict.has_key('dlabels'):
        dict['dlabels'] = np.array(['']).reshape(1,1)

    if typekey == 'U':
        ds = pff.UNF_dataset(new=dict)
    else:
        ds = pff.NUNF_dataset(new=dict)

    if wdf is None:  return ds
    else:
        _wdflist[wdf] = ds
        if rcpy is None:  return None
        if rcpy:  return cpy.deepcopy(ds)
        else:  return ds

__all__.append('hak')

def hak(prompt=None):
    '''\
Function to suspend processing until input is received from the keypad. Useful
for pausing between plots in multi-plot scripts.

Usage:
  hak([prompt])

Arguments:
  prompt: Optional prompt string.
          If not supplied, "<CR> to continue: " is used.

Return value: String containing the line input at the keyboard, or "EOF" if
              end-of-file (ctrl-D) is received.'''

    if prompt is None:
        #return pex.hak()
        return raw_input("<CR> to continue: ")
    else:
        #return pex.hak(prompt)
        return raw_input(prompt)

__all__.append('get_common_time')

def get_common_time(*args,**kwargs):
    '''\
Function to find a abcissa grid for two WDF arrays. If possible, it will attempt
to use the abscissa grid for at least one of the two arrays. If this is not
possible, it returns a uniformly-spaced grid over the intersection of the
abscissa ranges of the two arrays.

Usage:
  get_common_time(wd1,wd2)

Arguments:
  wd1/wd2: The indices of the two supplied WDF arrays.

Return value: The tuple (xcom,m1,m2), where xcom is the common abscissa grid,
              and the boolean values m1 and m2 are True if `xcom' matches
              the original abscissa grids of wd1 and wd2, respectively. If the
              supplied grids already have matching grids, (None,True,True)
              is returned.'''

    DN = 'GET_COMMON_TIME'
    argnames = [ 'wd1', 'wd2' ]

    res = utils.process_args(args, kwargs, DN, argnames, {}, [], extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]  ;  nr = [ 0,None ]
        print nl[res] + get_common_time.__doc__  ;  return None

    wd1 = res['wd1']
    wd2 = res['wd2']

    if type(wd1) is not types.IntType or type(wd2) is not types.IntType:
        print "WD1 and WD2 must both be integers"
        return None

    wlist = utils.parsewsl(_wdflist, [wd1,wd2], valid=True )
    nw = len(wlist)
    if nw != 2:
        if nw == 0:
            print "Neither WDF array provided contains data"
        else:
            if wlist[0] == wd1:  s = str(wd2)
            else: s = str(wd1)
            print "WDF array", s, "does not contain data"
        return None

    test = []
    x1 = getX(wd1)
    x2 = getX(wd2)
    dmin1 = np.diff(x1).min()
    dmin2 = np.diff(x2).min()
    test.append(0.005*dmin1)
    test.append(0.005*dmin2)
    rng1 = getXRange(wd1)
    rng2 = getXRange(wd2)
    test.append(1.0e-4*np.diff(rng1)[0])
    test.append(1.0e-4*np.diff(rng2)[0])
    maxdel = min(test)

    ##print maxdel,dmin1,dmin2,rng1,rng2

    need_new = True
    m1 = False
    m2 = False
    nx1 = getNp(wd1)  ;  nx2 = getNp(wd2)
    if nx1 != nx2:
        pass
    elif np.fabs(x1 - x2).max() > maxdel:
        pass
    else:
        need_new = False
        m1 = True
        m2 = True

    if not need_new:
        return (None,m1,m2)

    xlow = max(rng1[0],rng2[0])
    xhi =  min(rng1[1],rng2[1])
    xdel = xhi - xlow
    dt1 = dmin1 + (rng1[1] - rng1[0])/(nx1 - 1)
    dt2 = dmin2 + (rng2[1] - rng2[0])/(nx2 - 1)
    nxmin = 0.5*max(nx1,nx2)
    if xlow == rng1[0] and xhi  == rng1[1] and nx1 >= nxmin:
        m1 = True
    elif xlow == rng2[0] and xhi  == rng2[1] and nx2 >= nxmin:
        m2 = True
    ##print xlow,xhi

    if m1 and not m2:
        return (x1,m1,m2)
    elif m2 and not m1:
        return (x2,m1,m2)
            

    dmin = min(dmin1,dmin2)
    npnts = int(round(xdel/dmin)) + 1
    xcom = np.asarray(np.linspace(xlow,xhi,npnts),dtype=pff.PFFnp_float)
    if npnts == nx1 and np.fabs(xcom - x1).max() <= maxdel:
        xcom = x1
        m1 = True
    elif npnts == nx2 and np.fabs(xcom - x2).max() <= maxdel:
        xcom = x2
        m2 = True

    return (xcom, m1, m2)


def _is_wdfds(*args,**kwargs):
    '''\
Method to determine if a supplied PFF dataset is a legal candidate for a WDF
array. To be legal, it must be a single-block, 1D scalar Block-Grid dataset

Usage:  _is_wdfds(ds)

Arguments:
  ds: PFF dataset object to be tested.

Return value: True if legal, False if not.'''

    DN = ' _IS_WDFDS'
    argnames = [ 'ds' ]

    res = utils.process_args(args, kwargs, DN, argnames, {}, [], extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]  ;  nr = [ 0,None ]
        print nl[res] + _is_wdfds.__doc__  ;  return None

    ds = res['ds']

    if isinstance(ds,pff.blkgrid_dataset):
        if ds.nblk == 1 and ds.sdim == 1 and ds.adim == 1:  return True

    return False

__all__.append('filt')

def filt(*args, **kwargs):
    '''\
Apply a Butterworth or Boxcar filter to the data in a WDF array.

Usage:
  filt(wdf,[wdout], frequency=float|frange=list, [btype=string], \
       [order=int], [padlen=int])

Arguments:
  wdf:       WDF array to be filtered (Note:  The array's abscissa (x) values
             must be monotonically increasing).
  wdout:     WDF array to receive the filtered data. If not supplied, the input
             array (wdf) will be overwritten with the filtered data.
  frequency: The critical frequency of the filter, in units of 1/u, where u is
             the unit of the supplied WDF array's abscissa (x axis). This
             parameter is required for filter types `boxcar', `low' and `high'.
  frange:    A 2-element list or array containing the critical frequencies of
             the filter, in units of 1/u, where u is the unit of the supplied
             WDF array's abscissa (x axis). This parameter is required for
             filter types `pass' and 'stop'.
  btype:     Filter type. Legal values are `boxcar', `low', `high', `pass',
             and `stop'. All filters EXCEPT `boxcar' are Butterworth filters.
             If not supplied, `boxcar' will be used by default.
  order:     Positive integer providing the order of the filter. (Not used if
             btype = `boxcar')
  padlen:    The number of elements by which to extend x at both ends
             of axis before applying the filter. See documentation for
             scipy.signal.filtfilt for full details. If not supplied, no
             padding will be applied. (Not used if btype = `boxcar')

  ds:     PFF dataset to be placed in a WDF array.'''

    DN = 'FILT'
    argnames = [ 'wdf', 'wdout' ]
    optvals = [ None ]
    kwdefs = { 'frequency':None, 'btype':'boxcar', 'order':4, 'frange':None, \
               'padlen':0 }
    ##, '':, '':, '':, '':, '':, '':, 
    btLegal = ['boxcar','low','high','pass','stop']

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, \
                             optvals, extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + filt.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    okay = True
 
    if type(wdf) is not types.IntType: okay = False
    if not _wdflist.has_key(wdf):
        print DN+": WDF array",wdf,"does not contain valid data"
        okay = False
    if wdout is None:  wdout = wdf
    else:
        if type(wdout) is not types.IntType: okay = False
        elif wdout != wdf:
            _wdflist[wdout] = cpy.deepcopy(_wdflist[wdf])

    try:
        btype = btype.lower()
        bt = [i for i,t in enumerate(btLegal) if t.find(btype) == 0]
        nbt = len(bt)
        if nbt == 1: btype = btLegal[bt[0]]
        elif nbt == 0:
            print DN+": Illegal BTYPE:", repr(btype)
            okay = False
        else:
            print DN+": Illegal BTYPE:", repr(btype)
            okay = False
    except :
        print DN+": Illegal btype:", repr(btype)
        okay = False
        
    if btype == 'boxcar' or btype == 'low' or btype == 'high':
        if frequency is None:
            print DN+": FREQUENCY keyword required for BTYPE=", repr(btype)
            okay = False
        elif not utils.is_number(frequency)[0]:
            print DN+": FREQUENCY keyword must be a number"
            okay = False
    else:
        if frange is None:
            print DN+": FRANGE keyword required for BTYPE=", repr(btype)
            okay = False
        else:
            try:
                lims = np.asarray(frange,dtype=float)
                if lims.shape != (2,):
                    raise ValueError
                frequency = lims
            except ValueError:
                print DN+": FRANGE keyword must be a 2-element list" + \
                         " of numeric frequency bounds for BTYPE=", repr(btype)
                okay = False
    if type(order) is not types.IntType or order < 1:
        print DN+": ORDER keyword must be positive integer"
        okay = False
    if padlen == 0: extra = {}
    elif type(padlen) is not types.IntType:
        print DN+": PADLEN keyword must be a non-negative integer, or None"
        okay = False
    else: extra = {'padlen':padlen}

    if np.diff(getX(wdf)).min() < 0.0:
        print DN+": Abscissa array must be monotonically increasing"
        okay = False            
        
    if not okay:
        print "\n" + filt.__doc__  ;  return None

    ds = _wdflist[wdout]
    if ds.typekey == 'U':
        deltax = ds.dx[0][0]
    else:
        dsx = ds.make_uniform(quiet=1)
        if dsx is not None: ds = dsx
        deltax = np.diff(ds.x[0][0]).mean()

    y = ds.data[0]

    if btype == 'boxcar':
        yf = utils.boxcar(deltax,y,frequency)
    else:
        # find 1/Nyquist frequency -> Nyquist periond
        frequency *= (2.0*deltax)

        # this to avoid error messages generated in scipy/signal/_arraytools.py
        import warnings
        warnings.simplefilter(action='ignore', category=FutureWarning)

        b,a = scipy.signal.butter(order,frequency,btype)

        if min(len(b),len(a)) != order + 1:
            print(DN+": WARNING -- At this frequency, filter is badly " + \
                      "conditioned. Try a lower order, or a Boxcar filter.")
        yf = scipy.signal.filtfilt(b,a,y,**extra)

    ds.data[0] = yf
    return


__all__.append('wfft')
def wfft(*args, **kwargs):
    '''\
Function to take the fast Fourier transform (FFT) of a WDF array.

Usage:
  wfft(wd1,[wd2], [cmplx=wd3])

Arguments:
  wd1:   WDF array to be FFTed (Note:  The array's abscissa (x) values
         must be monotonically increasing).
  wd2:   WDF array to receive the FFT data. If not supplied, the input
         array (wdf) will be overwritten with the FFT data.
  cmplx: If supplied, a WDF array to receive the imaginary part of the complex
         FFT (the real part is returned in `wd2'). If not supplied, the
         magnitude of the FFT is returned in `wd2'.

Return Value: None'''

    DN = 'WFFT'
    argnames = [ 'wd1', 'wd2' ]
    optvals = [ None ]
    kwdefs = { 'cmplx':None }
    ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, \
                             optvals, extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + wfft.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    okay = True
 
    if type(wd1) is not types.IntType: okay = False
    if not _wdflist.has_key(wd1):
        print DN+": WDF array",wd1,"does not contain valid data"
        okay = False
    if wd2 is None:  wd2 = wd1
    else:
        if type(wd2) is not types.IntType: okay = False
        elif wd2 != wd1:
            _wdflist[wd2] = cpy.deepcopy(_wdflist[wd1])

    if cmplx is not None:
        if type(cmplx) is not types.IntType:
            print DN+": CMPLX keyword must provide an integer WDF index"
            okay = False
        ds3 = cmplx
    
    if np.diff(getX(wd1)).min() < 0.0:
        print DN+": Abscissa array must be monotonically increasing"
        okay = False            

    if not okay:
        print "\n" + wfft.__doc__  ;  return None

    ds = _wdflist[wd2]
    if ds.typekey == 'N':
        dsx = ds.make_uniform(quiet=1)
        if dsx is not None:  ds = dsx
        x0tmp = np.empty((1),dtype=pff.PFFnp_float)
        x0tmp[:] = ds.g_range[0,0,0]
        ds.x0 = [x0tmp]
        dxtmp = np.empty((1),dtype=pff.PFFnp_float)
        dxtmp[:] = (ds.g_range[0,0,1] - ds.g_range[0,0,0]) / \
            (ds.nx[0][0] - 1)
        ds.dx = [dxtmp]
        del ds.x
        ds.rawname='UF1'
        ds.rawtype=pff.UF1
        ds.typekey='U'
        ds = pff.UNF_dataset(new=ds.__dict__)
        _wdflist[wd2] = ds
    nt = ds.nx[0][0] - 1 # need # of bins, not bounds
    dt = ds.dx[0][0]

    ##print(nt,dt,type(nt),type(dt),float(dt))
    ##x1 = np.fft.rfftfreq(nt-1,d=dt) ## not in early numpy distributions
    yave = 0.5*(ds.data[0][1:] + ds.data[0][:-1])
    yc = np.fft.rfft(yave) * dt

    ds.nx[0][0] = nt // 2 + 1
    assert(ds.nx[0][0] == yc.size)
    ds.dx[0][0] = 1.0/(nt*dt)
    ds.x0[0][0] = 0.0
    ds.glabels = np.asarray([['Frequency']])
    ds.fill_grid_range()
    if cmplx is None:
        ds.data[0] = abs(yc)
        ds.title += ' -- FFT Magnitude'
    else:
        _wdflist[ds3] = cpy.deepcopy(_wdflist[wd2])
        ds.data[0] = yc.real
        ds.title += ' -- FFT Real part'
        _wdflist[ds3].data[0] = yc.imag
        _wdflist[ds3].title += ' -- FFT Imag part'

    return


__all__.append('dif')

def dif(wd1, wd2=None, **kwargs):
    '''\
Function to compute the derivative of a WDF array.

Usage:
  dif(wd1,[wd2],centered=True,clabel=None,xlabel=None,ylabel=None)

Arguments:
  wd1:      WDF array to be differentiated (Note:  The array's abscissa (x)
            values must be monotonically increasing).
  wd2:      WDF array to receive the derivative. If not supplied, the input
            array (wdf) will be overwritten with the derivative.
  centered: If True, the numerical derivative will be "centered", in which case
            the returned array will have one fewer datapoints than the original
            array. If not supplied, its value will be True or False,
            respectively, for a supplied non-uniform or uniform dataset.
  clabel:   Comment label for output dataset.
  xlabel:   Abscissa (x) axis label for output dataset.
  ylabel:   Ordinate (y) axis label for output dataset.

Return Value: None'''

    kwvalid = ['centered', 'clabel', 'xlabel', 'ylabel']
     
    okay = True
    if type(wd1) is not types.IntType: okay = False
    if not _wdflist.has_key(wd1):
        print "WDF array",wd1,"does not contain valid data"
        return None
    if wd2 is None:  wd2 = wd1
    else:
        if type(wd2) is not types.IntType: okay = False
        elif wd2 != wd1:
            _wdflist[wd2] = cpy.deepcopy(_wdflist[wd1])

    keys = kwargs.keys()
    centered = True
    clabel = None
    xlabel = None
    ylabel = None
    for k in keys:
        mat = utils.findbestmatch(kwvalid,k)
        lmat = len(mat)
        if lmat != 1:
            okay = False
            if lmat == 0:
                print "Could not match supplied keyword \"" + k + "\""
                print "Valid keywords:",str(kwvalid)
            else:
                print "Could not uniquely match supplied keyword \"" + k + "\""
                print "It potentially matches valid keywords",str(mat)
        else:
            mat = mat[0]
            if mat != "centered" and type(kwargs[k]) is not types.StringType:
                print mat, "must be a string"
                okay = False
            exec mat + " = kwargs[k]"

    if np.diff(getX(wd1)).min() < 0.0:
        print DN+": Abscissa array must be monotonically increasing"
        okay = False            

    if not okay:
        print dif.__doc__
        return None

    ds = _wdflist[wd2]
    tkey = ds.typekey
    if centered is None:
        if tkey == 'N':  centered = True
        else:            centered = False

    y = ds.data[0]
    if centered:
        x = getX(wd2)
        dy = np.divide(np.diff(y),np.diff(x))
        ds.nx[0][0] = len(dy)
        if tkey == 'U':
            ds.x0[0][0] += 0.5*ds.dx[0][0]
        else:
            ds.x[0][0] = np.multiply(0.5,np.add(x[:-1],x[1:]))
        ds.fill_grid_range()
    else:
        if tkey == 'U':
            dy = np.gradient(y,ds.dx[0][0])
        else:
            x = getX(wd2)
            dy = np.empty((ds.nx[0][0]),dtype=pff.PFFnp_float)
            if len(x) == 2:
                dy[:] = (y[1] - y[0])/d1[0]
            else:
                d1 = np.diff(x)
                d2 = np.add(d1[1:],d1[:-1])
                a = np.divide(d1[:-1],d1[1:])
                zp = np.subtract(y[2:],y[1:-1])
                zm = np.subtract(y[:-2],y[1:-1])
                num = np.subtract(np.multiply(zp,a),np.divide(zm,a))
                dy[1:-1] = np.divide(num,np.add(d1[1:],d1[:-1]))
                alp = d2[0]/d1[0]
                dy[0] = ((y[1] - y[0])*alp - (y[2] - y[0])/alp)/d1[1]
                alp = d2[-1]/d1[-1]
                dy[-1] = ((y[-1] - y[-2])*alp - (y[-1] - y[-3])/alp)/d1[-2]

    ds.data[0] = dy

    if clabel is not None:  ds.title = clabel
    if xlabel is not None:
        ds.glabels = np.reshape(np.array([xlabel]), (1,1))
    if ylabel is not None:
        ds.dlabels = np.reshape(np.array([ylabel]), (1,1))


__all__.append('wint')

def wint(wd1, wd2=None, **kwargs):
    '''\
Function to compute the integral of a WDF array.

Usage:
  wint(wd1,[wd2],centered=True,moment=0,clabel=None,xlabel=None,ylabel=None)

Arguments:
  wd1:      WDF array to be integrated (Note:  The array's abscissa (x)
            values must be monotonically increasing).
  wd2:      WDF array to receive the integral. If not supplied, the input
            array (wdf) will be overwritten with the computed integral.
  centered: If True, the numerical integral will be "centered", in which case
            the returned array will have one more datapoint than the original
            array. If not supplied, its value will be True or False,
            respectively, for a supplied uniform or non-uniform dataset.
  moment:   If supplied, the integral will be applied to x**moment * y(x),
            where x and y are the abcissa and ordinate values of the `wd1'
            array.
  clabel:   Comment label for output dataset.
  xlabel:   Abscissa (x) axis label for output dataset.
  ylabel:   Ordinate (y) axis label for output dataset.

Return Value: None'''

    kwvalid = ['centered', 'moment', 'clabel', 'xlabel', 'ylabel']
     
    okay = True
    if type(wd1) is not types.IntType: okay = False
    if not _wdflist.has_key(wd1):
        print "WDF array",wd1,"does not contain valid data"
        return None
    if wd2 is None:  wd2 = wd1
    else:
        if type(wd2) is not types.IntType: okay = False
        elif wd2 != wd1:
            _wdflist[wd2] = cpy.deepcopy(_wdflist[wd1])

    keys = kwargs.keys()
    centered = True
    moment = 0
    clabel = None
    xlabel = None
    ylabel = None
    for k in keys:
        mat = utils.findbestmatch(kwvalid,k)
        lmat = len(mat)
        if lmat != 1:
            okay = False
            if lmat == 0:
                print "Could not match supplied keyword \"" + k + "\""
                print "Valid keywords:",str(kwvalid)
            else:
                print "Could not uniquely match supplied keyword \"" + k + "\""
                print "It potentially matches valid keywords",str(mat)
        else:
            mat = mat[0]
            val = kwargs[k]
            if mat != "centered":
                if mat == "moment":
                    num, t = utils.is_number(val)
                    if not num or \
                       (t is types.FloatType and kwargs[k] < 0.0): 
                        print mat, "must be an integer or non-negative float"
                        okay = False
                elif type(val) is not types.StringType:
                    print mat, "must be a string"
                    okay = False
            exec mat + " = val"

    if np.diff(getX(wd1)).min() < 0.0:
        print DN+": Abscissa array must be monotonically increasing"
        okay = False            

    if not okay:
        print wint.__doc__
        return None

    ds = _wdflist[wd2]
    tkey = ds.typekey
    if centered is None:
        if tkey == 'N':  centered  = False
        else:            centered = True

    y = ds.data[0]
    ##print centered,moment,tkey
    if tkey =='N' or moment:
        x = getX(wd2)
    if centered:
        npts = ds.nx[0][0] + 1
        if tkey == 'U':
            dx = ds.dx[0][0]
            ds.x0[0][0] -= 0.5*ds.dx[0][0]
            intgrd = np.multiply(dx,y)
            if moment: xnew = x
        else:
            sx0 = ds.findSpare('X0',3)
            if sx0 is not None: x0 = pff.i2f(sx0)
            else: x0 = None
            xnew = utils.XfFromXh(x,x0)
            dxn = np.diff(xnew)
            intgrd = np.multiply(dxn,y)
            ds.x[0][0] = xnew
        iy = np.empty(npts,dtype=pff.PFFnp_float)
        iy[0] = 0.0
        if moment: intgrd = np.multiply(intgrd,np.power(x,moment))
        iy[1:] = np.cumsum(intgrd)
        ds.nx[0][0] = npts
        ds.fill_grid_range()
        ds.data[0] = iy
    else:
        iy = ds.data[0]
        ym = np.add(iy[:-1],iy[1:])
        mul = 1.0
        if tkey == 'U':
            dx = ds.dx[0][0]
            intgrd = np.multiply(0.5*dx,ym)
        else:
            dx = np.diff(x)
            mul = 0.5
            intgrd = np.multiply(dx,ym)
        if moment:
            xm = np.add(x[:-1],x[1:])
            mul *= 0.5**moment
            intgrd = np.multiply(mul,np.multiply(intgrd,np.power(xm,moment)))
        elif mul != 1.0:
            intgrd = np.multiply(mul,intgrd)
        iy[0] = 0.0
        iy[1:] = np.cumsum(intgrd)


    if clabel is not None:  ds.title = clabel
    if xlabel is not None:
        ds.glabels = np.reshape(np.array([xlabel]), (1,1))
    if ylabel is not None:
        ds.dlabels = np.reshape(np.array([ylabel]), (1,1))


__all__.append('wind')
def wind(*args, **kwargs):
    '''\
Function which "windows" a WDF array, i.e., limits the range of its abscissa (x)
variable. Additionally, the abscissa range can optionally be shifted such that
it starts at zero.

Usage:
  wind(wdf,[wdout], [xrange=False], [tshift=False]))

Arguments:
  wdf:     WDF array to be "windowed".
  wdout:   WDF array to receive the "windowed" data. If not supplied, the input
           array (wdf) will be overwritten.
  xrange:  A 2-element lists of numbers which define the lower and upper limits
           for the new abscissa range. If not supplied, a secondary window will
           display a plot of the WDF array, and the cursor can be used to
           select the beginning and ending abscissa values. Alternately, if
           `xrange' is a string whose first character is 'z', an opportunity to
           zoom in on a specific region of the plot is provided before the
           cursor is used to select the abscissa range.
  tshift:  If True, the array will be translated in x such that it begins at
           x = 0'''

    DN = 'WIND'
    argnames = [ 'wd1', 'wd2' ]
    optvals = [ None ]
    kwdefs = { 'xrange':False, 'tshift':False }
    ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, \
                             optvals, extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + wind.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    okay = True
 
    if type(wd1) is not types.IntType: okay = False
    if not _wdflist.has_key(wd1):
        print DN+": WDF array",wd1,"does not contain valid data"
        okay = False
    if wd2 is None:  wd2 = wd1
    else:
        if type(wd2) is not types.IntType: okay = False
        elif wd2 != wd1:
            _wdflist[wd2] = cpy.deepcopy(_wdflist[wd1])

    w = None
    zoom = False
    if xrange is not False:
        try:
            w = np.asarray(xrange)
            s = w.shape
            if len(s) == 1 and s[0] == 2:
                t = str(w.dtype)
                if t[:3] != 'int' and t[:5] != 'float': w = None
                xrange = True
            elif type(xrange) is types.StringType:
                if len(xrange) > 0 and xrange.upper()[0]== 'Z': zoom = True
                else: w = None
            else: w = None
        except:
            w = None

        if w is None:
            print DN + ': Invalid XRANGE keyword value'  ;  return None
    
    if zoom or w is None:  w = getWdfWind(wd2,zoom=zoom)

    ##print w,zoom,w.dtype
         
    ds = _wdflist[wd2]
    nc = ds.nx[0][0]
    xmin,xmax = (w.min(),w.max())
    grmin,grmax = ds.g_range[0,0,:]
    if ds.typekey == 'U':
        x0 = ds.x0[0][0] ; dx = ds.dx[0][0] ; dxi = 1.0/dx
        i1 = int(max(0.0,xmin - x0)*dxi + 0.999)
        i2 = min(nc,int((min(xmax,grmax) - x0)*dxi) + 1)
        x1st = i1*dx + x0
        if tshift:  x0new = max(0.0, x1st - xmin)
        else: x0new = x1st
    else:
        x = ds.x[0][0]
        i1 = np.where(x > xmin)[0][0]
        i2 = np.where(x > xmax)[0][0]
        xnew = x[i1:i2]
        if tshift: xnew -= xmin
    ncnew = i2 - i1
    if ncnew < 3:
        print DN + ': Selected window contains insufficient points'
        return None

    ##print ncnew,i1,i2,xmin,xmax
    if ds.typekey == 'U':
        ##print x1st,x0,x0new,dx
        ds.x0[0][0] = x0new
    else:
        ##print x[i1],xnew
        ds.x[0][0] = xnew
    ds.nx[0][0] = ncnew
    ds.data[0] = ds.data[0][i1:i2]
    ds.fill_grid_range()


    return


__all__.append('fctn')

def fctn(wd1,fstr,*args,**kwargs):
    '''Apply a NUMPY function to the data in a WDF array.

Usage:
  fctn(wdf,fstr,[wdout])

Arguments:
  wdf:    Integer index of WDF array to which function will be applied.
  wdout:  Integer index of WDF array to receive array modification. If not
          supplied, the result will be stored in WDF.
  fstr:   String containing name of NUMPY function. Function must take
          one numeric array argument and return a numeric array with
          the same shape.  For example, FSTR='exp' will invoke the
          numpy.exp function on the data in WDF. Other examples of
          legal numpy functions are: sin, cos, arcsin, arccos, sinh,
          log, and log10. For more information, consult the NumPy
          Reference Manual.
  clabel: Comment label for output dataset.
  xlabel: Abscissa (x) axis label for output dataset.
  ylabel: Ordinate (y) axis label for output dataset.

Return Value: None'''

    attr_map = { 'clabel':'title','xlabel':'glabels', 'ylabel':'dlabels' }
    kwvalid = attr_map.keys()
    okay = True
    try:
        i = dir(np).index(fstr)
        exec  'if type(np.'+fstr+') is not np.ufunc: okay = False'
    except ValueError:
        okay = False
    if not okay:
        print "\"" + fstr + "\" is not a valid function"
        return None
    argc = len(args)
    wdout = None
    if argc == 1:
        if type(args[0]) is not types.IntType:  okay = False
        else: wdout = args[0]
    elif argc > 1: okay = False

    ##print wd1,wdout,okay,fstr

    ##print wdout

    check = [wd1]
    vlist = utils.parsewsl(_wdflist,check,valid=True)
    ##print vlist
    clen = len(check)  ;  vlen = len(vlist)
    if vlen != clen:
        bad = ''
        if vlen == 0:
            bad = str(check[0])
            if clen > 1: bad += (' ' + str(check[1]))
        else:
            if vlist[0] == check[0]: bad = str(check[1])
            else: bad = str(check[0])
        print "The following WDF arrays are not valid: " + bad
        return None

    keys = kwargs.keys()
    okay = True
    attr_set = []

    ##print "keys:",keys

    for k in keys:
        mat = utils.findbestmatch(kwvalid,k)
        lmat = len(mat)
        if lmat != 1:
            okay = False
            if lmat == 0:
                print "Could not match supplied keyword \"" + k + "\""
                print "Valid keywords:",str(kwvalid)
            else:
                print "Could not uniquely match supplied keyword \"" + k + "\""
                print "It potentially matches valid keywords",str(mat)
        else:
            mat = mat[0]
            val = kwargs[k]
            if type(val) is not types.StringType:
                okay = False
                print  "Supplied value for", mat, "must be a string"
            else:
                sval = repr(val)
                if  mat == 'xlabel' or mat == 'ylabel':
                    ts = attr_map[mat][:4]
                    scmd = ts + " = np.reshape(np.array([" + sval + \
                           "]),(1,1))"
                    exec scmd
                    sval = ts
            if okay:
                scmd = "dso." + attr_map[mat] + " = " + sval 
                attr_set.append(scmd)

    ##print "attr_set:", attr_set
    ##print glabels

    ##return None

    dssave = None
    if not okay:
        return None

    if wdout is not None:
        if _wdflist.has_key(wdout):  dssave = w2i(wdout,copy=False)
        xfr(wd1,wdout)
    else:
        wdout = wd1

    dso = w2i(wdout,copy=False)
    reset = False
    try:
        exec 'dso.data[0] = np.'+fstr+'(dso.data[0])'
    except ValueError,e:
        print "\"" + fstr + "\" is not an allowed function"
        reset = True
        
    if reset:
        if dssave is not None:
            _wdflist[wdout] = dssave
        else:
            _wdflist[wdout] = dso
        return None

    for s in attr_set:  exec s

    ##print "bop returning", wdout
    return wdout

__all__.append('add')

def add(wd1,*args,**kwargs):
    '''Usage:
  add(wd1,wd2,[wdout])
    or
  add(wd1,0,value,[wdout])

Keyword arguments \'clabel\', \'xlabel\', and \'ylabel\' are also supported'''

    rval = binary_op('+',wd1,*args,**kwargs)
    if type(rval) == types.StringType and typerval[:4] == 'fail':
        print 'Error:'+rval[4:] 
        print add.__doc__
        rval = None

    return rval

__all__.append('sub')

def sub(wd1,*args,**kwargs):
    '''Usage:
  sub(wd1,wd2,[wdout])
    or
  sub(wd1,0,value,[wdout])

Keyword arguments \'clabel\', \'xlabel\', and \'ylabel\' are also supported'''

    rval = binary_op('-',wd1,*args,**kwargs)
    if type(rval) == types.StringType and rval[:4] == 'fail':
        print 'Error:'+rval[4:] 
        print sub.__doc__
        rval = None

    return rval

__all__.append('mul')

def mul(wd1,*args,**kwargs):
    '''Usage:
  mul(wd1,wd2,[wdout])
    or
  mul(wd1,0,value,[wdout])

Keyword arguments \'clabel\', \'xlabel\', and \'ylabel\' are also supported'''

    rval = binary_op('*',wd1,*args,**kwargs)
    if type(rval) == types.StringType and rval[:4] == 'fail':
        print 'Error:'+rval[4:] 
        print mul.__doc__
        rval = None

    return rval

__all__.append('div')

def div(wd1,*args,**kwargs):
    '''Usage:
  div(wd1,wd2,[wdout],[dzero=0.0])
    or
  div(wd1,0,value,[wdout],[dzero=0.0])

Keyword arguments \'clabel\', \'xlabel\', and \'ylabel\' are also supported'''
    
    kwdefs = { 'dzero':0.0 }
    kwvalid = kwdefs.keys()
    kwextra = {}

    for k in kwvalid: exec k + " = kwdefs[k]"
    keys = kwargs.keys()
    okay = True

    for k in keys:
        mat = utils.findbestmatch(kwvalid,k)
        lmat = len(mat)
        if lmat == 1:
            exec mat[0] + " = kwargs[k]"
        elif lmat == 0:
            kwextra[k] = kwargs[k]
        else:
            okay = False
            print "Could not uniquely match supplied keyword \"" + k + "\""
            print "It potentially matches valid keywords",str(mat)

    rval = binary_op('/',wd1,*args,**kwextra)
    if type(rval) == types.StringType and rval[:4] == 'fail':
        print 'Error:'+rval[4:] 
        print div.__doc__
        rval = None
    elif rval is not None:
        data = _wdflist[rval].data[0]
        bad = np.where(~(np.isfinite(data)))
        data[bad] = dzero

    return rval


__all__.append('wpow')

def wpow(wd1,*args,**kwargs):
    '''Usage:
  wpow(wd1,wd2,[wdout])
    or
  wpow(wd1,0,value,[wdout])

Keyword arguments \'clabel\', \'xlabel\', and \'ylabel\' are also supported'''
    
    rval = binary_op('^',wd1,*args,**kwargs)
    if type(rval) == types.StringType and rval[:4] == 'fail':
        print 'Error:'+rval[4:] 
        if not _tmp_quiet[0]: print wpow.__doc__
        rval = None
    elif rval is not None:
        data = _wdflist[rval].data[0]
        bad = np.where(~(np.isfinite(data)))
        data[bad] = 0.0

    return rval


__all__.append('rtp')

def rtp(wd1,value,*args,**kwargs):
    '''Usage:
  rtp(wd1,value,[wdout])

Keyword arguments \'clabel\', \'c=xlabel\', and \'ylabel\' are also supported.'''
    
    _tmp_quiet[0] = True
    rval = wpow(wd1,0,value,*args,**kwargs)
    _tmp_quiet[0] = False
    if type(rval) == types.StringType and rval[:4] == 'fail':
        print 'Error:'+rval[4:] 
        print rtp.__doc__
        rval = None
    elif rval is not None:
        data = _wdflist[rval].data[0]
        bad = np.where(~(np.isfinite(data)))
        data[bad] = 0.0

    return rval

def binary_op(op,wd1,*args,**kwargs):
    ##print args
    ##print kwargs
    attr_map = { 'clabel':'title','xlabel':'glabels', 'ylabel':'dlabels' }
    kwvalid = attr_map.keys()
    validops = "+-*/^"
    ##print 'binOp:',op,wd1,args,kwargs
    if validops.find(op) < 0:
        print "\"" + op + "\" is not a valid operation"
        return None
    argc = len(args)
    if argc < 1 or type(wd1) is not types.IntType or \
       type(args[0]) is not types.IntType:
        return 'fail Invalid or missing arguments'

    scalar = None
    nextarg = 1
    check = [wd1]
    ##print argc, args[0]
    if args[0] > 0:
        ##print "wd2 provided"
        wd2 = args[0]
        check.append(wd2)
    else:
        if argc < 2: return 'fail Missing scalar argument'
        nextarg = 2
        wd2 = None
        try:
            scalar = float(args[1])
            ##print "Scalar",wd2,nextarg,scalar,t
        except (TypeError, ValueError), e:
            return 'fail ' + str(e)

    ##print wd1,wd2,scalar,check

    wdout = None
    if argc > nextarg:
        wdout = args[nextarg]
        if type(wdout) is not types.IntType:
            return 'fail Invalid type for wdout'
        
    ##print wdout

    vlist = utils.parsewsl(_wdflist,check,valid=True)
    ##print vlist
    clen = len(check)  ;  vlen = len(vlist)
    if vlen != clen:
        bad = ''
        if vlen == 0:
            bad = str(check[0])
            if clen > 1: bad += (' ' + str(check[1]))
        else:
            if vlist[0] == check[0]: bad = str(check[1])
            else: bad = str(check[0])
        print "The following WDF arrays are not valid: " + bad
        return None

    keys = kwargs.keys()
    okay = True
    attr_set = []

    ##print "keys:",keys

    for k in keys:
        mat = utils.findbestmatch(kwvalid,k)
        lmat = len(mat)
        if lmat != 1:
            okay = False
            if lmat == 0:
                print "Could not match supplied keyword \"" + k + "\""
                print "Valid keywords:",str(kwvalid)
            else:
                print "Could not uniquely match supplied keyword \"" + k + "\""
                print "It potentially matches valid keywords",str(mat)
        else:
            mat = mat[0]
            val = kwargs[k]
            if type(val) is not types.StringType:
                okay = False
                print  "Supplied value for", mat, "must be a string"
            else:
                sval = repr(val)
                if  mat == 'xlabel' or mat == 'ylabel':
                    ts = attr_map[mat][:4]
                    scmd = ts + " = np.reshape(np.array([" + sval + \
                           "]),(1,1))"
                    exec scmd
                    sval = ts
            if okay:
                scmd = "dso." + attr_map[mat] + " = " + sval 
                attr_set.append(scmd)

    ##print "attr_set:", attr_set

    if not okay:
        return None

    if wdout is not None:
        xfr(wd1,wdout)
    else:
        wdout = wd1

    dso = w2i(wdout,copy=False)
    reset = False
    if scalar is not None:
        op2 = scalar
    else:
        ds2 = w2i(wd2,copy=False)
        xcom, m1, m2 = get_common_time(wdout,wd2)
        if xcom is None:
            op2 = ds2.data[0]
        else:
            nxc = len(xcom)
            newtype = 'U'
            if m2:
                op2 = ds2.data[0]
                if ds2.typekey == 'N': newtype = 'N'
            else:
                op2 = _fit2grid(ds2.getx(),ds2.data[0],xcom)

            if not m1:
                dso.data[0] = _fit2grid(dso.getx(),dso.data[0],xcom)
                if newtype == 'N':
                    if dso.typekey == 'N':
                        dso.x[0][0] = xcom
                        dso.nx[0][0] = nxc
                    else:  #  dso.typekey == 'U'
                        dso = _u2n(dso,xcom,True)
                        reset = True
                else:  #  newtype == 'U'
                    if dso.typekey == 'N':
                        dso = _n2u(dso,xcom,True)
                        reset = True
                    else:  #  dso.typekey == 'U'
                        dso.x0[0][0] = xcom[0]
                        dso.dx[0][0] = np.diff(xcom).mean()
                        dso.nx[0][0] = nxc

                dso.fill_grid_range()

    if op == '+':
        dso.data[0] = np.add(dso.data[0],op2)
    elif op == '-':
        dso.data[0] = np.subtract(dso.data[0],op2)
    elif op == '*':
        dso.data[0] = np.multiply(dso.data[0],op2)
    elif op == '/':
        dso.data[0] = np.divide(dso.data[0],op2)
    elif op == '^':
        dso.data[0] = np.power(dso.data[0],op2)

    for s in attr_set:  exec s

    if reset: _wdflist[wdout] = dso
    ##print "bop returning", wdout
    return wdout

__all__.append('getWdfWind')
def getWdfWind(*args,**kwargs):
    '''Returns an array containing the ordinate (Y) values of a WDF array.

Usage:
  getWdfWind(wdf, [window=bool|list], [stddev=bool] )

Arguments:
  wdf:   Integer index of the WDF array.

Return value: If successful, returns array of ordinate values of the WDF array,
              or None on error.'''

    DN = 'getWdfWind'
    argnames = ['wdf']
    optvals = [ ]
    kwdefs = { 'zoom':False }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + getWdfWind.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    if not _wdflist.has_key(wdf):
        print DN + ':',wdf, "does not have valid data"  ;  return None

    ds = _wdflist[wdf]
    try:
        fhandle = plt.figure("Specify Window")
    except ValueError:
        fhandle = plt.figure(0)
    if (zoom): plt.get_current_fig_manager().toolbar.zoom()
    plo(wdf)
    w = getAxisWind(zoom)
    plt.close(fhandle)
    return w


__all__.append('getAxisWind')
def getAxisWind(*args,**kwargs):
    '''

Usage:
  getAxisWind([zoom=bool])

Arguments:
  wdf:   Integer index of the WDF array.

Return value: If successful, returns array of ordinate values of the WDF array,
              or None on error.'''

    DN = 'getAxisWind'
    argnames = ['azoom']
    optvals = [ None ]
    kwdefs = { 'zoom':None }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + getAxisWind.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    #keys.sort()
    #for k in keys:   exec "print k, " + k

    if azoom is not None and zoom is not None:
        print DN + ": Invalid Syntax\n" + getAxisWind.__doc__
        return None
    if azoom == 0: zoom = False
    elif azoom is not None or zoom is not None: zoom = True
    else: zoom = False

    if zoom:
        hak("Hit <CR> when ready to select WINDOW endpoints: ")
        tb = plt.get_current_fig_manager().toolbar
        if tb.mode == 'zoom rect': tb.zoom()
        elif tb.mode == 'pan/zoom': tb.pan()
    print 'Use cursor to select two WINDOW endpoints'
    w = plots.cur(2)[:,0]
    return np.array([w.min(),w.max()])

def _n2u(dsin, xnew, alreadyuniform=False):
    if not isinstance(dsin,pff.NUNF_dataset):
        print "_n2u: input DS not NONUNIFORM"
        return None
    x0 = xnew[0]
    nx = len(xnew)
    rng = (xnew[-1] - x0)
    if not alreadyuniform:
        dx = np.diff(xnew).min()
        nx = np.int64(round(rng/dx)) + 1
        xu = np.asarray(np.linspace(x0,xnew[-1],nx),dtype=pff.PFFnp_float)
        data = _fit2grid(dsin.getx(),dsin.data[0],xu)
    else:
        data = dsin.data[0]


    dx = rng/(nx - 1)
    dirs = dir(dsin)
    udict = {'x0':[np.array([x0],dtype=pff.PFFnp_float)], \
             'dx':[np.array([dx],dtype=pff.PFFnp_float)], \
             'nx':[np.array([nx],dtype=pff.PFFnp_long)], \
             'data':[data], 'rawname':'UF1', 'rawtype':pff.UF1}
    for s in dirs:
        if s[0:2] == "__": continue
        exec "t = type(dsin." + s + ")"
        if t is not types.MethodType:
            if s == 'x' or s == 'nx' or s == 'data' or s == 'rawname' or \
               s == 'typekey' or s == 'rawtype'  : continue
            exec "udict['" + s + "'] = dsin." + s

    return pff.UNF_dataset(new=udict)
        
def _u2n(dsin, xnew, alreadyuniform=False):
    if not isinstance(dsin,pff.UNF_dataset):
        print "_u2n: input DS not UNIFORM"
        return None
    nx = len(xnew)

    if not alreadyuniform:
        data = _fit2grid(dsin.getx(),dsin.data[0],xnew)
    else:
        data = dsin.data[0]

    dirs = dir(dsin)
    ndict = {'x':[[xnew]], 'nx':[np.array([nx],dtype=pff.PFFnp_long)], \
             'data':[data], 'rawname':'NGD', 'rawtype':pff.NGD}
    for s in dirs:
        if s[0:2] == "__": continue
        exec "t = type(dsin." + s + ")"
        if t is not types.MethodType:
            if s == 'x0' or s == 'dx' or s == 'nx' or s == 'data' or \
              s == 'rawname' or  s == 'typekey' or s == 'rawtype'  : continue
            exec "ndict['" + s + "'] = dsin." + s

    print ndict['nx']
    return pff.NUNF_dataset(new=ndict)
        
def _fit2grid(oldx, oldy, newx):
    nx = len(oldx)
    fp = np.arange(nx)
    intrp = np.interp(newx,oldx,fp)
    f1, find = np.modf(intrp)
    ind = np.int32(find)
    w = np.where(ind < 0)[0]
    ind[w] = 0
    f1[w] = 0.0
    w = np.where(ind > nx - 2)[0]
    ind[w] = nx - 2
    f1[w] = 1.0
    return (1.0 - f1)*oldy[ind] + f1*oldy[ind+1]

_left_xr = None
_tmp_quiet = [False]

try:
    _wdflist
except NameError:
    _wdflist = {}

##print "1D loaded"
