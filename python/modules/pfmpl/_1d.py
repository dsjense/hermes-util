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

__doc__ = \
'''Set of PFMPL functions for reading, writing, plotting, and otherwise
manipulating one-dimensional PFF datasets.'''

__all__ = [ ]

__all__.append('re')
def re(*args, **kwargs):
    '''Read waveform data from a PFF file and store in WDF arrays.

Usage:
  re( wdfa, dataset, [fileid=integer], [xydata=bool] )

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
    kwdefs = { 'fileid':0, 'xydata':None }

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
           (X, Y1, Y2, ..., Ynvar)
  out:     First WDF array or list of WDF arrays to receive waveforms
  format:  format of file:
              0: only data values in the file maximum of LIMIT values
              1: Number of remaining lines in first line
              2: First NVAR lines containing data labels 
              3: First line: # of points, next NVAR lines: data labels
              4: EXCEL CSV format: First line contains NVAR data labels
           All formats support space, tab, and/or comma delimiters
  limit:   Maximum number of data-containing lines to be read
  skip:    Number of initial header lines in file to be ignored
  xydata:  If True, the data will be left on a nonuniform grid.
           If False, the data unconditionally converted to a uniform grid.
           If None, the data will be converted to a uniform grid only if its
           abscissa values are already uniformly spaced.
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

    ##print x.shape,y.shape,labs
    npts = len(x)

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
    dct['blabels'] = [ np.array(['']) ]
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
        if not xydata:  # None or False
            uds = ds.make_uniform(quiet=1)
            ##print uds, xydata
            if uds is None or xydata is not None:
                if uds is not None: ds = uds
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
                del ds.typekey
                ds = pff.UNF_dataset(new=ds.__dict__)
                    
        _wdflist[out[i]] = ds

    return npts

__all__.append('readasc')
def readasc(*args,**kwargs):
    '''Usage:
  readasc(file,nvar, [format=int], [limit=int], [skip=int], [dtype=numpy.type] )

Arguments:
  file:    file name, or Python file object
  nvar:    Number of dependent variables in an input line, i.e.,
           (X, Y1, Y2, ..., Ynvar)
  format:  format of file:
              0: only data values in the file maximum of LIMIT values
              1: Number of remaining lines in first line
              2: First NVAR lines containing data labels 
              3: First line: # of points, next NVAR lines: data labels
              4: EXCEL CSV format: First line contains NVAR data labels
           All formats support space, tab, and/or comma delimiters
  limit:   Maximum number of data-containing lines to be read
  skip:    Number of initial header lines in file to be ignored
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
    kwdefs = { 'format':0, 'limit':None, 'skip':0, \
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
    if type(format) is not types.IntType or format < 0 or format > 4:
        bad.append('format')
    if len(bad) > 0:
        print DN + ": The following arguments were incorrectly specified:",bad
        okay = False

    if not okay:
        print readasc.__doc__  ;  return None

    f.seek(0)
    for i in range(skip): f.readline()

    nvals = None
    labs = []
    bad = False
    lcount = 0
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
    
    if bad:
        print DN + ": EOF encountered while reading labels"
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
    x = np.empty((ns,),dtype=dtype)
    y = np.empty((ns,nvar),dtype=dtype,order='F')

    ex = regex.compile("[ ,\t\n\r]+")
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
            x[i] = vals[0]
            y[i,:] = vals[1:]
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
                        dt = 'a' + str(len(val))
                        ts = attr_map[k][:4]
                        scmd = ts + " = np.reshape(np.array([" + sval + \
                               "],dtype=(dt)),(1,1))"
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
      [lw=int], [charsize=int], [left=bool], [right=bool], \\
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
               'left':False, 'right':False, 'use_fig':None }
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

    if not okay:   return None

    f = plt.gcf()
    if not nogrid:
        f,ax = plots.adv_frame(leftright, use_fig=use_fig)
        if ax is None:  return None
        xr = xrange  ;  yr = yrange
        if (not right and xrange is None) or yrange is None:
            tlist = plist
            if not overlay: tlist = plist[0:1]
            for w in tlist:
                if not right and xrange is None:
                    mnx = getXRange(w)
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
                xr = utils.nice_bounds(xmin,xmax)[0:2]
            if yrange is None:
                yr = utils.nice_bounds(ymin,ymax)[0:2]
        if left:  _left_xr = xr
        else: _left_xr = None
        if right: xr = _left_xr
        ax.set_xlim(xr)
        ax.set_ylim(yr)
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
    for a in f.axes:
        iplt += len(a.lines)
    ##print wait, reset, nplts
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
                xmin,xmax = getXRange(w)
                xr = utils.nice_bounds(xmin,xmax)[0:2]
            if yrange is None:
                ymin,ymax = getYRange(w)
                yr = utils.nice_bounds(ymin,ymax)[0:2]
            ax.set_xlim(xr)
            ax.set_ylim(yr)
            if title == ".": ax.set_title(getLabel(w))
            if xlabel is None: ax.set_xlabel(getLabel(w,'X'))
            if ylabel is None: ax.set_ylabel(getLabel(w,'Y'))
            plots.set_frame_props(ax,lw,charsize,right)
        ls = plots.get_property(iplt,lmap)
        c = plots.get_property(iplt,cmap)
        ax.plot(getX(w),getY(w),label=getLabel(w),c=c,ls=ls,**pextra)
        reset = not overlay
        iplt += 1

    if legend is not None:
        handles = []  ;  labels = []
        for a in f.axes:
            h,l = a.get_legend_handles_labels()
            handles += h  ;  labels += l
        f.axes[0].legend(handles,labels,loc=legend,frameon=False, \
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
      [lw=int], [charsize=int] )

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

Note: The default values of the keywords denoted with '*' above, can be modified
      using the SETDEFAULT keyword with the PLO command.

Return value: If error encountered, returns None. Otherwise, 0 is returned'''

    DN = 'XPLO'
    argnames = [ 'wdx', 'wdy' ]
    optvals = [ ]
    kwdefs = { 'out':None }
    needed = argnames[:]  ;  needed.extend(kwdefs.keys())
    print id(argnames),id(needed)
    plopts_toss = [ 'overlay', 'legend' ]
               ##, '':, '':, '':, '':, '':, '':, 
    kwdefs.update(_plopts_cur) ## append settable option defaults
    for toss in plopts_toss:
        del kwdefs[toss]
    print kwdefs,args,len(args)

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]  ;  nr = [ 0,None ]
        print nl[res] + xplo.__doc__  ;  return nr[res]

    for k in needed:
        exec k + " = res[k]"
        del res[k]
    print wdx,wdy,out
    print res

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
    print xlabel,ylabel,title
    if xlabel is None:   xlabel = getLabel(wdx,'Y')
    if ylabel is None:   ylabel = getLabel(wdy,'Y')
    if title is None:   title = ylabel + ' vs. ' + xlabel
    print repr(xlabel),repr(ylabel),repr(title)
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
                        dt = 'a' + str(max(1,len(s)))
                        ds.glabels = np.reshape(np.array([s],dtype=(dt)), (1,1))
                    elif k == 'ylabel':
                        s = ds.dlabels[0][0] + res[k]
                        dt = 'a' + str(max(1,len(s)))
                        ds.dlabels = np.reshape(np.array([s],dtype=(dt)), (1,1))
                else:
                    if k == 'clabel':   ds.title = res[k]
                    elif k == 'tlabel': ds.typename = res[k]
                    else:
                      s = res[k]
                      dt = 'a' + str(max(1,len(s)))
                      if k == 'xlabel':
                        ds.glabels = np.reshape(np.array([s],dtype=(dt)), (1,1))
                      elif k == 'ylabel':
                        ds.dlabels = np.reshape(np.array([s],dtype=(dt)), (1,1))

    return len(clist)


__all__.append('i2w')
def i2w(*args,**kwargs):
    '''Convert X and Y arrays to a waveform dataset and/or WDF array. In a
different form, it can be used to copy a waveform dataset into a WDF array.

Usage:
  i2w(x, y, [wdf], [clabel=str], [tlabel=str], [file=str], [xlabel=str], \\
      [ylabel=str], [copy=True|False] )
    or
  i2w(ds, wdf, [clabel=str], [tlabel=str], [file=str], [xlabel=str], \\
      [ylabel=str], [copy=True|False])

Arguments:
  x:      list or numpy array containing abscissa (x) values.
  y:      list or numpy array containing ordinate (y) values.
  wdf:    if supplied, the produced dataset is placed in this WDF index.
  ds:     PFF dataset to be placed in a WDF array.
  clabel: Comment label for output dataset.
  tlabel: Type label for output dataset.
  xlabel: Abscissa (x) axis label for output dataset.
  ylabel: Ordinate (y) axis label for output dataset.
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
               'ylabel':None, 'copy':None }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]  ;  nr = [ 0,None ]
        print nl[res] + i2w.__doc__  ;  return None

    rcpy = res['copy']  ;  del res['copy']
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
    keys = res.keys()
    if res.has_key('ylabel'):
        print 'ylabel',res['ylabel'],repr(type(res['ylabel']))
    for k in keys:
        # Right now, all settable keywords are strings
        if type(res[k]) is not types.StringType:
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
    dict = {'data': [yy], 'apptype':3, 'adim':1, 'sdim':1, 'spare':[None], \
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
        gr = [xx[0], xx[-1]]
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
    if prompt is None:
        #return pex.hak()
        return raw_input("<CR> to continue: ")
    else:
        #return pex.hak(prompt)
        return raw_input(prompt)

__all__.append('get_common_time')

def get_common_time(wd1, wd2):
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
    
    if xlow == rng1[0] and xhi  == rng1[1] and dt1 <= dt2:
        m1 = True
    elif xlow == rng2[0] and xhi  == rng2[1] and dt2 <= dt1:
        m2 = True
    print xlow,xhi

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


def _is_wdfds(ds):
    if isinstance(ds,pff.blkgrid_dataset):
        if ds.nblk == 1 and ds.sdim == 1 and ds.adim == 1:  return True

    return False

__all__.append('dif')

def dif(wd1, wd2=None, **kwargs):
    '''Usage:
  dif(wd1,[wd2],centered=True,clabel=None,xlabel=None,ylabel=None)'''

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
        dt = 'a' + str(len(xlabel))
        ds.glabels = np.reshape(np.array([xlabel],dtype=(dt)), (1,1))
    if ylabel is not None:
        dt = 'a' + str(len(ylabel))
        ds.dlabels = np.reshape(np.array([ylabel],dtype=(dt)), (1,1))


__all__.append('wint')

def wint(wd1, wd2=None, **kwargs):
    '''Usage:
  wint(wd1,[wd2],centered=True,moment=0,clabel=None,xlabel=None,ylabel=None)'''

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

    if not okay:
        print wint.__doc__
        return None

    ds = _wdflist[wd2]
    tkey = ds.typekey
    if centered is None:
        if tkey == 'N':  centered  = False
        else:            centered = True

    y = ds.data[0]
    print centered,moment,tkey
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
            xnew = np.empty(npts,dtype=pff.PFFnp_float)
            xnew[0] = x[0] - 0.5*(x[1] - x[0])
            xnew[-1] = x[-1] + 0.5*(x[-1] - x[-2])
            xnew[1:-1] = np.multiply(0.5,np.add(x[:-1],x[1:]))
            dxn = np.diff(xnew)
            intgrd = np.multiply(dxn,y)
            ds.x[0][0] = xnew
        iy = np.empty(npts,dtype=pff.PFFnp_float)
        iy[0] = 0.0
        if moment: intgrd = np.multiply(intgrd,np.power(xnew,moment))
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
        dt = 'a' + str(len(xlabel))
        ds.glabels = np.reshape(np.array([xlabel],dtype=(dt)), (1,1))
    if ylabel is not None:
        dt = 'a' + str(len(ylabel))
        ds.dlabels = np.reshape(np.array([ylabel],dtype=(dt)), (1,1))


__all__.append('add')

def add(wd1,*args,**kwargs):
    '''Usage:
  add(wd1,wd2,[wdout])
    or
  add(wd1,0,value,[wdout])

Keyword arguments \'clabel\', \'xlabel\', and \'ylabel\' are also supported'''

    rval = binary_op('+',wd1,*args,**kwargs)
    if rval == 'fail':
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
    if rval == 'fail':
        print subract.__doc__
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
    if rval == 'fail':
        print muliply.__doc__
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
    if rval == 'fail':
        print divide.__doc__
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
    if rval == 'fail':
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
    if rval == 'fail':
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
        return 'fail'

    scalar = None
    nextarg = 1
    check = [wd1]
    ##print argc, args[0]
    if args[0] > 0:
        ##print "wd2 provided"
        wd2 = args[0]
        check.append(wd2)
    else:
        if argc < 2: return 'fail'
        nextarg = 2
        wd2 = None
        scalar = args[1]
        t = type(scalar)
        ##print "Scalar",wd2,nextarg,scalar,t
        if t is not types.IntType and t is not types.LongType and \
           t is not types.FloatType:
            return 'fail'

    ##print wd1,wd2,scalar,check

    wdout = None
    if argc > nextarg:
        wdout = args[nextarg]
        if type(wdout) is not types.IntType:
            return 'fail'
        
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
                    dt = 'a' + str(len(val))
                    ts = attr_map[mat][:4]
                    scmd = ts + " = np.reshape(np.array([" + sval + \
                           "],dtype=(dt)),(1,1))"
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
                        dso.x0[0][0] = ds2.x0[0][0]
                        dso.dx[0][0] = ds2.dx[0][0]
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
