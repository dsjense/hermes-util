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
import pff
import pff_ext as pex
import plots
from plots import _ovrplt
import utils
import _1d
import math
import types
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.lines
import matplotlib.cm
import matplotlib.patches
import copy as cpy

__all__ = [ ]

__all__.append('readgrd')
def readgrd(*args, **kwargs):
    '''Read in a grid dataset (block-grid with no attribute data).

Usage:
  readgrd( [struct], dsindex, [fileid=int] )

Arguments:
  struct:   Integer structure index for storing read dataset.
  dsindex:  Index of dataset to be read
  fileid:   Index of PFF file from which to read dataset.

Return value: If successful and STRUCT not specified, returns dataset read.
              Otherwise, returns 0 if successful and None on error'''

    DN = 'READGRD'
    argnames = ['a1','a2']
    optvals = [ None ]
    kwdefs = { 'fileid':0 }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + readgrd.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    okay = True
    strnum = None
    if a2 is None:  dsindex = a1
    else:
        dsindex = a2  ;  strnum = a1
        if type(strnum) is not types.IntType:
            print DN + ": Supplied structure number (" + str(strnum) + \
                   ") must be an integer"  ;  okay = False
    if type(dsindex) is not types.IntType:
        print DN + ": Supplied dataset index (" + str(dsindex) + \
              ") must be an integer"  ;  okay = False

    if not okay:
        print "\n" + readgrd.__doc__
        return

    ds = pff.read_dataset(dsindex,fileid)
    if _what_type(ds) != 'G':
        print DN + ": Unable to read dataset", dsindex, "from file", fileid
        return None

    ##print dsindex, strnum, ds.adim, ds.sdim, ds.apptype

    if strnum is None:  return ds
    _struclist[strnum] = ds  ;  return 0

__all__.append('readfld')
def readfld(*args, **kwargs):
    '''Read in a field dataset (block-grid with attribute data).

Usage:
  readfld( [struct], dsindex, [fileid=int] )

Arguments:
  struct:   Integer structure index for storing read dataset.
  dsindex:  Index of dataset to be read
  fileid:   Index of PFF file from which to read dataset.

Return value: If successful and STRUCT not specified, returns dataset read.
              Otherwise, returns 0 if successful and None on error'''

    DN = 'READFLD'
    argnames = ['a1','a2']
    optvals = [ None ]
    kwdefs = { 'fileid':0 }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + readfld.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    okay = True
    strnum = None
    if a2 is None:  dsindex = a1
    else:
        dsindex = a2  ;  strnum = a1
        if type(strnum) is not types.IntType:
            print DN + ": Supplied structure number (" + str(strnum) + \
                   ") must be an integer"  ;  okay = False
    if type(dsindex) is types.StringType:
        try:
            tmp = pex.getmatch(dsindex,fileid)
        except pex.PFF_Error, e:
            print DN + ": Error:", e
            return None
        if tmp[2] > 0:
            dsl = pff.buf2list(tmp)
            if len(dsl) != 1:
                print DN + ": Supplied string '" + dsindex + \
                   "' must match a single dataset"  ;  okay = False
            else: dsindex = dsl[0]
            print dsindex
        else:
            print DN + ": No datasets matching \"" + dataset + "\" found"
            return 0
    elif type(dsindex) is not types.IntType:
        print DN + ": Supplied dataset index (" + str(dsindex) + \
              ") must be an integer"  ;  okay = False

    if not okay:
        print "\n" + readfld.__doc__
        return None

    ds = pff.read_dataset(dsindex,fileid)
    if _what_type(ds) != 'F':
        print DN + ": Unable to read dataset", dsindex, "from file", fileid
        return None

    if strnum is None:  return ds
    _struclist[strnum] = ds  ;  return 0

__all__.append('readcon')
def readcon(*args, **kwargs):
    '''Read in a conductor dataset (vertex dataset with 4*N verticies and
with no attribute data).

Usage:
  readcon( [struct], dsindices, [fileid=int] )

Arguments:
  struct:     Integer structure index for storing read dataset
  dsindices:  Index/indices of dataset/s to be read (integer or list of
              integers)
  fileid:     Index of PFF file from which to read dataset/s.

Return value: If successful and STRUCT not specified, returns a quadlist3d
              dataset merged from the read datasets.
              Otherwise, returns 0 if successful and None on error'''

    DN = 'READCON'
    argnames = ['a1','a2']
    optvals = [ None ]
    kwdefs = { 'fileid':0 }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + readcon.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    okay = True
    strnum = None
    if a2 is None: dsindices = a1
    else:
        dsindices = a2  ;  strnum = a1
        dsindices = args[1]
        if type(strnum) is not types.IntType:
            print DN + ": Supplied structure number (" + str(strnum) + \
                   ") must be an integer"  ;  okay = False

    t = type(dsindices)
    if t is not types.ListType and t is not types.TupleType:
        if t is types.StringType:
            try:
                tmp = pex.getmatch(dsindices,fileid)
            except pex.PFF_Error, e:
                print DN + ": Error:", e
                return None
            if tmp[2] > 0:
                dsindices = pff.buf2list(tmp)
            else:
                print DN + ": No datasets matching \"" + dsindices + "\" found"
                return 0
        else:
            dsindices = [ dsindices ]

    ##print dsindices
    for ind in dsindices:
        if type(ind) is not types.IntType:
            print DN + ": Supplied dataset index (" + str(ind) + \
                  ") must be an integer"  ;  okay = False
        
    if not okay:
        print "\n" + readcon.__doc__
        return

    dsout = None
    for ind in dsindices:
        c = pff.read_dataset(ind,fileid)
        rtype = _what_type(c)
        okay = True
        if rtype == 'V':
            c = quadlist3d(c)
            if len(c.__dict__) == 0: okay = False
        elif rtype != 'Q': okay = False
        if not okay:
            print DN + ": Unable to read dataset", ind, "from file", fileid
            return None
        if dsout is None:  dsout = c
        else:   dsout.append(c)

    if strnum is None:  return dsout
    _struclist[strnum] = dsout  ;  return 0

__all__.append('readpar')
def readpar(*args, **kwargs):
    '''Read a particle dataset (vertex dataset with or without attribute data).

Usage:
  readpar( [struct], dsindex, [fileid=int], [electron=bool], [proton=bool], \\
           [amass=float] )

Arguments:
  struct:   Integer structure index for storing read dataset
  dsindex:  Index of dataset to be read (integer)
  electron: If set, normalized particle energy is multiplied by the electron
            rest energy (in MeV)
  proton:   If set, normalized particle energy is multiplied by the proton
            rest energy (in MeV)
  amass:    If set, normalized particle energy is multiplied by the rest energy
            (in MeV) of an ion with atomic mass AMASS (in AMU).
  fileid:   Index of PFF file from which to read dataset/s.

Return value: If successful and STRUCT not specified, returns dataset read.
              Otherwise, returns 0 if successful and None on error

Note: If the number of attributes is three or larger, an additional attribute
      will be appended to the list of attributes, the particle energy
      normalized to its rest energy. If ELECTRON, PROTON, or AMASS are 
      specified, the particle energy will not be normalized, and will be in
      units of MeV'''

    DN = 'READPAR'
    argnames = ['a1','a2']
    optvals = [ None ]
    kwdefs = { 'fileid':0, 'amass':0.0, 'electron':False, 'proton':False }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + readpar.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    okay = True
    strnum = None
    if a2 is None:  dsindex = a1
    else:
        dsindex = a2  ;  strnum = a1
        if type(strnum) is not types.IntType:
            print DN + ": Supplied structure number (" + str(strnum) + \
                   ") must be an integer"  ;  okay = False
    if type(dsindex) is not types.IntType:
        print DN + ": Supplied dataset index (" + str(dsindex) + \
              ") must be an integer"  ;  okay = False

    if not okay:
        print "\n" + readpar.__doc__
        return

    ds = pff.read_dataset(dsindex,fileid)
    # Note _what_type() returns None if ds is None
    if _what_type(ds) != 'V':
        print DN + ": Unable to read dataset", dsindex, "from file", fileid
        return None

    ##print dsindex, strnum, ds.nv, ds.adim, ds.sdim, ds.apptype

    adim = ds.adim  ;  sdim = ds.sdim  ;  app = ds.apptype
    if app >= 23 and app <= 29:
        # add new attribute -- normalized kinetic energy (gamma - 1)
        data = ds.data
        # do calculation in double precision
        s = np.array(np.square(data[:,0]),dtype='float')
        for i in range(1,3): s += np.square(data[:,i])
        s = np.multiply(1.11265e-17,s)  #  1./Clite^2
        s = np.sqrt(s + 1.0) - 1.0
        lab = 'Ion KE (MeV)'
        if amass > 0.0:
            s *= (amass*931.494) #  now in MeV
        elif electron:
            s *= 0.510999        #  now in MeV
            lab = 'Electon KE (MeV)'
        elif proton:
            s *= 938.272         #  now in MeV
        else:
            lab = 'KE/rest_mass'
        # convert back to single precision
        s = np.array(s,dtype=pff.PFFnp_float)
        s = s.reshape((ds.nv,1),order='F')
        ds.data = np.append(data,s,axis=1)
        adim += 1
        tmplab = ds.dlabels
        olen = int(str(tmplab.dtype)[2:])
        nlen = max(olen,len(lab))
        ##print "new dlabels", nlen
        dt = 'a' + str(nlen)
        newlab = np.empty((adim,),dtype=(dt))
        ds.dlabels = newlab
        newlab[:-1] = tmplab[...]
        newlab[-1] = lab
        ds.adim = adim
        
    if strnum is None:  return ds

    _struclist[strnum] = ds  ;  return 0

__all__.append('chastr')
def chastr(*args, **kwargs):
    '''Change one or more properties of a field, conductor, or particle dataset.

Usage:
  chastr(ds, [clabel=str], [xlabel=str], [x#label=str], [vlabel=str], \\
         [v#label=str], [mxdim=float], [mx#dim=float], [axdim=float], \\
         [ax#dim=float], [scale=float|int], [s#v=float|int], [space=bool|str] )

Arguments:
  ds:         PFF dataset (or structure index of dataset) to be changed.
              Supported dataset types are:
                 pff.blkgrd_dataset
                 pff.VTX_dataset
                 pfmpl._2d.quadlist3d
  clabel:     New dataset comment
  xlabel:     New dataset spatial label for all coordinate directions
  x#label:    New dataset spatial label for '#' coordinate direction, where
              '#' is in the range 1-sdim (sdim is number of spatial dimensions
              in the dataset)
  vlabel:     New dataset attribute label for all attributes
  v#label:    New dataset attribute label for '#' attribute, where '#' is in
              the range 1-adim (adim is number of attributes in the dataset)
  mxdim:      Multiplier factor for all spatial coordinates
  mx#dim:     Multiplier factor for '#' spatial coordinate only
  axdim:      Additive factor for all spatial coordinates
  ax#dim:     Additive factor for '#' spatial coordinate only
  scale:      Multiplier factor for all attribute data (see notes for SPACE)
  s#v:        Multiplier factor for '#' attribute data (see notes for SPACE)
  space:      Indicator for spatially dependent attribute data scaling
              If SPACE is a string or evaluates to True, S#V or SCALE must
              be an integer (K) in the range 1-sdim. If space is not a string,
              then the attribute data is scaled by the spatial coordinate K.
              For example, in cylindrical coordinates (r,phi,z), specifying
              S2V=1 would scale the data for attribute 2 by r. Similarly, if
              SPACE is a string containing a function of x (e.g., 'sin(x)'),
              the data would be scaled by that function. For example in the
              same cylindrical coordinate system, to scale all attribute by
              sin(phi), set SPACE='sin(x)' and SCALE=2. Note that a specified
              function string must be a legal function to apply to a
              numpy.ndarray object in the numpy namespace.

Return value: If successful, returns 0. Otherwise, None is returned'''

    DN = 'CHASTR'
    argnames = [ 'ds' ]
    optvals = [ ]
    kwdefs = { 'clabel':None, 'xlabel':None,  'vlabel':None, 'mxdim':None, \
               'axdim':None, 'scale':None, 'space':None }
               ##, '':, '':, '':, '':, '':, '':,
    wkeys = [ 'x#label', 'mx#dim', 'ax#dim', 'v#label', 's#v' ]

    res = utils.process_wargs(args, kwargs, DN, argnames, kwdefs, wkeys, \
                              optvals, extra=False)

    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + chastr.__doc__  ;  return None
    wres = res[1]
    res = res[0]

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    ##for i in range(len(wkeys)):
    ##    wr = wres[i]
    ##    ww = wkeys[i]
    ##    for k in wr.keys():
    ##        s = ww.replace('#',str(k))
    ##        print s + ":", wr[k]
      
    ds, itype = process_ds(ds, DN, \
                           (pff.blkgrid_dataset,pff.VTX_dataset,quadlist3d))
    if ds is None: return None

    is_blkgrd = False  ;  is_vtx = False  ;  is_quad = False  ;  nblk = 0
    if itype == 0:
        adim = ds.adim ; sdim = ds.sdim ; nblk = ds.nblk ; is_blkgrd = True
    elif itype == 1:
        adim = ds.adim  ;  sdim = ds.sdim  ;  is_vtx = True
    else:
        adim = 0  ;  sdim = 3  ;  is_quad = True

    okay = True
    bad = []
    if adim == 0:
        if scale is not None: bad += ['scale']
        if space is not None: bad += ['space']
        if vlabel is not None: bad += ['vlabel']
    dmap = {'mx':sdim, 'ax':sdim, 'x':sdim, 'v':adim, 's':adim }
    for i in range(len(wkeys)):
        wr = wres[i]  ;  ww = wkeys[i]
        opkey = ww[0:ww.find('#')]
        wdim = dmap[opkey]
        ##print i, opkey, wdim
        for k in wr.keys():
            if k < 1 or k > wdim:
                s = ww.replace('#',str(k))
                if wdim == 0:  bad += [ s ]
                else:
                    print DN + ": Illegal integer in", s + \
                          ": it must be between 1 and",wdim
                okay = False
    if len(bad) > 0:
        if len(bad) == 1:
            print DN + ": Keyword \'" +bad[0]+ "\' is invalid for this dataset"
        else:
            print DN + ": Keywords", bad, "are invalid for this dataset"
        okay = False

    sp_func = False
    if adim != 0:
        if space is not None:
          t = type(space)
          if t is types.StringType:
            tx = np.arange(1,2,dtype=pff.PFFnp_float)
            if utils.eval_numpy_function(tx,space,quiet=0) is None:
                print DN + ": provided SPACE string is not a valid function"
                okay = False
                sp_func = True
          elif t is not bool and t is not types.IntType:
            print DN + ": SPACE must True, False, an integer, or a string"
            okay = False
          fnd = False
          if scale is not None:
            fnd = True
            if type(scale) is not types.IntType or scale < 1 or scale > sdim:
              print DN + \
                  ": Keyword \'scale\' must be an integer between 1 and", sdim
              okay = False
          else:
            lc = wkeys.index('s#v')
            ww = wres[lc]
            for j in ww.keys():
              val = ww[j]
              fnd = True
              if type(val) is not types.IntType or val < 1 or val > sdim:
                print DN + ": Keyword \'s" + str(j) + \
                      "v\' must be an integer between 1 and", sdim
                okay = False
          if not fnd:
            print DN + ": Keyword \'space\' requires specifying \'scale\' " \
                  "and/or \'s#v\', where 1 <= # <=", adim 
            okay = False
            

    if not okay:
        print "\n" + chastr.__doc__  ;  return None

    if clabel is not None:  ds.title = clabel
    if xlabel is not None:
        tmplab = ds.glabels
        nlen = len(xlabel)
        olen = int(str(tmplab.dtype)[2:])
        if nlen != olen:
            ##print "new glabels"
            dt = 'a' + str(nlen)
            sh = tmplab.shape
            tmplab = np.empty(sh,dtype=(dt))
            ds.glabels = tmplab
        tmplab[...] = xlabel
    else:
        lc = wkeys.index('x#label')
        ww = wres[lc]
        lx = len(ww)
        if lx:
            tmplab = ds.glabels
            olen = int(str(tmplab.dtype)[2:])
            if lx == sdim: nlen = 0
            else:  nlen = olen
            keys = ww.keys()
            for j in keys:  nlen = max(nlen,len(ww[j]))
            if nlen != olen:
                ##print "new glabels", nlen
                dt = 'a' + str(nlen)
                sh = tmplab.shape
                newlab = np.empty(sh,dtype=(dt))
                ds.glabels = newlab
                newlab[...] = tmplab[...]
                tmplab = newlab
            ##print olen, nlen
            if nblk: dim0 = (slice(nblk), )
            else: dim0 = ()
            for j in keys:  tmplab[dim0 + (j-1, )] = ww[j]
            ##print tmplab, id(tmplab), id(ds.glabels)
            
    if vlabel is not None:
        tmplab = ds.dlabels
        nlen = len(vlabel)
        olen = int(str(tmplab.dtype)[2:])
        if nlen != olen:
            ##print "new dlabels"
            dt = 'a' + str(nlen)
            sh = tmplab.shape
            tmplab = np.empty(sh,dtype=(dt))
            ds.dlabels = tmplab
        tmplab[...] = vlabel
    elif adim > 0:
        lc = wkeys.index('v#label')
        ww = wres[lc]
        lv = len(ww)
        if lv:
            tmplab = ds.dlabels
            olen = int(str(tmplab.dtype)[2:])
            if lv == adim: nlen = 0
            else:  nlen = olen
            keys = ww.keys()
            for j in keys:  nlen = max(nlen,len(ww[j]))
            if nlen != olen:
                ##print "new dlabels", nlen
                dt = 'a' + str(nlen)
                sh = tmplab.shape
                newlab = np.empty(sh,dtype=(dt))
                ds.dlabels = newlab
                newlab[...] = tmplab[...]
                tmplab = newlab
            ##print olen, nlen
            if nblk: dim0 = (slice(nblk), )
            else: dim0 = ()
            for j in keys:  tmplab[dim0 + (j-1, )] = ww[j]

    reset = False
    if mxdim is not None:
        reset = not is_quad
        if is_blkgrd:
            x = ds.x
            for b in range(nblk):
                xb = x[b]
                for i in range(sdim):
                    xb[i] = np.multiply(xb[i],mxdim)
        elif is_vtx:
            ds.x = np.multiply(ds.x,mxdim)
        else:
            ds.scale(mxdim)
    else:
        lc = wkeys.index('mx#dim')
        ww = wres[lc]
        if len(ww):
            reset = not is_quad
            keys = ww.keys()
            if is_blkgrd:
                x = ds.x
                for j in keys:
                    mlt = ww[j]  ;  i = j - 1
                    for b in range(nblk):
                        x[b][i] = np.multiply(x[b][i],mlt)
            elif is_vtx:
                x = ds.x
                for j in keys:
                    mlt = ww[j]  ;  i = j - 1
                    x[i,...]  = np.multiply(x[i,...],mlt)
            else:
                mxd = [ None, None, None ]
                for j in keys: mxd[j-1] = ww[j]
                ds.scale(mxd)

    if axdim is not None:
        reset = not is_quad
        if is_blkgrd:
            x = ds.x
            for b in range(nblk):
                xb = x[b]
                for i in range(sdim):
                    xb[i] = np.add(xb[i],axdim)
        elif is_vtx:
            ds.x = np.add(ds.x,axdim)
        else:
            ds.shift(axdim)
    else:
        lc = wkeys.index('ax#dim')
        ww = wres[lc]
        if len(ww):
            reset = not is_quad
            keys = ww.keys()
            if is_blkgrd:
                x = ds.x
                for j in keys:
                    mlt = ww[j]  ;  i = j - 1
                    for b in range(nblk):
                        x[b][i] = np.add(x[b][i],mlt)
            elif is_vtx:
                x = ds.x
                for j in keys:
                    mlt = ww[j]  ;  i = j - 1
                    x[i,...]  = np.add(x[i,...],mlt)
            else:
                axd = [ None, None, None ]
                for j in keys: axd[j-1] = ww[j]
                ds.shift(axd)

    if scale is not None:
        if is_blkgrd:
            data = ds.data
            if space is not None: x = ds.x
            for b in range(nblk):
                db = data[b]
                if space is not None:
                    sdir = scale - 1
                    xm = x[b][sdir]
                    ##print b,sdir,xm,space,sp_func
                    if sp_func: xm = utils.eval_numpy_function(xm,space)
                    ##print xm
                    sh = [ 1 for i in range(db.ndim) ]  ;  sh[sdir] = len(xm)
                    xm = xm.reshape(sh)
                    ##print sh,xm.shape,db.shape,xm
                else:
                    xm = scale
                db *= xm
        elif is_vtx:
            data = ds.data
            if space is not None:
                x = ds.x
                sdir = scale - 1
                xm = x[sdir,:]
                if sp_func: xm = utils.eval_numpy_function(xm,space)
                if data.ndim == 2:  xm = xm.reshape((len(xm),1))
            else:
                xm = scale
            data *= xm
    else:
        lc = wkeys.index('s#v')
        ww = wres[lc]
        if len(ww):
            keys = ww.keys()
            if is_blkgrd:
                data = ds.data
                if space is not None: x = ds.x
                for j in keys:
                    mlt = ww[j]  ;  i = j - 1
                    for b in range(nblk):
                        db = data[b]
                        if space is not None:
                            sdir = mlt - 1
                            xm = x[b][sdir]
                            if sp_func: xm = utils.eval_numpy_function(xm,space)
                            sh = [ 1 for shi in range(db.ndim) ]
                            sh[sdir] = len(xm)
                            ##print i,b,sh[:-1],xm,db.shape
                            xm = xm.reshape(sh[:-1])
                            ##print xm.shape
                        else:
                            xm = mlt
                        db[...,i] *= xm
            elif is_vtx:
                data = ds.data
                for j in keys:
                    mlt = ww[j]  ;  i = j - 1
                    if space is not None:
                        x = ds.x
                        sdir = mlt - 1
                        xm = x[sdir,:]
                        if sp_func: xm = utils.eval_numpy_function(xm,space)
                        ###if data.ndim == 2:  xm = xm.reshape((len(xm),1))
                    else:
                        xm = mlt
                    data[...,i] *= xm

    if reset: ds.fill_grid_range()
            

    return 0


__all__.append('s2i')
def s2i(*args,**kwargs):
    '''Returns the dataset object corresponding to a STRUCTURE array.

Usage:
  s2i( [struct], [copy=bool] )

Arguments:
  struct: Integer index of the STRUCTURE array.
  copy:   If True, return a copy of the STRUCTURE array, rather than a reference
          to it. (Default: True)

Return value: If successful, returns the dataset object corresponding to the
              requested STRUCTURE array, or None on error'''

    DN = 'S2I'
    argnames = ['struct']
    optvals = [ ]
    kwdefs = { 'copy':True }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + s2i.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    if not _struclist.has_key(struct):
        print DN + ':',struct, "does not have valid data"
        return None

    if copy:  return  cpy.deepcopy(_struclist[struct])
    else:  return _struclist[struct]


__all__.append('i2s')
def i2s(*args,**kwargs):
    '''copies a field, particle, or conductor dataset into a STRUCTURE array.

Usage:
  i2s(ds, struct, [copy=bool])

Arguments:
  ds:     PFF dataset to be placed in a STRUCTURE array.
  struct: If supplied, the produced dataset is placed in this STRUCTURE index.
  copy:   If COPY is True, the STRUCTURE array is set to a copy of DS, otherwise
          it is set to a reference to DS. (Default: True)

Return value: None is returned'''

    DN = 'I2S'
    argnames = [ 'ds', 'struct' ]
    optvals = [ ]
    kwdefs = { 'copy':True }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]  ;  nr = [ 0,None ]
        print nl[res] + i2s.__doc__  ;  return None

    rcpy = res['copy']  ;  del res['copy']
    for k in argnames:  exec k + " = res[k]"

    okay = True
    ds, itype = process_ds(ds, DN, \
                           (pff.blkgrid_dataset,pff.VTX_dataset,quadlist3d))
    if ds is None: okay = False
    if struct is not None and type(struct) is not types.IntType:
        print DN + ": STRUCTURE must be an integer"  ;  okay = False
    if not okay:
        print "\n" + i2s.__doc__  ;  return None

    if ds is not None:
        if rcpy:   _struclist[struct] = cpy.deepcopy(ds)
        else:  _struclist[struct] = ds

    return None
         

###################### 

__all__.append('plotgrd')
def plotgrd(*args, **kwargs):
    '''Plot a grid slice from a grid dataset.

Usage:
  plotgrd(g, [x, y, znormal], [xrange=list], [yrange=list], [xlabel=str], \\
          [ylabel=str], [title=str], [conductor=ds], [cndargs=map], \\
          [overplot=bool], [aspect=bool|float] )

Arguments:
  g:          PFF dataset containing grid information, or structure index of
              dataset containing grid information
  x:          Direction for horizontal plot axis (integer)
  y:          Direction for vertical plot axis (integer)
  znormal:    For 3D data, ordinate value in 3rd (normal) dimension
  xrange:     Horizontal (x) range of plot
  yrange:     Vertical (y) range of plot
  xlabel:     Horizontal (x) axis label for output plot
  ylabel:     Vertical (y) axis label for output plot
  title:      Title for plot
  overplot:   If possible, plot grid over the previous plot
  aspect:     Aspect ratio of plot
  conductor:  Conductor dataset (or list of datasets) to be overploted.
              Must describe a valid conductor (some PFF Vertex datasets or
              pfmpl-2d quadlist3d dataset)
  cndargs:    A map (directory, of keyword/value pairs to be passed to the
              plotcon command if CONDUCTOR is specified.

Return value: If successful, returns the number of grid blocks in the plot.
              Otherwise, None is returned'''

    DN = 'PLOTGRD'
    argnames = [ 'g', 'x', 'y', 'znormal' ]
    optvals = [ None, None, None ]
    kwdefs = { 'xrange':None, 'yrange':None, 'xlabel':None,'ylabel':None,
               'title':None, 'conductor':None, 'cndargs':{}, 'use_fig':None,
               'overplot':False, 'aspect':None, 'lw':None, 'charsize':None,
               'outline':False, 'draw':True, 'polar':False }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=True)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + plotgrd.__doc__  ;  return None
    lineargs = res[1]
    res = res[0]

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    ##print "lineargs:"
    ##for k in lineargs.keys(): print k, lineargs[k]

    g, itype = process_ds(g, DN, pff.blkgrid_dataset)
    if g is None: return None

    if g.adim != 0 or g.sdim/2 != 1:
        print DN + ": provided grid is not recognized"  ;  return None

    sdim = g.sdim
    nblk = g.nblk
    typekey = g.typekey

    if overplot and _ovrplt.x == None:
        print DN + \
            ": previous plot status unknown -- overplot option not allowed"
        return None
    if x is None or y is None:
        if not overplot:
            if sdim == 2: print DN + ": X and Y required if not overplot plot"
            else: print DN + ": X, Y, and ZNORMAL required if not overplot plot"
            return None
        if x is None:
            x = _ovrplt.x
        if y is None:
            y = _ovrplt.y
        if znormal is None:
            znormal = _ovrplt.znorm

    if x < 1 or x > sdim:
        print DN + ": X must be in range 1-" + str(sdim)
        return None
    if y < 1 or y > sdim:
        print DN + ": Y must be in range 1-" + str(sdim)
        return None
    ix = x - 1
    iy = y - 1
    iz = 3 - ix - iy

    if sdim == 3 and not overplot and znormal is None:
        znormal = g.x[0][iz][0]
        test = [ i for i in nblk if nx[i][iz] != 1 or g.x[i][iz][0] != znormal ]
        if len(test):
            print DN + ": ZNORMAL required if not overplot plot"
            return None
    if znormal == None:
        if overplot: znormal = _ovrplt.znorm
        else: znormal = 0.0

    blks = []
    for b in range(nblk):
        rng = g.g_range[b,:,:]
        eps = g.g_eps[b,:]
        if sdim > 2 and \
          (rng[iz,0] > znormal+eps[iz] or rng[iz,1] < znormal-eps[iz]): continue
        blks.append(b)

    npblks = len(blks)
    if npblks == 0:
        print DN + ": No grid defined for coordinate", iz+1, "at", znormal
        return None
        
    if not lineargs.has_key('color'): lineargs['color'] = 'k'
    if not overplot:
        f,ax = plots.adv_frame(use_fig=use_fig)
        if xrange is None or yrange is None:
            grange = np.copy(g.g_range[nblk,:,:])
            if npblks < nblk:
                grange[:,0] = np.min(g.g_range[blks,:,0],axis=0)
                grange[:,1] = np.max(g.g_range[blks,:,1],axis=0)

            ##print blks,grange
            if xrange is None:
                xrange = utils.nice_bounds(grange[ix,0],grange[ix,1])[0:2]
            if yrange is None:
                yrange = utils.nice_bounds(grange[iy,0],grange[iy,1])[0:2]
        ax.set_xlim(xrange)
        ax.set_ylim(yrange)
        if aspect is False or aspect == 0: aspect = None
        elif aspect is True:  aspect = 1.0
        if aspect is not None: ax.set_aspect(aspect)
        #print ax.xaxis.get_smart_bounds(),ax.yaxis.get_smart_bounds() 
        if title is None: ax.set_title(g.title)
        else: ax.set_title(title)
        if xlabel is None: ax.set_xlabel(g.glabels[0,ix])
        else: ax.set_xlabel(xlabel)
        if ylabel is None: ax.set_ylabel(g.glabels[0,iy])
        else: ax.set_ylabel(ylabel)
        plots.set_frame_props(ax,lw,charsize)
        _ovrplt.set(x,y,znormal,xrange, yrange,ax,polar=polar)
    else:
        f = plt.gcf()
        xrange = _ovrplt.xr
        yrange = _ovrplt.yr
        ax =  _ovrplt.ax
        polar = _ovrplt.polar 
        if polar is True: polar = 1

    try:     #  Now try to draw grid lines
        for b in blks:
            eps = g.g_eps[b,:]
            #print qcond.locx
            nx = g.nx[b]
            for d in range(sdim):
                if d == iz: continue
                od = 3 - iz - d
                ya = np.copy(g.g_range[b,od,:])
                if d == ix:
                    xrd = xrange
                    xrod = yrange
                    mode = 0
                else:
                    xrd = yrange
                    xrod = xrange
                    mode = 1
                ya[0] = max(ya[0],xrod[0])
                ya[1] = min(ya[1],xrod[1])
                ##print ya, xrod
                if ya[0] > ya[1]+eps[od]: continue
                epsd = eps[d]
                tlow = xrd[0] - epsd
                thigh = xrd[1] + epsd
                if outline:
                    xa = g.g_range[b,d,:]
                    for x in xa:
                        if tlow > x or thigh < x: continue
                        if d == ix:
                            l = matplotlib.lines.Line2D([x,x],ya,**lineargs)
                        else:
                            l = matplotlib.lines.Line2D(ya,[x,x],**lineargs)
                        ax.add_line(l)
                    ##print len(ax.lines)
                else:
                    if typekey == 'N': xn = g.x[b][d]
                    else: xn = g.getx(d+1,b+1)
                    if tlow > xn[-1] or thigh < xn[0]: continue
                    ib = np.where(tlow < xn)[0][0]
                    ie = np.where(thigh > xn)[0][-1]
                    ##print tlow,thigh,d,ib,ie,xn[ib],xn[ie]
                    _fastgrid(mode,ax,xn[ib:ie+1],ya,**lineargs)

    except AttributeError, e:
        print DN + ": Error drawing gridlines:",e  ;  return None
    if conductor is not None:
        try:     #  Now try to overplot conductor
            plotcon(conductor,x, y, znormal, over=True, use_fig=use_fig, 
                    draw=draw, **cndargs)
        except AttributeError, e:
            print DN + ": Error overplotting conductor:",e  ;  return None
            
    if draw:  f.canvas.draw()


    return npblks

__all__.append('plotcon')
def plotcon(*args, **kwargs):
    '''Plot a conductor slice from a conductor dataset.

Usage:
  plotcon(cond, [x, y, znormal], [xrange=list], [yrange=list], [xlabel=str], \\
          [ylabel=str], [title=str], [overplot=bool], [aspect=bool|float] )

Arguments:
  cond:       PFF dataset containing conductor data, or structure index of
              dataset containing conductor information. It must describe a
              valid conductor (some PFF Vertex datasets or pfmpl-2d quadlist3d
              dataset), or a list of such values
  x:          Direction for horizontal plot axis (integer)
  y:          Direction for vertical plot axis (integer)
  znormal:    For 3D data, ordinate value in 3rd (normal) dimension
  xrange:     Horizontal (x) range of plot
  yrange:     Vertical (y) range of plot
  xlabel:     Horizontal (x) axis label for output plot
  ylabel:     Vertical (y) axis label for output plot
  title:      Title for plot
  overplot:   If possible, plot grid over the previous plot
  aspect:     Aspect ratio of plot

Return value: If successful, returns 0. Otherwise, None is returned'''

    DN = 'PLOTCON'
    argnames = [ 'cond', 'x', 'y', 'znormal' ]
    optvals = [ None, None, None ]
    kwdefs = { 'xrange':None, 'yrange':None, 'xlabel':None,'ylabel':None,  \
               'title':None, 'overplot':False, 'aspect':None, 'boxlw':None, \
               'charsize':None, 'use_fig':None, 'draw':True, 'polar':False, \
               'xypolar':False, 'dtmax':np.pi/30.0 }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=True)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + plotcon.__doc__  ;  return None
    lineargs = res[1]
    res = res[0]

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    ##print "lineargs:"
    ##for k in lineargs.keys(): print k, lineargs[k]

    quit = False
    if type(cond) is types.ListType:  clist = cond
    else: clist = [ cond ]
    qlist = []
    titles = []
    grange = None
    for c in clist:
        okay = True
        c, itype = process_ds(c, DN, (pff.VTX_dataset,quadlist3d))
        if c is None: okay = False
        else:
            if itype == 0:
                qcond = quadlist3d(c)
                if len(qcond.__dict__) == 0: okay = False
            else:  qcond = c
            if okay:
                if grange is None: grange = np.copy(qcond.g_range)
                else:
                    tmp = np.dstack( (grange, qcond.g_range) )
                    grange[:,0] = np.min(tmp[:,0,:],axis=1)
                    grange[:,1] = np.max(tmp[:,1,:],axis=1)
                qlist.append(qcond)
                titles.append(qcond.title)
            else:
                print DN + ": provided conductor is not recognized"

        if not okay: quit = True
    if quit:
        print "\n" + plotcon.__doc__  ;  return None

    if overplot and _ovrplt.x == None:
        print DN + \
            ": previous plot status unknown -- overplot option not allowed"
        okay = False
    if x is None or y is None or znormal is None:
        if not overplot:
            print DN + ": X, Y, and ZNORMAL required if not overplot plot"
            okay = False
        if x is None:
            x = _ovrplt.x
        if y is None:
            y = _ovrplt.y
        if znormal is None:
            znormal = _ovrplt.znorm

    if x < 1 or x > 3:
        print DN + ": X must be 1, 2, or 3"
        okay = False
    if y < 1 or y > 3:
        print DN + ": Y must be 1, 2, or 3"
        okay = False

    if not okay:
        print "\n" + plotcon.__doc__  ;  return None

    ix = x - 1
    iy = y - 1
    iz = 3 - ix - iy

    if not lineargs.has_key('lw'): lineargs['lw'] = 3
    if not overplot:
        polarAxes = not xypolar
        pflag = polar and polarAxes
        f,ax = plots.adv_frame(use_fig=use_fig,polar=pflag)
        if xrange is None:
            xrange = utils.nice_bounds(grange[ix,0],grange[ix,1])[0:2]
        if yrange is None:
            yrange = utils.nice_bounds(grange[iy,0],grange[iy,1])[0:2]
        if polar:
            aspect = 1.0
            if xypolar:
                xtmp,ytmp = RT2XYLimits(xrange,yrange)
                ##print xrange,yrange,xtmp,ytmp
                ax.set_xlim(xtmp)
                ax.set_ylim(ytmp)
            else:
                if xrange[0] < 0.7*xrange[1]: xrange =np.array([0.0,xrange[1]])
                ax.set_xlim(yrange) # doesn't do anything - always 0 to 2*pi
                ax.set_ylim(xrange)
        else:
            ax.set_xlim(xrange)
            ax.set_ylim(yrange)
        if aspect is False or aspect == 0: aspect = None
        elif aspect is True:  aspect = 1.0
        if aspect is not None:  ax.set_aspect(aspect)
        if title is None: ax.set_title(", ".join(titles))
        else: ax.set_title(title)
        if xlabel is None: ax.set_xlabel(qlist[0].glabels[ix])
        else: ax.set_xlabel(xlabel)
        if ylabel is None: ax.set_ylabel(qlist[0].glabels[iy])
        else: ax.set_ylabel(ylabel)
        ##print lw,charsize
        plots.set_frame_props(ax,boxlw,charsize)
        _ovrplt.set(x,y,znormal,xrange, yrange,ax,polar=polar)
    else:
        f = plt.gcf()
        xrange = _ovrplt.xr
        yrange = _ovrplt.yr
        ax =  _ovrplt.ax
        polar = _ovrplt.polar
        polarAxes = type(ax.axesPatch) is matplotlib.patches.Circle
        
    for qcond in qlist:
        if qcond.nqs:
            xyzmm = np.empty((2,3),dtype=pff.PFFnp_float,order='F')
    try:     #  Now try to draw conductors
        for qcond in qlist:
            locx = qcond.locx
            eps = qcond.g_eps
            for d in range(3):
                pz = (iz + 2 - d) % 3
                pt = 1 - pz
                if d != iz:
                    for q in range(locx[d],locx[d+1]):
                        pds = qcond.pds[:,:,q]
                        if pds[0,pz] > znormal+eps[pz] or \
                                pds[1,pz] < znormal-eps[pz]:
                            continue
                        ##print d,ix,iy,iz,pz,pt,pds[0,pz],znormal+eps[pz],znormal-eps[pz]
                        ##print pds,eps
                        if d == ix:
                            xa = np.array([pds[0,2],pds[0,2]])
                            ya = pds[:,pt]
                            if ya[0] > yrange[1] or ya[1] < yrange[0]: continue
                        else:
                            ya = np.array([pds[0,2],pds[0,2]])
                            xa = pds[:,pt]
                            if xa[0] > xrange[1] or xa[1] < xrange[0]: continue
                        if not polar:
                            l = matplotlib.lines.Line2D(xa,ya,**lineargs)
                        else:
                            if np.diff(ya)[0] > 1.01*dtmax:
                                ya,xa  = ThetaFill(ya,dtmax,f=xa.reshape((1,2)))
                                xa = xa.reshape((len(ya),))
                            if polarAxes:
                                l = matplotlib.lines.Line2D(ya,xa,**lineargs)
                            else:
                                xa1 = xa*np.cos(ya)
                                ya1 = xa*np.sin(ya)
                                l = matplotlib.lines.Line2D(xa1,ya1,**lineargs)
                        ax.add_line(l)
                if qcond.nqs:
                    locs = qcond.locs
                    xnc = d  ;  x1c = (xnc+1) % 3  ;  x2c = (xnc+2) % 3
                    off = 3 - d ; rmap = [ (off+i) % 3 for i in range(3) ]
                    for q in range(locs[d],locs[d+1]):
                        bnds = qcond.slant_bnds[:,:,q]
                        xyzb = bnds[:,rmap]
                        xyzmm[...] = xyzb
                        if xyzb[:,x2c].argmax() == 0:
                            xyzmm[:,x2c] = np.flipud(xyzb[:,x2c])
                        if xyzmm[0,iz] > znormal+eps[iz] or \
                           xyzmm[1,iz] < znormal-eps[iz]: continue
                        if xyzmm[0,ix] > xrange[1] or \
                           xyzmm[1,ix] < xrange[0]: continue
                        if xyzmm[0,iy] > yrange[1] or \
                           xyzmm[1,iy] < yrange[0]: continue
                        if xnc == iz:
                            xa = xyzb[:,ix]
                            ya = xyzb[:,iy]
                        else:
                            xn = xyzb[:,iz]
                            f1 = (znormal - xn[0])/(xn[1] - xn[0])
                            if xnc == ix:
                                xa = xyzb[:,ix]
                                v = (1.0 - f1)*xyzb[0,iy] + f1*xyzb[1,iy]
                                ya = [v,v]
                            elif xnc == iy:
                                ya = xyzb[:,iy]
                                v = (1.0 - f1)*xyzb[0,ix] + f1*xyzb[1,ix]
                                xa = [v,v]
                        if not polar:
                            l = matplotlib.lines.Line2D(xa,ya,**lineargs)
                        else:
                            if np.diff(ya)[0] > 1.01*dtmax:
                                ya,xa  = ThetaFill(ya,dtmax,f=xa.reshape((1,2)))
                                xa = xa.reshape((len(ya),))
                            if polarAxes:
                                l = matplotlib.lines.Line2D(ya,xa,**lineargs)
                            else:
                                xa1 = xa*np.cos(ya)
                                ya1 = xa*np.sin(ya)
                                l = matplotlib.lines.Line2D(xa1,ya1,**lineargs)
                        ax.add_line(l)
    except AttributeError, e:
        print DN + ": Error drawing conductor:",e  ;  return None
    
    if draw: f.canvas.draw()

    return 0

_plotflddefs = dict(xrange=None, yrange=None, nlevel=20, levels=None,
                    title=None, xlabel=None, ylabel=None, aspect=None,
                    cbar=None, cntour=1, pline=False, c_colors=None,
                    return_slice=False, lw=None, charsize=None,
                    polar=False, dtmax=np.pi/30.0)
_pfdopts_cur = {}
_pfdchmx = 0
for i in _plotflddefs.keys():
    _pfdchmx = max(_pfdchmx, len(i))
    _pfdopts_cur[i] = _plotflddefs[i]

__all__.append('plotfld')
def plotfld(*args, **kwargs):
    '''Plot a slice from a field dataset.

Usage:
  plotfld([ds], [comp], [norm, znorm], [transpose=bool], [xrange=list], \\
          [yrange=list], [xlabel=str], [ylabel=str], [title=str], \\
          [overplot=bool], [aspect=bool|float], [conductor=ds|list], \\
          [block=int|list], [multiply=float], [levels=list], [cntour=int], \\
          [cmap=str|cmap], [nlevel=int], [c_color=color|list], \\
          [cbar=str|list], [cbtitle=str], [return_slice=bool], \\
          [setdefault=bool], [showdefault=bool], [unset=bool], \\
          [xline=float|list], [yline=float|list], [hline=float|list], \\
          [vline=float|list], [pline=bool], [out=int], [xintegrate=bool|int], \\
          [yintegrate=bool|int], [reverse=bool], [start=int] )

Arguments:
  ds:           PFF dataset containing field data, or structure index of
                dataset containing field data. It must describe a valid
                2- or 3D field. Optional ONLY if SETDEFAULT, SHOWDEFAULT, or
                UNSET have been specified
  comp:         attribute # to be plotted. Also, 'mag' to plot vector magnitude,
                or list of attribute #'s to plot sqrt(sum(attributes**2))
  norm:         Coordinate direction for plot's normal (integer in range 1-sdim)
                X and Y axes form right-handed coordinate system with norm
  znorm:        For 3D data, ordinate value in 3rd (normal) dimension
  transpose     Transpose plot's X and Y axes (left-handed system)
  xrange:       Horizontal (x) range of plot
  yrange:       Vertical (y) range of plot
  xlabel:       Horizontal (x) axis label for output plot
  ylabel:       Vertical (y) axis label for output plot
  title:        Title for plot
  overplot:     If possible, plot grid over the previous plot
  aspect:       Aspect ratio of plot
  conductor:    Conductor dataset (or list of datasets) to be overplotted.
                Must describe a valid conductor (some PFF Vertex datasets or
                pfmpl-2d quadlist3d dataset)
  block:        Dataset block/s to be plotted. Integer block number, list of
                integer block numbers, or 'all'
  multiply:     Scale factor for plotted data
  levels:       2-element list containing data values mapped to the min and max
                colors in the color map
  cntour:       Contour mode (integer):
                  1: contours only
                  2: smooth shaded fill
                  3: filled contours
  nlevel:       Number of contour levels (cn=1 or 3 only)
  c_colors:     Contour color or list of contour colors (cn=1 or 3 only)
  cmap:         Mathplotlib color map to be used.
  cbar:         If set, color bar is to be plotted. Valid values are:
                  'h' or 'v', indicating horizontal or vertical placement
                  [x0,y0,dx,dy], indicating overlay position on plot axes, in
                     axes-normalized units.
  cbtitle:      Title for color bar
  setdefault:   If set, provided option values will be made default
  showtdefault: If set, current default option values will be printed
  unset:        If set, default option values will be reset to original values
 -- LINEOUT Options: --
  xline:      location/s for x-directed lineout. float or list of floats
  yline:      location/s for y-directed lineout. float or list of floats
  hline:      synonym for XLINE. If both specified, HLINE values will be used
  vline:      synonym for YLINE. If both specified, VLINE values will be used
  pline:      if True, lines will be drawn at locations of requested lineouts
  out:        integer indicating first WDF array to be used to store lineout
              data. If more than one lineout requested, remaining lineouts will
              be stored in consecutive WDF arrays following OUT. If not
              specified, the first consecutive block of unused WDF arrays
              encountered, starting at one, will be used.
 -- INTEGRATION Options: --
  xintegrate: If 0, no integration; if 1 (or True), data integrated in X
              direction before plotting; if 2, data is multiplied by X before
              integrating, if 3, data is multiplied by X^2 before integrating.
  yintegrate: If 0, no integration; if 1 (or True), data integrated in Y
              direction before plotting; if 2, data is multiplied by Y before
              integrating, if 3, data is multiplied by Y^2 before integrating.
  reverse:    If True, integration performed in negative X or Y direction
  start:      WDF array index for a function to supply initial values at the
              initial boundary of the integration. If not specified, 0 is used.

Return value: If error encountered, returns None. If RETURN_SLICE is True,
              a dataset containing the plotted 2D slice is returned.
              Otherwise, 0 is returned'''

    DN = 'PLOTFLD'
    argnames = [ 'ds', 'a1', 'a2', 'a3' ]
    optvals = [ None, None, None, None ]
    kwdefs = { 'transpose':False, 'conductor':None, 'overplot':False, \
               'block':'all','multiply':None, 'cmap':None,  'cbtitle':None, \
               'xline':None, 'yline':None, 'hline':None, 'vline':None, \
               'out':None, 'xintegrate':0, 'yintegrate':0, 'start':None, \
               'reverse':None, 'showdefault':False, 'setdefault':False, \
               'unset':False, 'use_fig':None }
               ##, '':, '':, '':, '':, '':, '':, 

    kwdefs.update(_pfdopts_cur) ## append settable option defaults
    ##print kwdefs

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=True)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + plotfld.__doc__  ;  return None
    kwextra = res[1]
    res = res[0]

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k
    ##print "kwextra:"
    ##for k in kwextra.keys(): print k, kwextra[k]

    if unset:
        for k in _plotflddefs.keys():
            _pfdopts_cur[k] = _plotflddefs[k]
        return

    if setdefault:
        for k in keys:
            if _pfdopts_cur.has_key(k): exec "_pfdopts_cur[k] = " + k
        if not showdefault:
            return

    if showdefault:
        blnk = ''
        for i in range(_pfdchmx):  blnk += ' '
        print "Default parameters for the PLOTFLD command are:"
        dkeys = _pfdopts_cur.keys()  ; dkeys.sort()
        for i in dkeys:
            l = len(i)
            tmp = "   " + i + blnk[l:] + " = "
            print tmp + str(_pfdopts_cur[i])
        return

    if ds is None:
        print DN + ": No dataset or structure index provided"  ;  return None
    ds, itype = process_ds(ds, DN, pff.blkgrid_dataset)
    if ds is None: return None

    if ds.sdim < 2 or ds.adim == 0:
        print DN +  ''': DS must have at least two spatial dimensions and
         at least one attribute'''
        return None

    if overplot and _ovrplt.x == None:
        print DN + \
            ": previous plot status unknown -- overplot option not allowed"
        return None

    if ds.rawname == 'NI3':
        ds = ds.clone()  # don't change data's type in the original dataset!!!
        data = []
        for d in ds.data:
            data.append(np.array(d,dtype=pff.PFFnp_float,order='F'))
        ds.data = data
    minargs = 1
    adim = ds.adim  ;  sdim = ds.sdim
    degen = np.where(ds.nx[0] == 1)[0]
    ldeg = len(degen)
    sdim_act = sdim
    if ldeg > 0:
        ##print degen
        for b in range(ds.nblk):
            dtest = np.where(ds.nx[b] == 1)[0]
            ##print dtest
            if ldeg != len(dtest):
                degen = None  ;  break
            else:
                tt = [ i for i in range(ldeg) if degen[i] != dtest[i] ]
                if len(tt):
                    degen = None  ;  break
        if degen is not None: sdim_act -= len(degen)
        ##print "degen:",degen

    comp = 0  ;  kvals = []  ;  vals = []
    cargs = [ a1, a2, a3 ]  ;  carg = 0
    if adim > 1:
        minargs += 1  ;  comp = a1 ; carg += 1
    if not overplot and sdim_act > 2: minargs += (2*(sdim_act-2))

    ##print cargs, carg, minargs, sdim_act
    if minargs > 4 or (minargs > 1 and cargs[minargs-2] is None):
        print DN + ":", minargs, "positional parameters are required"
        return None
    for i in range(2,sdim_act):
        kvals.append(cargs[carg])
        vals.append(cargs[carg+1])
        carg += 2

    okay = True
    slcomp = 1  ;  cmpid = 0
    if adim > 1:
        cokay = True
        t = type(comp)
        slcomp = 'mag'
        if t is types.StringType:
            if comp[:3] != 'mag':  cokay = False
            else: cmpid = 'mag'
        elif t is types.ListType or t is types.TupleType:
            used = [ 0 for i in range(adim+1) ]
            cmpid = []
            for c in comp:
                if type(c) is not types.IntType: cokay = False
                else:
                    cmpid.append(c-1)
                    if c > adim: c = 0
                    used[max(0,c)] += 1
            slcomp = 'all'
            if used[0]:   cokay = False
            elif len([ c for c in used if c == 1 ]) == adim:
                cmpid = 'mag'
                slcomp = 'mag'
            elif len(comp) == 1:  slcomp = comp[0]
        elif t is types.IntType:
            if comp < 1 or comp > adim: cmpid = 'mag'
            else: cmpid = comp - 1 ;  slcomp = comp
        else:   cokay = False
        if not cokay:
            print DN + ": Illegal value (',comp,') supplied for the component"
            okay = False
    if levels is not None:
        t = type(levels)
        if (t is not types.ListType and t is not types.TupleType) or \
           len(levels) == 1:
            print DN+": Illegal value (',levels,') supplied for LEVELS keyword"
            okay = False
 
    if cbar is not None:
        t = type(cbar)
        cbloc = None
        cborient = None
        if t is types.StringType:
            c = cbar[0].lower()
            if c == 'h':   cborient = 'horizontal'
            elif c == 'v': cborient = 'vertical'
        if t is types.ListType or t is types.TupleType or t is np.ndarray:
            if (t is np.ndarray and cbar.ndim != 1) or len(cbar) != 4:
                print DN + ": colorbar box format is: [x0,y0,dx,dy]"
                okay = False
            cbloc = cbar
        if cbtitle is not None and type(cbtitle) is not types.StringType:
            print DN + ": Supplied colorbar title must be a string"
            okay = False

    n_out = 0
    if hline is not None: xline = hline
    if xline is not None:
        tokay = True
        isnum, t = utils.is_number(xline)
        if isnum: xline = [ xline ]
        elif t is types.ListType or t is types.TupleType:
            xline = list(xline)
            for xl in xline:
                isnum, tdum = utils.is_number(xl)
                if not isnum: tokay = False
        else: tokay = False
        if not tokay:
            print DN + ": xline must be a number or list of numbers"
            okay = False
        else: n_out += len(xline)
    if vline is not None: yline = vline
    if yline is not None:
        tokay = True
        isnum, t = utils.is_number(yline)
        if isnum: yline = [ yline ]
        elif t is types.ListType or t is types.TupleType:
            yline = list(yline)
            for yl in yline:
                isnum, tdum = utils.is_number(yl)
                if not isnum: tokay = False
        else: tokay = False
        if not tokay:
            print DN + ": yline must be a number or list of numbers"
            okay = False
        else: n_out += len(yline)
    if xintegrate != 0:
        t = type(xintegrate)
        if t is bool: xintegrate = 1
        elif t is types.IntType:
            if xintegrate < 0 or xintegrate > 3:
                print DN + ": Invalid value: 0 <= XINTEGRATE <= 3"
                okay = False
        else:
            print DN + ": XINTEGRATE must be bool or integer between 0 and 3"
            okay = False
    if yintegrate != 0:
        t = type(yintegrate)
        if t is bool: yintegrate = 1
        elif t is types.IntType:
            if yintegrate < 0 or yintegrate > 3:
                print DN + ": Invalid value: 0 <= YINTEGRATE <= 3"
                okay = False
        else:
            print DN + ": YINTEGRATE must be bool or integer between 0 and 3"
            okay = False
    if yintegrate or xintegrate:
        if reverse is not None:
            t = type(reverse)
            if t is not bool and t is not types.IntType:
                print DN + ": REVERSE must be bool or integer"
                okay = False
        if start is not None:
            if type(start) is not types.IntType or \
               _1d.get_empty_wdf(start,start):
                print DN + ": START must specify a WDF array"
                okay = False
            wmin,wmax = _1d.getXRange(start)
        if yintegrate and xintegrate:
            print DN + ": XINTEGRATE and YINTEGRATE cannot both be specified"
            okay = False
            
    if not okay:
        print "\n" + plotfld.__doc__  ;  return None

    ##print comp, slcomp, cmpid, kvals, vals, kwextra, okay

    nblk = ds.nblk
    sh = 0
    if transpose: sh = 1
    if sdim_act > 2:
        if len(kvals):
            normid = kvals[0] - 1
            x1 = (normid+1+sh) % sdim
            x2 = (normid+2-sh) % sdim
        else: normid = None
        if overplot:
            ookay = True
            x1o,x2o,zno = (_ovrplt.x - 1, _ovrplt.y - 1, _ovrplt.znorm)
            if normid is not None:
                if x1 != x1o or x2 != x2o or vals[0] != zno:
                    print '''
WARNING: Supplied slice orientation and normal for an OVERPLOT plot are not
         consistent with previous plot. Previous settings will be used.'''
                    vals[0] = zno
                    kvals[0] = 4 - x1o - x2o
            else:
                vals.append(zno)
                kvals.append(4 - x1o - x2o)
            x1,x2 = (x1o,x2o)
            normid = kvals[0] - 1
        ##print vals, kvals, slcomp
        ##print ds.nblk, ds.data[0].shape
        sl = ds.get_slice(vals[0],comp=slcomp,coord=kvals[0],block=block)
        if sl is None:
            print DN + ": Error slicing dataset"
            return None
    elif sdim > 2:
        normid = degen[0]
        x1 = (normid+1+sh) % sdim
        x2 = (normid+2-sh) % sdim
        vals = [ ds.x[0][normid][0] ]
        kvals = [ normid+1 ]
        ##sl = cpy.deepcopy(ds)
        if type(cmpid) is types.ListType: tcomp = comp
        else: tcomp = slcomp
        ##print cmpid,slcomp,tcomp
        sl = ds.scalarize(comp=tcomp,block=block)
        cmpid = 0

        ##if type(slcomp) is not types.IntType:
            ##print DN + ": Not implemented yet"
            ##return None
    else:
        ##x1 = sh
        ##x2 = 1 - sh
        ##sl = ds
        ##vals = [ 0.0 ]
        print DN + ": Not implemented for sdim < 3"
        return None

    ##print normid,x1,x2,vals,kvals,slcomp
    ##print sl.data[0].shape,sl.nx,sl.x[0][2]
    
    imtrans = x1 < x2
    ##print "imtrans:", imtrans

    nbsl = sl.nblk

    grm = sl.g_range[nbsl,:,:]
    if cntour == 2:
        xrr = cpy.deepcopy(grm[x1,:])
        yrr = cpy.deepcopy(grm[x2,:])
    if not overplot:
        xrp = grm[x1,:]
        yrp = grm[x2,:]

        if xrange is not None: xrp = np.array(xrange)
        elif polar and cntour != 2:
            if xrp[0] < 0.7*xrp[1]: xrp =np.array([0.0,xrp[1]])
        if yrange is not None: yrp = np.array(yrange)
        ##if cntour == 2: pflag = False
        ##else:  pflag = polar
        pflag = polar and cntour != 2
        f,ax = plots.adv_frame(use_fig=use_fig,polar=pflag)
        if title is None: ax.set_title(ds.title)
        elif title is not None: ax.set_title(title)
        if xlabel is None: ax.set_xlabel(ds.glabels[0,x1])
        else: ax.set_xlabel(xlabel)
        if ylabel is None: ax.set_ylabel(ds.glabels[0,x2])
        else: ax.set_ylabel(ylabel)
        if polar:
            if cntour == 2:
                xtmp,ytmp = RT2XYLimits(xrp,yrp)
                ##print xrp,yrp,xtmp,ytmp
                ax.set_xlim(xtmp)
                ax.set_ylim(ytmp)
            else:
                ax.set_xlim(yrp)
                ax.set_ylim(xrp)
        else:
            ax.set_xlim(xrp)
            ax.set_ylim(yrp)
        plots.set_frame_props(ax,lw,charsize)
        _ovrplt.set(x1+1,x2+1,vals[0],xrp, yrp,ax,polar=polar)
    else:
        f = plt.gcf()
        ax = _ovrplt.ax
        xrp = _ovrplt.xr
        yrp = _ovrplt.yr
    ##print "AX:", ax

    pxf, pyf = f.canvas.get_width_height()
    if cntour == 2:
        if xrange is not None:
            xrr[0] = max(xrange[0],xrr[0])
            xrr[1] = min(xrange[1],xrr[1])
        if yrange is not None:
            yrr[0] = max(yrange[0],yrr[0])
            yrr[1] = min(yrange[1],yrr[1])
        ##print x1,xrp,xrr
        ##print x2,yrp,yrr
        polarLims = polar
        if polar:
            ##print xrr,yrr
            polarLims = [xrr,yrr]
            xrr, yrr = RT2XYLimits(xrr,yrr)
            ##print xrr,yrr
 
        px = int(pxf*ax.figbox.width + 0.5)
        py = int(pyf*ax.figbox.height + 0.5)
        figasp = py/float(px)
        dxf = xrp[1] - xrp[0]  ;  dyf = yrp[1] - yrp[0]
        dxi = xrr[1] - xrr[0]  ;  dyi = yrr[1] - yrr[0]
        ##print dxf,dxi,dyf,dyi
        ##print px,py,figasp
        if aspect is None or aspect is False or aspect == 0: aspect = 'auto'
        else:
            if aspect == 'equal' or aspect is True: aspect = 1.0
            iaspect = float(aspect)*dyf/dxf
            if iaspect > figasp: px = max(1,int(py/iaspect + 0.5))
            else:    py = max(1,int(px*iaspect + 0.5))
            ##print px,py,iaspect
            px = max(1,int(px*dxi/dxf + 0.5))
            py = max(1,int(py*dyi/dyf + 0.5))
            ##print px,py,iaspect
        imshape = [max(50,py), max(50,px)]
        if imtrans: imshape.reverse()
        imshape = tuple(imshape)
        ##print imshape
    else:
        if aspect is False or aspect == 0: aspect = None
        elif aspect is True:  aspect = 1.0
        if aspect is not None: ax.set_aspect(aspect)

    data = sl.data
    if type(cmpid) is types.ListType:
        nc = len(cmpid)
        c0 = cmpid[0]
        for b in range(nbsl):
            if nc == 1: ndata = np.fabs(data[b])
            else:
                ndata = np.square(data[b][...,c0])
            for c in cmpid[1:]:
                 ndata += np.square(data[b][...,c])
            if nc > 1:
                ndata = np.sqrt(ndata)
            data[b] = ndata
        sl.adim = 1
    if multiply is not None:
        for b in range(nbsl):  data[b] *= multiply
    ## do integration here
    if xintegrate:
        if start:
            gr = sl.g_range[-1,x2,:]
            if wmin > gr[0] or wmax < gr[1]:
                print DN+": START array's range insufficient for selected slice"
                return None
        _integrate_2d(sl,x1,x2,xintegrate,reverse,start)
        data = sl.data
    if yintegrate:
        if start:
            gr = sl.g_range[-1,x1,:]
            if wmin > gr[0] or wmax < gr[1]:
                print DN+": START array's range insufficient for selected slice"
                return None
        _integrate_2d(sl,x2,x1,yintegrate,reverse,start)
        data = sl.data

    vmin = data[0].min()  ;  vmax = data[0].max()
    for b in range(1,nbsl):
        vmin = min(vmin,data[b].min())  ;  vmax = max(vmax,data[b].max())
    ##print' vmin,vmax:',vmin,vmax,data[0].shape
    ####else: vlow =  data[0][.min()  ;  vhi = data[0].max()
    tdata = data
    if levels is None:
        vlow = vmin  ;  vhi = vmax
        if vlow == vhi:
            if vlow == 0:
                vlow = -1.0  ;  vhi = 1.0
            else:
                vlow *= 0.9  ;  vhi *= 1.1
    else:
        vlow = float(levels[0])  ;  vhi = float(levels[-1])
        if vmin < vlow or vmax > vhi:
            ##print 'CLIPPING',vlow,vhi,vmin,vmax
            tdata = []
            for b in range(nbsl):  tdata.append(np.clip(data[b],vlow,vhi))
    ##print vlow,vhi,vmin,vmax
    ##print 'ID:',id(ds.data[0]),id(sl.data[0]),id(data[0]),id(tdata[0])

    if cntour == 1 or cntour == 3:
        if cntour == 1:
            funct = "contour"  ;  extra=""
        else:
            funct = "contourf"  ;  extra=",extend=\'both\'"
        if c_colors is not None:
            t = type(c_colors)
            if t is types.ListType or t is types.TupleType:
                c_colors = tuple(c_colors)
            else: c_colors = (c_colors,)
            extra += ",colors=c_colors"
        else:
            extra += ",cmap=cmap"
            if type(cmap) is types.StringType: cmap = plt.get_cmap(cmap)
        dvl = (vhi - vlow)/max(nlevel,1)
        ##vlev = np.arange(vlow+0.5*dvl, vhi, dvl)
        vlev = np.linspace(vlow, vhi, max(nlevel,1)+1)
        ##print 'vlev',vlev, c_colors
        for b in range(nbsl):
            dat = tdata[b].squeeze()
            x = sl.x[b]
            if imtrans: dat = np.transpose(dat)
            if polar:
                xys = "to,x[x1]"  ;  dat = dat.transpose()
                ##print 'pdiff:',np.diff(x[x2]).max(),dtmax
                if np.diff(x[x2]).max() > 1.01*dtmax:
                    to, dato = ThetaFill(x[x2],dtmax,f=dat)
                else: to,dato = x[x2],dat
            else:
                xys = "x[x1],x[x2]"  ;  dato = dat
            ##print "im = ax." + funct + "(" + xys + ",dato,vlev" + extra + ")"
            exec "im = ax." + funct + "(" + xys + ",dato,vlev" + extra + ")"
    elif cntour == 2:
        vdef = 2.0*vlow - vhi
        if polar:
            ##print imtrans,xrr,yrr
            aspect = True
            ##return
        if imtrans:
            rast = rasterize(sl,imshape,vdef=vdef,xr=xrr,yr=yrr,polar=polar,
                             polarLims=polarLims)
            rast = np.transpose(rast)
        else: rast = rasterize(sl,imshape,vdef=vdef,xr=yrr,yr=xrr,polar=polar,
                               polarLims=polarLims)

        ##print rast.shape,rast.min(), rast.max()

        ext = xrr.tolist() + yrr.tolist()
        ##print xrp,yrp,ext
        im = ax.imshow(rast,origin='lower',extent=ext,aspect=aspect,vmin=vlow, \
                       vmax=vhi,cmap=cmap)

    if cbar is not None:
        if cbloc is None:  cax = None
        else:
            bnds = np.asarray(ax.figbox.bounds)
            xcb = np.asarray(cbloc)
            xab = np.arange(4,dtype='f4')
            xab[0:2] = bnds[0:2] + bnds[2:]*xcb[0:2]
            xab[2:] = bnds[2:]*xcb[2:]
            if xab[-2]*pxf > xab[-1]*pyf: cborient = 'horizontal'
            else: cborient = 'vertical'
            cax = f.add_axes(xab)
            ##print "CAX:", cax
        cb = f.colorbar(im,cax,orientation=cborient,extend='neither')
        if cbtitle is not None:
            cb.ax.set_title(cbtitle)
        plots.set_frame_props(cb.ax,lw,charsize)

    if n_out > 0:
        if  out is None:
            out = _1d.get_empty_wdf(count=n_out)
            nl = min(2,n_out)
            if n_out == 1: tail = " " + str(out)
            else: tail = "s " + str(out) + "-" + str(out+n_out-1)
            print "Lineout" + tail[:nl] + "will be written to WDF array" + tail
        if cmpid == 'mag' or type(cmpid) is types.ListType:
            dlab = ''.join(ds.dlabels[0,0])
        else: dlab = ''.join(ds.dlabels[0,cmpid])
        ##print sl.dlabels,dlab
        loax  = None
        if pline: loax = ax
        ##print "loax:",loax
    if xline is not None:
        con_ord = sl.connect_order(x1+1)
        ##print sl.data[0].shape,con_ord
        dirs = [normid,x2]
        if x2 < normid: dirs = [x2,normid]
        for yint in xline:
            xout,yout = _gen_lineout(sl,con_ord,x1,x2,yint,axes=loax,
                                     polar=polar)
            ##print 'xout:', xout.shape,xout
            ##print 'yout:', yout.shape,yout
            cstr = "(x" + str(dirs[0]+1) + ",x" + str(dirs[1]+1) + ") = "
            if dirs[0] == normid:
                lstr = "(" + str(vals[0]) + ',' + str(yint) + ") -- "
            else:
                lstr = "(" + str(yint) + ',' + str(vals[0]) + ") -- "
            clab = cstr + lstr + ds.title
            xlab = ax.get_xlabel()
            _1d.i2w(xout,yout,out,clab=clab,xlab=xlab,ylab=dlab,file=ds.file)
            out += 1
                

    if yline is not None:
        con_ord = sl.connect_order(x2+1)
        ##print con_ord
        dirs = [normid,x1]
        if x1 < normid: dirs = [x1,normid]
        for xint in yline:
            xout,yout = _gen_lineout(sl,con_ord,x2,x1,xint,axes=loax,dir='y',
                                     polar=polar)
            ##print 'xout:', xout.shape,xout
            ##print 'yout:', yout.shape,yout
            cstr = "(x" + str(dirs[0]+1) + ",x" + str(dirs[1]+1) + ") = "
            if dirs[0] == normid:
                lstr = "(" + str(vals[0]) + ',' + str(xint) + ") -- "
            else:
                lstr = "(" + str(xint) + ',' + str(vals[0]) + ") -- "
            clab = cstr + lstr + ds.title
            xlab = ax.get_ylabel()
            _1d.i2w(xout,yout,out,clab=clab,xlab=xlab,ylab=dlab,file=ds.file)
            out += 1

    if conductor is not None:
        try:     #  Now try to overplot conductor
            plotcon(conductor,x1+1, x2+1, vals[0], over=True, use_fig=use_fig,
                    **kwextra)
        except AttributeError, e:
            print DN + ": Error overplotting conductor:",e  ;  return None
            
    f.canvas.draw()

    if return_slice: return sl
    return 0

_plotvecdefs = dict(xrange=None, yrange=None, levels=None,
                    title=None, xlabel=None, ylabel=None, aspect='auto',
                    cbar=None, c_colors=None,
                    return_slice=False, lw=None, charsize=None,
                    polar=False, npmax=20, xygrid=None)
_pvdopts_cur = {}
_pvdchmx = 0
for i in _plotvecdefs.keys():
    _pvdchmx = max(_pvdchmx, len(i))
    _pvdopts_cur[i] = _plotvecdefs[i]

__all__.append('plotvec')
def plotvec(*args, **kwargs):
    '''Plot projections of vector data on a lattice in a slice of a field
dataset.

Usage:
  plotvec([ds], [norm, znorm], [transpose=bool], [xrange=list], \\
          [yrange=list], [xlabel=str], [ylabel=str], [title=str], \\
          [aspect=bool|float], [overplot=bool], [conductor=ds|list], \\
          [cndargs=map], [block=int|list], [multiply=float], [polar=bool], \\
          [xygrid=bool], [complist=list], [color=int|str], [levels=list], \\
          [cmap=str|cmap], [cbar=str|list], [cbtitle=str], \\
          [setdefault=bool], [showdefault=bool], [unset=bool], \\
          [return_slice=bool] )

Arguments:
  ds:           PFF dataset containing field data, or structure index of
                dataset containing field data. It must describe a valid
                2- or 3D field. Optional ONLY if SETDEFAULT, SHOWDEFAULT, or
                UNSET have been specified
  norm:         Coordinate direction for plot's normal (integer in range 1-sdim)
                X and Y axes form right-handed coordinate system with norm
  znorm:        For 3D data, ordinate value in 3rd (normal) dimension
  transpose     Transpose plot's X and Y axes (left-handed system). This is not
                supported in polar coordinates, where the radial component is
                assumed coordinate system is assumed to be right-handed (e.g.,
                {r,phi,z})
  xrange:       Horizontal (x) range of plot
  yrange:       Vertical (y) range of plot
  xlabel:       Horizontal (x) axis label for output plot
  ylabel:       Vertical (y) axis label for output plot
  title:        Title for plot
  aspect:       Aspect ratio of plot
  overplot:     If possible, plot grid over the previous plot. If OVERPLOT is
                True, the keywords NORM, ZNORM, TRANSPOSE, XRANGE, YRANGE,
                XLABEL, TITLE, ASPECT, and POLAR are ignored.
  conductor:    Conductor dataset (or list of datasets) to be overplotted.
                Must describe a valid conductor (some PFF Vertex datasets or
                pfmpl-2d quadlist3d dataset)
  cndargs:      A map (directory, of keyword/value pairs to be passed to the
                plotcon command if CONDUCTOR is specified.
  block:        Dataset block/s to be plotted. Integer block number, list of
                integer block numbers, or 'all'
  multiply:     Scale factor for plotted data
  polar:        0 or False -> plot data on a Cartesian grid (default) 
                1 or True  -> plot data on a MatPlotLib Polar grid
                2          -> plot data in polar coordinates using a MatPlotLib
                              rectangular grid
  xygrid:       If data is plotted in polar coordinates, forces displayed
                vectors to be arranged on a rectangular lattice, rather than
                a polar lattice.
                    True  -> arrange vectors on rectangular lattice
                    False -> arrange vectors on a polar lattice
                Defaults to False if ratio of dataset's minimum radius is less
                than 0.2 times its maximum radius. Otherwise, defaults to True.
  npmax:        Controls the size and density of the vectors plotted. This 
                value is the maximum size of lattice in either coordinate
                direction.
  complist:     2-element list of integers denoting the two vector components
                to be plotted. If not supplied, the 2 components in the plane
                of the slice will be used.
  color:        An integer or string. If color = 'mag', vectors are scaled
                from the supplied colormap based on the magnitude of
                the vector.  All other strings are interpreted as
                names of matplotlib colors. Integers are mapped
                (modulo 8) to 'k' (black), 'r' (red), 'g' (green), 'b'
                (blue), 'c' (cyan), 'm' (magenta), 'y' (yellow), and
                'orange'.
  levels:       2-element list containing data values mapped to the min and max
                colors in the color map (used only for COLOR='mag'
  cmap:         Mathplotlib color map to be used.
  cbar:         If set, color bar is to be plotted. Valid values are:
                  'h' or 'v', indicating horizontal or vertical placement
                  [x0,y0,dx,dy], indicating overlay position on plot axes, in
                     axes-normalized units.
  cbtitle:      Title for color bar
  setdefault:   If set, provided option values will be made default
  showtdefault: If set, current default option values will be printed
  unset:        If set, default option values will be reset to original values

Return value: If error encountered, returns None. If RETURN_SLICE is True,
              a dataset containing the plotted 2D slice is returned.
              Otherwise, 0 is returned'''

    DN = 'PLOTVEC'
    argnames = [ 'ds', 'a1', 'a2' ]
    optvals = [ None, None, None ]
    kwdefs = dict(transpose=False, conductor=None, overplot=False,
                  block='all',multiply=None, cmap=None,  cbtitle=None,
                  showdefault=False, setdefault=False, cndargs={},
                  unset=False, use_fig=None, complist=None, color='mag')

    kwdefs.update(_pvdopts_cur) ## append settable option defaults
    ##print kwdefs

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=True)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + plotvec.__doc__  ;  return None
    kwextra = res[1]
    res = res[0]

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    keys.sort()
    ##for k in keys:   exec "print k, " + k
    ##print "kwextra:"
    ##for k in kwextra.keys(): print k, kwextra[k]

    if unset:
        for k in _plotvecdefs.keys():
            _pvdopts_cur[k] = _plotvecdefs[k]
        return

    if setdefault:
        for k in keys:
            if _pvdopts_cur.has_key(k): exec "_pvdopts_cur[k] = " + k
        if not showdefault:
            return

    if showdefault:
        blnk = ''
        for i in range(_pvdchmx):  blnk += ' '
        print "Default parameters for the PLOTVEC command are:"
        dkeys = _pvdopts_cur.keys()  ; dkeys.sort()
        for i in dkeys:
            l = len(i)
            tmp = "   " + i + blnk[l:] + " = "
            print tmp + str(_pvdopts_cur[i])
        return

    if ds is None:
        print DN + ": No dataset or structure index provided"  ;  return None
    ds, itype = process_ds(ds, DN, pff.blkgrid_dataset)
    if ds is None: return None

    if ds.sdim < 2 or ds.adim != ds.sdim:
        print DN +  ''': DS must have at least two spatial dimensions and
         the same number of attributes'''
        return None

    if overplot and _ovrplt.x == None:
        print DN + \
            ": previous plot status unknown -- overplot option not allowed"
        return None

    minargs = 1
    adim = ds.adim  ;  sdim = ds.sdim
    degen = np.where(ds.nx[0] == 1)[0]
    ldeg = len(degen)
    sdim_act = sdim
    if ldeg > 0:
        ##print degen
        for b in range(ds.nblk):
            dtest = np.where(ds.nx[b] == 1)[0]
            ##print dtest
            if ldeg != len(dtest):
                degen = None  ;  break
            else:
                tt = [ i for i in range(ldeg) if degen[i] != dtest[i] ]
                if len(tt):
                    degen = None  ;  break
        if degen is not None: sdim_act -= len(degen)
        ##print "degen:",degen

    kvals = []  ;  vals = []
    cargs = [ a1, a2 ]  ;  carg = 0
    if not overplot and sdim_act > 2: minargs += (2*(sdim_act-2))

    ##print cargs, carg, minargs, sdim_act
    if minargs > 3 or (minargs > 1 and cargs[minargs-2] is None):
        print DN + ":", minargs, "positional parameters are required"
        return None
    if not overplot or cargs[carg+1] is not None:
        for i in range(2,sdim_act):
            kvals.append(cargs[carg])
            vals.append(cargs[carg+1])
            carg += 2

    ##print kvals,vals
    okay = True
    if complist is not None:
        t = type(complist)
        cokay = True
        if t is types.ListType or t is types.TupleType:
            slcomp = list(complist)
            if len(slcomp) != 2: cokay = False
            else:
                used = [ 0 for c in range(adim+1) ]
                for c in slcomp:
                    if c > adim: c = 0 
                    used[max(0,c)] += 1
                    if used[0] or len([s for s in used if s>1]):
                        cokay = False
        if not cokay:
            print DN + ": Illegal COMPLIST specification"  ;  okay = False

        ##print 'COKAY:',cokay
        ##print okay,slcomp

    if levels is not None:
        t = type(levels)
        if (t is not types.ListType and t is not types.TupleType) or \
           len(levels) == 1:
            print DN+": Illegal value (',levels,') supplied for LEVELS keyword"
            okay = False
 
    cbloc = None
    if cbar is not None:
        t = type(cbar)
        cborient = None
        if t is types.StringType:
            c = cbar[0].lower()
            if c == 'h':   cborient = 'horizontal'
            elif c == 'v': cborient = 'vertical'
        if t is types.ListType or t is types.TupleType or t is np.ndarray:
            if (t is np.ndarray and cbar.ndim != 1) or len(cbar) != 4:
                print DN + ": colorbar box format is: [x0,y0,dx,dy]"
                okay = False
            cbloc = cbar
        if cbtitle is not None and type(cbtitle) is not types.StringType:
            print DN + ": Supplied colorbar title must be a string"
            okay = False

    if not okay:
        print "\n" + plotvec.__doc__  ;  return None

    ##print comp, slcomp, cmpid, kvals, vals, kwextra, okay

    rcomps = None
    nblk = ds.nblk
    sh = 0
    if transpose:  sh = 1
    if sdim_act > 2:
        #print kvals,len(kvals)
        if len(kvals):
            normid = kvals[0] - 1
            x1 = (normid+1+sh) % sdim
            x2 = (normid+2-sh) % sdim
        else: normid = None
        if overplot:
            ookay = True
            x1o,x2o,zno = (_ovrplt.x - 1, _ovrplt.y - 1, _ovrplt.znorm)
            if normid is not None:
                if x1 != x1o or x2 != x2o or vals[0] != zno:
                    print '''
WARNING: Supplied slice orientation and normal for an OVERPLOT plot are not
         consistent with previous plot. Previous settings will be used.'''
                    vals[0] = zno
                    kvals[0] = 4 - x1o - x2o
            else:
                vals.append(zno)
                kvals.append(4 - x1o - x2o)
            x1,x2 = (x1o,x2o)
            normid = kvals[0] - 1
            transpose = x1 != (normid+1) % sdim
        ##print vals, kvals, slcomp
        ##print ds.nblk, ds.data[0].shape
        if complist is None:
            slcomp = []
            k = kvals[0]
            ##print k,adim
            for i in range(adim-1): slcomp.append(((k+i) % adim) + 1)
            if transpose:  slcomp.reverse()
        sl = ds.get_slice(vals[0],comp=slcomp,coord=kvals[0],block=block)
        if sl is None:
            print DN + ": Error slicing dataset"
            return None
    elif sdim > 2:
        normid = degen[0]
        x1 = (normid+1+sh) % sdim
        x2 = (normid+2-sh) % sdim
        vals = [ ds.x[0][normid][0] ]
        ##print normid,x1,x2,vals
        sl = ds
        ##if type(slcomp) is not types.IntType:
        ##    print DN + ": Not implemented yet"
        ##    return None
        rcomps = (x1+1,x2+1)
    else:
        ##x1 = sh
        ##x2 = 1 - sh
        ##sl = ds
        ##vals = [ 0.0 ]
        print DN + ": Not implemented for sdim < 3"
        return None
    
    imtrans = x1 < x2
    ##print "imtrans:", imtrans

    nbsl = sl.nblk
    data = sl.data
    if multiply is not None:
        if sl is ds:
            data = []
            for b in range(nbsl):  data.append(np.array(sl.data))
        for b in range(nbsl):  data[b] *= multiply

    ##for i in range(nbsl):
    ##    print ds.data[i][0,0,0,:],data[i][0,0,0,:]

    if polar is True: polar = 1

    need_predraw = False

    grm = sl.g_range[nbsl,:,:]
    if not overplot:
        xrp = grm[x1,:]
        yrp = grm[x2,:]

        if xrange is not None: xrp = np.array(xrange)
        elif polar == 1:
            if xrp[0] < 0.7*xrp[1]: xrp =np.array([0.0,xrp[1]])
        if yrange is not None: yrp = np.array(yrange)
        if polar == 1: pflag = True
        else:  pflag = False
        f,ax = plots.adv_frame(use_fig=use_fig,polar=pflag)
        if title is None: ax.set_title(ds.title)
        elif title is not None: ax.set_title(title)
        if xlabel is None: ax.set_xlabel(ds.glabels[0,x1])
        else: ax.set_xlabel(xlabel)
        if ylabel is None: ax.set_ylabel(ds.glabels[0,x2])
        else: ax.set_ylabel(ylabel)
        if polar:
            aspect = 1.0
            if polar == 2:
                xtmp,ytmp = RT2XYLimits(xrp,yrp)
                ##print xrp,yrp,xtmp,ytmp
                ax.set_xlim(xtmp)
                ax.set_ylim(ytmp)
            else:
                ax.set_xlim(yrp) # doesn't do anything - always 0 to 2*pi
                ax.set_ylim(xrp)
        else:
            ax.set_xlim(xrp)
            ax.set_ylim(yrp)
        plots.set_frame_props(ax,lw,charsize)
        if aspect != 'auto':
            ax.set_aspect(aspect)
            need_predraw = True
        _ovrplt.set(x1+1,x2+1,vals[0],xrp, yrp,ax,polar=polar)
    else:
        f = plt.gcf()
        ax = _ovrplt.ax
        xrp = _ovrplt.xr
        yrp = _ovrplt.yr
        polar = 0
        if _ovrplt.polar:
            if type(ax.axesPatch) is matplotlib.patches.Rectangle: polar = 2
            else: polar = 1
    ##print "AX:", ax

    ##print polar,type(ax.axesPatch),ax.get_xlim(),ax.get_ylim()

    xrr = cpy.deepcopy(grm[x1,:])
    yrr = cpy.deepcopy(grm[x2,:])
    xclip = yclip = False
    if xrange is not None:
        if xrange[0] > xrr[0]:
            xrr[0] = xrange[0]  ;  xclip = True
        if xrange[1] < xrr[1]:
            xrr[1] = xrange[1]  ;  xclip = True
    if yrange is not None:
        if yrange[0] > yrr[0]:
            yrr[0] = yrange[0]  ;  yclip = True
        if yrange[1] < yrr[1]:
            yrr[1] = yrange[1]  ;  yclip = True

    if color == 'mag':
        if type(cmap) is types.StringType: cmap = plt.get_cmap(cmap)
        elif cmap is None: cmap = plt.get_cmap()
        smap = matplotlib.cm.ScalarMappable(cmap=cmap)
        fake = np.zeros((2,2))
        if cbar is not None:
            ### NEED to check clipped data range
            if levels is not None:
                dlow,dhi = levels
            else:
                xg = yg = None
                if xclip: xg = xrr
                if yclip: yg = yrr
                dlow,dhi = slice_data_range(sl,x1,x2,xg,yg,range(2))
                ##print dlow,dhi
    
            if cbloc is None:
                asp = ax.get_aspect()
                im = ax.imshow(fake,cmap=cmap,vmin=dlow,vmax=dhi)
                cb = f.colorbar(im,None,orientation=cborient)
                if cbtitle is not None:   cb.ax.set_title(cbtitle)
                plots.set_frame_props(cb.ax,lw,charsize)
                del ax.images[-1]
                ax.set_aspect(asp)
            need_predraw = True
        ##s = raw_input("continue: ")

    if polar and xygrid is None:
        if grm[x1,0] < 0.2*grm[x1,1]: xygrid = True
        else: xygrid = False
    polarLims = False
    if polar and xygrid:
        polarLims = [xrr,yrr]
        xrr, yrr = RT2XYLimits(xrr,yrr)

    vmin = data[0].min()  ;  vmax = data[0].max()
    for b in range(1,nbsl):
        vmin = min(vmin,data[b].min())  ;  vmax = max(vmax,data[b].max())

    vdef = 2.0*vmin - vmax
    ##print vmin,vmax,vdef,xrr,yrr,need_predraw
    
    if need_predraw: f.canvas.draw()

    fpix = np.array(f.canvas.get_width_height())
    px = fpix[0]*ax.figbox.width
    py = fpix[1]*ax.figbox.height
    axpix = np.array((px,py))
    if cbloc is not None:
        bnds = np.asarray(ax.figbox.bounds)
        xcb = np.asarray(cbloc)
        xab = np.arange(4,dtype='f4')
        xab[0:2] = bnds[0:2] + bnds[2:]*xcb[0:2]
        xab[2:] = bnds[2:]*xcb[2:]
        if xab[-2]*fpix[0] > xab[-1]*fpix[1]: cborient = 'horizontal'
        else: cborient = 'vertical'
        ##print 'bnds:',bnds,fpix,xab,xcb
        cax = f.add_axes(xab)
        asp = ax.get_aspect()
        ##print "CAX:", cax
        im = ax.imshow(fake,cmap=cmap,vmin=dlow,vmax=dhi)
        cb = f.colorbar(im,cax,orientation=cborient)
        if cbtitle is not None:   cb.ax.set_title(cbtitle)
        plots.set_frame_props(cb.ax,lw,charsize)
        del ax.images[-1]
        ax.set_aspect(asp)
        ##s = raw_input("continue: ")

    pixmax = axpix.max()
    if not polar or xygrid:
        xshape = np.array(npmax*axpix/pixmax + 0.499,dtype='i4')
        pixlen = (axpix/xshape).min()
    else:
        if polar == 1:
            rt = max(xrr[0],0.25*xrr[1])
            xsr = npmax*(0.5*np.diff(xrr)[0]/np.diff(xrp)[0])
            xst = npmax*(np.diff(yrr)/np.pi)
            xshape = np.array([xsr,xst],dtype='i4')
            pixlen = (axpix/xshape).min()
        else:
            pixrate = axpix[0]/np.diff(ax.get_xlim())[0]
            rtst = max(0.25*xrp[1],xrp[0])
            lrt = np.array([np.diff(xrp)[0],rtst*np.diff(yrp)[0]])
            lmax = lrt.max()
            xshape = np.array((npmax/lmax)*lrt,dtype='i4')
            pixlen = pixrate*(lrt/xshape).min()
            
    hplen = 0.5*0.9*pixlen

    ##print npmax,axpix,xshape,pixmax/xshape,hplen,complist

    prflag = polar and xygrid
    if imtrans:
        imshape = tuple(xshape) ; xg = xrr ; yg = yrr
    else:
        imshape = tuple(np.flipud(xshape)) ; xg = yrr ; yg = xrr
    ##if polar: print 'ras',imshape,xg,yg,prflag,polarLims
    rast = rasterize(sl,imshape,vdef=vdef,xr=xg,yr=yg,polar=prflag,
                   polarLims=polarLims,complist=rcomps)
    if not imtrans:
        r1 = rast ; rast = ()
        for r in r1: rast += ( np.transpose(r), )
    
    ##print type(rast),len(rast),imshape,xg,yg
    ##for r in rast:   print r.shape
    ##print np.where(rast[0] == vdef)
    w = np.where(rast[0] != vdef)
    lims = (xrr,yrr)  ;  grds = ()
    for i,g in enumerate(lims):
        s = xshape[i]
        grds += ( (g[1]-g[0])/s*np.arange(0.5,s,dtype=pff.PFFnp_float)+g[0], )
    ##print grds

    ##nvecs = w[0].size
    if polar == 1:
        rng = ax.get_ylim()
        drng = np.diff(rng)[0]
        vrng = 2.0*drng*np.ones((2))
    else:
        vrng = np.empty((2))
        tmp = ax.get_xlim()
        vrng[0] = tmp[1] - tmp[0]
        tmp = ax.get_ylim()
        vrng[1] = tmp[1] - tmp[0]

    axpixinv = vrng/axpix
    anno_mode = 'data'

    xg,yg = grds
    ex,ey = rast
    emag = np.sqrt(np.add(np.square(ex),np.square(ey)))
    ##print emag.shape,emag[w].size,emag[w].min(),emag[w].max()
    emagmax = emag[w].max()
    emaglow = 5.0e-5*emagmax
    notzero = np.where(emag[w] > emaglow)[0]
    w = (w[0][notzero],w[1][notzero])
    ##print emag[w].size,emag[w].min(),emag[w].max()
    if color == 'mag':
        ##print 'smap.set_clim:',emagmax
        if cbar is None:
            asp = ax.get_aspect()
            dlow,dhi = 0.0,emagmax
            im = ax.imshow(fake,cmap=cmap,vmin=dlow,vmax=dhi)
            del ax.images[-1]
            ax.set_aspect(asp)
        smap.set_clim(dlow,dhi)
        cols = smap.to_rgba(emag[w])
    else:
        cmap = plots.bld_map(color)
        acolor = plots.get_property(0,cmap)
        cols = np.empty((w[0].size,4))
        cols[...] = matplotlib.colors.colorConverter.to_rgba(acolor)

    if polar == 2: xrp[0] = 0.0
    for k,i in enumerate(w[0]):
        j = w[1][k]  ;  emg = emag[i,j]
        if polar:
            if xygrid:
                x,y = xy = (xg[i],yg[j])
                rsq = np.sum(np.square(xy))
                if rsq != 0.0:  cs,sn = np.divide(xy,math.sqrt(rsq))
                else: cs,sn = (1.0,0.0)  # not np.arctan2(0.,0.) = 0.0
                ##print k,x,y,rsq,cs,sn,ex[i,j],ey[i,j]
            else:
                cs,sn = (np.cos(yg[j]), np.sin(yg[j]))
                x,y = (xg[i]-xrp[0])*np.array([cs,sn])
            exy =  np.array([ex[i,j]*cs - ey[i,j]*sn, ex[i,j]*sn + ey[i,j]*cs])
        else:
            x,y = (xg[i],yg[j])
            exy = np.array([ex[i,j], ey[i,j]])
        p0 =  np.array([x,y])
        pdel = (hplen/emg)*exy*axpixinv
        col = cols[k]
        ax.annotate('',p0+pdel,p0-pdel,arrowprops=dict(arrowstyle='->', \
                    color=col),xycoords=anno_mode)

    if color == 'mag' and cbar is not None:
        cb.set_clim(0.,emagmax)
        if len(ax.images): del ax.images[-1]
    if conductor is not None:
        ##print 'CONDUCTOR',x1,x2,vals,use_fig,cndargs
        try:     #  Now try to overplot conductor
            plotcon(conductor,x1+1, x2+1, vals[0], over=True, use_fig=use_fig,
                     **cndargs)
        except AttributeError, e:
            print DN + ": Error overplotting conductor:",e  ;  return None
            
    f.canvas.draw()
    if return_slice: return sl
    return 0
    
__all__.append('plotpar')
def plotpar(*args, **kwargs):
    '''Plot various data from a particle dataset.

Usage:
  plotpar(ds, [x, y] , [weight], [xrange=list], [yrange=list], [xlabel=str], \\
          [ylabel=str], [title=str], [overplot=bool], [aspect=bool|float], \\
          [zrange=list], [zscale=float], [zplane=float], [conductor=ds|list], \\
          [cmap=str|cmap], [cbar=str|list], [cbtitle=str]], \\
          [psymbol=str|int], [symsize=int], [edgecolors=str], [w1=list, \\
          [w2=list, ...]] )

Arguments:
  ds:         PFF dataset containing particle data, or structure index of
              dataset containing particle data.
  x:          Direction for horizontal plot axis (integer). If negative,
              indicates attribute dimension.
  y:          Direction for vertical plot axis (integer). If negative,
              indicates attribute dimension.
  weight:     If supplied, color scaling dimension (integer). If negative,
              indicates attribute dimension.
  xrange:     Horizontal (x) range of plot
  yrange:     Vertical (y) range of plot
  xlabel:     Horizontal (x) axis label for output plot
  ylabel:     Vertical (y) axis label for output plot
  title:      Title for plot
  overplot:   If possible, plot grid over the previous plot
  aspect:     Aspect ratio of plot
  zrange:     2-element list containing data values mapped to the min and max
              colors in the color map
  w1,w2,...   Attribute window filter specification (list with 3 or more
              elements). Plot can be windowed in real space or attribute space.
              The last two elements are the minimum and maximum values of the
              window parameter; the first value(s) are the spatial or attribute
              parameters using the window. Here positive and negative values
              indicate spatial or attribute data, respectively.
  zscale:     Multiplicative scaling factor for WEIGHT data
  zplane:     Value of the 3rd dimension for the CONDUCTOR overplot. If not
              supplied, the midpoint of the dataset's range in that dimension
              is used.
  conductor:  Conductor dataset (or list of datasets) to be overplotted.
              Must describe a valid conductor (some PFF Vertex datasets or
              pfmpl-2d quadlist3d dataset)
  cmap:       Mathplotlib color map to be used.
  cbar:       If set, color bar is to be plotted. Valid values are:
                'h' or 'v', indicating horizontal or vertical placement
                [x0,y0,dx,dy], indicating overlay position on plot axes, in
                   axes-normalized units.
  cbtitle:    Title for color bar
  psymbol:    Symbol type used for scatter plot (see 'marker' option for
              Mathplotlib scatter function for legal values)
  symsize:    Size of symbol (in pixels^2)
  edgecolors: Color of symbol outline

Return value: If successful, returns 0. Otherwise, None is returned'''

    DN = 'PLOTPAR'
    argnames = [ 'ds', 'a1', 'a2', 'a3' ]
    optvals = [ None, None, None ]
    kwdefs = { 'xrange':None, 'yrange':None, 'xlabel':None,'ylabel':None,  \
               'title':None, 'overplot':False, 'aspect':None, 'zrange':None, \
               'zscale':None, 'zplane':None, 'conductor':None, 'cmap':None, \
               'cbar':None, 'cbtitle':None, 'psymbol':'o', 'symsize':9, \
               'edgecolors':'none', 'lw':None, 'charsize':None, 'use_fig':None }
               ##, '':, '':, '':, '':, '':, '':, 
    wkeys = [ 'w#' ]

    res = utils.process_wargs(args, kwargs, DN, argnames, kwdefs, wkeys, \
                              optvals, extra=True)

    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + plotpar.__doc__  ;  return None
    wres = res[1]
    scatterargs = res[2]
    res = res[0]

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    ##for i in range(len(wkeys)):
    ##    wr = wres[i]  ;   ww = wkeys[i]
    ##    for k in wr.keys():
    ##        s = ww.replace('#',str(k))   ;   print s + ":", wr[k]
    ##print scatterargs

    ds, itype = process_ds(ds, DN, pff.VTX_dataset)
    if ds is None: return None
    okay = True

    windows = [ ]
    wr = wres[0]
    for k in wr:  windows.append(wr[k])

    if cbar is not None:
        t = type(cbar)
        cbloc = None
        cborient = None
        if t is types.StringType:
            c = cbar[0].lower()
            if c == 'h':   cborient = 'horizontal'
            elif c == 'v': cborient = 'vertical'
        if t is types.ListType or t is types.TupleType or t is np.ndarray:
            if (t is np.ndarray and cbar.ndim != 1) or len(cbar) != 4:
                print DN + ": colorbar box format is: [x0,y0,dx,dy]"
                okay = False
            cbloc = cbar
        if cbtitle is not None and type(cbtitle) is not types.StringType:
            print DN + ": Supplied colorbar title must be a string"
            okay = False

    if not okay:
        print "\n" + plotpar.__doc__  ;  return None

    x = None  ;  y = None  ;  weight = None
    cargs = [ a1, a2, a3 ]  ;  carg = 0
    nargs = len([ i for i in cargs if i is not None])
    ##print nargs
    if nargs < 2 and not overplot:
        print DN + ": invalid # of positional arguments:",nargs+1
        print "\n" + plotpar.__doc__  ;  return None

    if overplot and _ovrplt.x == None:
        print DN + \
            ": previous plot status unknown -- overplot option not allowed"
        return None

    if (nargs % 2) == 1: weight = cargs[-1]
    sdim = ds.sdim  ;  adim = ds.adim
    if nargs >= 2:   x,y = cargs[0:2]
    elif overplot:
        x = _ovrplt.x  ;  y = _ovrplt.y
    xokay = True  ;  yokay = True  ;  wokay = True
    if x == 0: xokay = False
    elif x > 0:
        if x > sdim: xokay = False
        else: ix = x - 1
    elif x < 0:
        if -x > adim: xokay =False
        else: ix = -x - 1
    if y == 0: yokay = False
    elif y > 0:
        if y > sdim: yokay = False
        else: iy = y - 1
    elif y < 0:
        if -y > adim: yokay =False
        else: iy = -y - 1
    if not xokay:  print DN + ": X argument is not valid"
    if not yokay:  print DN + ": Y argument is not valid"
    if weight is not None:
        w = weight
        if w == 0: wokay = False
        elif w > 0:
            if w > sdim: wokay = False
            else: iw = w - 1
        elif w < 0:
            if -w > adim: wokay =False
            else: iw = -w - 1
        if not wokay:  print DN + ": WEIGHT argument is not valid"
    if not xokay or not yokay or not wokay:
        return None
    ##print x,y,ix,iy,weight

    data = ds.data  ;  xdata = ds.x
    grange = ds.g_range
    xrng = xrange  ;  yrng = yrange
    if not overplot:
        f,ax = plots.adv_frame(use_fig=use_fig)
        if xrange is None or yrange is None:
            if xrange is None:
                if x > 0: xr = grange[ix,:]
                else:
                    d = data[:,ix]  ;  xr = [d.min(), d.max()]
                xrng = utils.nice_bounds(xr[0],xr[1])[0:2]
            if yrange is None:
                if y > 0: yr = grange[iy,:]
                else:
                    d = data[:,iy]  ;  yr = [d.min(), d.max()]
                yrng = utils.nice_bounds(yr[0],yr[1])[0:2]
        ax.set_xlim(xrng)
        ax.set_ylim(yrng)
        if aspect is False or aspect == 0: aspect = None
        elif aspect is True:  aspect = 1.0
        if aspect is not None: ax.set_aspect(aspect)
        #print ax.xaxis.get_smart_bounds(),ax.yaxis.get_smart_bounds() 
        if title is None: ax.set_title(ds.title)
        else: ax.set_title(title)
        if xlabel is None:
            if x > 0: ax.set_xlabel(ds.glabels[ix])
            else: ax.set_xlabel(ds.dlabels[ix])
        else: ax.set_xlabel(xlabel)
        if ylabel is None:
            if y > 0: ax.set_ylabel(ds.glabels[iy])
            else: ax.set_ylabel(ds.dlabels[iy])
        else: ax.set_ylabel(ylabel)
        plots.set_frame_props(ax,lw,charsize)
        if zplane is None:
            if x < 0 or y < 0 or sdim < 3: zplane = 0.0
            else:
                iz = 3-ix-iy
                zplane = 0.5*np.sum(grange[iz,:])
        _ovrplt.set(x,y,zplane,xrng, yrng,ax)
    else:
        xrng = _ovrplt.xr
        yrng = _ovrplt.yr
        ax =  _ovrplt.ax

    npar = ds.nv
    ind = None
    if xrange is not None:
        indo = ind
        if ind is None: indo = slice(npar)
        if x > 0: xd = xdata[ix,indo]
        else:     xd = data[indo,ix]
        nind = np.where(xd > xrng[0])[0]
        if len(nind) == 0:
            print DN + ": No particles meet filtering criteria"  ;  return 0
        if ind is None: ind = nind
        else: ind = ind[nind]
        ##print 'x0:',len(ind)
        if x > 0: xd = xdata[ix,ind]
        else:     xd = data[ind,ix]
        nind = np.where(xd < xrng[1])[0]
        if len(nind) == 0:
            print DN + ": No particles meet filtering criteria"  ;  return 0
        ind = ind[nind]
        ##print 'x0:',len(ind)
    if yrange is not None:
        indo = ind
        if ind is None: indo = slice(npar)
        if y > 0: yd = xdata[iy,indo]
        else:     yd = data[indo,iy]
        nind = np.where(yd > yrng[0])[0]
        if len(nind) == 0:
            print DN + ": No particles meet filtering criteria"  ;  return 0
        if ind is None: ind = nind
        else: ind = ind[nind]
        ##print 'y0:',len(ind)
        if y > 0: yd = xdata[iy,ind]
        else:     yd = data[ind,iy]
        nind = np.where(yd < yrng[1])[0]
        if len(nind) == 0:
            print DN + ": No particles meet filtering criteria"  ;  return 0
        ind = ind[nind]
        ##print 'y1:',len(ind)

    itmp = 0
    for win in windows:
        lims = win[-2:]
        ##print win[:-2], lims
        for z in win[:-2]:
            zmlt = False
            if z == weight and zscale is not None: zmlt = True
            itmp += 1
            indo = ind
            if ind is None: indo = slice(npar)
            if z > 0: xd = xdata[z-1,indo]
            else:     xd = data[indo,-z-1]
            if zmlt: xd = np.multiply(xd,zscale)
            nind = np.where(xd > lims[0])[0]
            if len(nind) == 0:
                print DN + ": No particles meet filtering criteria"  ;  return 0
            if ind is None: ind = nind
            else: ind = ind[nind]
            ##print 'x'+str(itmp)+'0:',len(ind)
            if z > 0: xd = xdata[z-1,ind]
            else:     xd = data[ind,-z-1]
            if zmlt: xd = np.multiply(xd,zscale)
            nind = np.where(xd < lims[1])[0]
            if len(nind) == 0:
                print DN + ": No particles meet filtering criteria"  ;  return 0
            ind = ind[nind]
            ##print 'x'+str(itmp)+'1:',len(ind)

    indo = ind
    if ind is None:  indo = slice(npar)

    if x > 0: xd = xdata[ix,indo]
    else:     xd = data[indo,ix]
    if y > 0: yd = xdata[iy,indo]
    else:     yd = data[indo,iy]
    if ind is not None:
        npar = len(ind)
        ##print npar,len(yd),yd.min(),yd.max()
        if xrange is None:
            ax.set_xlim(utils.nice_bounds(xd.min(),xd.max())[0:2])
        if yrange is None:
            ax.set_ylim(utils.nice_bounds(yd.min(),yd.max())[0:2])
    if weight is None:
        sc = ax.scatter(xd,yd,s=symsize,marker=psymbol,edgecolors=edgecolors, \
                        **scatterargs)
    else:
        if weight > 0: wd = xdata[iw,indo]
        else:          wd = data[indo,iw]
        ##print "ID",id(wd),wd.flags
        if zscale is not None: wd = np.multiply(wd,zscale)
        ##print "ID",id(wd),wd.flags
        if zrange is None:  vmin,vmax = (None,None)
        else: vmin,vmax = tuple(zrange[:2])
        sc = ax.scatter(xd,yd,s=symsize,marker=psymbol,edgecolors=edgecolors, \
                        c=wd, cmap=cmap, vmin=vmin, vmax=vmax, **scatterargs)
        
    if cbar is not None:
        if cbloc is None:  cax = None
        else:
            bnds = np.asarray(ax.figbox.bounds)
            xcb = np.asarray(cbloc)
            xab = np.arange(4,dtype='f4')
            xab[0:2] = bnds[0:2] + bnds[2:]*xcb[0:2]
            xab[2:] = bnds[2:]*xcb[2:]
            pxf, pyf = f.canvas.get_width_height()
            if xab[-2]*pxf > xab[-1]*pyf: cborient = 'horizontal'
            else: cborient = 'vertical'
            cax = f.add_axes(xab)
            ##print "CAX:", cax
        cb = f.colorbar(sc,cax,orientation=cborient)
        if cbtitle is not None:
            cb.ax.set_title(cbtitle)
        plots.set_frame_props(cb.ax,lw,charsize)
 
    if conductor is not None:
        plotcon(conductor,x, y, zplane, over=True, use_fig=use_fig, **condargs)
            
    f.canvas.draw()

    return npar


__all__.append('shelp')
def shelp(*args,**kwargs):
    '''List information about the structure arrays.

Usage:
  shelp( [first], [last], [file=bool], [full=bool] )

Arguments:
  first:    Integer index of first structure to be listed, a list of integer
            structure numbers to be listed, or a string to be matched to
            structure comments (titles). If LAST is specified, FIRST must be
            an integer <= LAST. If FIRST is a non-positive integer, information
            for all valid structures will be printed.
  last:     Integer index of last structure to be listed.
  file:     If True, filename will be included in output for brief mode.
  full:     If True, full, detailed structure information will be printed.
            If False, only a brief listing (structure number, comment) is given.
            If not specified, a detailed listing will be given if FIRST is an
            integer and LAST is not specified, or if FIRST is a list.

Return value: Returns 0 if successful, or None on error'''

    DN = 'SHELP'
    argnames = ['first','last']
    optvals = [ None,None ]
    kwdefs = { 'file':False, 'full':None }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + shelp.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    detailed = False
    if type(first) == types.IntType and first <= 0:
        slist = _struclist.keys()
        slist.sort()
    else:
        slist = utils.parsewsl(_struclist, first, last, name='STRUCT', \
                               stringOK=True)
        if slist is None:
            return None

        if (type(first) == types.IntType and last is None) or \
           type(first) == types.ListType:
            detailed = True

    if len(slist) == 0:
        print 'Specified STRUCT arrays are empty'
        return None

    if full is not None:
        if not full: detailed = False
        else:        detailed = True

    if detailed:
        for i in slist:
            print " -----------------"
            print "Structure array number" , i
            ds = _struclist[i]
            ds, itype = process_ds(ds, DN, \
                           (pff.blkgrid_dataset,pff.VTX_dataset,quadlist3d))
            if ds is None:
                print "Unknown dataset type"
            else:
                if itype == 0:
                    adim = ds.adim ; sdim = ds.sdim ; nblk = ds.nblk
                    if adim == 0:  print "GRID STRUCTURE"
                    else:  print "FIELD STRUCTURE"
                elif itype == 1:
                    adim = ds.adim  ;  sdim = ds.sdim
                    print "PARTICLE STRUCTURE"
                else:
                    adim = 0  ;  sdim = 3
                    print "CONDUCTOR STRUCTURE"
                print " Dataset Type:", ds.typename
                print " Dataset Comment:", ds.title
                print " File:", ds.file
                print ' Number of spatial dimensions:', sdim
                gl = ds.glabels  ;  gr = ds.g_range
                if itype == 0:
                    gl = gl[0]  ;  gr = gr[nblk]
                print " Spatial labels:",gl
                print " Range in each Spatial dimension:"
                fmt = "   %" + str(gl.dtype)[2:] + "s    %s"
                for i in range(sdim): print fmt % (gl[i], gr[i])
                if itype == 0:
                    print ' Number of blocks:',nblk
                    print ' Size of the spatial arrays for each block:'
                    print '   Block    Size of each dimension'
                    for i in range(nblk):  print "%7d      %s" % (i+1,ds.nx[i])
                    if adim: print ' Number of vector components:', adim
                    if adim:
                        print " Range of data in each vector dimension:"
                        data = ds.data ; s1 = ""  ;  s2 = ""  ;  s3 = ""
                        mnmx = np.empty((adim,2),dtype=data[0].dtype)
                        for b in range(nblk):
                            bdat = data[b]
                            for i in range(adim):
                              exec "mnmx[i,0] = "+ s1 + "bdat[...,i].min()" + s3
                              exec "mnmx[i,1] = "+ s2 + "bdat[...,i].max()" + s3
                            if b == 0:
                                s1 = "min(mnmx[i,0],"
                                s2 = "max(mnmx[i,1],"  ;  s3 = ")"
                elif itype == 1:
                    print ' Number of particles:', ds.nv
                    print ' Number of attributes:', adim
                    if adim:
                        print " Range of data in each attribute:"
                        data = ds.data
                        mnmx = np.empty((adim,2),dtype=data.dtype)
                        for i in range(adim):
                            mnmx[i,0] = data[:,i].min()
                            mnmx[i,1] = data[:,i].max()
                elif itype == 2:
                    cnts = np.diff(ds.locx)
                    print ' Number of slant surfaces:', ds.nqs
                    print ' Number of surfaces normal to X-axis:', cnts[0]
                    print ' Number of surfaces normal to Y-axis:', cnts[1]
                    print ' Number of surfaces normal to Z-axis:', cnts[2]
                    print ' Total number of conductor surfaces:', ds.nq + ds.nqs
                if adim:
                    for i in range(adim):   print "%5d   %s" % (i+1,mnmx[i,:])
    else:
        print "Structures with Valid Data are Listed:"
        print "Structure   Comment",
        if file: print ": File"
        else: print ""
        jd = max(int(math.log10(max(slist)))+1,5)
        fmt="%"+str(jd)+"d"
        for i in range(12-jd): fmt += " "
        fmt += "%s%s"
        for i in slist:
            fstr = ''
            if file: fstr = ": " + _struclist[i].file
            print fmt % ( i, _struclist[i].title, fstr )
    return 0


__all__.append('delstr')
def delstr(*args,**kwargs):
    '''Delete one or more STRUCTURE arrays.

Usage:
  delstr( [first], [last], [quiet=bool] )

Arguments:

  first: Integer index of first Structure array to be deleted, a list
         of integer Structure array indices to be deleted, or a string
         to be matched to Structure array comments (titles). If LAST
         is specified, FIRST must be an integer <= LAST. If FIRST is a
         non-positive integer, all valid Structure arrays will be
         deleted. If FIRST is a string, all Structure arrays whose
         comments match the string will be deleted.

  last:  Integer index of last Structure array to be deleted.
  quiet: If True, no message confirming deletion will be printed.

Return value: If successful, returns the number of Structure arrays deleted,
              or None on error'''

    DN = 'DELSTR'
    argnames = ['first','last']
    optvals = [ None,None ]
    kwdefs = { 'quiet':False }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + delstr.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    if type(first) == types.IntType and first <= 0:
        dlen = len(_struclist)  ;  _struclist.clear()
    else:
        dlist = utils.parsewsl(_struclist, first, last, name='STRUCT', \
                               stringOK=True)
        if dlist is None:  return None
        dlen = len(dlist)
        for w in dlist:  del _struclist[w]

    if not quiet:  print dlen, 'Structure Arrays have been Cleared'

    return dlen
    

__all__.append('get_empty_str')
def get_empty_str(*args,**kwargs):
    '''Finds one or more consecutive empty (unused) STRUCTURE arrays.

Usage:
  get_empty_str( [first], [last=int], [count=int] )

Arguments:
  first: Integer indicating the array index to begin searching for empty
         STRUCTURE arrays. If non-positive, 1 will be used.
  last:  Integer index of last array location to be searched. (Default: None)
  count: Number of consecutive array locations that to be empty. (Default: 1)

Return value: If successful, returns the index of the first empty
              STRUCTURE array, or None if a consecutive block of empty
              arrays could not be found (this can occur only if LAST
              is specified).'''

    DN = 'GET_EMPTY_STR'
    argnames = ['first']
    optvals = [ 1 ]
    kwdefs = { 'last':None, 'count':1 }
               ##, '':, '':, '':, '':, '':, '':, 

    res = utils.process_args(args, kwargs, DN, argnames, kwdefs, optvals, \
                             extra=False)
    if res == 0 or res == 1:
        nl = [ "", "\n" ]
        print nl[res] + get_empty_str.__doc__  ;  return None

    keys = res.keys()
    for k in keys: exec k + " = res[k]"

    ##keys.sort()
    ##for k in keys:   exec "print k, " + k

    first = max(first,1)
    return utils.get_empty_wsl(_struclist,first,last,count)


__all__.append('rasterize')
def rasterize(sl,shape,vdef=-1.0e30,xr=None,yr=None,block=None,polar=False,
              polarLims=None,complist=None):
    ##print "rasterize:",shape,vdef,xr,yr,block
    if not isinstance(sl,pff.blkgrid_dataset):
        print "SL must be a PFF Block-Grid Dataset"
        return None
    t = type(shape)
    if (t is not types.ListType and t is not types.TupleType) or \
       len(shape) != 2:
        print "SHAPE must be a 2-element list or tuple of integers"
        return None
    nblk = sl.nblk
    sdim = sl.sdim
    t = type(block)
    okay = True
    if block is None or block == 'all':
        useblks = range(nblk)
    else:
        if t is types.IntType:
            if block < 1 or block >= nblk: okay = False
            else: useblks = [block - 1]
        elif t is types.ListType or t is types.TupleType:
            useblks = list(np.subtract(block,1))
            useblks.sort()
            used = [ 0 for i in range(nblk+1) ]
            for b in useblks:
                if b < 0: b = nblk
                used[min(nblk,b)] += 1
                if used[nblk] or len([s for s in used if s>1]): okay = False
        else: okay = False
    if not okay:
        print "Illegal BLOCK specification"
        return None

    keep = [] ; degen = []
    b0 = useblks[0]
    sh = sl.data[b0].shape
    if len(sh) == sdim + 1:
        if complist is None:
            multi = sh[-1]
            complist = range(multi)
        else:
            t = type(complist)
            if t is types.IntType: complist = [complist]
            elif t is not types.ListType and t is not types.TupleType:
                okay = False
            if okay:
                shextra = sh[-1]
                multi = len(complist)
                tst = np.asarray(complist)
                if multi > shextra or tst.max() > shextra or tst.min() < 1:
                    okay = False
                else: complist = (tst - 1).tolist()
            if not okay:
                print "Error in COMPLIST specification"
                return None
        sh = sh[:-1]
    else:  multi = 0
    ##print 'multi,sh',multi,sh
    if len(sh) != sdim: okay = False
    else:
        okay = True
        for i in range(sdim):
            if sh[i] == 1: degen += [i]
            else: keep += [i]
        g_range = sl.g_range
        if len(useblks) == nblk:
            gr = g_range[nblk,:,:]
        else:
            gr = np.empty((sdim,2),dtype=pff.PFFnp_float)
            gr[:,0] = g_range[useblks,:,0].min(axis=0)
            gr[:,1] = g_range[useblks,:,1].max(axis=0)

        for b in range(b0+1,nblk):
            if not b in useblks: continue
            sh = sl.data[b].shape
            if multi > 0:
                if sh[-1] != multi:  okay = False
                sh = sh[:-1]
            if len(sh) != sdim: okay = False
            ik = 0 ; idg = 0
            for i in range(sdim):
                if sh[i] == 1:
                    tlist = degen
                    it = idg
                else:
                    tlist = keep
                    it = ik
                if len(tlist) > it:
                    if tlist[it] != i:  okay = False
                else:   okay = False
                if sh[i] == 1: idg += 1
                else: ik += 1
    if not okay or len(keep) != 2:
        print "Supplied dataset is not a scalar 2D slice"
        return None
    if xr is None: xr = gr[keep[0],:]
    if yr is None: yr = gr[keep[1],:]

    dx = xr[1] - xr[0]
    dy = yr[1] - yr[0]
    px = shape[0]  ;  py = shape[1]
    xn = dx/px*np.arange(0.5,px,dtype=pff.PFFnp_float) + xr[0]
    yn = dy/py*np.arange(0.5,py,dtype=pff.PFFnp_float) + yr[0]
    ##print px,py,dx,dy,xn[::px-1],yn[::py-1]
    new = vdef*np.ones(tuple(shape),dtype=pff.PFFnp_float,order='F')
    ##print 'vdef,shape:',vdef,shape
    if multi > 0:
        rval = (new,)
        for i in range(multi-1):  rval += \
                (vdef*np.ones(tuple(shape),dtype=pff.PFFnp_float,order='F'),)
    else: rval = new

    byind = []
    byf1 = []
    ##print len(xn),xn
    for com in range(max(1,multi)):
        if com > 0: new = rval[com]
        for b in useblks:
            if polar:
                xrr,yrr = polarLims
                ##print "b:",b
                r = sl.x[b][keep[0]]
                w = np.where(np.logical_and(xrr[0] <= r, r <= xrr[1]))[0]
                r = r[w]
                roff = w[0]
                nr = len(r)
                t = sl.x[b][keep[1]]
                w = np.where(np.logical_and(yrr[0] <= t, t <= yrr[1]))[0]
                t = t[w]
                toff = w[0]
                nt = len(t)
                if multi > 0: data = sl.data[b][...,complist[com]]
                else: data = sl.data[b]
                thetamin = sl.g_range[nblk,keep[1],0]
                sh2d = tuple([i for i in data.shape if i>1 ])
                d2d =  np.reshape(data,sh2d,order='A')
                for iy in range(py):
                    y = yn[iy]  ;  ysq = y*y
                    rn = np.sqrt(np.square(xn) + ysq)
                    tn = np.arctan2(y,xn)
                    w = np.where(tn < thetamin)[0]
                    tn[w] += 2.0*np.pi
                    fp = np.arange(nr)
                    intrp = np.interp(rn,r,fp,left=-1.0,right=-1.0)
                    rf1, find = np.modf(intrp)
                    rind = np.int32(find)
                    ##print x,xn,xind
                    w = np.where(rind > nr - 2)[0]
                    rind[w] = nr - 2
                    rf1[w] = 1.0
                    fp = np.arange(nt)
                    intrp = np.interp(tn,t,fp,left=-1.0,right=-1.0)
                    tf1, find = np.modf(intrp)
                    tind = np.int32(find)
                    w = np.where(tind > nt - 2)[0]
                    ##print nt,w
                    tind[w] = nt - 2
                    tf1[w] = 1.0
                    ##print sh2d
                    wr = np.where(rind != -1)[0]
                    reducedtind = tind[wr]
                    wt = np.where(reducedtind != -1)[0]
                    w = wr[wt]
                    ##print np.where(xind == -1)[0]
                    rf0 = 1.0 - rf1
                    tf0 = 1.0 - tf1
                    dmm = np.empty((len(w),)) ; dmp = np.empty((len(w),))
                    dpm = np.empty((len(w),)) ; dpp = np.empty((len(w),))
                    ##djunk = d2d[rind[w],tind[w]]
                    ##if iy == 0: print 'djunk',djunk.shape
                    rf0 = rf0[w] ; rf1 = rf1[w] ; tf0 = tf0[w] ; tf1 = tf1[w]
                    rind = rind[w] + roff ; rindp = rind+1
                    tind = tind[w] + toff ; tindp = tind+1
                    new[w,iy] = rf0*(tf0*d2d[rind, tind] + \
                                     tf1*d2d[rind, tindp]) + \
                                rf1*(tf0*d2d[rindp,tind] + tf1*d2d[rindp,tindp])
            else:
                ##print "b:",b
                x = sl.x[b][keep[0]]
                nx = sl.nx[b][keep[0]]
                fp = np.arange(nx)
                intrp = np.interp(xn,x,fp,left=-1.0,right=-1.0)
                xf1, find = np.modf(intrp)
                xind = np.int32(find)
                ##print x,xn,xind
                w = np.where(xind > nx - 2)[0]
                xind[w] = nx - 2
                xf1[w] = 1.0
                y = sl.x[b][keep[1]]
                ny = sl.nx[b][keep[1]]
                fp = np.arange(ny)
                intrp = np.interp(yn,y,fp,left=-1.0,right=-1.0)
                yf1, find = np.modf(intrp)
                yind = np.int32(find)
                w = np.where(yind > ny - 2)[0]
                ##print ny,w
                yind[w] = ny - 2
                yf1[w] = 1.0
                byind.append(yind)
                byf1.append(yf1)
                if multi > 0: data = sl.data[b][...,complist[com]]
                else: data = sl.data[b]
                sh2d = tuple([i for i in data.shape if i>1 ])
                ##print sh2d
                d2d =  np.reshape(data,sh2d,order='A')
                wx = np.where(xind != -1)[0]
                wy = np.where(yind != -1)[0]
                ##print np.where(xind == -1)[0]
                f1 = xf1[wx]
                f0 = 1.0 - f1
                yf0 = 1.0 - yf1
                xuse = xind[wx]
                xusep = xuse + 1
                ##print w,ind[w],f1[w]
                ##print new.shape,d2d.shape,wx,xuse,xusep
                for i in wy:
                    ##print i
                    j = yind[i]
                    ##print j
                    new[wx,i] = f0*(yf0[i]*d2d[xuse,j] + \
                                    yf1[i]*d2d[xuse,j+1]) + \
                                f1*(yf0[i]*d2d[xusep,j] + yf1[i]*d2d[xusep,j+1])
            
    return rval
    

def process_ds(ds, DN, ok_types=None):

    if type(ds) is types.IntType:
        if _struclist.has_key(ds):  ds = _struclist[ds]
        else:
            print DN + ": Struct",ds,"is empty"  ;  return (None,None)
    if ok_types is None:
        if type(ds) is not types.InstanceType:
            print DN + ": Struct has invalid type:",type(ds)
            return (None,None)
        return (ds,0)
    else:
        t = type(ok_types)
        if t is not types.ListType and t is not types.TupleType:
            ok_types = [ok_types]
        for i in range(len(ok_types)):
            ty = ok_types[i]
            if type(ty) is not types.ClassType:
                print DN + ":", ty, "is not a class"  ;  return (None,None)
            if isinstance(ds,ty):
                return (ds,i)
    print DN + ": DS must be a Supported Dataset"
    return (None,None)


def grid_match(ds1,ds2):
    if not isinstance(ds1,pff.blkgrid_dataset) or \
       not isinstance(ds1,pff.blkgrid_dataset):
        print "GRID_MATCH: Datasets must be instances of pff.blkgrid_dataset"
        return None
    nblk = ds1.nblk  ;  sdim = ds1.sdim
    # number of blocks and spatial dimensionality must match
    if nblk != ds2.nblk or sdim != ds2.sdim:   return False

    #now check grid in each block
    for i in range(nblk):
        # number of points in each dimension must match
        if len(np.where(ds1.nx[i] != ds2.nx[i])[0]) > 0: return False
        x1 = ds1.x[i] ; x2 = ds2.x[i] ; e1 = ds1.g_eps[i] ; e2 = ds1.g_eps[i]
        for j in range(sdim):
            # for each dimension, check grids for sameness (within epsilon)
            eps = min(e1[j],e2[j])
            if np.fabs(np.subtract(x1[j],x2[j])).max() > eps: return False

    return True


def _integrate_2d(sl,xi,xt,itype,reverse,start):
    ##print "In integrate_2d",xi,xt,reverse,start
    # build new data arrays
    ndata = []
    ##print id(sl), id(sl.data), id(ndata)
    iz = 0 
    if reverse: iz = -1
    for b in range(sl.nblk):
        od = sl.data[b]
        sh = od.shape
        zloc = [ slice(sh[i]) for i in range(od.ndim) ]
        zloc[xi] = iz
        nd = np.empty(sh,order='F',dtype=pff.PFFnp_float)
        if not start: nd[zloc] = 0.0
        else:
            x = sl.x[b][xt]
            svals = np.empty((sh[xt],),dtype=pff.PFFnp_float)
            for i in range(sh[xt]):
                svals[i] = _1d.getYVal(start,x[i])
            nsh = list(sh)
            del nsh[xi]
            nd[zloc] = svals.reshape(nsh,order='F')
        ndata.append(nd)
        ##print nd.shape, sh, zloc
        ##print nd
    con_ord = sl.connect_order(xi+1, not reverse)
    ##print con_ord
    for con in con_ord:
        b = con[0]
        od = sl.data[b]
        nd = ndata[b]
        sh = od.shape
        sl0 = [ slice(sh[i]) for i in range(od.ndim) ]  ;  sl1 = sl0[:]
        sl0[xi] = slice(0,-1)
        sl1[xi] = slice(1,None)
        if reverse: wslice = sl0
        else: wslice = sl1
        ##print sl0,sl1,wslice
        x = sl.x[b][xi]
        mlt = np.multiply(0.5,np.diff(x))
        if itype > 1:
            xf = 0.5*(x[:-1] + x[1:])
            if itype == 3: xf = np.square(xf)
            mlt *= xf
        msh = [ 1 for i in range(od.ndim) ]
        msh[xi] = sh[xi] - 1
        mlt = mlt.reshape(msh,order='F')
        nd[wslice] = np.multiply(mlt,np.add(od[sl0],od[sl1]))
        cnd = nd
        if reverse:
            wslice[xi] = slice(None,None,-1)
            cnd = nd[wslice]
        np.cumsum(cnd,axis=xi,out=cnd) 
        clist = con[1]
        for c in clist:
            if c[2] == iz:
                sl0[xi] = iz
                sl1[xi] = c[1]
                idir = xi
                for tinfo in c[3:]:
                    idir = (idir+1) % 3  ; delt = tinfo[2]
                    sl0[idir] = slice(tinfo[1],tinfo[1] + delt)
                    sl1[idir] = slice(tinfo[0],tinfo[0] + delt)
                ##print sl0,sl1
                ndata[c[0]][sl0] = nd[sl1]
    sl.data = ndata

def _gen_lineout(sl,con_ord,xldir, xidir, xival, axes=None,dir='x',polar=False):
    xout = None  ;  yout = None
    lastblk = sl.nblk
    for con in con_ord:
        b = con[0]
        lo = sl.get_slice(xival,coord=xidir+1,block=b+1,quiet=True)
        if lo is not None:
            ##print lo.nblk,len(lo.x),len(lo.x[0]),b,x1
            x = lo.x[0][xldir]
            dd = np.squeeze(lo.data[0])
            if xout is None:
                xout = x
                yout = dd
            else:
                connected = False  ;  clist = con[1]
                for c in clist:
                    if c[0] == lastblk:
                        connected = True  ;  break
                ##print "connected:",connected
                if connected:
                    xout[-1] += x[0]   ;  xout[-1] *= 0.5
                    yout[-1] += dd[0]  ;  yout[-1] *= 0.5
                    xout = np.append(xout,x[1:],axis=0)
                    yout = np.append(yout,dd[1:],axis=0)
                else:
                    tmp = np.zeros((2),dtype=pff.PFFnp_float)
                    yap = np.append(tmp,dd,axis=0)
                    tmp[0] = 2.0*xout[-1] - xout[-2]
                    tmp[1] = 2.0*x[0] - x[1]
                    xap = np.append(tmp,x,axis=0)
                    xout = np.append(xout,xap,axis=0)
                    yout = np.append(yout,yap,axis=0)
            lastblk = b
            if axes:
                if (dir == 'x' and not polar) or (dir != 'x' and polar):
                    if polar:
                        th,junk = ThetaFill(lo.g_range[0,xldir,:],0.03*np.pi)
                        rv = xival + np.zeros((len(th),))
                    else:
                        rv,th = [xival,xival], lo.g_range[0,xldir,:]
                    l = matplotlib.lines.Line2D(th,rv,c='w')
                else:
                    l = matplotlib.lines.Line2D([xival,xival],
                                                lo.g_range[0,xldir,:],c='w')
                axes.add_line(l)
    return (xout,yout)


def _what_type(ds):
    if isinstance(ds,pff.blkgrid_dataset):
        if ds.adim == 0 and ds.sdim/2 == 1: return 'G'
        else: return 'F'
    elif isinstance(ds,pff.VTX_dataset):
        return 'V'
    elif isinstance(ds,quadlist3d):
        return 'Q'
    else:
        return None


def _fastgrid(mode,ax,xi,yi,**lineargs):
    ##print 'fastgrid',mode,xi.shape,xi[0],xi[-1],yi
    nl = len(xi)
    npts = 2*nl
    xo = np.empty(npts,dtype=pff.PFFnp_float)
    yo = np.empty(npts,dtype=pff.PFFnp_float)
    ytmpl = yi[[0,-1,-1,0]]
    nrep = npts/4
    ntile = 4*nrep
    yo[0:ntile] = np.tile(ytmpl,nrep)
    if npts > ntile:  yo[-2:npts] = ytmpl[0:2]
    j = 0
    for i in range(0,npts,2):
        xo[i:i+2] = xi[j]  ;  j += 1
    if mode == 0:  l = matplotlib.lines.Line2D(xo,yo,**lineargs)
    else:  l = matplotlib.lines.Line2D(yo,xo,**lineargs)

    ax.add_line(l)
    ##print 'end fastgrid',len(ax.lines)

def ThetaFill(t,dtmax,f=None):
    c0 = len(t)
    if f is None:
        fout = None  ;  nr = 0
    else:
        if f.ndim != 2:
            print "ThetaFill: Supplied F must be 2-dimensional"
            return (None,None)
        nr, c0t  = f.shape
        if c0t != c0:
            print "ThetaFill: Supplied F dimensions are not consistent with T"
            return (None,None)
    c0 = len(t)
    dt = np.diff(t)
    xn = dt/dtmax
    n = np.asarray(xn,dtype='i4')
    inds = np.where(xn-n > .01)
    n[inds] += 1
    locs = np.zeros((c0,),dtype='i4')
    locs[1:] = np.cumsum(n)
    cn = locs[-1] + 1
    tout = np.empty((cn,),dtype=t.dtype)
    if f is not None: fout = np.empty((nr,cn),dtype=f.dtype)
    off, end = 0, False
    for i in range(c0-1):
        if i == c0-2: off,end = 1,True
        s = slice(locs[i],locs[i+1]+off)
        tout[s] = np.linspace(t[i],t[i+1],n[i]+off,endpoint=end)
        for j in range(nr):
            fout[j,s] = np.linspace(f[j,i],f[j,i+1],n[i]+off,endpoint=end)

    #print 'ThetaFill',tout.shape,
    #if fout: print fout.shape
    
    return (tout,fout)

def RT2XYLimits(rr,tr):
    rmx = rr[1]  ;  rmn = rr[0]
    tmx = tr[1]  ;  tmn = tr[0]
    xr = [-rmx,rmx]  ;  yr = [-rmx,rmx]
    cost = np.cos(tr)
    sint = np.sin(tr)
    up = 0.5*np.pi
    if tr[0] < -0.001:  # assume on (-pi, pi) sheet
        dwn = -up
        ##print up,dwn
        cosmn = float(cost.min())
        if cosmn < 0.0: xr[0] = rmx*cosmn
        else: xr[0] = rmn*cosmn
        if tmx < 0.0 or tmn > 0.0:
            cosmx = float(cost.max())
            if cosmx > 0.0: xr[1] = rmx*cosmx
            else: xr[1] = rmn*cosmx
    else:               # assume on (0, 2*pi) sheet
        dwn = 3.0*up
        ##print up,dwn
        cosmx = float(cost.max())
        if cosmx > 0.0: xr[1] = rmx*cosmx
        else: xr[1] = rmn*cosmx
        if tmx < np.pi or tmn > np.pi:
            cosmn = float(cost.min())
            if cosmn < 0.0: xr[0] = rmx*cosmn
            else: xr[0] = rmn*cosmn

    if tmx < up or tmn > up:
        sinmx = float(sint.max())
        if sinmx > 0.0: yr[1] = rmx*sinmx
        else: yr[1] = rmn*sinmx
    if tmx < dwn or tmn > dwn:
        sinmn = float(sint.min())
        if sinmn < 0.0: yr[0] = rmx*sinmn
        else: yr[0] = rmn*sinmn
            
    return np.array(xr), np.array(yr)

def slice_data_range(sl,x1,x2,xg,yg,comps):
    nblk = sl.nblk
    sdim = sl.sdim
    sh = sl.data[0].shape
    shextra = 0
    if len(sh) > sdim:
        shextra = sh[sdim]
        t = type(comps)
        if t is types.IntType: comps = [comps]
        elif t is not types.ListType and t is not types.TupleType:
            print 'slice_data_range: Bad COMPS',b,x1,x2,sh,i
            return

    for b in range(nblk):
        data = sl.data[b]
        nx = sl.nx[b]
        if xg is None:
            x1slice = slice(nx[x1])
        else:
            x = sl.x[b][x1]
            x1slice = np.where(np.logical_and(xg[0] <= x, x <= xg[1]))[0]
            if len(x1slice) == 0:
                print "X-skip block",b
                continue
        if yg is None:
            x2slice = slice(nx[x2])
        else:
            x = sl.x[b][x2]
            x2slice = np.where(np.logical_and(yg[0] <= x, x <= yg[1]))[0]
            if len(x2slice) == 0:
                print "Y-skip block",b
                continue

        print b,x1slice,x2slice

        sdim = sl.sdim
        dim = ()
        sh = data.shape
        for i in range(sdim):
            if ( i == x1 ): dim += ( x1slice, )
            elif ( i == x2 ): dim += ( x2slice, )
            elif ( sh[i] == 1 ): dim += ( 0, )
            else:
                print 'slice_data_range: Bad Slice',b,x1,x2,sh,i
                return
        if len(sh) > dim: dim += comps

        if shextra and len(comps) > 1:
            atmp = np.sqrt(np.sum(np.square(data[dim]),axis=-1))
        else:
            atmp = data[dim]

        if b == 0:  dmin,dmax = ( atmp.min(), atmp.max() )
        else:
            dmin,dmax = ( min(dmin,atmp.min()), max(dmax,atmp.max()) )

    return (dmin,dmax)

        
__all__.append('quadlist3d')

class quadlist3d:
    def __init__(self, vrtx_ds = None, title=None, typename=None, copy=True):

        if vrtx_ds is None:
            # return empty container
            self.apptype = pff.PFF_DEFAULT
            self.file = ''
            self.g_eps = None
            self.g_range = None
            self.glabels = None
            self.locx = []
            self.nq = 0
            self.nqs = 0
            self.pds = None
            self.rawname = 'QLIST'
            self.spare = None
            self.title = title
            self.typename = typename
        else:
            v = vrtx_ds
            if not isinstance(v,pff.VTX_dataset) or (v.nv % 4) != 0 or \
               v.sdim != 3 or v.adim != 0:
              print "QUADLIST3D: " + \
                    "Supplied Vertex Dataset does not define a legal conductor"
              return None
            self.rawname = 'QLIST'
            nv = v.nv
            nq = nv/4
            x = v.x
            dt = x.dtype
            pds = np.empty((2,3,nq),dtype=dt,order='F')
            wrk = np.empty((2,3),dtype=dt,order='F')
            segs = np.empty((3,4),dtype=dt,order='F')
            iq = 0  ;  iqs = 0
            nrmdir = 0
            x1dir = 1
            x2dir = 2
            cnts = np.zeros((3),dtype=pff.PFFnp_int)
            #print nrmdir,x1dir,x2dir,iq
            need_sort = False
            se = []
            for i in range(0,nv,4):
                q = x[:,i:i+4]
                wrk[0,:] = q.min(axis=1)
                wrk[1,:] = q.max(axis=1)
                delta = np.diff(wrk,axis=0).squeeze()
                dmin = np.where(delta == delta.min())[0][0]
                if delta[dmin] > v.g_eps[dmin]:
                    ##print "non-coordinate-conforming quads not supported yet"
                    segs[:,:-1] = np.diff(q,axis=1)
                    segs[:,-1] = q[:,0] - q[:,3]
                    iex = 3
                    prd = 0
                    ##print segs
                    for i in [0,1]:
                        w = np.where(np.fabs(segs[:,i]) > v.g_eps)[0]
                        ##print w,v.g_eps
                        lw = len(w)
                        if lw == 1:
                            iex = w[0]
                            sgn = 1 - 2*i
                        elif lw == 2: 
                            prd = segs[w,i].prod()
                    ##print iex
                    if prd and iex < 3:
                        if np.fabs(segs[:,0:2] + segs[:,2:4]).max() < \
                           v.g_eps[iex]:
                            ##print "this is a slant quad extruded in", iex,sgn,prd
                            iqs += 1
                            se.append( (sgn*(iex+1),iqs) )
                            w = [ (iex+i) % 3 for i in range(3) ]
                            pds[...,-iqs] = wrk[:,w]
                            if prd < 0: pds[:,2,-iqs] = np.flipud(pds[:,2,-iqs])
                            ##print pds[...,iq]
                else:
                    if dmin < nrmdir: need_sort = True
                    if nrmdir != dmin:
                        nrmdir = dmin
                        x1dir = (dmin + 1) % 3
                        x2dir = (dmin + 2) % 3
                        #print nrmdir,x1dir,x2dir,iq
                    pds[:,0:2,iq] = wrk[:,[x1dir,x2dir]]
                    pds[0,2,iq] = wrk[0,nrmdir]
                    crx = np.cross(q[:,1] - q[:,0],q[:,3] - q[:,0])
                    pds[1,2,iq] = np.sign(crx[nrmdir])*(nrmdir+1)
                    ##print iq,dmin

                    cnts[dmin] += 1
                    iq += 1
            ##print "se:", iq,iqs,se
            assert(iq + iqs == nq)
            nq = iq  ;  nqs = iqs
            if nqs > 0:
                locs = [0] ; le = 0
                slant_bnds = np.empty((2,3,nqs),dtype=dt,order='F')
                slant_extr = [ ]
                for i in range(3):
                    ip = i + 1
                    ##w = [ -s[1] for s in se if abs(s[0]) == ip ]
                    w1 = [ i for i in range(nqs) if abs(se[i][0]) == ip ]
                    w = [ -se[i][1] for i in w1 ]
                    lw = len(w)  ;  lb = le  ;  le += lw
                    ##print w, w1, lb, le
                    locs.append(le)
                    slant_extr += [ se[i][0] for i in w1 ]
                    if lw:  slant_bnds[...,lb:le] = pds[...,w]
            locx = [ 0 ]
            for i in range(3):  locx.append(locx[i]+cnts[i])
            ### TESTSORT pds[1,2,52:54] = [-2.0,-1.0] ; need_sort = 1
            ### TESTSORT locx[1] += 1 ; locx[2] += 2
            if need_sort:
                npds = np.empty((2,3,nq),dtype=dt,order='F')
                st = [ (abs(pds[1,2,i]),i) for i in range(nq) ]
                tcnt = 0
                for i in range(3):
                    ip = i + 1
                    w = [ s[1] for s in st if s[0] == ip ]
                    tcnt += len(w)
                    npds[...,locx[i]:locx[i+1]] = pds[...,w]
                assert(tcnt == nq)
                pds = npds
            elif nqs > 0:
                pds.resize((2,3,nq))
                    
            self.nq = nq
            self.nqs = nqs
            self.pds = pds
            self.locx = locx
            if nqs > 0:
                self.locs = locs
                self.slant_bnds = slant_bnds
                self.slant_extr = slant_extr
            self.apptype = v.apptype
            self.title = v.title
            self.typename = v.typename
            self.file = v.file
            if copy:
                self.spare = cpy.deepcopy(v.spare)
                self.glabels = cpy.deepcopy(v.glabels)
                self.g_range = cpy.deepcopy(v.g_range)
                self.g_eps = cpy.deepcopy(v.g_eps)
            else:
                self.spare = v.spare
                self.glabels = v.glabels
                self.g_range = v.g_range
                self.g_eps = v.g_eps

    def scale(self,factor):
        okay = True
        isnum, t = utils.is_number(factor)
        if isnum:
            mlt = [ factor for i in range(3) ]
            inds = [ i for i in range(3) ]
        elif t is types.ListType or t is types.TupleType:
            if len(factor) != 3:
                print "FACTOR list must have 3 elements"
                okay = False
            else:
                mlt = list(factor)
                inds = [ i for i in range(3) if mlt[i] is not None ]
                for i in inds:
                    isnum, t = utils.is_number(mlt[i])
                    if not isnum:
                        print "FACTOR[" + str(i) + "] is not a number"
                        okay = False
        else:
            print "FACTOR must be an scalar number or a 3-element list"
            okay = False

        if not okay:
            return 1

        l1 = self.locx[0]
        pds = self.pds
        for n in range(3):
            l0 = l1  ;  l1 = self.locx[n+1]
            if l0 >= l1: continue
            ix1 = (n + 1) % 3  ;  ix2 = (n + 2) % 3
            if n in inds:   pds[0,2,l0:l1] *= mlt[n]
            if ix1 in inds: pds[:,0,l0:l1] *= mlt[ix1]
            if ix2 in inds: pds[:,1,l0:l1] *= mlt[ix2]

        if self.nqs > 0:
            l1 = self.locs[0]
            bnds = self.slant_bnds
            for n in range(3):
                l0 = l1  ;  l1 = self.locs[n+1]
                ##print n,l0,l1
                if l0 >= l1: continue
                ix1 = (n + 1) % 3  ;  ix2 = (n + 2) % 3
                ##print n,ix1,ix2
                if n in inds:   bnds[:,0,l0:l1] *= mlt[n]
                if ix1 in inds: bnds[:,1,l0:l1] *= mlt[ix1]
                if ix2 in inds: bnds[:,2,l0:l1] *= mlt[ix2]

        gr = self.g_range
        ge = self.g_eps
        for n in range(3):
            if n in inds:
                m = mlt[n]
                gr[n,:] *= m
                ge[n] *= m
                

        return 0

    def shift(self,factor):
        okay = True
        isnum, t = utils.is_number(factor)
        if isnum:
            plus = [ factor for i in range(3) ]
            inds = [ i for i in range(3) ]
        elif t is types.ListType or t is types.TupleType:
            if len(factor) != 3:
                print "FACTOR list must have 3 elements"
                okay = False
            else:
                plus = list(factor)
                inds = [ i for i in range(3) if plus[i] is not None ]
                for i in inds:
                    isnum, t = utils.is_number(plus[i])
                    if not isnum:
                        print "FACTOR[" + str(i) + "] is not a number"
                        okay = False
        else:
            print "FACTOR must be an scalar number or a 3-element list"
            okay = False

        if not okay:
            return 1

        l1 = self.locx[0]
        pds = self.pds
        for n in range(3):
            l0 = l1  ;  l1 = self.locx[n+1]
            if l0 >= l1: continue
            ix1 = (n + 1) % 3  ;  ix2 = (n + 2) % 3
            if n in inds:   pds[0,2,l0:l1] += plus[n]
            if ix1 in inds: pds[:,0,l0:l1] += plus[ix1]
            if ix2 in inds: pds[:,1,l0:l1] += plus[ix2]

        if self.nqs > 0:
            l1 = self.locs[0]
            bnds = self.slant_bnds
            for n in range(3):
                l0 = l1  ;  l1 = self.locs[n+1]
                if l0 >= l1: continue
                ix1 = (n + 1) % 3  ;  ix2 = (n + 2) % 3
                if n in inds:   bnds[:,0,l0:l1] += plus[n]
                if ix1 in inds: bnds[:,1,l0:l1] += plus[ix1]
                if ix2 in inds: bnds[:,2,l0:l1] += plus[ix2]

        gr = self.g_range
        ge = self.g_eps
        for n in range(3):
            if n in inds: gr[n,:] += plus[n]
                

        return 0

    def append(self,quadlist,title=None):
        t = type(quadlist)
        okay = True
        if t is types.ListType or t is types.TupleType:
            qlist = list(quadlist)
            for q in qlist:
                if not isinstance(q,quadlist3d):  okay = False
        elif isinstance(quadlist,quadlist3d):
            qlist = [ quadlist ]
        else:
            okay = False

        if not okay:
            print "Supplied argument/s not all QUADLIST3D objects"
            return 1

        alist = [ self ] + qlist
        nlist = len(alist)
        if title is None:
            title = self.title
            for q in qlist: title += ', ' + q.title
        nq = self.nq  ;  nqs = self.nqs
        for q in qlist:
            nq += q.nq  ; nqs += q.nqs
        cnts = np.empty((nlist,3),dtype=pff.PFFnp_int)
        locn = np.zeros((4,),dtype=pff.PFFnp_int)
        pos = np.empty((3,),dtype=pff.PFFnp_int)
        gr = np.empty((nlist,3,2),dtype=pff.PFFnp_float)
        for i in range(nlist):
            q = alist[i]
            cnts[i,:] = np.diff(q.locx)
            gr[i,...] = q.g_range
        locn[1:] = cnts.sum(axis=0).cumsum()
        pos[:] = locn[:-1]
        ngr = self.g_range
        ngr[:,0] = gr[:,:,0].min(axis=0)
        ngr[:,1] = gr[:,:,1].max(axis=0)
        self.g_eps[:] = pff.GridEpsilonFactor*np.diff(ngr,axis=1).squeeze()
 
        ##print cnts
        ##print locn,pos,nq,nqs
        npds = np.empty((2,3,nq),dtype=pff.PFFnp_float,order='F')
        for i in range(nlist):
            q = alist[i]
            pds = q.pds
            locx = q.locx
            for j in range(3):
                l0 = pos[j]  ;  l1 = l0 + cnts[i,j]
                npds[...,l0:l1] = pds[...,locx[j]:locx[j+1]]
                pos[j] = l1

        self.title = title
        ##print npds.shape,pos
        self.nq = nq
        self.locx = list(locn)
        self.pds = npds

        if nqs > 0:
            for i in range(nlist):
                q = alist[i]
                if q.nqs > 0:  cnts[i,:] = np.diff(q.locs)
                else: cnts[i,:] = 0
            locn[1:] = cnts.sum(axis=0).cumsum()
            pos[:] = locn[:-1]
            ##print cnts
            ##print locn,pos,nq,nqs
            nbnds = np.empty((2,3,nqs),dtype=pff.PFFnp_float,order='F')
            extr = range(nqs)
            for i in range(nlist):
                q = alist[i]
                if q.nqs > 0:
                    bnds = q.slant_bnds
                    locs = q.locs
                    for j in range(3):
                        l0 = pos[j]  ;  l1 = l0 + cnts[i,j]
                        sl = slice(locs[j],locs[j+1])
                        nbnds[...,l0:l1] = bnds[...,sl]
                        extr[l0:l1] = q.slant_extr[sl]
                        pos[j] = l1
            
            ##print npds.shape,pos
            self.nqs = nqs
            self.locs = list(locn)
            self.slant_bnds = nbnds
            self.slant_extr = extr

        return 0

    def VTX_dataset(self):
        new = {'adim':0 ,'data':None ,'dlabels':None ,'rawname':'VTX' , \
               'rawtype':5 ,'rfu':None ,'sdim':3 }
        same = ['apptype', 'file', 'title', 'typename' ]
        same += ['g_eps', 'g_range', 'glabels', 'spare' ]

        for k in same: exec "new[\'" + k + "\']= self." + k

        nq = self.nq
        nqs = self.nqs
        nv = 4*(nq + nqs)
        pds = self.pds
        x = np.empty((3,nv),dtype=pds.dtype,order='F')

        iv = 0
        q = np.empty((3,4), dtype=pds.dtype, order='F')
        if nq > 0:
            locx = self.locx
            for l in range(3):
                nrmid = l
                ix1 = (nrmid + 1) % 3
                ix2 = (nrmid + 2) % 3
                trv = [ix1,ix2]
                for iq in range(locx[l],locx[l+1]):
                    pds0 = pds[...,iq]
                    q[nrmid,0:4] = pds0[0,2]
                    ##print q[trv,0].shape, pds0[:,0].shape
                    q[trv,0] = pds0[1,0:2]
                    q[trv,2] = pds0[0,0:2]
                    if pds0[1,2] > 0:
                        iv1 = 1  ;  iv3 = 3
                    else:
                        iv1 = 3  ;  iv3 = 1
                    q[:,1] = q[:,2]
                    q[:,3] = q[:,2]
                    q[ix2,iv1] = pds0[1,1]
                    q[ix1,iv3] = pds0[1,0]

                    niv = iv + 4
                    x[:,iv:niv] = q
                    iv = niv
        if nqs > 0:
            bnds = self.slant_bnds
            extr = self.slant_extr
            locs = self.locs
            for l in range(3):
                iex = l
                ix1 = (iex + 1) % 3
                ix2 = (iex + 2) % 3
                fmap = [iex,ix1,ix2]
                for iqs in range(locs[l],locs[l+1]):
                    bnds0 = bnds[...,iqs]
                    ##print q[trv,0].shape, bnds0[:,0].shape
                    if extr[iqs] > 0:
                        iv1 = 1  ;  iv3 = 3
                    else:
                        iv1 = 3  ;  iv3 = 1
                    q[fmap,0] = bnds0[1,:]
                    q[fmap,2] = bnds0[0,:]
                    q[fmap,iv1] = bnds0[1,:]
                    q[iex,iv1] = bnds0[0,0]
                    q[fmap,iv3] = bnds0[0,:]  ;  q[iex,iv3] = bnds0[1,0]
                    niv = iv + 4
                    x[:,iv:niv] = q
                    iv = niv

        new['nv'] = nv
        new['x'] = x
        ##keys = new.keys()
        ##keys.sort()
        ##print keys
        ##for k in keys: print repr(k) + ":", repr(new[k])
        return pff.VTX_dataset(new=new)

    def write(self,id=0,precision=None):
        self.VTX_dataset().write(id=id,precision=precision)


try:
    _struclist
except NameError:
    _struclist = {}

##print "2D loaded"
