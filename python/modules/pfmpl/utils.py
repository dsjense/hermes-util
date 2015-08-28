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
import math
import types
import re
import numpy as np

import pff

def _testpa(nopt=0,extra=True, *args, **kwargs):
    print nopt,extra,args,kwargs

    ##, '':, '':, '':, '':, '':, '':, 
    kwdefs = { 'a':None, 'b':None, 'c':None, 'd':None, 'e':True, 'f':999, \
               'x1':'x1', 'x2':'x2'}
    argnames=['p0', 'p1', 'p2', 'p3', 'p4', 'p5']
    optvals = None
    if nopt > 0:
        itmp = min(nopt,len(argnames))
        ioff = 100 + len(argnames) - itmp
        optvals = [ i + ioff for i in range(itmp) ]

    res = process_args(args, kwargs, 'TEST', argnames, kwdefs, optvals, extra)

    if res == 0: print "Empty call"
    elif res == 1: print "ERROR:", res
    else:
        if extra:
            print 'EXTRA:',res[1]
            res = res[0]
        keys = res.keys()
        keys.sort()
        for k in keys: print k, res[k]
    

def _testwpa(nopt=0,extra=True, *args, **kwargs):
    print nopt,extra,args,kwargs

    ##, '':, '':, '':, '':, '':, '':, 
    kwdefs = { 'a':None, 'b':None, 'c':None, 'd':None, 'e':True, 'f':999, \
               'x1':'x1', 'x2':'x2'}
    argnames=['p0', 'p1', 'p2', 'p3', 'p4', 'p5']
    optvals = None
    if nopt > 0:
        itmp = min(nopt,len(argnames))
        ioff = 100 + len(argnames) - itmp
        optvals = [ i + ioff for i in range(itmp) ]
    wkeys = [ 'x#label', 'v#label', 'ax#dim', 'mx#dim', 's#v' ]

    res = process_wargs(args, kwargs, 'TEST', argnames, kwdefs, wkeys, \
                        optvals, extra)

    if res == 0: print "Empty call"
    elif res == 1: print "ERROR:", res
    else:
        if extra:
            print 'EXTRA:',res[2]
        print 'WRES:'
        for r in res[1]: print r
        print 'RES:'
        res = res[0]
        keys = res.keys()
        keys.sort()
        for k in keys: print k, res[k]
    

def process_wargs(args, kwargs, modname, argnames, kwdefs, wkeys, \
                  optargvals=None, extra=True):

    blnk = re.sub("."," ",modname)
    nargs = len(args)
    nkws = len(kwargs)
    if nargs + nkws == 0: return 0
    nargsmx = len(argnames)
    nopt = 0
    if optargvals is not None:  nopt = len(optargvals)
    nreqrd = nargsmx - nopt

    nwkeys = len(wkeys)
    w_vals = [ {} for i in range(nwkeys) ]
    
    kwvalid = kwdefs.keys()
    kwfind = kwvalid + wkeys
    argvals = {}
    for k in range(nopt):
        name = argnames[k+nreqrd]
        argvals[name] = optargvals[k]

    okay = True
    if nargs < nreqrd:
        verb = sngprl[min(1,nreqrd-1)]
        print modname + ":", nreqrd, "positional parameter" + verb + " required"
        okay = False
    if nargs > nargsmx:
        verb = sngprl[min(1,nargsmx-1)]
        print modname + ": only",nargsmx,"positional parameter"+verb+" allowed"
        okay = False
    else:
        for k in range(nargs):
            name = argnames[k]
            argvals[name] = args[k]
        
    for k in kwvalid: argvals[k] = kwdefs[k]
    keys = kwargs.keys()

    extras = {}
    for k in keys:
        mat = findbestnummatch(kwfind,k)
        lmat = len(mat)
        if lmat == 1:
            mat = mat[0]
            ln = mat.find('#')
            if ln < 0: argvals[mat] = kwargs[k]
            else:
                num = int(k[ln])
                loc = wkeys.index(mat)
                w_vals[loc][num] = kwargs[k]
        elif lmat == 0:
            if extra:  extras[k] = kwargs[k]
            else:
                okay = False
                print modname + ": Could not match supplied keyword \"" + k + \
                      "\""
                print blnk + "  Valid keywords:",str(kwvalid)
        else:
            okay = False
            print modname + ": Could not uniquely match supplied keyword \"" + \
                  k + "\""
            print blnk + "  It potentially matches valid keywords",str(mat)

    if not okay: return 1

    if extra:  return (argvals, w_vals, extras)
    else:      return (argvals, w_vals)
    

def process_args(args, kwargs, modname, argnames, kwdefs, optargvals=None, \
                 extra=True):

    blnk = re.sub("."," ",modname)
    nargs = len(args)
    nkws = len(kwargs)
    if nargs + nkws == 0: return 0
    nargsmx = len(argnames)
    nopt = 0
    if optargvals is not None:  nopt = len(optargvals)
    nreqrd = nargsmx - nopt
    
    kwvalid = kwdefs.keys()
    argvals = {}
    for k in range(nopt):
        name = argnames[k+nreqrd]
        argvals[name] = optargvals[k]

    okay = True
    sngprl = [ ' is', 's are' ]
    if nargs < nreqrd:
        verb = sngprl[min(1,nreqrd-1)]
        print modname + ":", nreqrd, "positional parameter" + verb + " required"
        okay = False
    if nargs > nargsmx:
        verb = sngprl[min(1,nargsmx-1)]
        print modname + ": only",nargsmx,"positional parameter"+verb+" allowed"
        okay = False
    else:
        for k in range(nargs):
            name = argnames[k]
            argvals[name] = args[k]
        
    for k in kwvalid: argvals[k] = kwdefs[k]
    keys = kwargs.keys()

    extras = {}
    for k in keys:
        mat = findbestmatch(kwvalid,k)
        lmat = len(mat)
        if lmat == 1:   argvals[mat[0]] = kwargs[k]
        elif lmat == 0:
            if extra:  extras[k] = kwargs[k]
            else:
                okay = False
                print modname + ": Could not match supplied keyword \"" + k + \
                      "\""
                print blnk + "  Valid keywords:",str(kwvalid)
        else:
            okay = False
            print modname + ": Could not uniquely match supplied keyword \"" + \
                  k + "\""
            print blnk + "  It potentially matches valid keywords",str(mat)

    if not okay: return 1

    if extra:  return (argvals, extras)
    else:      return argvals


def findbestmatch(keys,cmd):
    mat = []; found = None
    for k in keys:
        if k.find(cmd) == 0:
            mat.append(k)
            if k == cmd: return [k]
    return mat


def parsewsl(wslist, first, last=None, count=None, name='WDF', valid=False, \
             stringOK=False,):
    existing = valid or stringOK

    if existing:
        keys = wslist.keys()
        keys.sort()
        if len(keys) == 0:
            print 'All', name, 'arrays are empty'
            return None

    if last is not None and count is not None:
        print 'LAST and COUNT cannot both be specified'
        return None
    if last is not None and \
       (type(last) is not types.IntType or type(first) is not types.IntType):
        print 'If LAST is specified, FIRST and LAST must both be integers'
        return None
    if count is not None:
        if type(count) is not types.IntType or type(first) is not types.IntType:
            print 'If COUNT is specified, FIRST and COUNT must both be integers'
            return None
        first = range(first,first+count)

    if type(first) == types.IntType:
        if last is None:
            if existing:
                outlist = [s for s in [ first ] if wslist.has_key(s) ]
            else:
                outlist = [ first ]
            ##detailed = True
        else:
            if existing:
                top = last
                if top <= 0: top = max(keys)
                outlist = [ s for s in keys if (s>=first and s<=top) ]
            else:
                outlist = range(first,last+1) 

    elif type(first) == types.ListType:
        if existing:
            outlist = [ s for s in first if wslist.has_key(s) ]
        else:
            outlist = first
        ##detailed = True
    elif stringOK and type(first) == types.StringType:
        slist = [ wslist[s].title for s in keys ]
        #print keys
        #print slist
        indicies = pff.scanlist(slist,first)
        #print indicies
        outlist = [ keys[s] for s in indicies ]
        #print outlist
    else:
        print 'Cannot process FIRST argument'
        return None

    return outlist


def get_empty_wsl(wslist,first=1,last=None,count=1):
    s0 = first
    s = s0
    cnt = 0
    while cnt < count:
        if last is not None and s > last:  return None
        while wslist.has_key(s):
            cnt = 0
            s = s + 1
            if last is not None and s > last:  return None
        if cnt == 0: s0 = s
        cnt += 1
        s += 1
    return s0


def eval_numpy_function(x,op,quiet=False):
    try:
        exec 'from numpy import * ; y = ' + op
        return y
    except Exception, e:
        if not quiet: 
            s = str(type(e))
            i1 = s.find('.') + 1
            i2 = s.find('.',i1)
            print s[i1:i2] + ":", e
        return None

def findbestnummatch(keys,cmd):
    mat = []; found = None
    for k in keys:
        tcmd = find_nums(k,cmd)
        if k.find(tcmd) == 0:
            mat.append(k)
            if k == tcmd: return [k]
    return mat

def find_nums(key,cmd):
    lk = len(key)
    w=[ i for i in range(lk) if key[i] == '#']
    if len(w):
        t = list(cmd)
        lc = len(cmd)
        for i in w:
            if i >= lc: break
            if t[i].isdigit(): t[i] = '#'
        return "".join(t)
    else:
        return cmd

def is_number(val):
    t = type(val)
    if t is types.IntType or t is types.LongType or t is types.FloatType:
        return True, t
    else:
        return False, t


def nice_number(value, round_=False):
    '''nice_number(value, round_=False) -> float'''
    exponent = math.floor(math.log(value, 10))
    fraction = value / 10 ** exponent

    if round_:
        if fraction < 1.5: nice_fraction = 1.
        elif fraction < 3.: nice_fraction = 2.
        elif fraction < 7.: nice_fraction = 5.
        else: niceFraction = 10.
    else:
        if fraction <= 1: nice_fraction = 1.
        elif fraction <= 2: nice_fraction = 2.
        elif fraction <= 5: nice_fraction = 5.
        else: nice_fraction = 10.

    return nice_fraction * 10 ** exponent

def nice_bounds(axis_start, axis_end, num_ticks=10):
    '''
    nice_bounds(axis_start, axis_end, num_ticks=10) -> tuple
    @return: tuple as (nice_axis_start, nice_axis_end, nice_tick_width)
    '''
    axis_width = axis_end - axis_start
    if axis_width == 0:
        nice_tick = 0
    else:
        nice_range = nice_number(axis_width)
        nice_tick = nice_number(nice_range / (num_ticks -1), round_=True)
        axis_start = math.floor(axis_start / nice_tick) * nice_tick
        axis_end = math.ceil(axis_end / nice_tick) * nice_tick

    return axis_start, axis_end, nice_tick

def XfFromXh(xh,xf0=None):
    # If xf0 supplied, assumes xh = 0.5*(xf[:-1] + yf[1:])
    hlen = xh.size
    if xf0 is not None:
        tdxh = 2.0*np.diff(xh)
        dxft = np.empty(hlen+1, dtype=np.single)
        dxft[0] = xf0
        dxft[1] = 2.0*(xh[0] - xf0)
        for i in range(hlen-1):
            dxft[i+2] = tdxh[i] - dxft[i+1]
        xf = np.cumsum(dxft)
        if (xh - xf[:-1]).min() <= 0 or (xf[1:] - xh).min() <= 0:
            print "XfFromXh: Warning -- inconsistent value of `xf0' supplied"
            print "          default value will be used"
            xf0 = None

    if xf0 is None:
        xf = np.empty(hlen+1, dtype=np.single)
        xf[1:-1] = 0.5*(xh[:-1] + xh[1:])
        xf[0] = 2.0*xh[0] - xf[1]
        xf[-1] = 2.0*xh[-1] - xf[-2]

    return xf
