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
import __builtin__
import exceptions
import math
import types
import sys
import copy as cpy
import os.path
import numpy as np
import pff_ext as pex

__doc__ = '''Module pff'''

class PFF_Error(exceptions.Exception):
    def __init__(self, value="PFF error"):
        self.value = value
    def __str__(self):
        return repr(self.value)

## \brief Opens a PFF file
#
#  \param[in] file  String containing name of file to be opened.
#  \param[in] mode  Mode in which file is to be opened. Valid values are:
#                     \li "r"  for read-only access (default)
#                     \li "w"  for write-only access
#                     \li "rw" for read-write access
#  \returns  \li integer ID index associated with file, or
#            \li \a None, if an error occurred
def open(file, mode="r"):
    try:
        return pex.open(file,mode)
    except pex.PFF_Error, e:
        print "Error:", e

## \brief Close a PFF file.
#
#  \param[in] id If positive, integer ID index of file to be closed. Otherwise,
#                all open files are closed.

#  \returns  \li Number of files closed, or
#            \li \a None on error
def close(id = 0):
    try:
        return pex.close(id)
    except pex.PFF_Error, e:
        print "Error:", e

## \brief Print list of open PFF files to terminal.
#
#  \param[in] range Tuple containing first and last file ID index to be listed.
#                   If second index is <= 0, highest index in use is used. If
#                   not supplied, all open files are listed.
#  \param[in] width Maximum width of listing, in characters

#  \returns  \li \a None if error parsing arguments, or
#            \li    0, indicates success, or
#            \li    otherwise, some other error occurred
def show(range=(1,-1),width=80):
    return pex.filelist(range,width)

## \brief Sets the current active PFF file.
#
#  \param[in] id Integer ID index of file to become the active file.
#
#  \returns  \li \a None
def set(id):
    try:
        return pex.setcurfile(id)
    except pex.PFF_Error, e:
        print "Error:", e

## \brief Print list of datasets in an open PFF file to terminal.
#
#  \param[in] id    Integer ID index of file whose datasets are to be listed.
#                   If \a id <= 0, current active file is assumed.
#  \param[in] range Tuple containing first and last dataset index to be listed.
#                   If second index is <= 0, highest index of last dataset is
#                   used. If not supplied, all datasets in file  are listed.
#  \param[in] match String used to match the titles of the datasets to be
#                   listed. Limited wildcarding is supported. The following
#                   characters have special meaning when encountered in the
#                   search string:
#                     \li * is matched by 0 to n characters
#                     \li ? is matched by exactly 1 character
#                     \li ^ as a first character anchors the match to the
#                         beginning of the comment substring
#                     \li $ as a final character anchors the match to the end
#                         of the comment substring
#                     \li \ escapes * and ? to be used literally anywhere in 
#                         the search string and escapes ^ (and $) at the
#                         beginning  only (and end only) of the search string
#                         to force ^ (or $) to be interpreted literally.
#  \param[in] width Maximum width of listing, in characters

#  \returns  \li \a None
def dir(id=0, range=(1,-1), match="",width=80):
    try:
        return pex.dslist(id, range, match, width)
    except pex.PFF_Error, e:
        print "Error:", e
        return None

def i2f(ival, keep=False, offset=False):
    '''Function to decode a floating point number from three 15-bit unsigned
integers.

If the number to be decoded is greater than the largest value that can
be stored in a 32-bit float (overflow), a power-of-10 integer offset
is also returned if OFFSET is true. In the case of underflow, a
negative power-of-10 offset will be returned if KEEP is true. If either
OFFSET or KEEP are true, this integer offset is always returned.

Usage:
  i2f( ival, keep=0, offset=0 )

Arguments:
  ival:   A list or array of at least 3 integer values that can
          be represented as 15-bit integers (in the range 0-32767,
          inclusive).
  keep:   If true, a negative power-of-ten integer offset value
          is returned for an underflowing decoded float. Note that
          KEEP being true implies that OFFSET is also true.
  offset: If true, a power-of-ten integer offset will additionally
          be returned. It will be non-zero only if the decoded float
          overflows, or if the decoded float underflows AND KEEP is
          true.

Return value: If successful, and KEEP and OFFSET are not true, the
              decoded float value is returned. If either KEEP or
              OFFSET are true, a tuple containing the decoded float
              value and an integer power-of-ten offset are returned.
              On error, None is returned.'''
    err = 0
    try:
        iv = np.asarray(ival,dtype=PFFnp_int)
        if len(iv) < 3: raise ValueError
    except ValueError,e:
        print 'Invalid type supplied for argument ival'
        return None
    k = 0 ; o = 0
    if keep: k = 1
    if offset: o = 1
    ia = ( 'i', iv.itemsize, iv.tostring())
    try:
        return pex.u_i2f(ia[:3],k,o)
    except pex.PFF_Error, e:
        print "Error:", e
        return None
    
def i2d(ival):
    '''Function to decode a double from five 15-bit unsigned integers.

Usage:
  i2d( ival )

Arguments:
  ival:   A list or array of at least 5 integer values that can
          be represented as 15-bit integers (in the range 0-32767,
          inclusive).

Return value: If successful, the decoded double value is returned. On
              error, None is returned.'''

    err = 0
    try:
        iv = np.asarray(ival,dtype=PFFnp_int)
        if len(iv) < 5: raise ValueError
    except ValueError,e:
        print 'Invalid type supplied for argument ival'
        return None
    ia = ( 'i', iv.itemsize, iv[:5].tostring())
    try:
        return pex.u_i2d(ia)
    except pex.PFF_Error, e:
        print "Error:", e
        return None
    
def i2l(ival):
    '''Function to decode a long int from three 15-bit unsigned integers.

Usage:
  i2l( ival )

Arguments:
  ival:   A list or array of at least 3 integer values that can
          be represented as 15-bit integers (in the range 0-32767,
          inclusive).

Return value: If successful, the decoded long (numpy.long) value is
              returned. On error, None is returned.'''

    err = 0
    try:
        iv = np.asarray(ival,dtype=PFFnp_int)
        if len(iv) < 3: raise ValueError
    except ValueError,e:
        print 'Invalid type supplied for argument ival'
        return None
    ia = ( 'i', iv.itemsize, iv[:3].tostring())
    try:
        return np.long(pex.u_i2l(ia))
    except pex.PFF_Error, e:
        print "Error:", e
        return None
    
def f2i(fval):
    '''Function to encode a float as three 15-bit unsigned integers.

Usage:
  f2i( fval )

Arguments:
  fval:   The float value to be encoded.

Return value: If successful, the encoded integer array is returned. On
              error, None is returned.'''

    err = 0
    try:
        f = np.float32(fval)
    except ValueError,e:
        print 'Invalid type supplied for argument fval'
        return None
    try:
        r = pex.u_f2i(f)
        return buf2nparray(r)
    except pex.PFF_Error, e:
        print "Error:", e
        return None
    
def d2i(dval):
    '''Function to encode a double as five 15-bit unsigned integers.

Usage:
  d2i( dval )

Arguments:
  dval:   The double value to be encoded.

Return value: If successful, the encoded integer array is returned. On
              error, None is returned.'''

    err = 0
    try:
        d = np.double(dval)
    except ValueError,e:
        print 'Invalid type supplied for argument dval'
        return None
    try:
        r = pex.u_d2i(d)
        return buf2nparray(r)
    except pex.PFF_Error, e:
        print "Error:", e
        return None
    
def l2i(lval):
    '''Function to encode a long int as five 15-bit unsigned integers.

Usage:
  l2i( lval )

Arguments:
  lval:   The long int value to be encoded.

Return value: If successful, the encoded integer array is returned. On
              error, None is returned.'''

    err = 0
    try:
        l = long(lval)
    except ValueError,e:
        print 'Invalid type supplied for argument lval'
        return None
    try:
        r = pex.u_l2i(l)
        return buf2nparray(r)
    except pex.PFF_Error, e:
        print "Error:", e
        return None
    
def c2i(str):
    '''Function to encode a string as an array of  unsigned 16-bit integers.

Usage:
  c2i( str )

Arguments:
  str:   The string value to be encoded.

Return value: If successful, the encoded integer array is returned. On
              error, None is returned.'''

    err = 0
    tstr = type(str)
    if tstr != types.StringType and tstr != np.string_:
        print 'Invalid type supplied for argument str'
        return None
    clen = len(str)  ;  wlen = (clen+1)/2
    ia = np.empty(wlen,dtype=np.intc)
    for i,j in enumerate(range(1,clen,2)):
        ia[i] = 256*ord(str[j-1]) + ord(str[j])
    if clen % 2:  ia[-1] = 256*ord(str[-1]) + ord(' ')
    return ia
    
def i2c(ia):
    '''Function to decode a string from an array of unsigned 16-bit integers.

Usage:
  i2c( ia )

Arguments:
  ia:   The integer array containing the encoded string.

Return value: If successful, string is returned. On error, None is returned.'''

    err = 0
    try:
        d = np.asarray(ia,dtype=np.intc)
        if d.ndim != 1 or d.max() > 65535 or d.min() < 0: raise ValueError
    except ValueError:
        print "i2c: DATA must be a 1D NUMPY.NDARRAY object of integer*2 type"
        return None
    wlen = len(d)
    str = ''
    c1 = np.bitwise_and(d,255)
    c0 = np.bitwise_and(np.right_shift(d,8),255)
    for i in range(wlen):
        str += (chr(c0[i]) + chr(c1[i]))
    return str.strip()

def joinSpare(imap):
    '''Function to take a python dictionary of key/value pairs and encode
it into and integer*2 array. Each supplied key is a python string, and
each value is an integer*2 array.

Keys are encoded by converting them in 2-byte integers, 2 per word,
followed by ": " encoded in a single 2-byte integer, followed by the
associated integer array. Multiple key/value pairs are separated by
"; ", encoded in a single 2-byte integer.

Usage:
  joinSpare(imap)

Arguments:
  imap:  Python of python dictionary of key/value pairs

Return value: If successful, the encoded integer array is returned. On
              error, None is returned.

See also: findSpare()'''

    s = np.array([],dtype=np.intc)
    if type(imap) is not types.DictType:
        print "joinSpare: Argument must be Dictionary"
        return None
    first = True
    colon = c2i(':')
    semi = c2i(';')
    for k in imap.keys():
        if not first: s = np.concatenate((s,semi))
        try:
            d = np.asarray(imap[k],dtype=np.intc)
            if d.ndim != 1: raise ValueError
            if d.size and (d.max() > 65535 or d.min() < 0): raise ValueError
            sk = c2i(k.strip())
            if sk is None: return None
        except ValueError:
            print "joinSpare: imap[" + k + \
                  "] must be a 1D array of integer*2 values"
            return None
        s = np.concatenate((s,sk,colon,d))
        first = False
    return s

def findSpare(spare,key,size=0):
    '''Function to find and return the integer*2 array associated with 
the string `key' in an encoded array previously constructed by the
`joinSpare()' method.

Keys are encoded in the `spare' array as 2-byte integers, 2 per word,
followed by ": " encoded in a single 2-byte integer, followed by the
associated integer array. Multiple key/value pairs are separated by
"; ", encoded in a single 2-byte integer.

Usage:
  findSpare(spare,key,size=0)

Arguments:
  spare:  Array of 2-byte integers previously construct with the
          `joinSpare()` function.
  key:    Python string containing the key associated with the requested
          array.
  size:   If greater than zero, the size of the array to be returned.
          Otherwise, the array is assumed to be terminated by the next
          "; " encoded integer that occurs in the `spare' array, or if
          one is not found, the end of the `spare' array

Return value: If successful, the 2-byte integer array associated with
              the supplied key is returned. On error, None is
              returned.

See also: joinSpare()'''

    if spare is None: return None
    try:
        s = np.asarray(spare,dtype=np.intc)
    except ValueError:
        print "findSpare: 'spare' must be a 1D array of integer*2 values"
        return None
    if type(key) is not types.StringType:
        print "findSpare: 'key' must be string" ; return None
    if spare.size == 0: return None
    colons = np.where(s == c2i(':'))[0]
    if colons.size == 0: return None
    semis = np.where(s == c2i(';'))[0]
    cs = i2c(s)
    extra = ''
    key = key.strip()
    if len(key) % 2: extra = ' '
    srch = key + extra + ': '
    lfind = cs.find(srch)
    if lfind < 0 or lfind%2: return None
    ioff = (lfind + len(srch))/2
    last = s.size
    if size == 0:
        nextsemi = np.where(semis > ioff)[0]
        if nextsemi.size > 0: last = semis[nextsemi[0]]
    elif size + ioff <= last: last = ioff + size
    else:  print \
       "FindSpare: requested 'size' is not available, array will be truncated"
    return s[ioff:last]
    
def pff_precision(value=None, file=None, show=False, default=False, all=False, \
                  ignore=False):
    if show:
        vcode = -1
    elif value is None:
        print "PFF_PRECISION: Parameter \"value\" must be provided"
        return None
    else:
        vcode = value

    if default or all:
        fcode = -1
        if all: fcode = -2
    elif file is None:
        fcode = 0
    else:
        fcode = file

    try:
        rval = pex.fp_precision(fcode,vcode)
    except pex.PFF_Error, e:
        if ignore: raise
        else: print "PFF_PRECISION:", e

    dims = (2, rval[2]/2)
    return buf2nparray(rval,dims)

def readhdr(dsindex=0, id=0):
    try:
        hdr = pex.readhdr(dsindex,id)
        if hdr:
            rfu = hdr['rfu']
            if rfu[2] >  0:
                hdr['rfu'] = buf2nparray(rfu)
            else:
                hdr['rfu'] = None

        return hdr
    except pex.PFF_Error, e:
        print "Error:", e

def read_dataset(dsindex=0, id=0):
    try:
        handle = None
        hdr = readhdr(dsindex, id)
        if hdr:
            handle = hdr['handle']

            typekey = ds_typenames.get(hdr['rawtype'])[0]
            if typekey == 'U':
                ds = UNF_dataset(header=hdr, id=id)
            elif typekey == 'N':
                ds = NUNF_dataset(header=hdr, id=id)
            elif typekey == 'V':
                ds = VTX_dataset(header=hdr, id=id)
            elif typekey == 'I':
                ds = IFL_dataset(header=hdr, id=id)

            pex.advance_ds_pointer(id)
            return ds

    finally:
        if handle: pex.releaseDSHandle(handle)

def scanlist(list,findstring,exact=0,match=1):
    l = len(list)
    if l == 0: return []
    buf = '\0'.join(list) + '\0'
    tmp = pex.scanlist(l,buf,findstring,exactcase=exact,match=match)
    return buf2list(tmp)

# Utilities


def bld_label_array(buf,dims):
    list = buf.split('\0')
    mx = 1
    for s in list:  mx = max(mx,len(s))
    dt = "".join(['a',str(mx)])
    #print "dt:",dt, dims,list
    if len(dims) > 1:
        return np.copy(np.reshape(np.array(list,dtype=(dt)), dims))
    else:
        return np.array(list,dtype=(dt),copy=True)

def buf2nparray(rlist,dims=0):
    if ( rlist is None ): return None
    #print len(rlist), rlist[0], rlist[1], rlist[2], len(rlist[3])
    if (dims == 0): dims = (rlist[2],)
    dt = rlist[0] + str(rlist[1])
    #print rlist[3], dt, dims
    tmp = np.frombuffer(rlist[3],dtype=dt)
    #print dt, dims
    if len(dims) > 1:
        return np.copy(np.reshape(tmp,dims,'F'))
    else:
        return np.copy(tmp)

def buf2list(rlist):
    return buf2nparray(rlist).tolist()

def getLastSliceBlkMap():
    return _lastSliceBlkMap

def _get_rstr(v,full):
    if type(v) is types.ListType:
        s = "["
        pref = ""
        for x in v:
            s += pref + _get_rstr(x,full)
            pref = ", "
        s += "]"
    elif type(v) is np.ndarray:
        if full or v.size < 20:  s = v.__repr__()
        else:
            s = "<ndarray, shape=" + str(v.shape) + ", dtype=" + \
                str(v.dtype) +">"
    else:
        s = v.__str__()
    return s

class dataset:
    "Abstraction of a general PFF dataset"

    def __init__(self,header, id=0):
        self.rawtype = header['rawtype']

        self.rawname = ds_typenames.get(self.rawtype)
        self.typekey = self.rawname[0]

        self.apptype = header['apptype']
        self.title = " ".join(header['title'].split())
        self.typename = header['typename']
        self.rfu = header['rfu']
        self.file = os.path.abspath(pex.getfilename(id))

    def __str__(self):
        return self.info_string()

    def printall(self,f=sys.stdout):
        needClose = False
        if type(f) is types.StringType:
            try:
                f = __builtin__.open(f,'w')
                needClose = True
            except IOError, e:
                print >>sys.stderr, "pff_dataset.printall:",e
                return

        print >>f, self.info_string(True)
        if needClose: f.close()

    def info_string(self,full=False):
        m = self.__dict__
        s = ''
        keys = m.keys()
        keys.sort()
        for k in keys:
            s += k + ": "
            v = m[k]
            s += _get_rstr(v,full) + "\n"
        return s[:-1]

    def dir(self):
        return self.__dict__.keys()

    def dup(self,new,copy=True):
        if copy:
            s1 = " = cpy.deepcopy(new['"
            s2 = "'])"
        else:
            s1 = " = new['"
            s2 = "']"

        for k in new.keys():
            exec "self." + k + s1 + k + s2

        
    def header_tuple(self):
        # (rawtype, apptype, typename, title)
        d = self.__dict__

        if d.has_key('rawtype') and type(self.rawtype) == types.IntType:
            tup = ( self.rawtype, )
        else: tup = ( PFF_DEFAULT, )

        if d.has_key('apptype') and type(self.apptype) == types.IntType:
            tup += ( self.apptype, )
        else: tup += ( PFF_DEFAULT, )

        if d.has_key('typename') and type(self.typename) == types.StringType:
            tup += ( self.typename, )
        else: tup += ( '', )

        if d.has_key('title') and type(self.title) == types.StringType:
            tup += ( self.title, )
        else: tup += ( '', )

        return tup

    def write(self,id=0,precision=None,ignore=False):
        reset = None
        if precision is not None:
            t = type(precision)
            if (t is not types.IntType and t is not types.LongType) or \
               precision < 0 or precision > 2:
                print "Illegal precision value. It will be ignored"
            else:
                reset = pff_precision(precision,id,ignore=ignore)
                if reset is None: return None

        self.ds_write(id,ignore=ignore)

        if reset is not None:
            pff_precision(reset[1,0],id)

        return 0

    def findSpare(self,key,size=0,block=0):
        if not self.__dict__.has_key('spare'): return None
        return findSpare(self.spare,key,size)
        
class IFL_dataset(dataset):
    "Abstraction of a PFF IFL dataset"

    def __init__(self,ds=0,id=0,header=None,new=None, copy=True):
        if new is not None:
            if type(new) == types.DictType:
                self.dup(new,copy=copy)

            self.typekey = 'I'
            return
        handle = 0
        try:
            if header:
                hdr = header
            else:
                hdr = readhdr(ds,id)

            if hdr:
                handle = hdr['handle']
                try:
                    thistype = ds_typenames.get(hdr['rawtype'])[0]
                    if 'I' == thistype:
                        # initialize base class
                        dataset.__init__(self,hdr, id=id)

                      # load numeric data
                        get_num_arrays = pex.get_num_arrays
                        rlist = get_num_arrays(handle, "iarray")
                        if rlist is None:  self.iarray = None
                        else: self.iarray = buf2nparray(rlist)
                        rlist = get_num_arrays(handle, "farray")
                        if rlist is None:  self.farray = None
                        else: self.farray = buf2nparray(rlist)
                        rlist = get_num_arrays(handle, "flist")
                        if rlist is None:  self.flist = None
                        else: self.flist = buf2nparray(rlist)
                    else:
                        raise pex.PFF_Error, "Not an IFL dataset"
            
                finally:
                    # if no header supplied, then this method owns the handle
                    if not header and handle: pex.releaseDSHandle(handle)
        except pex.PFF_Error, e:
            print "Error:", e


    def clone(self):
        return IFL_dataset(new=self.__dict__)


    def ds_write(self,id=0,ignore=False):
        try:
            htup = self.header_tuple()
            ##print htup
            iarray = self.iarray
            ia = ( 'i', )
            if iarray is not None: ia += (iarray.itemsize, iarray.tostring() )
            else: ia += ( 1, '' )

            farray = self.farray
            fa = ( 'f', )
            if farray is not None: fa += (farray.itemsize, farray.tostring() )
            else: fa +=  ( 1, '' )

            flist = self.flist
            fl = ( 'f', )
            if flist is not None: fl += (flist.itemsize, flist.tostring() )
            else: fl += ( 1, '' )

            pex.write_ifl(htup, ia, fa, fl, id)

        except pex.PFF_Error, e:
            if ignore: raise
            else: print "Error:", e

        
class VTX_dataset(dataset):
    "Abstraction of a PFF vertex dataset"

    def __init__(self,ds=0,id=0,header=None,new=None, copy=True):
        if new is not None:
            if type(new) == types.DictType:
                # if copying, need to preserve 'F' order of multidim arrays
                if copy:
                    if new['adim'] > 1 and new.has_key('data'):
                        sdata = new['data']  ;  new['data'] = None
                    else: sdata = None
                    if new.has_key('x'):
                        sx = new['x']  ;  new['x'] = None
                    else: xs = None
                self.dup(new,copy=copy)
                if copy:
                    if sdata is not None:
                        self.data = sdata.copy(order='F') ; new['data'] = sdata
                    if sx is not None:
                        self.x = sx.copy(order='F')  ;  new['x'] = sx

            self.typekey = 'V'
            return
        try:
            if header:
                hdr = header
            else:
                hdr = readhdr(ds,id)

            if hdr:
                handle = hdr['handle']
                try:
                    thistype = ds_typenames.get(hdr['rawtype'])[0]
                    if 'V' == thistype:
                        # initialize base class
                        dataset.__init__(self,hdr, id=id)

                        sdim = hdr['sdim']
                        adim = hdr['adim']

                        # load all dataset labels
                        get_labels = pex.get_labels
                        if ( sdim > 0 ):
                            buf = get_labels(handle, 'S')
                            self.glabels = bld_label_array(buf, (sdim,))
                        else: self.glabels = None
                        if ( adim > 0 ):
                            buf = get_labels(handle, 'A')
                            self.dlabels = bld_label_array(buf, (adim,))
                        else: self.dlabels = None

                        #load numeric data
                        get_num_arrays = pex.get_num_arrays
                        rlist = get_num_arrays(handle, "spare")
                        self.spare = buf2nparray(rlist)
                        rlist = get_num_arrays(handle, "x")
                        nv = rlist[2]/sdim
                        self.x = buf2nparray(rlist, (sdim,nv))
                        data = None
                        if adim > 0:
                            for i in range(adim):
                                rlist = get_num_arrays(handle, "data", shift=i)
                            ##data.append(buf2nparray(rlist))
                                if adim > 1:
                                    if i == 0:
                                        tmp = buf2nparray(rlist)
                                        dt = tmp.dtype
                                        data = np.empty((nv,adim),dt,'F')
                                        data[:,0] = tmp
                                    else:
                                        data[:,i] = buf2nparray(rlist)
                                else:
                                    data = buf2nparray(rlist)

                        g_range = np.empty((sdim,2),dtype=PFFnp_float)
                        g_eps = np.empty((sdim),dtype=PFFnp_float)
                        self.g_range = g_range
                        self.g_eps = g_eps
                        self.fill_grid_range()
                        ##x = self.x
                        ##g_range[:,0] = x.min(axis=1)
                        ##g_range[:,1] = x.max(axis=1)
                        ##g_eps = GridEpsilonFactor*(g_range[:,1] - g_range[:,0])

                        self.sdim = sdim
                        self.adim = adim
                        self.nv = nv
                        self.data = data
                    else:
                        raise pex.PFF_Error, "Not a VERTEX dataset"
            
                finally:
                    # if no header supplied, then this method owns the handle
                    if not header and handle: pex.releaseDSHandle(handle)
        except pex.PFF_Error, e:
            print "Error:", e
    
        
    def clone(self):
        return VTX_dataset(new=self.__dict__)


    def fill_grid_range(self):
        g_range = self.g_range
        g_eps = self.g_eps
        x = self.x
        g_range[:,0] = x.min(axis=1)
        g_range[:,1] = x.max(axis=1)
        g_eps[...] = GridEpsilonFactor*(g_range[:,1] - g_range[:,0])


    def ds_write(self,id=0,ignore=False):
        try:
            htup = self.header_tuple()
            #print htup

            stup = (self.sdim, self.adim, self.nv )
            #print stup

            lablist = self.glabels.tolist()
            if self.dlabels is not None:
                lablist.extend(self.dlabels.tolist())
            nul = '\0'
            labbuf =nul.join(lablist) + nul
            #print labbuf, len(labbuf)
                
            spare = self.spare
            ia = ( 'i', )
            if spare is not None: ia += (spare.itemsize, spare.tostring() )
            else: ia += ( 1, '' )
            #print ia, len(ia[2])
                
            x = self.x
            xa = ( 'f', )
            if x is not None: xa += (x.itemsize, x.tostring(order='F') )
            else: xa += ( 1, '' )
            #print xa, len(xa[2])

            data = self.data
            ##print self.adim, data
            if self.adim > 0 and data is not None:
                da = ( str(data.dtype)[0], data.itemsize, \
                       data.tostring(order='F') )
##                da += (data[0].itemsize, )
##                if self.adim == 1:
##                    da += (data[0].tostring(), )
##                else:
##                    ax = data[0].ndim
##                    shp = data[0].shape + (1,)
##                    ctup = ()
##                    for i in range(self.adim):
##                        ctup += (data[i].reshape(shp,order='F'),)
##                    da += (np.concatenate(ctup,axis=ax).tostring(order='F'), )
            else: da = ( 'E', 1, '' )
            ##print da, len(da[2])

            pex.write_vtx(htup, stup, labbuf, ia, xa, da, id)

        except pex.PFF_Error, e:
            if ignore: raise
            else: print "Error:", e

        
class blkgrid_dataset(dataset):
    "Abstraction of a uniform or nonuniform multiblock grid dataset"

    def __init__(self, header, id=0):

        # initialize base class
        dataset.__init__(self,header, id=id)

        handle = header['handle']
        sdim = header['sdim']
        adim = header['adim']
        nblk = header['nblk']

        # load all dataset labels
        get_labels = pex.get_labels
        if ( sdim > 0 ):
            buf = get_labels(handle, 'S')
            self.glabels = bld_label_array(buf, (nblk,sdim))
        else: self.glabels = None
        if ( adim > 0 ):
            buf = get_labels(handle, 'A')
            #print buf
            self.dlabels = bld_label_array(buf, (nblk,adim))
        else: self.dlabels = None
        buf = get_labels(handle, 'B')
        self.blabels = bld_label_array(buf, (nblk,))

        #load numeric data
        get_num_arrays = pex.get_num_arrays

        spare = []
        nx = []
        data = []
        for b in range(nblk):
            #print "blk: ",b 
            rlist = get_num_arrays(handle, "spare", b)
            spare.append(buf2nparray(rlist))
            rlist = get_num_arrays(handle, "nx", b)
            nxb = buf2nparray(rlist)
            nx.append(nxb)
            npt = 1
            dims = ()
            for i in range(sdim):
                npt *= nxb[i]
                dims += (nxb[i],)
            ##dblk = []
            #print "dims:", dims
            if adim > 0:
                for i in range(adim):
                    rlist = get_num_arrays(handle, "data", b, i)
                    if adim > 1:
                        if i == 0:
                            tmp = buf2nparray(rlist, dims)
                            dt = tmp.dtype
                            dblk = np.empty(dims + (adim,),dt,'F')
                            dblk[...,i] = tmp
                        else:
                            dblk[...,i] = buf2nparray(rlist, dims)
                    else:
                        dblk = buf2nparray(rlist, dims)
                data.append(dblk)

        self.sdim = sdim
        self.adim = adim
        self.nblk = nblk
        self.nx   = nx
        self.spare = spare
        self.data = data

    def _initialize(self):
        nblk = self.nblk
        sdim = self.sdim
        g_range = np.empty((nblk+1,sdim,2),dtype=PFFnp_float)
        g_eps = np.empty((nblk+1,sdim),dtype=PFFnp_float)
        for blk in range(nblk):
            for crd in range(sdim):
                t = self.get_grid_minmax(crd,blk,_no_check=True)
                g_eps[blk,crd] = GridEpsilonFactor*(t[1] - t[0])
                g_range[blk,crd,:] = np.array(t)
        g_range[nblk,:,0] = g_range[:nblk,:,0].min(axis=0)
        g_range[nblk,:,1] = g_range[:nblk,:,1].max(axis=0)
        g_eps[nblk,:] = np.max(g_eps[:nblk,:],axis=0)

        self.g_range = g_range
        self.g_eps = g_eps


    def fill_grid_range(self):
        nblk = self.nblk
        sdim = self.sdim
        g_range = self.g_range
        g_eps = self.g_eps
        for blk in range(nblk):
            for crd in range(sdim):
                t = self.get_grid_minmax(crd,blk,_no_check=True)
                g_eps[blk,crd] = GridEpsilonFactor*(t[1] - t[0])
                g_range[blk,crd,:] = np.array(t)
        g_range[nblk,:,0] = g_range[:nblk,:,0].min(axis=0)
        g_range[nblk,:,1] = g_range[:nblk,:,1].max(axis=0)
        g_eps[nblk,:] = np.max(g_eps[:nblk,:],axis=0)


    def check_block_and_coord(self,comp=None,coord=None,block=None):
        rtup = ()
        if comp != "SKIP":
            adim = self.adim
            if comp is None:
                if adim > 1:
                    msg = "component index required for " + str(adim) + \
                        "-vector data"
                    raise PFF_Error(msg)
                cmpid = 0
            elif comp == 'all' or comp == 'mag':
                cmpid = comp
            else:
                t = type(comp)
                if t is types.ListType or t is types.TupleType:
                    cmpid = [ i-1 for i in comp ]
                else:
                    if t is not types.IntType or comp < 1 or comp > adim:
                        raise PFF_Error("illegal component index")
                    cmpid = comp - 1
            rtup += (cmpid, )
        sdim = self.sdim
        if coord is None:
            if sdim > 1:
                msg = "coordinate index required for " + str(sdim) + "D data"
                raise PFF_Error(msg)
            crdid = 0
        else:
            if coord < 1 or coord > sdim:
                raise PFF_Error("illegal coordinate index")
            crdid = coord - 1
        rtup += (crdid, )
        nblk = self.nblk
        if block is None:
            if nblk > 1:
                raise PFF_Error("block index required for multi-block data")
            blkid = 0
        else:
            if block < 1 or block > nblk:
                raise PFF_Error("illegal block index")
            blkid = block - 1
        rtup += (blkid, )

        return rtup


    def get_slice(self,xval,comp='all',coord=None,block='all',quiet=False):
        global _lastSliceBlkMap
        t = type(block)
        oneblk = False
        nblk = self.nblk
        ##print 'GETSLICE:',comp
        if t == types.StringType and block.lower() == "all":
            brange = range(nblk)
            bchk = 1
            bdellist = []
        elif t == types.ListType or t == types.TupleType:
            brange = list(np.subtract(block,1))
            brange.sort()
            used = [ 0 for b in range(nblk+1) ]
            for b in brange:
                if b < 0: b = nblk
                used[min(nblk,b)] += 1
            if used[nblk] or len([s for s in used if s>1]):
                print "Illegal block specification"
                return None
            bdellist = [ i for i in range(nblk) if used[i] == 0]
            bchk = 1
            
        else:
            oneblk = True
            bchk = block

        try:
            #print comp, coord, bchk
            cmpid,crdid,blkid = self.check_block_and_coord(comp, coord, bchk)
            #print 'GETSLICE:',cmpid

            blist = []
            #print '1:',blkid,bdellist
            if oneblk:
                brange = range(blkid,blkid+1)
                bdellist  = range(blkid) + range(blkid+1,nblk)

            blkMap = cpy.deepcopy(brange)
            try:
                xv = np.asarray(xval,dtype=np.single)
                if xv.ndim == 1:
                    if xv.size != len(brange): raise ValueError
                elif xv.ndim > 1: raise ValueError
                else:
                    xv = [ xval for b in brange ]
                xv = list(xv)
            except ValueError:
                print "Illegal XVAL specification"
                return None

            xvdel = []
            for i,blk in enumerate(brange):
                f1, indx = self.find_blk_intercept(xv[i], crdid, blk)

                #print blk, f1, indx, x[indx:indx+2]
                
                if indx < 0:
                    sl = None
                    bdellist.append(blk)
                    xvdel.append(i)
                    ##print '2:',blk,bdellist
                else:
                    sl = self.get_slice_from_index(indx,f1,cmpid,crdid,blk)
                    blist.append(sl)
            
            while xvdel:
                i = xvdel.pop()
                del blkMap[i]
                del xv[i]
            bdellist.sort()
            #print '3:',bdellist
            newblks = nblk - len(bdellist)
            #print len(blist), bdellist, newblks
            assert(newblks == len(blist))
            assert(newblks == len(xv))
            if newblks == 0:
                if not quiet: print "No slice found based on input criteria"
                return None
            new = cpy.deepcopy(self)
            new.nblk = newblks
            if comp != 'all': new.adim = 1
            new.dlabels = np.delete(new.dlabels,bdellist,0)
            new.glabels = np.delete(new.glabels,bdellist,0)
            new.blabels = np.delete(new.blabels,bdellist,0)
            new.g_range = np.delete(new.g_range,bdellist,0)
            new.g_eps = np.delete(new.g_eps,bdellist,0)
            while bdellist:
                blk = bdellist.pop()
                del new.spare[blk]
                del new.nx[blk]
                new.del_blk_grid(blk)
            del new.data
            new.data = blist
            new.set_slice_grid(crdid,xv)
            if newblks < nblk:
                g_range = new.g_range
                g_range[newblks,:,0] = g_range[:newblks,:,0].min(axis=0)
                g_range[newblks,:,1] = g_range[:newblks,:,1].max(axis=0)
            new.g_range[:newblks,crdid,:] = np.reshape(xv,(newblks,1))
            new.g_range[newblks,crdid,0] = min(xv)
            new.g_range[newblks,crdid,1] = max(xv)

            if new.rawtype == NV3 and new.adim == 1: new.rawtype = NF3;
            _lastSliceBlkMap = blkMap
            return new

        except PFF_Error, e:
            print "Error:", e.value

    def scalarize(self,comp='mag',block='all',quiet=False):
        t = type(block)
        oneblk = False
        nblk = self.nblk
        if self.adim < 2:
            if not quiet:
                print "PFF.SCALARIZE: Dataset already scalar - returning self"
            return self.clone()
        ##print 'SCALARIZE:',comp
        if t == types.StringType and block.lower() == "all":
            brange = range(nblk)
            bchk = 1
            bdellist = []
        elif t == types.ListType or t == types.TupleType:
            brange = list(np.subtract(block,1))
            brange.sort()
            used = [ 0 for b in range(nblk+1) ]
            for b in brange:
                if b < 0: b = nblk
                used[min(nblk,b)] += 1
            if used[nblk] or len([s for s in used if s>1]):
                print "Illegal block specification"
                return None
            bdellist = [ i for i in range(nblk) if used[i] == 0]
            bchk = 1
        else:
            oneblk = True
            bchk = block

        try:
            ##print comp, bchk
            cmpid,crdid,blkid = self.check_block_and_coord(comp, 1, bchk)
            ##print 'SCALARIZE:',cmpid

            new = self.clone()
            if block != 'all':
                blist = []
                ##print '1:',blkid,bdellist
                if oneblk:
                    brange = range(blkid,blkid+1)
                    bdellist  = range(blkid) + range(blkid+1,nblk)
            
                for blk in brange:  blist.append(new.data[blk])
                bdellist.sort()
                ##print '3:',bdellist
                newblks = nblk - len(bdellist)
                ##print len(blist), bdellist, newblks
                assert(newblks == len(blist))
                if newblks == 0:
                  if not quiet: print "No blocks found based on input criteria"
                  return None
                new.nblk = newblks
                new.dlabels = np.delete(new.dlabels,bdellist,0)
                new.glabels = np.delete(new.glabels,bdellist,0)
                new.blabels = np.delete(new.blabels,bdellist,0)
                new.g_range = np.delete(new.g_range,bdellist,0)
                new.g_eps = np.delete(new.g_eps,bdellist,0)
                while bdellist:
                    blk = bdellist.pop()
                    del new.spare[blk]
                    del new.nx[blk]
                    new.del_blk_grid(blk)
                del new.data
                new.data = blist
                if newblks < nblk: self.fill_grid_range()
            new.adim = 1

            need_mag = False
            adim = self.adim
            if cmpid == 'all' or cmpid == 'mag':
                if adim > 1: need_mag = True
                crange = range(adim)
                cdellist = range(1,self.sdim)
            elif type(cmpid) is types.IntType:
                crange = range(cmpid,cmpid+1)
                cdellist = [ c for c in range(self.sdim) if c != cmpid ]
            else:
                crange = cmpid
                if len(crange) > 1:  need_mag = True
                cdellist = [ c for c in range(self.sdim) if c != cmpid[0] ]
            new.dlabels = np.delete(new.dlabels,cdellist,1)

            ndata = []
            dt = new.data[0].dtype
            for od in new.data:
                nd = np.empty(shape=od.shape[:3],dtype=PFFnp_float,order='f')
                if need_mag:
                    nd[...] = np.sqrt(np.sum(np.square(od[...,crange]),
                                                              axis=self.sdim))
                else: nd[...] = od[...,cmpid]
                ndata.append(nd)
            new.data = ndata

            if new.rawtype == NV3:
                new.rawtype = NF3 ; new.rawname = 'NF3'
            return new

        except PFF_Error, e:
            print "Error:", e.value


    def get_slice_from_index(self,index,frac1,compid=0,crdid=0,blkid=0):
        nx = self.nx[blkid]
        frac0 = 1.0 - frac1
        dim0 = (); dim1 = () ; dimsl = () ; dimallo = ()
        need_mag = False
        adim = self.adim
        if compid == 'all' or compid == 'mag':
             if adim > 1:
                  need_mag = compid == 'mag'
                  crange = range(adim)
             else:
                  crange = range(1)
        elif type(compid) is types.IntType:
             crange = range(compid,compid+1)
        else:
            crange = compid

        for i in range(self.sdim):
            if i == crdid:
                dim0 += ( slice(index,index+1), )
                dim1 += ( slice(index+1,index+2), )
                dimsl += ( slice(1), )
                dimallo += ( 1, )
            else:
                s = (slice(nx[i]), )
                dim0 += s
                dim1 += s
                dimsl += s
                dimallo += ( nx[i], )

        n_adim = len(crange)
        if adim > 1:
            if n_adim > 1:
                s = (crange, )
                dim0 += s
                dim1 += s
                dimsl += (slice(n_adim), )
                dimallo += ( n_adim, )
            else:
                dim0 += (crange[0], )
                dim1 += (crange[0], )

        xb = self.data[blkid]
        dt = xb.dtype
        ##print 'qq',crange, dt, dimsl, dimallo, dim0, dim1,blkid,xb.shape
        xintrp = np.zeros(dimallo,dtype=dt,order='F')
        ##print 'qq',frac0, frac1, xintrp[dimsl].shape,xb[dim1].shape
        if frac0 != 0.0: xintrp[dimsl] += frac0*xb[dim0]
        if frac1 != 0.0: xintrp[dimsl] += frac1*xb[dim1]
        if need_mag:
            return np.sqrt(np.sum(np.square(xintrp),axis=self.sdim))
        else:
            return xintrp

    def ds_write(self,id=0,ignore=False):
        htup = self.header_tuple()
        #print htup

        stup = (self.sdim, self.adim, self.nblk )
        #print stup
        try:

            pex.bld_multiblkds(htup, stup)

            for i in range(self.nblk):
                lablist = self.glabels[i].tolist()
                if self.dlabels is not None and self.dlabels[i] is not None:
                    lablist.extend(self.dlabels[i].tolist())
                if self.blabels is not None:
                    lablist.append(self.blabels[i])
                nul = '\0'
                labbuf =nul.join(lablist) + nul
                #print lablist, repr(labbuf), len(labbuf)
                
                spare = self.spare[i]
                sp = ( 'i', )
                if spare is not None: sp += (spare.itemsize, spare.tostring() )
                else: sp += ( 1, '' )
                #print sp, len(sp[2])
                
                nx = self.nx[i]
                nxa = ( 'i', )
                if nx is not None: nxa += (nx.itemsize, nx.tostring() )
                else: nxa += ( 1, '' )
                #print nxa, len(nxa[2])
                
                xa = self.xblk_tuple(i)
                #print xa, len(xa[2])

                data = self.data[i]
                if self.adim > 0 and data is not None:
                    da = ( str(data.dtype)[0], data.itemsize, \
                           data.tostring(order='F') )
                else: da = ( 'Empty', 1, '' )
                #print da, len(da[2])
                pex.fill_multiblkds(labbuf, sp, nxa, xa, da)

            pex.write_multiblkds(id)
        except pex.PFF_Error, e:
            if ignore: raise
            else: print "Error:", e

    def findSpare(self,key,size=0,block=0):
        if not self.__dict__.has_key('spare'): return None
        return findSpare(self.spare[block],key,size)
               
class NUNF_dataset(blkgrid_dataset):
    "Abstraction of a PFF nonuniform grid multiblock dataset"

    def __init__(self,ds=0,id=0,header=None,new=None, copy=True):
        if new is not None:
            if type(new) == types.DictType:
                # if copying, need to preserve 'F' order of multidim arrays
                if copy and new.has_key('data'):
                    sdata = new['data']  ;  new['data'] = None
                else: sdata = None
                self.dup(new,copy=copy)
                if copy:
                    if sdata is not None:
                        dat = []
                        for bdata in sdata:  dat.append(bdata.copy(order='F'))
                        self.data = dat  ;  new['data'] = sdata

            self.typekey = 'N'
            return
        try:
            if header:
                hdr = header
            else:
                hdr = readhdr(ds,id)

            if hdr:
                handle = hdr['handle']
                try:
                    thistype = ds_typenames.get(hdr['rawtype'])[0]
                    if 'N' == thistype:
                        # initialize base class
                        blkgrid_dataset.__init__(self,hdr, id=id)

                        sdim = self.sdim
                        adim = self.adim
                        nblk = self.nblk

                        #load numeric X data
                        get_num_arrays = pex.get_num_arrays

                        x = []
                        for b in range(nblk):
                            #print "blk: ",b 
                            blkx = []
                            for i in range(sdim):
                                rlist = get_num_arrays(handle, "x", b, i)
                                blkx.append(buf2nparray(rlist))
                            x.append(blkx)
                        self.x = x

                        self._initialize()
                    else:
                        raise pex.PFF_Error, "Not a NUNF dataset"

                finally:
                    # if no header supplied, then this method owns the handle
                    if not header and handle: pex.releaseDSHandle(handle)

        except pex.PFF_Error, e:
            print "Error:", e


    def clone(self):
        return NUNF_dataset(new=self.__dict__)


    def get_grid_minmax(self,coord=None,block=None,_no_check=False):
        if _no_check: # here coord and are decremented to storage index
            crdid, blkid = (coord, block)
            #print "no_check", crdid, blkid
        else:
            crdid, blkid = self.check_block_and_coord("SKIP", coord, block)
            #print "check", crdid, blkid
        x  = self.x[blkid][crdid]
        return ( x[0], x[-1])

    
    def find_blk_intercept(self, xval, crdid=0, blkid=0):
        x = self.x[blkid][crdid]
        nx = self.nx[blkid][crdid]

        if xval <= x[0] - self.g_eps[blkid,crdid]:
            f1, indx = (0., -1)
        elif xval >= x[-1] + self.g_eps[blkid,crdid]:
            f1, indx = (0., -1)
        else:
            findx = max(0,np.interp(xval,x,np.arange(nx,dtype=x.dtype)))
            f1, indx = np.modf(findx)

        indx = int(indx)
        if indx > nx-2:
            indx = nx - 2
            f1 =1.0
        return (f1, indx)

    def del_blk_grid(self,blkid):
        del self.x[blkid]

    def set_slice_grid(self,crdid,xval):
        nblk = self.nblk
        for i in range(nblk):
            nxb = self.nx[i]
            xb = self.x[i]
            dt = xb[crdid].dtype
            xb[crdid] = xval[i] * np.ones((1,),dtype=dt,order='F')
            nxb[crdid] = 1

    def make_uniform(self,coord='all',block='all',quiet=False):
        sdim = self.sdim
        crange = None
        if type(coord) == types.StringType and coord.lower() == "all":
            crange = range(self.sdim)
            coorchk = 1
        elif type(coord) == types.ListType:
            crange = []
            for crd in coord:
                if crd < 1 or crd > sdim:
                    raise PFF_Error("illegal coordinate index")
                crange.append(crd-1)
            coorchk = crange[0]
        else:
            coorchk = coord

        if type(block) == types.StringType and block.lower() == "all":
            brange = range(self.nblk)
            allblks = True
            bchk = 1
        else:
            allblks = False
            bchk = block

        try:
            #print comp, coord, bchk
            crdid,blkid = self.check_block_and_coord('SKIP', coorchk, bchk)

            arange = range(self.adim)
            if not crange:
                crange = range(crdid,crdid+1)
            if not allblks:
                brange = range(blkid,blkid+1)

            new = cpy.deepcopy(self)
            cnt = 0
            ##print 'cnt:',cnt
            for blk in brange:
                xb = new.x[blk]
                nxb = new.nx[blk]
                
                bdata = new.data[blk]
                #dt = bdata[0].dtype
                for crd in crange:
                    x = xb[crd]
                    nx = nxb[crd]
                    needs = False
                    if nx > 2:
                        dx = np.diff(x)
                        mn,mx = (dx.min(), dx.max())
                        if 1. - mn/mx > UNIFORM_CRITERIA: needs = True
                    if needs:
                        cnt += 1
                        ##print blk, crd, cnt, "needs it"
                        nxn = np.rint((x[-1] - x[0])/mn) + 1
                        fp = np.arange(nx)
                        xn = np.empty((nxn),dtype=PFFnp_float)
                        xn[:] = np.asarray(np.linspace(x[0],x[-1],nxn),
                                           dtype=PFFnp_float)
                        intrp = np.interp(xn[1:-1],x,fp)
                        f1, find = np.modf(intrp)
                        ind = np.int32(find)
                        nsh = ()
                        fsh = ()
                        ndim = ()
                        dim0 = ()
                        dim1 = ()
                        blist = []
                        for s in range(sdim):
                            if s == crd:
                                nsh += (nxn,)
                                fsh += (len(f1),)
                                ndim += (slice(1,-1),)
                                dim0 += (ind,)
                                dim1 += (ind+1,)
                                blist.append(0)
                            else:
                                sl = slice(nxb[s])
                                nsh += (nxb[s],)
                                fsh += (1,)
                                ndim += (sl,)
                                dim0 += (sl,)
                                dim1 += (sl,)
                                blist.append(sl)
                        if self.adim > 1:
                            sl = slice(self.adim)
                            nsh += (self.adim,)
                            fsh += (1,)
                            ndim += (sl,)
                            dim0 += (sl,)
                            dim1 += (sl,)
                            blist.append(sl)
                            
                        f1_3 = f1.reshape(fsh,order='F')

                        dold = bdata
                        dnew = np.empty(nsh,dtype=PFFnp_float,order='F')
                        bnd = tuple(blist)
                        dnew[bnd] = dold[bnd]
                        blist[crd] = -1
                        bnd = tuple(blist)
                        dnew[bnd] = dold[bnd]
                        dnew[ndim] = (1.0 - f1_3)*dold[dim0] + \
                                     f1_3*dold[dim1]
                        bdata = dnew
                        nxb[crd] = nxn
                        del xb[crd]
                        xb.insert(crd, xn)
                    ##else:  print blk,crd,"doesn't need it"
                new.data[blk] = bdata
            if cnt == 0:
                if not quiet:
                    print "NUNF_dataset.make_uniform(): dataset already uniform"
                return None
            else:
                return new

        except PFF_Error, e:
            print "Error:", e.value

    def connect_order(self,coord,up=True):
#
# Returns an ordered list of blocks and corresponding connectivity information
# Each list element is a a tuple (b_info) with two elements:
#   b_info[0] -- block index
#   b_info[1] -- list of all block connections for the block
# Each element in the block connection list is itself a tuple (b_cnct) with
# (sdim +2) elements:
#   b_cnct[0]  -- index of connecting block
#   b_cnct[1]  -- normal coordinate index of the connection 
#   b_cnct[2]  -- normal coordinate index of the connection for connecting block
#   b_cnct[3]  -- 3-element tuple (trans_cnct) w/ connection info in 1st 
#                 tranverse direction
#    ...
#   b_cnct[-1] -- 3-element tuple w/ connection info in last tranverse direction
# Each transverse connection tuple contains:
#   trans_cnct[0] -- beginning coordinate index of connection
#   trans_cnct[1] -- beginning coordinate index of connection for connecting blk
#   trans_cnct[2] -- number of points in the connection
#
        nblk = self.nblk
        if nblk == 1:
            return [ [0, []] ]
        sdim = self.sdim
        if coord < 1 or coord > sdim:
            print "Illegal value for COORD"
            return None
        cid = coord - 1
        l = 1
        dirs = [ (cid+i) % sdim for i in range(sdim) ]
        ##print "dirs:", dirs
        if up: l = 0
        tlist = []
        gr = self.g_range
        for i in range(nblk):
            tlist.append( ( gr[i,cid,l], i ) )
        tlist.sort()
        if not up: tlist.reverse()

        ##print tlist
        olist = [ s[1] for s in tlist ]
        blist = [ (s,[]) for s in olist ]
        ##print olist, blist
        geps = self.g_eps[nblk]
        nx = self.nx
        for i in range(nblk):
            xt = gr[i,cid,1]
            for j in range(nblk):
                if j == i: continue
                xb = gr[j,cid,0]
                ##print xt, xb, abs(xt-xb), geps[cid]
                if abs(xt - xb) < geps[cid]:
                    kt = olist.index(i) ; kb = olist.index(j)
                    tt = (j,-1,0)
                    tb = (i,0,-1)
                    overlap = True
                    for d in dirs[1:]:
                      xlow = max(gr[i,d,0],gr[j,d,0])
                      xhi = min(gr[i,d,1],gr[j,d,1])
                      ##print d,xlow,xhi, geps[d]
                      if xhi+geps[d] >= xlow:
                        it = int(np.sum(self.find_blk_intercept(xlow,d,i))+0.5)
                        ih = int(np.sum(self.find_blk_intercept(xhi,d,i))+0.5)
                        tcnt = ih - it + 1
                        ib = int(np.sum(self.find_blk_intercept(xlow,d,j))+0.5)
                        ih = int(np.sum(self.find_blk_intercept(xhi,d,j))+0.5)
                        bcnt = ih - ib + 1
                        assert(tcnt == bcnt)
                        tt += ((it,ib,tcnt),)
                        tb += ((ib,it,bcnt),)
                      else:
                        overlap = False
                        break
                    if overlap:    
                        blist[kt][1].append(tt)
                        blist[kb][1].append(tb)

        return blist

    def xblk_tuple(self, blk = 0):
        x = self.x[blk]
        xa = ( 'f', )
        if x is not None:
            if self.sdim == 1:
                xa += (x[0].itemsize, x[0].tostring(order='F') )
            else:
                ctup = ()
                for i in range(self.sdim):
                    ctup += (x[i],)
                xall = np.concatenate(ctup)
                xa += (xall.itemsize, xall.tostring(order='F') )
        else: xa += ( 1, '' )
        return xa

    def getx(self,coord=1,blk=1):
        crdid, blkid = self.check_block_and_coord("SKIP", coord, blk)
        return self.x[blkid][crdid].copy(order='F')

        
class UNF_dataset(blkgrid_dataset):
    "Abstraction of a PFF uniform grid multiblock dataset"

    def __init__(self,ds=0,id=0,header=None,new=None, copy=True):
        if new is not None:
            if type(new) == types.DictType:
                # if copying, need to preserve 'F' order of multidim arrays
                if copy and new.has_key('data'):
                    sdata = new['data']  ;  new['data'] = None
                else: sdata = None
                self.dup(new,copy=copy)
                if copy and sdata is not None:
                    dat = []
                    for bdata in sdata:  dat.append(bdata.copy(order='F'))
                    self.data = dat  ;  new['data'] = sdata
                    
            self.typekey = 'U'
            return
        try:
            if header:
                hdr = header
            else:
                hdr = readhdr(ds,id)

            if hdr:
                handle = hdr['handle']
                try:
                    thistype = ds_typenames.get(hdr['rawtype'])[0]
                    if 'U' == thistype:
                        # initialize base class
                        blkgrid_dataset.__init__(self,hdr, id=id)

                        sdim = self.sdim
                        adim = self.adim
                        nblk = self.nblk

                        #load numeric X data

                        x0 = []
                        dx = []
                        for b in range(nblk):
                            #print "blk: ",b 
                            rlist = pex.get_num_arrays(handle, "x", b)
                            tmp = buf2nparray(rlist)
                            x0.append(tmp[:sdim])
                            dx.append(tmp[sdim:])
                        self.x0 = x0
                        self.dx = dx

                        self._initialize()
                    else:
                        raise pex.PFF_Error, "Not an UNF dataset"

                finally:
                    # if no header supplied, then this method owns the handle
                    if not header and handle: pex.releaseDSHandle(handle)
        except pex.PFF_Error, e:
            print "Error:", e


    def clone(self):
        return UNF_dataset(new=self.__dict__)


    def get_grid_minmax(self,coord=None,block=None,_no_check=False):
        if _no_check: # here coord and are decremented to storage index
            crdid, blkid = (coord, block)
            #print "no_check", crdid, blkid
        else:
            crdid, blkid = self.check_block_and_coord("SKIP", coord, block)
            #print "check", crdid, blkid
        x0  = self.x0[blkid][crdid]
        return ( x0 , x0 + (self.nx[blkid][crdid] - 1)*self.dx[blkid][crdid] )

    
    def find_blk_intercept(self, xval, crdid=0, blkid=0):
        x0 = self.x0[blkid][crdid]
        dx = self.dx[blkid][crdid]
        nx = self.nx[blkid][crdid]
        nxm1 = nx - 1

        if xval <= x0 - self.g_eps[blkid,crdid]:
            f1, indx = (0., -1)
        elif xval >= x0 + (nxm1)*dx + self.g_eps[blkid,crdid]:
            f1, indx = (0., -1)
        else:
            findx = max(0,min(nxm1,(xval - x0)/dx))
            f1, indx = np.modf(findx)
        
        indx = int(indx)
        if indx == nxm1:
            indx -= 1
            f1 =1.0

        return (f1, indx)

    def del_blk_grid(self,blkid):
        del self.x0[blkid]
        del self.dx[blkid]

    def set_slice_grid(self,crdid,xval):
        nblk = self.nblk
        for i in range(nblk):
            self.nx[i][crdid] = 1
            self.x0[i][crdid] = xval[i]
            # dx value doesn't matter, so just leave as is

    def xblk_tuple(self, blk = 0):
        arr = np.concatenate((self.x0[blk],self.dx[blk]))
        xa = ( 'f', arr.itemsize, arr.tostring(order='F') )
        return xa

    def getx(self,coord=1,blk=1):
        crdid, blkid = self.check_block_and_coord("SKIP", coord, blk)
        xb = self.x0[blkid][crdid]
        nx = self.nx[blkid][crdid]
        xe = xb + (nx-1)*self.dx[blkid][crdid]
        return np.asarray(np.linspace(xb,xe,nx),dtype=PFFnp_float)

# define module variables
ds_typenames = pex.get_type_names()
##print ds_typenames

PFFctype_sizes = pex.get_ctype_sizes()
PFFnp_int = np.dtype('int' + str(8*PFFctype_sizes['i']))
PFFnp_long = np.dtype('int' + str(8*PFFctype_sizes['l']))
PFFnp_float = np.dtype('float' + str(8*PFFctype_sizes['f']))

_lastSliceBlkMap = None

GridEpsilonFactor = 5.0e-5

for i in ds_typenames.keys():
    exec ds_typenames[i] + "=" + str(i)

UNIFORM_CRITERIA = 0.001

PFF_DEFAULT = -3

FP_REDUCED = 0
FP_FULL = 1
FP_ORDFULL = 2
