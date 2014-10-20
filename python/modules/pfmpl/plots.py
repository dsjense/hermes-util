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
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.lines
import types

def cur(multi=1,fig=None):
    if fig is not None:
        f = plt.figure(fig)
    else: f = plt.gcf()
    xc = f.ginput(n=multi)
    if multi == 1:
        return np.array(xc[0],dtype='f4',order='F')
    else:
        return np.array(xc,dtype='f4',order='F')

def pwin(num=None,close=False,**figargs):
    if close:
        if num is None:
            return plt.close()
        else:
            return plt.close(num)

    return plt.figure(num,**figargs)

def set_pmulti(*args):
    return _pmulti.set(*args)

def get_pmulti(*args):
    return _pmulti.get()

def show_pmulti():
    print _pmulti.get()

def adv_frame(*args,**kwargs):
    return _pmulti.advance(*args,**kwargs)

def set_frame_props(ax,lw,charsize,right=None):
    xax = ax.xaxis
    yax = ax.yaxis
    if right is None: right = ax.yaxis.label_position == 'right'
    bottom = ax.xaxis.label_position == 'bottom'
    if lw is not None:
        for k in ax.spines.itervalues(): k.set_linewidth(lw)
        for l in xax.get_ticklines():
            l.set_markersize(2*lw+2)
            l.set_markeredgewidth(lw)
        for l in yax.get_ticklines():
            l.set_markersize(2*lw+2)
            l.set_markeredgewidth(lw)
    if charsize is not None:
        ax.title.set_size(charsize)
        xax.label.set_size(charsize)
        yax.label.set_size(charsize)
        for t in xax.get_major_ticks():
            if bottom: t.label.set_fontsize(charsize)
            else:  t.label2.set_fontsize(charsize)
        for t in yax.get_major_ticks():
            if right: t.label2.set_fontsize(charsize)
            else: t.label.set_fontsize(charsize)
 
class _overplot:

    def __init__(self):
        self.x = None

    def set(self,x,y,znorm,xr,yr,ax,polar=False):
        self.x = x
        self.y = y
        self.znorm = znorm
        self.xr = xr
        self.yr = yr
        self.ax = ax
        self.polar = polar

class _multiplot:

    dflt = (0,1,1,0)

    def __init__(self):
        self.current = 0
        self.nrows = 1
        self.ncols = 1
        self.nplts = 1
        self.row_major = 0
        self.last_left = None

    def get(self):
        return ( self.current, self.ncols, self.nrows, self.row_major )

    def set(self,*args):
        rtuple = ( self.current, self.ncols, self.nrows, self.row_major )
        argc = len(args)
        #print argc
        #if argc > 0: print type(args[0])
        if argc == 1 and type(args[0]) == types.TupleType:
            t = args[0]
            argc = len(t)
        else:
            t = args
        self.current, self.ncols, self.nrows, self.row_major = \
            t[:argc] + _multiplot.dflt[argc:]

        self.nplts = self.ncols * self.nrows

        return rtuple

    def advance(self,leftRight=None, use_fig=None, polar=False):
        ##print 'advance:',leftRight,use_fig
        ax = None
        if use_fig is None:
            f = plt.gcf()
            nplt = self.current
            if leftRight != 'R':
                if nplt == 0:  f.clear()
                self.current += 1
            elif self.last_left is None:
                print 'RIGHT plot must follow a LEFT plot'
                return (None,None)

            if not self.row_major: cnt = self.current
            else:
                ic = nplt % self.ncols
                ir = nplt/self.ncols
                cnt = ir + self.nrows*ic + 1
            
            if polar: kwextra = dict(projection='polar')
            else: kwextra= {}
            if leftRight != 'R':
                ax = f.add_subplot(self.nrows,self.ncols,cnt,**kwextra)
                if leftRight == 'L':
                    ax.yaxis.tick_left()
                    self.last_left = ax
                self.current %= self.nplts
            else:
                last_ax = self.last_left
                self.last_left = None
                ax = f.add_subplot(self.nrows,self.ncols,cnt, sharex=last_ax,
                                   frameon=False,**kwextra)
                ax.yaxis.tick_right()
                ax.yaxis.set_label_position("right")
        else:  # use_fig is not None
            if type(use_fig) is types.TupleType:
                f = use_fig[0]
                if  len(use_fig) > 1: ax = use_fig[1]
            else:  f = use_fig
            if ax is None:  ax = f.gca()

        ##print 'out:', id(f),ax
        return (f,ax)


def get_property(indx, map=None):
    cols = map
    if map is None: cols = _color_list
    n = len(cols)
    return cols[indx % n]


def bld_map(key,ltype='color'):
    okay = True
    if ltype == 'color':  deflist = _color_list
    elif ltype == 'line':  deflist = _line_list
    if key is None: rmap = deflist
    else:
        nlist = len(deflist)
        t = type(key)
        if t is types.IntType:
            rmap = [deflist[key % nlist]]
        elif t is types.StringType:
            rmap = [key]
        elif t is types.ListType or t is types.TupleType:
            rmap = []
            for k in key:
                t = type(k)
                if t is types.IntType:
                    rmap.append(deflist[k % nlist])
                elif t is types.StringType:
                    rmap.append(k)
                else: okay = False    
        else: okay = False    
    if okay: return rmap
    else: return None



_color_list = [ 'k', 'r', 'g', 'b', 'c', 'm', 'y', 'orange' ]
_line_list  = [ '-', ':', '--', '-.' ]

try:
    _pmulti
except NameError:
    _pmulti = _multiplot()

try:
    _ovrplt
except NameError:
    _ovrplt = _overplot()
