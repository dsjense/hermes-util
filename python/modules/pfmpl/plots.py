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
from __future__ import print_function, division

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.lines

__doc__ = \
'''Set of PFMPL functions for managing plots, including multiple axes per
figure, and obtaining coordinates from a "cursor click".'''

def cur(multi=1,fig=None):
    '''\
Function to select x,y data points from a figure using the cursor.

Usage:
  cur( multi=1, fig=None )

Arguments:
  multi:  Number of points to be obtained
  fig:    Matplotlib figure object to be sampled. If not specified, the current
          active figure will be used.

Return value: A numpy float ndarray containing the x,y data pairs.
              If multi=1, the returned array is 1D with 2 elements (x and y),
              if multi>1, it is 2D with shape (multi,2)'''

    if fig is not None:
        f = plt.figure(fig)
    else: f = plt.gcf()
    xc = f.ginput(n=multi)
    if multi == 1:
        return np.array(xc[0],dtype='f4',order='F')
    else:
        return np.array(xc,dtype='f4',order='F')

def pwin(num=None,close=False,**figargs):
    '''\
Function to open or close matplotlib figure windows.

Usage:
  pwin(num=None,close=False,**figargs)

Arguments:
  num:   Number of figure to be opened/closed. For newer versions of matplotlib
         that support it, it can be a string that will be the name of the
         window. If `close' is False and the figure window is already open, it
         is made the current active window.
  close: If True, the window specified by `num' will be closed, or if `num' is
         not specified, the current window is closed.

Note: Any other keyword arguments are passed on to the matplotlib.pyplot.figure
      method.

Return value: If close is False, returns the matplotlib.figure.Figure object.
              Otherwise, None is returned.'''

    if close:
        if num is None:
            return plt.close()
        else:
            return plt.close(num)

    return plt.figure(num,**figargs)

def set_pmulti(*args):
    '''\
Function to set the value of the "pmulti" variable, which controls multi-axis
plotting. It emulates the behavior of IDL's !pmulti system variable.

Usage:
  set_pmulti(current=0, Nrow=1, Ncol=1, rowMajor=0)
   or
  set_pmulti(tuple)

Arguments:
  current:  The index of the next axis to be plotted, where 0 the upper-left
            axis, and Nrow*Ncol-1 is the index of the the lower-right axis. If
            rowMajor evaluates as True, successive plots first move from
            right-to-left before moving down to the next row. If False,
            sucessive plots move down the column before moving to the next
            column.
  Nrow:     The number of rows of axes for the figure
  Ncol:     The number of columns of axes for the figure
  rowMajor: The order in which axes are used for successive plot commands.
  tuple:    A tuple with length 0 to 4, whose elements are interpreted as
            (current, Nrow, Ncol, rowMajor). If less than 4 elements, missing
            values are interpreted as (0,1,1,0).

Returns: A tuple containing the original "pmulti" variable state:
         (current, Nrow, Ncol, rowMajor)'''

    return _pmulti.set(*args)

def get_pmulti(*args):
    '''\
Function to retrieve the value of the "pmulti" variable, which controls
multi-axis plotting. It emulates the behavior of IDL's !pmulti system variable.

Usage:
  get_pmulti()

Arguments: None

Returns: A tuple containing the current "pmulti" variable state, i.e.:
         (current, Nrow, Ncol, rowMajor)'''

    return _pmulti.get()

def show_pmulti():
    '''\
This function print the value of the "pmulti" variable, which controls
multi-axis plotting. It emulates the behavior of IDL's !pmulti system variable.

Usage:
  show_pmulti()

Arguments: None

Returns: None'''

    print(_pmulti.get())

def adv_frame(*args,**kwargs):
    '''\
Function which must be used to advance the current axes index using the pmulti
functionality, returning a figure object and an axes object to be used for the
next plot.

Usage:
  adv_frame(leftRight=None, use_fig=None, polar=False)

Arguments:
  leftRight: If leftRight is 'L', the axes is assumed to have only a left
             vertical axis, in which case the next call to this function must
             be with `leftRight' = 'R' indicating a plot with only a right
             vertical axis. If `leftRight' is None, a regular axis will be
             created.
  use_fig:   If None, the current figure will be used to create the subplot
             axis, in accordance with the current "pmulti" state. Otherwise,
             the "pmulti" framework is not used, and `use_fig' is used to
             determine the figure and axes returned. If a tuple, the first
             element is the figure object, and the second, if provided, is the
             axis object. If not a tuple, it is simply the figure object.
  polar:     If true, the created axes will be a polar projection (theta,r).

  Returns: A tuple containing the figure object and axes to be used for the
           next plot.'''

    return _pmulti.advance(*args,**kwargs)

def set_frame_props(ax,lw,charsize,right=None):
    '''\
Function to modify the line width and character size of an axes object.

Usage:

  set_frame_props(ax,lw,charsize,right=None)

Arguments:
  ax:        Axes whose properties are to be set.
  lw:        New line width, in pixels.
  charsize:  New character size, in points.
  right:     If True, modifies the right axis label; if False, modifies the
             left axis label. If None, it checks position of the y-axis label
             to determine which label to modify.

Returns: None'''

    xax = ax.xaxis
    yax = ax.yaxis
    if right is None: right = ax.yaxis.label_position == 'right'
    bottom = ax.xaxis.label_position == 'bottom'
    if lw is not None:
        for k in ax.spines.values(): k.set_linewidth(lw)
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
        self.polar = False

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
        #print(argc)
        #if argc > 0: print(type(args[0]))
        if argc == 1 and type(args[0]) == tuple:
            t = args[0]
            argc = len(t)
        else:
            t = args
        self.current, self.ncols, self.nrows, self.row_major = \
            t[:argc] + _multiplot.dflt[argc:]

        self.nplts = self.ncols * self.nrows

        return rtuple

    def advance(self,leftRight=None, use_fig=None, polar=False):
        ##print('advance:',leftRight,use_fig,self.current)
        ax = None
        if use_fig is None:
            f = plt.gcf()
            nplt = self.current
            if leftRight != 'R':
                if nplt == 0:  f.clear()
                ##self.current += 1
            elif self.last_left is None:
                print('RIGHT plot must follow a LEFT plot')
                return (None,None)

            if self.row_major: cnt = self.current + 1
            else:
                ic = nplt % self.ncols
                ir = nplt//self.ncols
                cnt = ir + self.nrows*ic + 1
            
            if polar: kwextra = dict(projection='polar')
            else: kwextra= {}
            if leftRight != 'R':
                ##print('advframe:',self.nrows,self.ncols,cnt,kwextra)
                ax = f.add_subplot(self.nrows,self.ncols,cnt,**kwextra)
                ##print('notR:',ax,self.nrows,self.ncols,self.current,cnt)
                if leftRight == 'L':
                    ##print('uuu',self.last_left,kwextra,self.nrows,self.ncols,cnt)
                    ax.yaxis.tick_left()
                    self.last_left = ax
                ##self.current %= self.nplts
            else:
                last_ax = self.last_left
                self.last_left = None
                if cnt == 0: cnt = self.nplts
                ##print('xxx',last_ax,kwextra,self.nrows,self.ncols,cnt)
                ax = f.add_subplot(self.nrows,self.ncols,cnt, sharex=last_ax,
                                   frameon=False,**kwextra)
                ##print('R or None:',ax,self.nrows,self.ncols,self.current,cnt)
                ax.yaxis.tick_right()
                ax.yaxis.set_label_position("right")

            if leftRight != 'L': self.current  = (self.current + 1) % self.nplts

        else:  # use_fig is not None
            if type(use_fig) is tuple:
                f = use_fig[0]
                if  len(use_fig) > 1: ax = use_fig[1]
            else:  f = use_fig
            if ax is None:  ax = f.gca()

        ##print('out:', id(f),ax,self.current,self.nplts)
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
        if t is int:
            rmap = [deflist[key % nlist]]
        elif t is str:
            rmap = [key]
        elif t is list or t is tuple:
            rmap = []
            for k in key:
                t = type(k)
                if t is int:
                    rmap.append(deflist[k % nlist])
                elif t is str:
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
