from __future__ import print_function, division

import os

__doc__ = \
'''Portable versions of various python functions. They fall into two
categories:
   1. Python 3 functions that were implemented as statements in python 2.
      This module provides versions that work in python 3.
   2. Library modules that moved or changed their behavior between python 2
      and python 3. A version-neutral compromise function is chosen that
      can be implemented in both versions 2 and 3.'''

def ptPrint(*args,**kwargs):
    '''\
Implementation of a portable version-3-syntax print statement'''

    print(*args,**kwargs)

def pathWalk(path, visitfunc, arg):
    '''\
Implementation of a portable version-2-syntax path.walk function'''

    for root,dirs,files in os.walk(path):
        visitfunc(arg, root, files)

def ptExec(*_strng):
    '''\
Implementation of a portable compromise between version-2 exec and version-3
exec. This method takes a string containing a python command and returns a
directory containing the local namespace of the exec. This allows the caller
to access any variables set by the command string. For example,
     var1 = ptExec('var1 = 45.5')['var1']'''

    exec(*_strng)
    del _strng
    return locals()
