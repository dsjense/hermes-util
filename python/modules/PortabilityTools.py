from __future__ import print_function, division

__doc__ = \
'''Portable versions of various python 3 functions that were implemented
as statements in python 2. This module provides versions that work in
python 3'''

def ptPrint(*args,**kwargs):
    '''\
Implementation of a portable version-3-syntax print statement'''
    print(*args,**kwargs)
