import exceptions
import types
import os.path

__doc__ = \
'''Portable versions of various python 3 functions that were implemented
as statements in python 2. This module provides versions that work in
python 2.'''

def ptPrint(*args,**kwargs):
    '''\
Implementation of a portable version-3-syntax print statement, but using
version 2 syntax'''

    end = '\n'
    sep = ' '
    f = None
    if kwargs.has_key('sep'):
        sep = kwargs['sep']
        del kwargs['sep']
    if kwargs.has_key('end'):
        end = kwargs['end']
        del kwargs['end']
        # get rid of trailing ' ', since print statement will add it on anyway
        if len(end) > 0 and end[-1] == ' ': end = end[:-1]
    if kwargs.has_key('file'):
        f = kwargs['file']
        del kwargs['file']
    if len(kwargs) > 0:
        raise exceptions.TypeError, \
            "'" + kwargs.keys()[0] + "' is an invalid keyword argument for pt()"

    s = ''
    for arg in args: s += str(arg) + sep
    if len(sep) > 0: s = s[:-1] + end # get rid of last 'sep' and add 'end'
    if f is None:
        ##print 'to stdout'
        print s,
    else:
        # Note print statement will trap errors if f is not an open file
        ##print 'to',f.name
        print >>f, s,

def pathWalk(path, visitfunc, arg):
    '''\
Implementation of a portable version-2-syntax path.walk function'''

    os.path.walk(path, visitfunc, arg)

def ptExec(_strng):
    '''\
Implementation of a portable compromise between version-2 exec and version-3
exec. This method takes a string containing a python command and returns a
directory containing the local namespace of the exec. This allows the caller
to access any variables set by the command string. For example,
     var1 = ptExec('var1 = 45.5')['var1']'''

    exec _strng
    del _strng
    return locals()
