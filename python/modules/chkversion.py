# $Id$
# 
# Copyright (2019) David Seidel.
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

__doc__ = \
'''Utilities for checking if various Hermes directories in sys.path are
appropriate for the version of python currently being used.'''

import sys, os
import re as regex

# deal with differences Version 2 print statement and Version 3 print function 
vi = list(sys.version_info)
py_maj_ver = vi[0]
if py_maj_ver < 3:
    from PortabilityTools2_5 import ptPrint
else:
    from PortabilityTools import ptPrint


def checkExtensionPath():
    '''\
Function to check if Python's module search path will find the hermes
pff extension module (pff_exe) appropriate to the version of python
currently being run, and if it is not, fix it as needed.
'''

    hroot = os.getenv("HERMES_ROOT")
    if hroot is None:
        ptPrint('Unknown Hermes root')
        return

    sep = os.path.sep
    exdir = os.path.join(hroot,'python','extensions') + sep
    r =  regex.compile(exdir + '.*-(\d\.\d)$')

    for i,a in enumerate(sys.path):
        m = r.search(a)
        if m is not None:
            old = m.group(1)
            new = '.'.join([str(j) for j in vi[:2]])
            ##ptPrint('found:',i,a,old,new)
            if old != new:
                newdir = a.replace(old,new)
                if os.path.isdir(newdir):
                    sys.path[i] = newdir
                    ##ptPrint('replacing',a,'with',newdir)
                else:
                    msg = 'WARNING: PFF extension module directory not found: '\
                          +  newdir
                    ptPrint(msg)
            
    
def setPathForPre26Version():
    '''\
Function to set Python's module search path to include the hermes
pre-2.6 legacy module directory, "modules2.5", rather than the
standard post-2.5 module directory, "modules".'''

    hroot = os.getenv("HERMES_ROOT")
    if hroot is None:
        ptPrint('Unknown Hermes root')
        return

    sep = os.path.sep
    pdir = os.path.join(hroot,'python') + sep
    rmod = regex.compile(pdir + '(modules)('+sep+'|$)')

    for i,a in enumerate(sys.path):
        m = rmod.match(a)
        if m is not None:
            s1 = a[:m.start(1)]
            s2 = a[m.end(1):]
            newdir = s1 + 'modules2.5' + s2
            ##ptPrint('found mod:',i,a,newdir)
            if os.path.isdir(newdir):
                sys.path[i] = newdir
            else:
                msg = 'WARNING: PFF v2.5 module directory not found: '\
                    +  newdir
                ptPrint(msg)


def checkForPre26Version():
    '''\
Function to check if Python's module search path will find the hermes
"modules" directory appropriate to the version of python currently being run,
and if it is not, fix it as needed. Specifically, for python versions earlier
than 2.6, sets the path to look in the pre-2.6 legacy module directory'''

    py_ver = vi[2] + 100*(vi[1] +100*vi[0])
    if py_ver < 20600: setPathForPre26Version()

def getPythonExecutables():

    py_exe_list =  os.getenv("HERMES_PYTHON_TARGETS").split(':')
    py_ver_list =  os.getenv("HERMES_PYTHON_VERSIONS").split(':')

    ###py_ver_list = ['20503','20703','20801','20902','30701','30205']
    ###py_exe_list = \
    ###['python2.5','python2.7','python2.8','python2.9','python3.7','python3.2']
    first_3 = 30000
    first_2 = 20800  ##fixme
    ver2exe = None
    ver25exe = None
    ver3exe = None
    ver25 = ver2 = ver3 = 0
    for i,ver in enumerate(py_ver_list):
        iver = int(ver)
        ##ptPrint(i,ver,iver)
        if iver < first_2 and iver > ver25:
            ver25 = iver
            ver25exe = py_exe_list[i]  ##fixme
        if iver < first_3 and iver >= first_2 and iver > ver2:
            ver2 = iver
            ver2exe = py_exe_list[i]
        if iver >= first_3 and iver > ver3:
            ver3 = iver
            ver3exe = py_exe_list[i]

    return (ver25exe, ver2exe, ver3exe)
