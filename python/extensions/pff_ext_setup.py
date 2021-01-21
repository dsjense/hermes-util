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
# Setup file for building pff_ext extension module
#

from __future__ import print_function

from setuptools import setup, Extension

import os,sys,re

def bld_def_list():
    reg = re.compile('\$\(BLDDIR\)')
    f = open("pythonext_dep_list")
    fline = True
    while fline:
        fline = f.readline()  ;  line = fline.strip()
        ##print line, reg.match(line)
        if reg.match(line) is not None: break

    ##print line
    toks = line.split(' ')
    last = toks[-1] != '\\'
    if not last: toks.pop()
    deps = toks[2:]
    while not last:
        line = f.readline().strip()
        toks = line.split(' ')
        last = toks[-1] != '\\'
        if not last: toks.pop()
        deps.extend(toks)
    f.close()
    ##print deps
    return deps

def get_shr_ext(sys_type):
    hr = os.getenv('HERMES_ROOT')
    mkopt = os.path.join(hr,'etc','makeopts.'+sys_type)
    f = open(mkopt,'r')
    buf = f.read()
    f.close()
    ist = buf.find('SHR_EXT')
    if ist < 0: return 'so'
    inl =  buf.find('\n',ist)
    return buf[ist:inl].split(' ')[-1]

if __name__ == '__main__':
    hl = os.getenv('HERMES_LIB')
    hst = os.getenv('HERMES_SYS_TYPE')
    incdir = os.path.join(hl,'pffc')
    libdir = os.path.join(hl,'debug',hst)

    ##verstr = '.'.join([str(i) for i in sys.version_info[0:2]])
    deps = bld_def_list()
    print(len(deps), 'dependencies found')
    ##print len(deps), deps
    defines = [('PY_MAJOR_VERSION',sys.version_info[0])]
    ##print('defines:',defines)
    
    setup(name="pff_ext",version="1.0",
          ext_modules=[Extension("pff_ext",["pff_ext.cc"],
                                 include_dirs=[incdir],
                                 library_dirs=[libdir],
                                 libraries=["pffc_pic"],
                                 undef_macros=['NDEBUG'],
                                 define_macros=defines,
                                 depends=deps,
                                 extra_compile_args=[])
                       ])
