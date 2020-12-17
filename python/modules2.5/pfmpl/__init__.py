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
import exceptions

__doc__ = \
'''Set of PFMPL functions for reading, writing, plotting, and otherwise
manipulating PFF datasets.'''

class PFMPL_Error(exceptions.Exception):
    def __init__(self, value="PFMPL error"):
        self.value = value
    def __str__(self):
        return repr(self.value)

_load_list = [ '_1d',  '_2d', 'files', 'plots' ]

import os.path

_pkg_name = os.path.basename(__path__[0])
for _mod in _load_list:
    ##print 'from %s.%s import *' % (_pkg_name, _mod)
    exec 'from %s.%s import *' % (_pkg_name, _mod)
