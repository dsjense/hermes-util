##  #!/usr/bin/env python
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
# This script finds all the modules, packages, and extension modules in the
# Hermes Python directory tree constructs "make" dependencies for generating
# HTML documentation
#

import sys,os,pydoc

maj_ver = sys.version_info[0]
if maj_ver < 3:
    from PortabilityTools2_5 import ptPrint
else:
    from PortabilityTools import ptPrint

import chkversion
chkversion.checkExtensionPath()

argc = len(sys.argv)

if argc < 2 or argc > 4:
    ptPrint('Usage: ' + os.path.basename(sys.argv[0]) + \
            ' module [outfile] [v2.5]')
    sys.exit(1)
elif argc == 2:
   module = sys.argv[1]
   out = None
elif argc >= 3:
   module = sys.argv[1]
   out = sys.argv[2]

if argc == 4: chkversion.setPathForPre26Version()

##ptPrint('calling writedoc for', module)
pydoc.writedoc(module)
##ptPrint('finished calling writedoc')

if out is not None:
    hname = module + '.html'
    ##ptPrint(hname,out)
    os.rename(hname,out)

sys.exit(0)
