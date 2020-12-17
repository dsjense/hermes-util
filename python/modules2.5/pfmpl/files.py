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
import pff

def openpff(file,mode="rw"):
    return pff.open(file,mode)

def closepff(id=0):
    return pff.close(id)

def setpff(id=0):
    return pff.set(id)

def dirpff(id=0, range=(1,-1), match="",width=80):
    return pff.dir(id,range,match,width)

def showpff(range=(1,-1),width=80):
    return pff.show(range,width)

