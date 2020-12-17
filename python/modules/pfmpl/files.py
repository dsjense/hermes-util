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
    '''\
Opens a PFF file

Usage:
  openpff( file, [mode=string] )

Arguments:
  file:  String containing name of file to be opened.
  mode:  Mode in which file is to be opened. Valid values are:
             "r"  for read-only access (default)
             "w"  for write-only access
             "rw" for read-write access

Return Value:  integer ID index associated with file, or
               None, if an error occurred'''

    return pff.open(file,mode)

def closepff(id=0):
    '''\
Closes a PFF file

Usage:
  closepff( id )

Arguments:
  id:  If positive, integer ID index of file to be closed. Otherwise,
       all open files are closed. Default value: 0

Return Value: Number of files closed, or None, if an error occurred'''

    return pff.close(id)

def setpff(id):
    '''\
Sets the current active PFF file.

Usage:
  setpff(id)

Arguments:
  id: Integer ID index of file to become the active file.

Return value: None'''

    return pff.set(id)

def dirpff(id=0, range=(1,-1), match="",width=80):
    '''\
Print list of datasets in an open PFF file to terminal.

Usage:
  dirpff( [id=int], [range=tuple, [match=str], [width=int])

Arguments:
  id:    Integer ID index of file whose datasets are to be listed.
         If id <= 0, current active file is assumed. Default: 0
  range: Tuple containing first and last dataset index to be listed.
         If second index is <= 0, highest index of last dataset is
         used. If not supplied, all datasets in file  are listed.
  match: String used to match the titles of the datasets to be
         listed. Limited wildcarding is supported. The following
         characters have special meaning when encountered in the
         search string:
           * is matched by 0 to n characters
           ? is matched by exactly 1 character
           ^ as a first character anchors the match to the
             beginning of the comment substring
           $ as a final character anchors the match to the end
             of the comment substring
           \ escapes * and ? to be used literally anywhere in 
             the search string and escapes ^ (and $) at the
             beginning only (and end only) of the search string
             to force ^ (or $) to be interpreted literally.
         Default: "", which matches all dataset titles
  width: Maximum width of listing, in characters. Default: 80

  Return value:  None'''

    return pff.dir(id,range,match,width)

def showpff(range=(1,-1),width=80):
    '''\
Print list of open PFF files to terminal.

Usage:
  showpff( [range=tuple], [width=int] )

Arguments:
  range:   Tuple containing first and last file ID index to be listed.
           If second index is <= 0, highest index in use is used. If
           not supplied, all open files are listed. Default: (1,-1)
  width:   Maximum width of listing, in characters

Return value: None if error parsing arguments, or
              0, indicates success, or
              otherwise, some other error occurred'''
    return pff.show(range,width)

