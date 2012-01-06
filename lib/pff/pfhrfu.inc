c***********************************************************************
c     $Id$
c     
c     Copyright (2008) Sandia Corporation. Under the terms of
c     Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
c     Government retains certain rights in this software.
c     
c     Hermes is free software: you can redistribute it and/or modify
c     it under the terms of the GNU Lesser General Public License as
c     published by the Free Software Foundation, either version 3 of
c     the License, or (at your option) any later version.
c     
c     Hermes is distributed in the hope that it will be useful, but
c     WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU Lesser General Public License for more details.
c     
c     You should have received a copy of the GNU Lesser General
c     Public License along with Hermes.  If not, see
c     <http://www.gnu.org/licenses/>.
c     
C_Groups @(#)
c ----------------------------------------------------------------------
c
c   Summary:
c
c     Include file for storing the "Reserved-for-Future-Use (RFU) array
c     that has been read from the PFF file header.
c
c ----------------------------------------------------------------------
c
c   Parameters needed in this module, but not supplied:  None
c
c ----------------------------------------------------------------------
c
c   Explanation of parameters and common-block variables:
c
c     RFUMAX  -  dimensioned space for RFU values
c     rfunum  -  # of non-default values in array from last read
c     rfuval  -  array of values from last read
c
c ----------------------------------------------------------------------
c
c   Declare and set any necessary parameters:
c
      integer   RFUMAX
      parameter ( RFUMAX = 10 )
c
c   Declare variables and define the common block:
c
      integer  rfunum, rfuval(RFUMAX)
c
      common   /pfhrfu/ rfunum, rfuval
c
c***********************************************************************
