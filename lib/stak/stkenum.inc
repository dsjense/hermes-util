c
c ----------------------------------------------------------------------
c   $Id$
c   
c   Copyright (2008) Sandia Corporation. Under the terms of
c   Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
c   Government retains certain rights in this software.
c   
c   Hermes is free software: you can redistribute it and/or modify
c   it under the terms of the GNU Lesser General Public License as
c   published by the Free Software Foundation, either version 3 of
c   the License, or (at your option) any later version.
c   
c   Hermes is distributed in the hope that it will be useful, but
c   WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU Lesser General Public License for more details.
c   
c   You should have received a copy of the GNU Lesser General
c   Public License along with Hermes.  If not, see
c   <http://www.gnu.org/licenses/>.
c   
C_Groups @(#)
c ----------------------------------------------------------------------
c> \file stkenum.inc
c> \brief Enumeration parameters for STAK memory management software.
c
c ----------------------------------------------------------------------
c
c> \name Enumeration of pointer types supported
c> \note If new values are added to this enumeration list:
c>       \li the parameter LASTPTR in stak.inc must be changed.
c>       \li stkinit.F must be changed to initialize the new entries in 
c>           the typebyts and alignbyts arrays.
c
c> \{
c>  indicates pointer to integer
      integer INTPTR 
c>  indicates pointer to real
      integer FLTPTR
c>  indicates pointer to integer*2
      integer I2PTR
c>  indicates pointer to integer*4
      integer I4PTR
c>  indicates pointer to logical
      integer LOGPTR
c>  indicates pointer to logical*1
      integer L1PTR
c>  indicates pointer to double precision
      integer DBLPTR
c>  indicates pointer to real*4
      integer R4PTR
c>  indicates pointer to real*8
      integer R8PTR
c>  indicates pointer to pointer
      integer PTRPTR
c
c> \}
      parameter ( INTPTR = 0  , FLTPTR = -1 , I2PTR = -2  , I4PTR = -3 , 
     &            LOGPTR = -4 , L1PTR = -5  , DBLPTR = -6 , R4PTR = -7 ,
     &            R8PTR  = -8 , PTRPTR = -9 )
c
c>  minimum logical unit number for specifying error messages (<= MINLU will
c>  result in suppression of messages)
      integer MINLU
      parameter ( MINLU = -1 )
c
