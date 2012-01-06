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
c
c     Enumeration parameters for STAK memory management software.
c
c ----------------------------------------------------------------------
c
c     Parameters
c       DBLPTR   -  indicates pointer to double precision
c       FLTPTR   -  indicates pointer to real
c       INTPTR   -  indicates pointer to integer
c       I2PTR    -  indicates pointer to integer*2
c       I4PTR    -  indicates pointer to integer*4
c       LOGPTR   -  indicates pointer to logical
c       L1PTR    -  indicates pointer to logical*1
c       R4PTR    -  indicates pointer to real*4
c       R8PTR    -  indicates pointer to real*8
c       PTRPTR   -  indicates pointer to pointer
c
c       NOTE:  if new values are added to this enumeration list, the 
c              parameter LASTPTR in stak.inc must be changed.  In 
c              addition, stkinit.f must be changed to initialize the 
c              new entries in the typebyts and alignbyts arrays.
c
c       MINLU    -  logical unit numbers specified for error messages
c                   ( <= MINLU will result in suppression of messages)
c
c ----------------------------------------------------------------------
c
      integer INTPTR, FLTPTR, I2PTR, I4PTR, LOGPTR, L1PTR, DBLPTR, 
     &        R4PTR, R8PTR, PTRPTR
      parameter ( INTPTR = 0  , FLTPTR = -1 , I2PTR = -2  , I4PTR = -3 , 
     &            LOGPTR = -4 , L1PTR = -5  , DBLPTR = -6 , R4PTR = -7 ,
     &            R8PTR  = -8 , PTRPTR = -9 )
c
      integer MINLU
      parameter ( MINLU = -1 )
c
