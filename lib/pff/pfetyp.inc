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
c     Include file containing enumeration parameters for PFF's raw
c     dataset types.
c
c ----------------------------------------------------------------------
c
c   Explanation of the parameters:
c
c     PFNTYP  -  Maximum # of dataset types; type # "PFNTYP" is always
c                the "unknown" dataset type
c     PFTDIR  -  dataset type # of DIR (directory)
c     PFTIFL  -  dataset type # of IFL (Integer/Float List)
c     PFTNF3  -  dataset type # of NF3 (non-uniform, 3D, Flt.-point data)
c     PFTNG3  -  dataset type # of NG3 (non-uniform, 3D, Grid data)
c     PFTNGD  -  dataset type # of NGD (nD vectors on 1-block mD space)
c     PFTNV3  -  dataset type # of NV3 (non-uniform, 3D, Vector Flt. data)
c     PFTUF1  -  dataset type # of UF1 (uniform, 1D, Floating-point data)
c     PFTUF3  -  dataset type # of UF3 (uniform, 3D, Floating-point data)
c     PFTVTX  -  dataset type # of VTX (mD Vertex Data w/ nD attributes)
c     PFTNI3  -  dataset type # of NI3 (non-uniform, 3D, Integer data)
c     PFTUNK  -  unknown dataset type
c
c ----------------------------------------------------------------------
c
c   Declare and set the parameters:
c
      integer   PFNTYP, PFTUNK
      parameter ( PFNTYP = 10 , PFTUNK = PFNTYP )
c
      integer   PFTDIR, PFTIFL, PFTNF3, PFTNV3, PFTUF1, PFTUF3, PFTVTX,
     1          PFTNGD, PFTNG3, PFTNI3
      parameter ( PFTDIR = 0 , PFTUF3 = 1 , PFTUF1 = 2 , PFTNF3 = 3 )
      parameter ( PFTNV3 = 4 , PFTVTX = 5 , PFTIFL = 6 , PFTNGD = 7 )
      parameter ( PFTNG3 = 8 , PFTNI3 = 9)
c
c***********************************************************************
