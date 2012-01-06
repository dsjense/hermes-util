      subroutine p2derm
     &      ( cim,imax,jmax,kb,ke,kn,normal,er )
c
c
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
c***********************************************************************
c
c ----------------------------------------------------------------------
c
c     Summary:
c
c       - This routine modifies the relative dielectric constant when
c         Neumann boundary conditions are present so that the buffer cell
c         dielecrtic constant is the appropriate image of the dielectric
c         constant of the cell with the Neumann Boundary condition.
c         Since Dirichlet boundary conditions take precident over Neumann
c         boundary conditions, no modifactions are made if a Dirichlet
c         boundary condition is present.
c ----------------------------------------------------------------------
c
c     Input:
c       cim     -  cell information matrix (single block form)
c       er      -  relative dielectric constant array
c       imax    -  maximum grid index in i-direction
c       jmax    -  maximum grid index in j-direction
c       kb,ke   -  beginning and ending points for periodic and Neumann
c                  surfaces
c       kn      -  index of normal plane along Neumann and periodic
c                  surfaces
c       normal  -  normal direction indicator for Neumann and periodic
c                  surfaces
c
c ----------------------------------------------------------------------
c
c     Output:
c       er      -  relative dielectric constant array
c
c ----------------------------------------------------------------------
c
c
c     Declare variables:
c
c ... Passed variables:
c
      integer imax,jmax,kb,ke,kn,normal
      integer cim(0:imax,0:jmax)
      real er(0:imax,0:jmax)
c
c ... internal variables:
c
c ... loop indices:
      integer k
c ... scalars:
c
c ..... loop over surface
c
        do 51 k=kb,ke
c
c ....... normal in "i" direction
c
          if(abs(normal).eq.1)then
c
            if(cim(kn,k).eq.0)then
c.............no Dirichlet boundary condition
c
                if(normal.gt.0)then
c.. ............. BC at lower end of grid
                  er(0,k)=er(1,k)
                else
c ............... BC at upper end of grid
                  er(imax,k)=er(imax-1,k)
                endif
            endif
          else
c
c ....... normal in "j" direction
            if(cim(kn,k).eq.0)then
c.............no Dirichlet boundary condition
c
              if(normal.gt.0)then
c.. ............. BC at lower end of grid
                  er(k,0)=er(k,1)
              else
c ............... BC at upper end of grid
                  er(k,jmax)=er(k,jmax-1)
              endif
            endif
          endif
c
 51   continue
      return
      end
