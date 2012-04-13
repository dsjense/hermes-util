      program tfloat
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
C_Groups main standalone
c***********************************************************************
c
      integer   ierr, ipval, ioff10, loop, k, pf2max, pf2min
      real      fvalm, fvals, fvalb, xmult, pfrmin, prec
      integer   ival(3),jval(3),kval(3)
c
      ierr = 0
c
      ival(1) = 0
      ival(2) = 0
c
      jval(1) = 32767
      jval(2) = 32767
c
      kval(1) = 10922
      kval(2) = 21845
c
      call pf_flt_limits(pf2max, pf2min, pfrmin)
c
      print *,'Current Macro Values:'
      print *,'MAX_EXP: ',PF2MAX
      print *,'MIN_EXP: ',PF2MIN
      print *,'FLT_MIN: ',PFRMIN

c
   10 continue
c
        print*,'Enter PF2 parameter value'
        read(*,*,end=20) ipval
        ival(3) = (ipval + 8192)*2
        jval(3) = ival(3)
        kval(3) = ival(3)
c
        call pfui2f (.FALSE.,kval,fvalm,ioff10,ierr)
        if (ierr.ne.0)then
          print *,' ui2f min error = ',ierr
          ierr = 0
        endif
c
        call pfui2f (.FALSE.,ival,fvals,ioff10,ierr)
        if (ierr.ne.0)then
          print *,' ui2f min error = ',ierr
          ierr = 0
        endif
c
        call pfui2f (.FALSE.,jval,fvalb,ioff10,ierr)
        if (ierr.ne.0)then
          print *,' ui2f max error = ',ierr
          ierr = 0
        endif
c
        loop = abs(ipval)
c
        if (ipval.le.0) then
          xmult = 2.0
        else
          xmult = 0.5
        endif
c
        prec = fvalm
        do 22 k=1,loop
          prec = prec*xmult
   22   continue
c
c       prec = prec + 9.313225746e-10
        prec = prec*1.50
c
        print *,'Decoded MAX value = ',fvalb
        print *,'Decoded MIN value = ',fvals
        print *,'Precision     = ',prec
        if ( ipval.gt.0 ) then
          prec = 1.0/fvalb
          print *,'1.0/MAX value     = ',prec
          if (prec.ne.0) then
            print *,'1/(1/MAX) value   = ',1.0/prec
          else
            print *,'1/(1/MAX) value   = INF'
          endif
        endif
c
      goto 10
c
   20 continue
c
      stop
      end
