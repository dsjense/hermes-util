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
      implicit none
c
      integer   ipval, k, cnt
      real      fvalm, fvals, fvalb, xmult, prec, rmaxinv, del
      logical   needchk, fail, prec_bad, rmin_bad, exist, doprint
      logical   rmax_bad
c
      integer pf2min, pf2max
      real pfrmin, pfrmax
      common /pfstuff/ pf2min, pf2max, pfrmin, pfrmax

      real getfloat

      call pf_flt_limits(pf2max, pf2min, pfrmin, pfrmax)
c
      print *,'Current Float Limits:'
      print *,'  MAX_EXP: ',pf2max
      print *,'  MIN_EXP: ',pf2min
      print 300,'  FLT_MIN: ',pfrmin,pfrmin
      print 300,'  FLT_MAX: ',pfrmax,pfrmax
c
      doprint = .FALSE.
      inquire(file='CHG_TFLOAT_MAX',exist=exist)
      if (exist) then
        print'(a,i3,a,$)','Enter new value for MAX_EXP (<',pf2max,'): '
        read *,pf2max
        doprint = .TRUE.
      endif
c
c
      fail = .FALSE.
      ipval = pf2max
      xmult = 0.5
      needchk = .TRUE.
      cnt = 0
      do while ( needchk )
c
        fvalm = getfloat(ipval,10922,21845)
        fvals = getfloat(ipval,0,0)
        fvalb = getfloat(ipval,32767,32767)
c
        prec = fvalm
        do k=1,ipval
          prec = prec*xmult
        end do
c
        prec = prec*1.50
c
        prec_bad = prec.ne.1.0
        rmaxinv = 1.0/fvalb
        rmin_bad = rmaxinv.gt.pfrmin
        rmax_bad = fvalb.gt.pfrmax

        if ( prec_bad .OR. rmin_bad .OR. rmax_bad .OR. 
     &       ipval.ne.pf2max .OR. doprint) then
          print *,'EXP value:',ipval
          if ( rmax_bad ) then
            print 400,'Decoded MAX value = ',fvalb,fvalb,' FAIL'
            fail = .TRUE.
          else
            print 300,'Decoded MAX value = ',fvalb,fvalb
          endif
          print 300,'Decoded MIN value = ',fvals,fvals
          print *,'Precision     = ',prec
          print 300,'1.0/MAX value     = ',rmaxinv,rmaxinv
          if (rmaxinv.ne.0) then
            print *,'1/(1/MAX) value   = ',1.0/rmaxinv
          else
            print *,'1/(1/MAX) value   = INF'
          endif
          if ( prec_bad ) then
            print *,'PRECISION - 1: ',prec - 1.0,' FAIL'
            fail = .TRUE.
          endif
          if ( rmin_bad ) then
            del = pfrmin - rmaxinv
            if (del.eq.0.0) then
              print 200, del,pfrmin,rmaxinv
            else
              print 100, del
            endif
            fail = .TRUE.
          endif
          ipval = ipval - 1
          cnt = cnt + 1
          if (cnt.gt.5) then
            needchk = .FALSE.
            print *,'**** SANITY CHECK ****'
          endif
        endif
        if (.NOT. prec_bad) needchk = .FALSE.
      end do
c
      ipval = pf2min
      xmult = 2.0
      needchk = .TRUE.
      cnt = 0
      do while ( needchk )
c
        fvalm = getfloat(ipval,10922,21845)
        fvals = getfloat(ipval,0,0)
        fvalb = getfloat(ipval,32767,32767)
c
        prec = fvalm
        do k=1,-ipval
          prec = prec*xmult
        end do
c
        prec = prec*1.50
c
        prec_bad = prec.ne.1.0
        rmin_bad = fvals.gt.pfrmin

        if ( prec_bad .OR. rmin_bad .OR.
     &       ipval.ne.pf2min .OR. doprint ) then
          print *,'EXP value:',ipval
          print 300,'Decoded MAX value = ',fvalb,fvalb
          print 300,'Decoded MIN value = ',fvals,fvals
cc          print *,'Decoded MIN value = ',fvals, pfrmin - fvals
          print *,'Precision     = ',prec, prec - 1.0
          if ( prec_bad ) then
            print *,'PRECISION - 1: ',prec - 1.0,' FAIL'
            fail = .TRUE.
          endif
          if ( rmin_bad ) then
            del = pfrmin - fvals
            if (del.eq.0.0) then
              print 200, del,pfrmin,fvals
            else
              print 100, del
            endif
            fail = .TRUE.
          endif
          ipval = ipval + 1
          cnt = cnt + 1
          if (cnt.gt.5) then
            needchk = .FALSE.
            print *,'**** SANITY CHECK ****'
          endif
        endif
        if (.NOT. prec_bad) needchk = .FALSE.
      end do

      if (fail) then
        print *,'**** TFLOAT FAILED ****'
      else
        print *,'TFLOAT PASSED'
      endif

 100  format(' FLT_MIN - 1.0/MAX:',1pe17.9,'  FAIL')
 200  format(' FLT_MIN - 1.0/MAX:',1pe17.9,'  ',2z10,'  FAIL')
 300  format(a,1pe15.8,'  0x',z8.8)
 400  format(a,1pe15.8,'  0x',z8.8,a)

      end

      real function getfloat(i2,a1,a2)

      implicit none

      integer i2, a1, a2

      double precision RTWO15
      parameter ( RTWO15 = 1.d0/32768.0d0 )

      integer i2m, i2x, i2rest
      real xsign, twooff

      integer pf2min, pf2max
      real pfrmin, pfrmax
      common /pfstuff/ pf2min, pf2max, pfrmin, pfrmax

c     This is the basics of the algorithm from PFUI2F
      xsign = 1.0
      i2m   = i2 - 1
      i2x    = max( i2m, pf2min + 2 )
      i2rest = i2m - i2x
      xsign  = xsign*(2.**i2rest)
      twooff = 2.**i2x
      getfloat = xsign*( ( (a2*RTWO15 + a1)*RTWO15 + 1.d0 ) * twooff )

      return
      end
