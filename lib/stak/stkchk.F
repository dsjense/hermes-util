c ----------------------------------------------------------------------
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
c> \file stkchk.F
c> \brief File containing integer functions \ref stkchk and \ref addr2hex
c>        of the STAK library.
c>
c> \addtogroup PublicInterface
c>   \{
c>
c ----------------------------------------------------------------------
c
c> \brief This function performs a check of the dynamically allocated memory 
c>        bin (or bins), looking for overwrites of the control or guard words 
c>        in the bin.
c>
c> \param[in] bin   bin to be checked (if bin=0, all allocated bins will be
c>                  checked)
c>
c> \returns
c>  \li  0, Successful completion
c>  \li >0, Indicates detected corruption of one or more of the  bins. If
c>          checking more than one bin, bits are set for corrupted bins, bit 1
c>          corresponding to bin 1, ... (e.g., a return value of 5 indicates
c>          corruption of bins 1 & 3)
c>  \li -1, Bin out of range
c>  \li -2, Bin/bins has/have not been initialized
c>
c>  \note If an error is detected, diagnostic/debugging information is written
c>        to the Fortran unit specified by the \e luerr variable provided in
c>        the first call to \ref stkinit, if \e luerr > MINLU (see stkenum.inc).
c>
      integer function stkchk ( bin )
c
      implicit none
c
c     Machine dependent include file
#include "mdptr.h"
c
c     Passed variables
      integer bin
c
c     Common blocks
#include "stak.inc"
#include "stkenum.inc"
c
c     Local variables
      integer i, errtmp, m, frst, last, icnt, type, bndwrds
      integer ierr, ng, name_err, ndump, jlow, jhi, size, hsize
      integer d, lh
      HU_SIZE_T j, k, kb, ke, l, off, leng, jdump(2)
      logical names_good
      character hex*20, blnk*20

      parameter ( bndwrds = HU_PTR_BYTES/4 )
c
      integer a(0:1)
      pointer ( a_ptr, a )
c
#if HU_PTR_BYTES != 4
      HU_SIZE_T bnd(0:0)
      pointer ( bnd_ptr, bnd )
#endif
c
c     Called functions
      HU_PTR_TYPE getbptr
      integer addr2hex, h2c, lstchr
      integer check_name, get_name

      data blnk / ' ' /
c
c ----------------------------------------------------------------------
c
c ... Make sure bin is in legal range
c
      if ( bin.lt.0 .OR. bin.gt.NBINS ) then
        stkchk = -1
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkchk:  bin out of range - ',bin
        endif
        return
      endif
c
c ... Has cookie been set indicating proper initialization?
c
      if ( cookie.ne.MAGIC ) then
        stkchk = -2
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkchk:  stack has not been initialized'
        endif
        return
      endif
c
c ... bin=0 is special case indicating check of all bins
c
      if ( bin.eq.0 )then
        frst = 1
        last = NBINS
      else
        frst = bin
        last = bin
      endif
c
c ... initialize error counter and loop over all bins to be checked
c ... loop backwards so that error bits are ordered sensibly
c
      stkchk = 0
      do i=last,frst,-1
        errtmp = 0
        name_err = 0
c
c ..... only need to check memory if it was in fact allocated
c
        if ( biggest(i).gt.0 ) then
c
c ....... set local pointer to point to requested memory bin
c
          a_ptr = getbptr(i)
c
c ....... traverse control links from end of allocated memory in bin
c ....... to beginning, looking for corrupted links
c
          j = next(i)
          icnt = count(i)
          ndump = 1

          names_good = .FALSE.
c ....... If name info structure exists, is it corrupted?
          if ( snames(i) ) then
            name_err = check_name(i,icnt,j)
            if ( name_err.eq.0 ) then
c ........... structure appears to be intact, so we can use info from it
              names_good = .TRUE.
            else
c ........... structure has been corrupted
              write (lustk,200) i, name_err
            endif              
          endif
c
          ng = (ngard(i) - 1)/bndwrds + 1
c
   10     continue
c
c ......... if location > 0, then we need to traverse next link
c
            if ( j.gt.0 ) then
c
c ........... get length stored in trailing control word and compute
c ........... address of leading control word
c
              j = j - bndwrds
#if HU_PTR_BYTES == 4
              l = a(j)
#else
              bnd_ptr = loc(a(j))
              l = bnd(0)
#endif
c
              if ( names_good ) then
c
c ............. Utilize name info structure if it exists
c
                ierr = get_name( i, icnt, off, leng, type, hname )
                if ( ierr.ne.0 ) print*,'stkchk: getname error ',ierr
                if ( l. ne. leng ) then
c ............... Upper control word does not match info
                  errtmp = 1
                  jdump(1) = j
                else
                  j = j - l - bndwrds
#if HU_PTR_BYTES == 4
                  l = a(j)
#else
                  bnd_ptr = loc(a(j))
                  l = bnd(0)
#endif
                  if ( l .ne. leng ) then
c ................. Lower control word does not match info
                    errtmp = -1
                    jdump(1) = j
                  endif
                endif

              else
c
c ............. Name info structure not available, so do the best we can
c
c ............. if length or leading control word's address is negative, 
c ............. upper link must be corrupted
c
                j = j - l - bndwrds
                if ( l.lt.0 .OR. j.lt.0 ) then
                  errtmp = 1
                  jdump(1) = j + l + bndwrds
c
c ............... if lengths in control words don't match, links are 
c ............... corrupted
c
                else
#if HU_PTR_BYTES == 4
                  leng = a(j)
#else
                  bnd_ptr = loc(a(j))
                  leng = bnd(0)
#endif
                  if ( leng.ne.l ) then
                    errtmp = 2
c ................. Since no way to know which link is bad, dump both
                    jdump(1) = j
                    jdump(2) = j + l + bndwrds
                    ndump = 2
                  endif
                endif
              endif
c
c ........... if not the 1st allocation, check guard words
c
              if ( icnt.gt.1 ) then
                if ( errtmp.eq.0 .AND. j.gt.0 ) then
                  jlow = j - ng*bndwrds
                  jhi  = j - bndwrds
                  do k=jhi,jlow,-bndwrds
#if HU_PTR_BYTES == 4
                    l = a(k)
#else
                    bnd_ptr = loc(a(k))
                    l = bnd(0)
#endif
                   if ( k.le.0 .OR. l.ne.-k ) then
                      errtmp = 3
                      jdump(1) = k
                      goto 15
                    endif
                  end do
   15             continue
                  j = jlow
                endif
              else
c ............. if 1st allocation, check that we're at start of block
                if ( errtmp.eq.0 .AND. j.ne.0 ) errtmp = 4
              endif
c
c ........... process errors
c
              if ( errtmp.ne.0 ) then
c ............. write error diagnostics to file if requested.
                if ( lustk.gt.MINLU ) then
c ............... write header for bin, including name info, if available
                  write(lustk,100)i,icnt
                  if ( names_good ) then
                    ierr = h2c(hname,HMAX,cname )
                    ierr = lstchr(cname)
                    write(lustk,150) cname(1:ierr), leng, off, type
                  endif
                  if ( errtmp.eq.1 ) then
c ................. Upper control link corrupted
                    lh = addr2hex(loc(a(jdump(1))),hex)
                    write (lustk,210)'Upper',jdump(1),blnk(lh+3:),
     &                               hex(1:lh),l
                  else if ( errtmp.eq.-1 ) then
c ................. Lower control link corrupted
                    l = a(jdump(1))
                    lh = addr2hex(loc(a(jdump(1))),hex)
                    write (lustk,210)'Lower',jdump(1),blnk(lh+3:),
     &                               hex(1:lh),l
                  else if ( errtmp.eq.2 ) then
c ..................control links not consistent
                    lh = addr2hex(loc(a(jdump(1))),hex)
                    write (lustk,220) a(jdump(1)),a(jdump(2)),
     &                                jdump(1),blnk(lh+3:),hex(1:lh)
                    lh = addr2hex(loc(a(jdump(2))),hex)
                    write (lustk,221) jdump(2),blnk(lh+3:),hex(1:lh)
                  else if ( errtmp.eq.3 ) then
c ..................guard cells below allocation are corrupted
                    lh = addr2hex(loc(a(jdump(1))),hex)
                    write (lustk,230) jdump(1),blnk(lh+3:),hex(1:lh)
                    do k=max(0,jlow),jhi
                      lh = addr2hex(loc(a(k)),hex)
                      write (lustk,231) k,a(k),hex(1:lh)
                    end do
                  else if ( errtmp.eq.4 ) then
c ..................1st allocation not at start of memory block
                    write (lustk,240) j
                    jdump(1) = j
                  endif
c
                  size = 400
                  if ( ndump.eq.2 ) then
c ................. if 2 locations of interest are close enough, dump just
c ................. one region
                    if ( jdump(2) - jdump(1) .le. size - 20 ) then
                      ndump = 1
                      jdump(1) = (jdump(2) + jdump(1))/2
                    endif
                  endif
c ............... dump "size" words, divided in one or two batches
                  size = size/ndump
                  hsize = size/2
                  do d=1,ndump
c ................. Center location of interest in dump region to extent
c ................. possible, adust to multiples of 10 word offsets
                    kb = max(0,jdump(d)-hsize)
                    k  = mod(kb,10)
                    kb = kb - k
                    if ( k.ge.5 ) kb = kb + 10
                    ke = kb+size-1
                    if ( ke.ge.next(i) ) then
                      ke = next(i) - 1
                      kb = max(0,next(i)-size)
                      kb = kb - mod(kb,10)
                    endif
                    do k=kb,ke,10
                      l = min(9,ke-k)
                      write(lustk,'(i8,10z9)')k,(a(k+m),m=0,l)
                    end do
c ................. if between two dumps, add ellipsis line
                    if (ndump.eq.2 .AND. d.eq.1 ) 
     &                 write(lustk,'(a)') ' . . .'
                  end do
                endif
c.............. error detected, go to next bin
                goto 20
              endif
c
c ........... proceed to next link if no errors yet
c
              icnt = icnt - 1
              goto 10
            endif
c
   20     continue
c
        endif
c
c ..... set error bit for this bin
c
        stkchk = 2*stkchk
        if ( errtmp.ne.0 .OR. name_err.ne.0 ) stkchk = stkchk + 1
c
      end do
c
      if ( stkchk.gt.0 .AND. bin.eq.0 ) then
        do i=1,frst-1
          stkchk = 2*stkchk
        enddo
      endif
c
      return
c
  100 format('stkchk:  stack corrupted for bin ',i5,/,
     &       '         variable # = ',i20)
  150 format('         variable name  = ',a,/,
     &       '         variable length = ',i20,/,
     &       '         variable FWA    = ',i20,/,
     &       '         variable type   = ',i5)
  200 format('         Stack''s name info structure corrupted for bin ',
     &       i5,':',/,
     &       '           CHECK_NAME returned:  ',i10)
  210 format('         ',a,' control word corrupted:',/,
     &       '           offset:  ',i20,/,
     &       '           address: ',a,'0X',a,' (hexadecimal)',/,
     &       '           value:   ',i20)
  220 format('         Upper and lower control word mismatch:',/,
     &       '           low value:    ',i20,/,
     &       '           high value:   ',i20,/,
     &       '           low offset:   ',i20,/,
     &       '           low address:  ',a,'0X',a,' (hexadecimal)')
  221 format('           high offset:  ',i20,/,
     &       '           high address: ',a,'0X',a,' (hexadecimal)')
  230 format('         preceding guard cells corrupted:',/,
     &       '           offset of bad guard cell:  ',i20,/,
     &       '           address of bad guard cell: ',a,'0X',a,/,
     &       '           guard cell locations, values, (addresses):')
  231 format('             ',i5,i20,'   (0X',a,')')
  240 format('         First allocation not at start of memory block:',/,
     &       '           block offset: ',i20)
c
      end
c>  \}
c  END of PublicInterface group

c> \addtogroup PrivateInterface
c>   \{
c
c> \brief This function converts a Fortran pointer address to a string
c>        containing a hexadecimal representation of that address.
c>
c> The returned string is stripped of leading and trailing blanks, as well
c> as any leading zeros. It is \b not prefixed with '0x'.
c>
c> \param[in]  addr    pointer address
c> \param[out] hstrng  string containing hexadecimal representation of address
c>
c> \returns Length of returned string
      integer function addr2hex(addr,hstrng)

      HU_PTR_TYPE addr
      character hstrng*(*)

      integer BUFLEN
      parameter ( BUFLEN = 16 )

      integer i, frst
      character buf*(BUFLEN), frmt*20

      write(frmt,100) BUFLEN

      write(buf,frmt) addr

      do i=1,BUFLEN
        if ( buf(i:i).ne.' ' ) then
          frst = i
          goto 1
        endif
      end do

 1    continue

      hstrng = buf(frst:BUFLEN)

      addr2hex = BUFLEN + 1 - frst

 100  format('(z',i8,')')

      return
      end
c>  \} END of PublicInterface group
