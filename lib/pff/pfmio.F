#if !defined(BYTE_SWAP)
# ifdef HU_ENDIAN_IS_LSB_FIRST
#  define BYTE_SWAP
# endif
#endif
#if !defined(NO_I2)
# if defined(CRAsys)
#  define NO_I2
# endif
#endif
      subroutine pfmio ( iop, lun, irec, u, ierr )
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
c       - This routine is a MACHINE-DEPENDENT routine to perform perform
c         I/O on the particular machine's default integer data type.
c         I/O is to an ANSI-standard direct access file.  All integers
c         are assumed to be 16-bit signed integers (-32768 to 32767).
c         An attempt to write integers outside this range will result in
c         loss of data !!!!
c
c     Machine-Dependent Summary:
c
c         Values for EOFSTAT
c            ABSOFT              -   -1
c            AIX                 -    1
c            DECOSF              -   36
c            WIN32               -   36 (Intel)
c            HP                  -  922
c            IRIX                -  168
c            LINUX_I64           -   36 (Intel)
c            LINUX_X86           -  253 (PGI)
c            LINUX_X86           -   36 (Intel)
c            MSF                 -   -1
c            SOLARIS             - 1066
c
c %%%%%%%%%%%%%%%%%%%%%%  Cray (UNICOS) Version !!!!!  %%%%%%%%%%
c
c       - Cray integer data type is 64-bit only; therefore there are 4
c         integers in each word of packing array.  Shifting and masking
c         is done to pack & unpack integers from packed integer array.
c         Note that the sign bit is NOT extended on unpacking; it must
c         be manually extended.  The unpacked input array is mapped to a
c         2-dimensional array (NPACK,MLENP).
c
c %%%%%%%%%%%%%%%%%%%%%%  SUN  Version !!!!!  %%%%%%%%%%%%%%%%%%%%%%%%%%
c
c       - Older versions of SUN f77 detected reading beyond
c         end-of-data on a direct access file with the "end=" option
c         (NOT ANSI !!). The IOSTAT flag returned in that case was -1.
c         Newer versions of f77 and f90 are ANSI-compliant -- EOF can
c         only be detected via the IOSTAT flag with value 1066.
c
c %%%%%%%%%%%%%%%%%%%%%%%  IRIS Version !!!!!  %%%%%%%%%%%%%%%%%%%%%%%%%
c
c       - EOF can only be detected via the IOSTAT flag; for
c         IRIS, the error is:
c                   27 -- "I/O on illegal record attempted"
c       - for IRIX 6.4 and MIPSpro compilers version 7.2.1 and later
c                   168
c       - for IRIX 6.4 and MIPSpro f90 compiler version 7.2.1 and later
c                   4016
c
c %%%%%%%%%%%%%%%%%%  Microsoft Fortran Version !!!!!  %%%%%%%%%%%%%%%%%
c
c       - Since MSF has integer*2 data type, use it to avoid bitwise
c         packing. MS fortran detects reading beyond end-of-data on a
c         direct access file by returning IOSTAT=-1 (NOT ANSI !!).  In 
c         this case, the "err=" option is not taken, but as specified in
c         the standard, the "end=" option is not compatible w/ direct 
c         access.  Also MS fortran used a different syntax for 
c         hexadecimal literal constants than other machines.
c
c %%%%%%%%%%% DEC Visual Fortran (Windows NT) Version !!!!!  %%%%%%%%%%%
c
c       - Uses a different syntax for hexadecimal literal constants than 
c         other machines.
c
c %%%%%%%%%%%%%%%%%%%%%%   ABSoft Mac Version !!!!!  %%%%%%%%%%%%%%%%%%%
c
c       - Since ABSoft has integer*2 data type, use it to avoid bitwise
c         packing. EOF is detected by returning IOSTAT=-1 (NOT ANSI !!).  
c         However, the "err=" branch is taken, NOT the "end=".
c
c ----------------------------------------------------------------------
c
c     Input:
c       iop     -  operation code, 0 for read, otherwise write
c       irec    -  record # for I/O operation
c       lun     -  logical unit # for i/o operation
c       u       -  array of integers to be packed & written (WRITE Mode)
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       u       -  array of integers to be unpacked & read (READ Mode)
c       ierr    -  error flag;
c                     -1,   EOF detected (not really an error yet)
c                      0,   No error
c                      1,   I/O Error on Read
c                      2,   I/O Error on Write
c                      3,   Invalid OPcode
c
c ----------------------------------------------------------------------
c
c     Internals:
#if defined(NO_I2)
c       i       -  loop index over packed array
c       j       -  loop index over integers in each packed word
#else
c       j       -  loop index over packed array
#endif
c       p       -  (*)  packed data integer array

c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     pfbsiz  -  LBUF, PBPW
c     pfstfg  -  RE, WR
c
c ... Included common decks:
c ... pferrs
c       r:    OFFLUN, ludbg
c
#include "pfbsiz.inc"
#include "pfstfg.inc"
#include "pferrs.inc"
c
c ----------------------------------------------------------------------
c
c ... Parameters:
      character PFMODU*6
      parameter ( PFMODU = 'PFMIO' )
c
      integer   EOFSTA
      integer   NPACK
#ifdef NO_I2
c
      parameter ( NPACK = 4 )
c
      integer   NBITS, MSK, MFILL, MSIGN
      parameter ( NBITS  = 8*PBPW )
      parameter ( MSK    = (2**NBITS)-1 )
      parameter ( MFILL  = x'ffffffffffff0000' )
      parameter ( MSIGN  = x'0000000000008000' )
#else
      parameter ( NPACK = 1 )
#endif
#if defined(CRAsys)
c UNICOS 5.1 uses 122 , 6.1 uses 1001
      parameter ( EOFSTA = 1001 )
#elif defined(MSFsys) || defined(ABSOFTsys)
      parameter ( EOFSTA = -1 )
#else
      parameter ( EOFSTA = HU_EOF_STAT )
#endif
#ifdef BYTE_SWAP
      integer*2 M1, M2
# if defined(MSFsys)
      parameter (M1 = #ff00)
      parameter (M2 = #00ff)
# else
      parameter (M1 = 'ff00'x)
      parameter (M2 = '00ff'x)
# endif
      integer*2 b_swap, i2
#endif
      integer   MLENP
      parameter ( MLENP = LBUF/NPACK )
c
c     Declare variables:
c
c ... Passed:
      integer   iop,lun,irec,ierr
#if defined(NO_I2)
      integer   u(NPACK,MLENP)
#else
      integer   u(LBUF)
#endif
c
c ... Internal
#if defined(NO_I2)
      integer   j,i
      integer   p(MLENP)      
#else
      integer   j
      integer*2 p(MLENP)
#endif
c
c ----------------------------------------------------------------------
c
c ... machine-dependent statement function for byte-swap
c
#if defined(BYTE_SWAP)
# if defined(PGNsys)
c ... The Paragon has a bug in ishftc.
c ... Hence, this statement (a workaround) is used
      b_swap(i2) = ior( iand( M2, ishftc( iand(M1,i2), 8, 16) ) ,
     &                  ishft ( iand(M2,i2), 8) )
# else
      b_swap(i2) = ior( ishftc( iand(M1,i2), 8, 16) ,
     &                  ishft ( iand(M2,i2), 8) )
# endif
#endif
c      
c
c ----------------------------------------------------------------------
c
c
c ... write status message if DEBUG is on
c
      if (ludbg.ne.OFFLUN)then
        write (ludbg,1000) PFMODU, 'Entry', ierr
cc        write (ludbg,*) PFMODU, ': r=',irec
      endif
c
c ... return if error flag is not zero on entry
c
      if(ierr.ne.0)then
        call pfserr ( PFMODU, 0, 0, 'Error Flag Non-zero on Entry' )
        goto 999
      endif
c
      if ( iop.eq.RE) then
c
#if defined(NO_I2)
c ..... read packed array from direct access file.
c
# if defined(CRAsys)
        read(lun,rec=irec,err=10,iostat=ierr) (p(j),j=1,MLENP)
# endif
c
c ..... move somewhere else so not to screw up p array
c
        do 1 j=1,MLENP
          u(1,j) = p(j)
    1   continue
c
c ..... unpack remaining words
c
        do 3 i=NPACK,2,-1
c
          do 2 j=1,MLENP
c
c ......... grab low 16 bits, then shift for next pass.
c
            u(i,j) = and( u(1,j), MSK )
c
c ......... extend sign bit
c
            if ( and(u(i,j),MSIGN) .ne. 0 ) then
              u(i,j) = or(MFILL,u(i,j))
            endif
c
c ......... shift over one packed word for next pass
c
            u(1,j) = shiftr(u(1,j),NBITS)
c
    2     continue
c
    3   continue
c
c
c ..... no shift on first word packed in each word
c
        do 4 j=1,MLENP
c
c ....... all that's left is 16 bits, don't need to "and"
c
c ....... extend sign bit
c
          if( and(u(1,j),MSIGN) .ne. 0 )then
            u(1,j) = or(MFILL,u(1,j))
          endif
c
    4   continue
#else
c ..... read packed array from direct access file.
c
# if defined(HU_USE_END_FOR_DA_EOF)
        read(lun,rec=irec,err=10,end=10,iostat=ierr) (p(j),j=1,MLENP)
# else
        read(lun,rec=irec,err=10,iostat=ierr) (p(j),j=1,MLENP)
# endif
# if defined(MSFsys)
        if ( ierr.ne.0 ) goto 10
# endif
c
        do 1 j=1,MLENP
c
# ifdef BYTE_SWAP
c ....... reverse byte order
c
          p(j) = b_swap(p(j))
c
# endif
c ....... move values in pack buffer to unpacked array
c
          u(j) = p(j)
c
    1   continue
#endif
c
      else if ( iop .eq. WR ) then
c
#if defined(NO_I2)
c ..... no shift on first word packed in each word
c
        do 5 j=1,MLENP
c
c ....... mask down to "NBITS" bits
c
          p(j)=and(u(1,j),MSK)
c
    5   continue
c
c ..... pack remaining words
c
        do 7 i=2,NPACK
c
          do 6 j=1,MLENP
c
c ......... shift already-packed words out of the way,
c ......... mask word being packed down to "NBITS" bits,
c ......... and combine with aready-packed words using bitwise OR
c
            p(j)=or(shiftl(p(j),NBITS),and(u(i,j),MSK))
c
    6     continue
c
    7   continue
c
c ..... write packed array to direct access file.
# if defined(CRAsys)
c
        write(lun,rec=irec,err=20) (p(j),j=1,MLENP)
# endif
#else
        do 2 j=1,MLENP
c
c ....... move unpacked values to pack buffer
c
          p(j) = u(j)
# ifdef BYTE_SWAP
c
c ....... reverse byte order
c
          p(j) = b_swap(p(j))
# endif
c
    2   continue
c
c ..... write packed array to direct access file.
c
        write(lun,rec=irec,err=20) (p(j),j=1,MLENP)
#endif
c
      else
c
c ..... Invalid OPcode
c
        ierr = 3
        call pfserr ( PFMODU, ierr, lun, 'Invalid OPcode')
c
      endif
c
      goto 999
c
   10 continue
c
c ..... Process READ errors -- check for EOF condition as special case
c
cc        print*, 'EOFSTA = ',ierr, '  ???'
c
        if ( ierr.eq.EOFSTA ) then
c
c ....... Process EOF condition  (not really an error; calling program
c                                 must decide)
c
          ierr = -1
c
        else
c
          ierr = 1
          call pfserr ( PFMODU, ierr, lun, 'I/O Error on Read' )
c
        endif
c
        goto 999
c
   20 continue
c
c ..... Process WRITE errors -- All are errors
c
        ierr = 2
        call pfserr ( PFMODU, ierr, lun, 'I/O Error on Write' )
c
  999 continue
c
c ... write status message if DEBUG is on and return
c
      if (ludbg.ne.OFFLUN)then
cc        write (ludbg,*) PFMODU, ': r=',irec
        write (ludbg,1000) PFMODU, 'Exit', ierr
      endif
c
      return
c
 1000 format(' %PFF-',a,'-DBG:  ',a,' Error Status = ',i5)
c
      end

