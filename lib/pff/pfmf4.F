#if !defined(BYTE_SWAP)
# ifdef HU_ENDIAN_IS_LSB_FIRST
#  define BYTE_SWAP
# endif
#endif
#if defined(CRAsys)
# define MF4_NOT_IMPLEMENTED
#else
# define MF4_IMPLEMENTED
#endif
      subroutine pfmf4 ( iop, nf, ibuf, farr, ierr )
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
c       - This routine is a MACHINE-DEPENDENT routine that, in write mode,
c         breaks a float up into a pair of 2-byte integers, and in read 
c         mode, converts a pair of 2-byte integers back into a float.
c       - The format for the integers is defined by breaking the IEEE 
c         32-bit representation of the float value and equivalencing it 
c         to a two-element integer*2 array.  This means that byte-swapping
c         is needed, the order of the two-element array must be reversed.
c
c     Machine-Dependent Summary:
c
c %%%%%%%%%%%%%%%   IEEE internal representation !!!!!  %%%%%%%%%%%%%%%%
c
c       - Just equivalence each float to integer*2 array.  Reverse order
c         to byte-swap.
c       - If BYTE_SWAP is defined, all bytes are reversed
c       - If MF4_NOT_IMPLEMENTED is defined, error code is returned
c
c ----------------------------------------------------------------------
c
c     Input:
c       iop     -  operation code, 0 for read, otherwise write
c       nf      -  # of real values supplied/requested
c       ibuf    -  (*) buffer that supplies encoded integers (READ Mode)
c       farr    -  (*) array that supplies floats to be encoded (WRITE Mode)
c       ierr    -  if not zero, return with no operation
c
c ----------------------------------------------------------------------
c
c     Output:
c       ibuf    -  (*) buffer that receives encoded integers (WRITE Mode)
c       farr    -  (*) array that receives the decoded floats (READ Mode)
c       ierr    -  error flag;
c                      0,   No error
c                      1,   Invalid OPcode
c                      2,   Not implemented
c
c ----------------------------------------------------------------------
c
c ... Passed:
      integer iop, nf, ierr
      integer ibuf(1:2*nf)
      real    farr(1:nf)
c
c     Internals:
c
c ----------------------------------------------------------------------
c     Call common blocks and associated parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c
c ... Included common decks:
c     pfstfg  -  RE, WR
c     pfbsiz
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
      parameter ( PFMODU = 'PFMF4' )
c
c     Declare variables:
c
c ... Internal
c
#if defined(MF4_IMPLEMENTED)
      integer L1, L2
# ifdef BYTE_SWAP
      parameter (L1=2, L2=1)
# else
      parameter (L1=1, L2=2)
# endif
c
      integer   i, j
      real      x
      integer*2 ix(2)
c
      equivalence (x,ix)
#endif
c
c ----------------------------------------------------------------------
c
c
c ... write status message if DEBUG is on
c
      if (ludbg.ne.OFFLUN)then
        write (ludbg,1000) PFMODU, 'Entry', ierr
      endif
c
c ... return if error flag is not zero on entry
c
      if(ierr.ne.0)then
        call pfserr ( PFMODU, 0, 0, 'Error Flag Non-zero on Entry' )
        goto 999
      endif
c
#if defined(MF4_IMPLEMENTED)
      j = 1
      if ( iop.eq.RE) then
c
        do i=1,nf
          ix(L1) = ibuf(j)
          ix(L2) = ibuf(j+1)
          farr(i) = x
          j = j + 2
        end do
c
      else if ( iop .eq. WR ) then
c
        do i=1,nf
          x = farr(i)
          ibuf(j)   = ix(L1)
          ibuf(j+1) = ix(L2)
          j = j + 2
        end do
c
      else
c
c ..... Invalid OPcode
c
        ierr = 1
        call pfserr ( PFMODU, ierr, 0, 'Invalid OPcode')
c
      endif
#else
c ... Not implemented
c
      ierr = 2
      call pfserr ( PFMODU, ierr, 0, 'Not implemented')
#endif
c
c ----------------------------------------------------------------------
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

