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
c> \file stkload.F
c> \brief File containing integer function \ref stkload of the STAK library.
c>
c> \addtogroup PublicInterface
c>   \{
c
c ----------------------------------------------------------------------
c
c> \brief This function loads the contents of a specified set of bins from
c>        a file written by \ref stkdmp.
c> \note  This routine ONLY reads the data written by the second write
c>        statement in \ref stkdmp, i.e., it only reads the data from the bin.
c>        The calling program must have previously read the 1st record
c>        containing the bin # and other bin setup information, and used that
c>        information to initialize the bin with a call to \ref stkinit.
c>
c> \param[in] ludmp Logical unit of an open file to read info from
c> \param[in] frmt  character variable containing a valid Fortran format
c>                  description to read data from file (e.g.,
c>                  '<TT>(10z10)</TT>', OR, if \e frmt is blank (' '), data
c>                  is written unformatted.
c> \param[in] bin   number of bin to be loaded
c> \param[in] lbin  # of words of data to be read into the bin
c>
c> \returns
c>  \li  0, Successful completion
c>  \li -1, Bin out of range
c>  \li -2, Bin/bins has/have not been initialized
c>  \li -3, Insufficient storage in bin
c>
c> \par Example:
c>    The program \e tstak uses \ref stkload to read data from a file
c>    written by \ref stkdmp and can be found in the test subdirectory.
c> \sa stkdmp stkinit
c
      integer function stkload(ludmp, frmt, bin, lbin)
c
      implicit none
c
c ----------------------------------------------------------------------
c
#include "mdptr.h"
c following defines are related to doxygen
#define HU_SIZE_t  HU_SIZE_T
c
c     Passed variables
      integer   ludmp, bin
      HU_SIZE_t lbin
      character frmt*(*)
c     Common blocks
#include "stak.inc"
#include "stkenum.inc"
c
c     Local variables
      HU_SIZE_T j
c
cc      integer a_ptr, a(0:1)
      integer a(0:1)
      pointer ( a_ptr, a )
c
c     Called functions
      HU_PTR_TYPE getbptr
c
c ----------------------------------------------------------------------
c
c ... Make sure bin is in legal range
c
      if ( bin.lt.1 .OR. bin.gt.NBINS ) then
        stkload = -1
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkload:  bin out of range - ',bin
        endif
        return
      endif
c
c ... Has cookie been set indicating proper initialization?
c
      if ( cookie.ne.MAGIC ) then
        stkload = -2
        if ( lustk.gt.MINLU ) then
          write(lustk,*)'stkload:  stack has not been initialized'
        endif
        return
      endif
c
c ... is there enough allocated memory in the bin?
c
      if ( lbin.gt.biggest(bin) ) then
        stkload = -3
        if ( lustk.gt.MINLU ) then
          write(lustk,100) 'stkload:  insufficient storage in bin - ',
     &                   bin,lbin,biggest(bin)
        endif
        return
      endif
c
c ... set local pointer to point to requested memory bin
c
      a_ptr = getbptr(bin)
c
      if ( frmt.eq.' ' ) then
        if( lbin.gt.0 ) read(ludmp) (a(j),j=0,lbin-1)
      else
        if( lbin.gt.0 ) read(ludmp,frmt) (a(j),j=0,lbin-1)
      endif
c
      stkload = 0
c
      return
  100 format(a,/,'  bin: ',i5,/,
     &           '  # of data words supplied: ',i10,/,
     &           '  # of words available: ',i10)
      end
c>  \} END of PublicInterface group
