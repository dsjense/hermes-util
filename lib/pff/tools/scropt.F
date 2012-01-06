      subroutine scropt(ttyout,comand,opcode,ierr)
c
c **********************************************************************
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
C_Groups bldpff  
c **********************************************************************
c
c     - Reads the next input token and translates that to a PFF
c       scroll control value.
c     - If invalid element is encountered, a usage message is written
c     - Valid elements are:
c         Element    Description                      OPCODE
c         -------    -----------                      ------
c         u*p        scroll up                          -2
c         d*own      scroll down                        -1
c         ]          position at end of file            -3
c         #          position at location #              #
c         NULL       position same as last time          0
c
c     Input:
c       comand  -  the command being parsed; used in improper usage
c                  message
c       ttyout  -  logical unit for improper usage message; if 0, no
c                  message
c       ierr    -  if not zero, return with no operation
c
c     Output:
c       opcode  -  operation code for scroll routines
c       ierr    -  error status flag;
c                      0,   No error
c                      1,   Illegal element
c
c ----------------------------------------------------------------------
c
c     Internal:
c       eol     -  IOPACK End-Of-Line indicator
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed:
      integer   opcode, ttyout, ierr
      character comand*(*)
c
c ----------------------------------------------------------------------
c     Call common blocks and parameter modules
c ----------------------------------------------------------------------
c
c ... Included parameter decks:
c     iopar     -  ENDLN, ALFA
c
c ... Included common decks:
c
#include "iopar.inc"

c     Internal:
      character legal(3)*8, token*40
      integer   next, ncl, status, type
 
c     External:
c
      integer   nxttyp, intpar, lstmch
      character chrpar*132
c
      data legal / 'down', 'up' , ']' /
c
c ----------------------------------------------------------------------
c
c
c ... return if error flag is not zero on entry
c
      if(ierr.ne.0) return
c
c ----------------------------------------------------------------------
c
      status = 0
      next = nxttyp(' ',status)
c
      if ( next .eq. ENDLN ) then
        opcode = 0
        return
      endif
c
      if ( next .eq. ALFA ) then
        token = chrpar (RQD, 'scroll_char', ' ', ' ', status, ncl)
        type = lstmch(token, legal, 3)
        if ( type.gt.0 ) then
          opcode = -type
          return
        endif
      else
         opcode = intpar(RQD, 'scroll_position', 0, ' ', status)
         if ( status .eq. 0 ) return
      endif

      ierr = 1
      if (ttyout.ne.0) then
        write(ttyout,*) 'Usage:  ',comand,' [ # | u[p] | d[own] | ] ]'
      endif
c
      return
      end
