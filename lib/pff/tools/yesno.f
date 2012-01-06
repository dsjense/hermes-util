      logical function yesno(luin,luout,prompt)
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
c     This function:
c        1)  Prints a user-supplied character prompt to the output file
c        2)  Reads a character string from the input file
c        3)  Returns TRUE if the first non-blank character is Y or y;
c                    FALSE otherwise
c
c     Input:
c       luin    -  logical unit # of input file
c       luout   -  logical unit # of output file
c       prompt  -  user supplied prompt
c
c     Output:
c       None
c
c     Return Values:
c       = .TRUE.,   if first nonblank character of input string is 'Y'
c                   or 'y'
c       = .FALSE.,  otherwise
c
c
      integer   luin,luout
      character prompt*(*)
c
      character  ans1*1, ans*80
      integer    i
c
      ans = prompt // '  (y/-)'
c
      call mdprmt ( luout, ans )
      read(luin,'(a)') ans
c
      if ( ans.eq.' ' ) then
        ans1 = ' '
      else
        do i=1,len(ans)
          if ( ans(i:i).ne.' ' ) then
            ans1 = ans(i:i)
            call fxcase(ans1,'L')
            goto 1
          endif
        end do
      endif
c
 1    continue
c
      if(ans1.eq.'y')then
        yesno = .TRUE.
      else
        yesno = .FALSE.
      endif
c
      return
      end
