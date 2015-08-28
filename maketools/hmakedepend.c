/* C_Groups
   $Id$
   
   Copyright (2008) Sandia Corporation. Under the terms of
   Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
   Government retains certain rights in this software.
   
   Hermes is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation, either version 3 of
   the License, or (at your option) any later version.
   
   Hermes is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
   
   You should have received a copy of the GNU Lesser General
   Public License along with Hermes.  If not, see
   <http://www.gnu.org/licenses/>.
   
*/

#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include "mkdep.h"
#include "hmakedepend_usage.h"

extern jmp_buf *MKDEP_JMP_BUF;

int main(int argc, char *argv[])
{
  int i, rval;
  jmp_buf jBuf;
  
  for(i=1;i<argc;++i) if (strcmp(argv[i],"-h")==0) {
    hmakedepend_usage(stdout,0);
    return 0;
  }
  /* set a non-local jump to handle errors from fatalerr */
  /* set MKDEP_JMP_BUF so that fatalerr knows where to jump */
  MKDEP_JMP_BUF = &jBuf;
  if ( !setjmp(jBuf) ) {
    /* gets here after setjmp call */
    rval = mkdep(argc,argv);
    printf("rval: %d\n",rval);
  }
  else rval = 1; /* gets here after fatalerr's longjmp call */


  if (rval) hmakedepend_usage(stderr,1);
  return rval;
}
