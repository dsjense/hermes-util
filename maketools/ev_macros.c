/* C_Groups hermes
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

#include "ev_macros.h"
#include "def.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct s_EV_Macro EV_Macro;

struct s_EV_Macro {
  char *name;
  char *value;
  EV_Macro *next;
};

static EV_Macro *EV_top = 0;
static EV_Macro *EV_last = 0;

void EV_Add(const char *name)
{
  EV_Macro *evm = (EV_Macro *) malloc(sizeof(EV_Macro));
  evm->name = copy(name);
  evm->value = 0;
  evm->next = 0;
  if ( EV_top ) EV_last->next = evm;
  else EV_top = evm;

  EV_last = evm;
}

int EV_Init()
{
  int rval = 0;
  EV_Macro *evm = EV_top;
  EV_Macro *next;
  EV_Macro *lastok = 0;
  while ( evm ) {
    evm->value = getenv(evm->name);
    next = evm->next;
    if ( ! evm->value ) {
      ++rval;
      fprintf(stderr,"EV_Init: %s is not defined\n",evm->name);
      if ( evm == EV_top ) EV_top = next;
      else lastok->next = next;
      free(evm->name);
      free(evm);
    }
    else {
      lastok = evm;
    }
    evm = next;
  }
  return rval;
}

char *EV_Subst_Macro(char *string)
{
  char *mat, *nsub, *sub = string;
  int len, loc, vlen, nlen;
  EV_Macro *evm = EV_top;
  while ( evm ) {
    vlen = strlen(evm->value);
    nlen = strlen(evm->name);
    while ( (mat = strstr(sub,evm->value)) ) {
      len = strlen(sub) - vlen + nlen + 3;
      loc = mat - sub;
      mat += vlen;
      nsub = (char *) malloc(len+1);
      strncpy(nsub,sub,loc);
      sprintf(nsub+loc,"$(%s)%s",evm->name,mat);
      if ( sub != string ) free(sub);
      sub = nsub;
    }
    evm = evm->next;
  }

  return sub;
}
#ifdef TEST_EV
int main(int argc, char *argv[])
{
  EV_Macro *evm;
  char buf[1024];
  char *val = 0, *c1 = 0, *c2 = 0;
  int i;

  for(i=1;i<argc;++i) EV_Add(argv[i]);

  printf("EV_Init returns %d\n",EV_Init());

  evm = EV_top;
  while ( evm ) {
    printf("%s = %s\n",evm->name,evm->value);
    if ( c1 == 0 ) c1 = evm->value;
    c2 = evm->value;
    evm = evm->next;
  }

  sprintf(buf,"PREFIX : nothing/SUFFIX");
  val = EV_Subst_Macro(buf);
  if ( val != buf ) printf("VAL != BUF: %s *** %s\n",val,buf);
  else printf("VAL == BUF\n");

  for(i=1;i<argc;++i) {
    if ( (val=getenv(argv[i])) ) {
      sprintf(buf,"PREFIX : %s/SUFFIX",val);
      val = EV_Subst_Macro(buf);
      printf("%s *** %s\n",buf,val);
    }
  }

  if ( c2 ) {
    sprintf(buf,"PREFIX : %s : %s/SUFFIX",c1,c2);
    val = EV_Subst_Macro(buf);
    printf("buf: %s\nval: %s\n",buf,val);
  }

  return 0;
}
#endif
