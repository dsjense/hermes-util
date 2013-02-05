/*
 * $Id$
 * 
 * Copyright (2008) Sandia Corporation. Under the terms of
 * Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
 * Government retains certain rights in this software.
 * 
 * Hermes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 * 
 * Hermes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General
 * Public License along with Hermes.  If not, see
 * <http://www.gnu.org/licenses/>.
 * 
 *
 * program to determine the number of bytes in a pointer
 *
 * C_Groups main standalone
 */

#include <stdio.h>
#include <sys/types.h>

int main ()
{
  printf("Size of int type is %d bytes\n",(int)sizeof(int));
  printf("Size of long type is %d bytes\n",(int)sizeof(long));
  printf("Size of float type is %d bytes\n",(int)sizeof(float));
  printf("Size of double type is %d bytes\n",(int)sizeof(double));
  printf("Size of pointer type is %d bytes\n",(int)sizeof(char*));
  printf("Size of size_t type is %d bytes\n",(int)sizeof(size_t));
  printf("Size of off_t type is %d bytes\n",(int)sizeof(off_t));
  return 0;
}
