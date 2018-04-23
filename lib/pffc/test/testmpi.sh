#!/bin/sh
#  $Id: testmpi.sh,v 1.1.1.1 2012/01/06 19:48:16 mfpasik Exp $
#  
#  Copyright (2008) Sandia Corporation. Under the terms of
#  Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
#  Government retains certain rights in this software.
#  
#  Hermes is free software: you can redistribute it and/or modify
#  it under the terms of the GNU Lesser General Public License as
#  published by the Free Software Foundation, either version 3 of
#  the License, or (at your option) any later version.
#  
#  Hermes is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Lesser General Public License for more details.
#  
#  You should have received a copy of the GNU Lesser General
#  Public License along with Hermes.  If not, see
#  <http://www.gnu.org/licenses/>.
#  

# usage: testmpi.sh [ mpirun arguments ]
#        common arguments needed: -machinefile filename

f_suffix ()
{
  nprc=$1
  nblk=$2
  shft=$3

  ip=0
  suf=""
  while [ $ip -lt $nprc ]; do
    ib=`expr $ip + $shft`
    if [ $ib -ge $nprc ]; then ib=`expr $ib - $nprc`; fi
    if [ $ib -lt $nblk ]; then suf="$suf$ib"; fi
    ip=`expr $ip + 1`
  done
  echo $suf
}

if [ -x testmpi ]; then
  for append in '' -a; do
    for prec in r f; do
      cmd="testmpi -$prec $append"
      if [ "$append" = "-a" ]; then cp samples/empty.pff ./pars.pff; fi
      $cmd > tmplog
      err=$?
      cmp_err=0
      if [ $err -eq 0 ]; then
        cmp -s pars.pff samples/${prec}p0.pff
        cmp_err=$?
      fi
      if [ \( $err -eq 0 \) -a \( $cmp_err -eq 0 \) ]; then
        echo ${cmd}: PASSED
      else
        echo ${cmd}: FAILED
        cat tmplog
      fi
    done
  done
else
  echo "**** Cannot find executable: testmpi"
fi

if [ ! -x mtestmpi ]; then
  echo "**** Cannot find executable: mtestmpi"
  exit
fi

if [ -n "$MPI_ROOT" ]; then MPIRUN=$MPI_ROOT/bin/mpirun
else MPIRUN=mpirun; fi
max_procs=4
max_blk=3
for append in '' -a; do
  for prec in r f; do
    np=1
    while [ $np -le $max_procs ]; do
      blk=$np
      if [ $blk -gt $max_blk ]; then blk=$max_blk; fi
      while [ $blk -gt 0 ]; do
        shft=0
        while [ $shft -lt $np ]; do
          fsuf=`f_suffix $np $blk $shft`
          cmd="$MPIRUN -np $np $* mtestmpi -$prec $append $blk $shft"
          p_cmd="mpirun -np $np $* mtestmpi -$prec $append $blk $shft"
          if [ "$append" = "-a" ]; then cp samples/empty.pff ./pars.pff; fi
          $cmd > tmplog
          err=$?
          cmp_err=0
          if [ $err -eq 0 ]; then
            cmp -s pars.pff samples/${prec}p$fsuf.pff
            cmp_err=$?
          fi
          if [ \( $err -eq 0 \) -a \( $cmp_err -eq 0 \) ]; then
            echo ${p_cmd}: PASSED
          else
            echo ${p_cmd}: FAILED
            cat tmplog
            if [ $cmp_err -ne 0 ]; then
              cmp pars.pff samples/${prec}p$fsuf.pff
            fi
          fi
          shft=`expr $shft + 1`
        done  ## shift loop
        blk=`expr $blk - 1`
      done  ## blk loop
      np=`expr $np + 1`
    done  ## proc loop
  done ## precision loop
done ## append loop

rm -f pars.pff tmplog


