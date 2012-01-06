#!/bin/sh
#  $Id$
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
  for prec in r f; do
    cmd="testmpi -$prec"
    $cmd > tmplog
    cmp -s pars.pff samples/${prec}p0.pff
    if [ $? -eq 0 ]; then
      echo ${cmd}: PASSED
    else
      echo ${cmd}: FAILED
      cat tmplog
    fi
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
for prec in r f; do
  np=1
  while [ $np -le $max_procs ]; do
    blk=$np
    if [ $blk -gt $max_blk ]; then blk=$max_blk; fi
    while [ $blk -gt 0 ]; do
      shft=0
      while [ $shft -lt $np ]; do
        fsuf=`f_suffix $np $blk $shft`
        cmd="$MPIRUN -np $np $* mtestmpi -$prec $blk $shft"
        p_cmd="mpirun -np $np $* mtestmpi -$prec $blk $shft"
        $cmd > tmplog
        cmp -s pars.pff samples/${prec}p$fsuf.pff
        if [ $? -eq 0 ]; then
          echo ${p_cmd}: PASSED
        else
          echo ${p_cmd}: FAILED
          cat tmplog
          cmp pars.pff samples/${prec}p$fsuf.pff
        fi
        shft=`expr $shft + 1`
      done  ## shift loop
      blk=`expr $blk - 1`
    done  ## blk loop
    np=`expr $np + 1`
  done  ## proc loop
done ## precision loop

rm -f pars.pff tmplog


