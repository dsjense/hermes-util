#!/bin/sh

# $Id$
# 
# Copyright (2008) Sandia Corporation. Under the terms of
# Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
# Government retains certain rights in this software.
# 
# Hermes is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
# 
# Hermes is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General
# Public License along with Hermes.  If not, see
# <http://www.gnu.org/licenses/>.
# 

#
#  The following line must be modified to specify the name of the
#  directory in which the Hermes Utilities are installed, or alternately,
#  the environment variable HERMES_ROOT must be predefined.
#
### HERMES_ROOT=/users/user/hermes; export HERMES_ROOT

if [ -z "$HERMES_ROOT" ]; then
  echo "hermesenv.sh: Warning: Not customized for this installation !!" 2>&1
  return 1
fi

#
#  The following lines must be modified to build or use the CIDL library
#  (or alternately, the environment variable IDLROOT can be predefined)
#
# If IDLROOT is an existing directory, it is used to locate idl. Otherwise,
# if IDLROOT is a non-empty string, the script attempts to  determine
# IDLROOT by searching the PATH environment variable for an executable "idl".

#if [ -z "$IDLROOT" ]; then
# @%@%@%@%@% is presumably NOT the name of a directory
### IDLROOT=@%@%@%@%@%  or IDLROOT=/usr/local/rsi
#fi

# For 64-bit architectures, if USE_32BIT_IDL is a non-empty string, a 32-bit
# version of IDL is assumed, e.g.,
### USE_32BIT_IDL=1; export USE_32BIT_IDL

#  The following line must be modified to specify the names of architectures
#  that are cross-compiled from this machine
extra_ARCH_LIST=''

#  The following lines need to be modified if you want to take advantage of
#  any of the libraries' parallel capabilities, which are implemented using
#  the MPI standard. 
#
#  The following line must be modified to locate the root directory for MPI
### MPI_ROOT="/usr/local/mpich"; export MPI_ROOT
#  The following line must be modified to provide the Link arguments for MPI
### MPI_LIB="-L$MPI_ROOT/lib -lmpich"; export MPI_LIB
#
# Find system type
#
OSNAME=`uname -s`
case $OSNAME in
    "SunOS")
	case `uname -r` in
	    4*)  HERMES_SYS_TYPE="sunos" ;;
	    5*)  HERMES_SYS_TYPE="solaris" ;;
	### 5*)  HERMES_SYS_TYPE="solaris_64" ;;
	esac
	;;

    "HP-UX")
	HU_SYSTEM=HP; export HU_SYSTEM
	case `uname -r` in
	    A.09.*)  HERMES_SYS_TYPE="hp9" ;;
	    B.10.*)  HERMES_SYS_TYPE="hp10" ;;
	esac
	;;

    "Linux")
	case `uname -m` in
	    i686)    HERMES_SYS_TYPE="linux_x86_gnu" ;;
	### i686)    HERMES_SYS_TYPE="linux_x86_intel" ;;
	    ia64)    HERMES_SYS_TYPE="linux_ia64_intel" ;;
	    x86_64)  HERMES_SYS_TYPE="linux_x86_64_gnu" ;;
	### x86_64)  HERMES_SYS_TYPE="linux_x86_64_intel" ;;
	    *)       HERMES_SYS_TYPE="linux" ;;
	esac
	;;

    "Darwin")
        case `uname -m` in
            i386)    HERMES_SYS_TYPE="darwin_x86_64_intel" ;;
            *)       HERMES_SYS_TYPE="darwin";;
        esac
        ;;

    "IRIX"|"IRIX64")
	HU_SYSTEM=IRI; export HU_SYSTEM
	HERMES_SYS_TYPE=irix
	;;

    "AIX")
        HU_SYSTEM=AIX; export HU_SYSTEM
	HERMES_SYS_TYPE=aix
        ;;

    "OSF1")
        HU_SYSTEM=DECOSF; export HU_SYSTEM
	HERMES_SYS_TYPE=decosf
        ;;

    "UNICOS")
	HU_SYSTEM=CRA; export HU_SYSTEM
	HERMES_SYS_TYPE=unicos
	;;

    *)
#       Probably a UNICOS machine !!!
	HU_SYSTEM=CRA; export HU_SYSTEM
	HERMES_SYS_TYPE=unicos
	;;

esac

# if more than one system is using this file (via NFS), might need to adjust
# things based on which system this is
if [ "$HERMES_SYS_TYPE" = "solaris" ]; then
  # MPI_ROOT=/usr/local/apps/mpich/1.2.7p1/shmem
  # MPI_LIB="-L$MPI_ROOT/lib -lmpich -lnsl -lrt -lthread -lnsl"
  # export MPI_ROOT MPI_LIB
  true  # need some command in "THEN" list
elif [ "$HERMES_SYS_TYPE" = "solaris_64" ]; then
  # MPI_ROOT=/usr/local/apps/mpich/1.2.7p1/shmem
  # MPI_LIB="-L$MPI_ROOT/lib -lmpich -lnsl -lrt -lthread -lnsl"
  # export MPI_ROOT MPI_LIB
  extra_ARCH_LIST='solaris'
elif [ "$HERMES_SYS_TYPE" = "linux_x86_pgi_gnu" ]; then
  # if [ "$hostname" = "laptop" ]; then
  #   MPI_ROOT=/usr/local/apps_local/mpich/1.2.6/shmem/pgi
  # else
  #   MPI_ROOT=/usr/local/apps/mpich/1.2.7p1/p4/pgi
  # fi
  # MPI_LIB="-L$MPI_ROOT/lib -lmpich"
  # export MPI_ROOT MPI_LIB
  true  # need some command in "THEN" list
elif [ "$HERMES_SYS_TYPE" = "linux_x86_intel" ]; then
  # MPI_ROOT=/usr/local/apps/mpich/1.2.7p1/p4/intel
  # MPI_LIB="-L$MPI_ROOT/lib -lmpich"
  # intellib="`which icc | sed 's?bin/icc$??'`lib"
  # if [ -z "$LD_LIBRARY_PATH" ]; then LD_LIBRARY_PATH="$intellib"
  # else   LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$intellib"; fi
  # export LD_LIBRARY_PATH
  true  # need some command in "THEN" list
elif [ "$HERMES_SYS_TYPE" = "linux_x86_64_pgi_gnu" ]; then
  # MPI_ROOT=/usr/local/apps/mpich/1.2.7p1/p4/pgi
  # MPI_LIB="-L$MPI_ROOT/lib -lmpich"
  # export MPI_ROOT MPI_LIB
  # USE_32BIT_IDL=1; export USE_32BIT_IDL
  extra_ARCH_LIST='linux_x86_pgi_gnu'
elif [ "$HERMES_SYS_TYPE" = "linux_x86_64_gnu" ]; then
  # MPI_ROOT=/usr/local/apps/mpich/1.2.7p1/p4/pgi
  # MPI_LIB="-L$MPI_ROOT/lib -lmpich"
  # export MPI_ROOT MPI_LIB
  # USE_32BIT_IDL=1; export USE_32BIT_IDL
  extra_ARCH_LIST='linux_x86_gnu'
elif [ "$HERMES_SYS_TYPE" = "linux_x86_64_intel" ]; then
  # MPI_ROOT=/usr/local/apps/mpich/1.2.7p1/p4/intel
  # MPI_LIB="-L$MPI_ROOT/lib -lmpich"
  # USE_32BIT_IDL=1; export USE_32BIT_IDL
  # intellib="`which icc | sed 's?bin/icc$??'`lib"
  # if [ -z "$LD_LIBRARY_PATH" ]; then LD_LIBRARY_PATH="$intellib"
  # else   LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$intellib"; fi
  # export LD_LIBRARY_PATH
  extra_ARCH_LIST='linux_x86_intel'
elif [ "$HERMES_SYS_TYPE" = "darwin_x86_64_intel" ]; then
  # unset IDLROOT
  # USE_32BIT_IDL=1; export USE_32BIT_IDL
  extra_ARCH_LIST='darwin_x86_intel'
fi

HERMES_ARCH_LIST=$HERMES_SYS_TYPE

# if extra_ARCH_LIST is set, add it to HERMES_ARCH_LIST
if [ -n "$extra_ARCH_LIST" ]; then
  HERMES_ARCH_LIST="$HERMES_ARCH_LIST $extra_ARCH_LIST"
fi

HERMES_GLOBAL=$HERMES_ROOT/etc/
HERMES_BIN=$HERMES_ROOT/bin
HERMES_LIB=$HERMES_ROOT/lib
export HERMES_GLOBAL HERMES_SYS_TYPE HERMES_ARCH_LIST HERMES_BIN HERMES_LIB
#
#  Add HERMES_BIN to the users current path.
#
echo ":$PATH:" | grep ":$HERMES_BIN:" > /dev/null
if [ $? != 0 ]; then
   addtopath=$HERMES_BIN/$HERMES_SYS_TYPE
   if echo "$HERMES_SYS_TYPE" | grep "_64_" > /dev/null; then
     dir32=`echo "$HERMES_SYS_TYPE" | sed 's/_64_/_/'`
     addtopath=$addtopath:$HERMES_BIN/$dir32
   fi
   PATH=$addtopath:$HERMES_BIN:$PATH; export PATH
fi

#
#  If MPI_ROOT is defined add $MPI_ROOT/bin to the users current path.
#
if [ -n "$MPI_ROOT" -a -d "$MPI_ROOT" ]; then
  export MPI_ROOT MPI_LIB
  if ! echo ":$PATH:" | grep ":$MPI_ROOT/bin:" > /dev/null; then
    PATH=$MPI_ROOT/bin:$PATH; export PATH
  fi
fi

# Define list of python executables that will use the Hermes' python tools.
# If you don't plan to use Hermes' python tools, leave HERMES_PYTHON_TARGETS
# blank or undefined.
# WARNING: Do NOT use the target "python" in this list; use only
# version-qualified python executable names.
# NOTE: If multiple python executables are specified, make sure that the first
#       listed is NOT earlier than version 2.6 
### HERMES_PYTHON_TARGETS=python3.7:python2.7; export HERMES_PYTHON_TARGETS

if [ -n "$HERMES_PYTHON_TARGETS" -a -d $HERMES_ROOT/python ]; then

  versions=""
  tlist=`echo $HERMES_PYTHON_TARGETS | \
         awk -F: '{s="";for(i=1; i<=NF; ++i) s = s " " $i; print s}'`
  pre26ver=0
  post25ver=1
  bad_exe=""
  for t in $tlist; do
    which $t >/dev/null 2>&1
    if [ $? -ne 0 ]; then
      bad_exe="$bad_exe $t"
    else
      nv=`$t -V 2>&1 | awk '{split($2,a,"."); print a[3]+100*(a[2]+100*a[1]) }'`
      if [ $nv -lt 20600 ]; then pre26ver=1
      elif [ $nv -ge 20600 ]; then post25ver=1; fi
      versions=$versions:$nv
    fi
  done
  if [ -n "$bad_exe" ]; then
    echo "WARNING: The following Python executables could not be found and will
         be removed from HERMES_PYTHON_TARGETS:
             $bad_exe"
    tlist=":$HERMES_PYTHON_TARGETS:"
    for bad in $bad_exe; do
      tlist=`echo $tlist | awk '{sub(":" "'$bad'",""); print}'`
    done
    unset bad
    HERMES_PYTHON_TARGETS=`echo $tlist | \
                           awk '{sub("^:","");sub(":$","");print}'`
  fi    
  HERMES_PYTHON_VERSIONS=`echo $versions | awk '{sub(":",""); print}'`
  export HERMES_PYTHON_TARGETS HERMES_PYTHON_VERSIONS

  pyexe=`echo "$HERMES_PYTHON_TARGETS" | awk -F: '{print $1}'`

  # This provides the python version that python shell scripts invoked with
  # the the first line of the executable file containing "#!/usr/bin/env python"
  if [ ! -f $HERMES_BIN/python ]; then
    setPythonLink $pyexe
  fi

  echo "primary Python executable: $pyexe"

  # Add Hermes python modules to PYTHONPATH
  #
  pbase=$HERMES_ROOT/python
  mod_dir=$pbase/modules
  # if the only python version specified is pre-2.6, then need to use
  # Hermes' pre-2.6 modules directory
  if [ $pre26ver -eq 1 -a $post25ver -eq 0 ]; then
    mod_dir=$pbase/modules2.5
  fi
  pver=`$pyexe -V 2>&1| awk '{split($2,a,".");print a[1] "." a[2]}'`
  needed="$pbase/extensions/$HERMES_SYS_TYPE-$pver $mod_dir"
  if [ -z "$PYTHONPATH" ]; then
    PYTHONPATH=`echo $needed | \
                awk '{s=$NF; for(i=NF-1;i>0;--i) s = s ":" $i; print s}'`
  else
    for p in $needed; do
      if ! echo ":$PYTHONPATH:" | grep ":$p:" >/dev/null; then
        PYTHONPATH=$p:$PYTHONPATH
      fi
    done
    unset p
  fi
  export PYTHONPATH
  unset pxexe needed pver mod_dir pbase bad_exe pre26ver post25ver versions
  unset tlist t nv

  # If you plan to use both pre-2.6 and post-2.5 versions of python,
  # you need to define the PYTHONSTARTUP variable in hermesenv.sh,
  # if it is not already in your environment. The specified startup
  # file should contain code to make sure the proper hermes module
  # directory is on the python path, following the directions provided
  # in $HERMES_ROOT/README.
  ### PYTHONSTARTUP=$HOME/.pythonrc; export PYTHONSTARTUP

fi

#
#  IDL/PFIDL environmental variables (needed for Mercury w/ IDL-graphics)
#
if [ -n "$IDLROOT" ]; then
  if [ ! -d $IDLROOT ]; then
    idlexe=`which idl 2>/dev/null | grep '/idl$'`
    if [ -n "$idlexe" ]; then
      IDLROOT=`echo $idlexe | \
               awk -F/ '{ for(i=NF;i>0;--i) if ($i == "bin") break
                          if($1 != "") printf "%s", $1
                          for(j=2;j<i-1;++j)printf "/%s", $j; printf "\n"}'`
    fi
    if [ -d $IDLROOT ]; then
      IDL_DIR=$IDLROOT/idl
      export IDLROOT IDL_DIR
    else
      echo "hermesenv.sh: Warning: IDLROOT cannot be located !!" 2>&1
    fi
  fi
fi

#
#  Set up help environment if files exist
#
tiohelp=$HERMES_ROOT/doc/Tiolib.pdf
if [ -f $tiohelp ]; then
  TIOhelp=$tiohelp; export TIOhelp
fi
# if acroread is not available, set TIO_help_reader to an alternate PDF reader
##TIO_help_reader=okular; export TIO_help_reader

bldpffhelp=$HERMES_ROOT/doc/Bldpff.pdf
if [ -f $bldpffhelp ]; then
  BLDPFFhelp=$bldpffhelp; export BLDPFFhelp
fi
