#-----------------------------------------------------------------------
#     $Id$
#-----------------------------------------------------------------------
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
### setenv HERMES_ROOT /users/user/hermes

if ( $?HERMES_ROOT == 0 ) then
  echo "hermesenv.csh: Warning: Not customized for this installation !!" 2>&1
  return 1
endif

#
#  The following lines must be modified to build or use the CIDL library
#  (or alternately, the environment variable IDLROOT can be predefined)
#
# If IDLROOT is an existing directory, it is used to locate idl. Otherwise,
# if IDLROOT is a non-empty string, the script attempts to  determine
# IDLROOT by searching the PATH environment variable for an executable "idl".

### typical ### if ( $?IDLROOT == 0 ) setenv IDLROOT /usr/local/rsi
### typical ### if ( $?IDLCUSTOM == 0 ) setenv IDLROOT @%@%@%@%@%
# @%@%@%@%@% is presumably NOT the name of a directory

# For 64-bit architectures, if USE_32BIT_IDL is a non-empty string, a 32-bit
# version of IDL is assumed, e.g.,
### setenv USE_32BIT_IDL 1

#  The following line must be modified to specify the names of architectures
#  that are cross-compiled from this machine
set extra_ARCH_LIST = ''

#
# Find system type
#
set OSNAME=`uname -s`
switch ($OSNAME)
case "SunOS":
	switch (`uname -r`)
	    case "4*":
	      setenv HERMES_SYS_TYPE sunos
            breaksw
	    case "5*":
	      setenv HERMES_SYS_TYPE solaris
	  ### setenv HERMES_SYS_TYPE solaris_64
            breaksw
        endsw
        breaksw
case "HP-UX":
        setenv SYSTEM HP
	switch (`uname -r`)
	    case "A.09.*":
	      setenv HERMES_SYS_TYPE hp9
            breaksw
	    case "B.10.*":
	      setenv HERMES_SYS_TYPE hp10
            breaksw
        endsw
        breaksw
case "Linux":
	switch (`uname -m`)
	    case "i686":
	      setenv HERMES_SYS_TYPE linux_x86_pgi_gnu
	      #### setenv HERMES_SYS_TYPE linux_x86_intel
            breaksw
	    case "ia64":
	      setenv HERMES_SYS_TYPE linux_ia64_intel
            breaksw
	    case "x86_64":
	      setenv HERMES_SYS_TYPE linux_x86_64_pgi_gnu
	      #### setenv HERMES_SYS_TYPE linux_x86_64_intel
            breaksw
	    default:
	      setenv HERMES_SYS_TYPE linux
        endsw
        breaksw
case "Darwin":
        setenv HERMES_X11_LINK "-L/usr/X11R6/lib -lX11"
	switch (`uname -m`)
	    case "i386":
	      setenv HERMES_SYS_TYPE darwin_x86_64_intel
            breaksw
	    default:
	      setenv HERMES_SYS_TYPE darwin
        endsw
        setenv SYSTEM IRI
case "IRIX":
case "IRIX64":
        setenv SYSTEM IRI
        setenv HERMES_SYS_TYPE irix
        breaksw
case "OSF1":
        setenv SYSTEM DECOSF
        setenv HERMES_SYS_TYPE decosf
        breaksw
case "UNICOS":
#  Don't break out for UNICOS; let fall through to default
default:
#       Probably a UNICOS machine !!!
        setenv SYSTEM CRA
        setenv HERMES_SYS_TYPE unicos
endsw
if ( $?SYSTEM == 0 ) setenv SYSTEM ''#
#-----------------------------------------------------------------------
#
# if more than one system is using this file (via NFS), might need to adjust
# things based on which system this is
# For systems handling more than one version of MPI, set MPI_ROOT and
# MPI_LIB for the default option, and add environment variables with
# a "_ver" suffix for other versions (see linux_x86_pgi_gnu example)
if ( $HERMES_SYS_TYPE == "solaris" ) then
  # setenv MPI_ROOT /usr/local/mpich_shmem
  # setenv MPI_LIB "-L$MPI_ROOT/lib -lmpich -lthread"
  # set extra_ARCH_LIST = tflop
else if ( $HERMES_SYS_TYPE == "solaris_64" ) then
  # setenv MPI_ROOT /usr/local/mpich_shmem
  # setenv MPI_LIB "-L$MPI_ROOT/lib -lmpich -lthread"
  set extra_ARCH_LIST = solaris
else if ( $HERMES_SYS_TYPE == "decosf" ) then
  # setenv MPI_ROOT ""
  # setenv MPI_LIB "-lfmpi -lmpi -lelan"
else if ( $HERMES_SYS_TYPE == "linux_x86_pgi_gnu" ) then
  # setenv MPI_ROOT /usr/local/apps/mpich/1.2.7p1/p4/pgi
  # setenv MPI_LIB "-L$MPI_ROOT/lib -lmpich"
# gm
##  setenv MPI_ROOT_gm /usr/local/apps/mpich/1.2.6/gm/pgi
##  setenv MPI_LIB_gm "-L$MPI_ROOT_gm/lib -lmpich -L/usr/local/koopa/gm/lib -lgm"
else if ( $HERMES_SYS_TYPE == "linux_x86_64_pgi_gnu" ) then
  # setenv MPI_ROOT /usr/local/apps/mpich/1.2.7p1/p4/pgi
  # setenv MPI_LIB "-L$MPI_ROOT/lib -lmpich"
else if ( $HERMES_SYS_TYPE == "linux_x86_64_intel" ) then
  # setenv MPI_ROOT /usr/local/apps/mpich/1.2.7p1/p4/intel
  # setenv MPI_LIB "-L$MPI_ROOT/lib -lmpich"
else if ( $HERMES_SYS_TYPE == "linux_ia64_intel" ) then
  # setenv MPI_ROOT ""
  # setenv MPI_LIB "-lmpi"
  true  # need some command in "THEN" list
else if ( $HERMES_SYS_TYPE == "darwin_x86_64_intel" ) then
  # setenv MPI_ROOT /usr/local/apps/mpich/1.2.7p1/darwin
  # setenv MPI_LIB "-L$MPI_ROOT/lib -lmpich"
  set extra_ARCH_LIST = darwin_x86_intel
endif

# Presently, MPI_ROOT_C is used for the MPI version of pffc -- it only needs
# to be defined if you build this library
if ( $?MPI_ROOT != 0 ) then
  setenv MPI_ROOT_C $MPI_ROOT
endif

setenv HERMES_ARCH_LIST $HERMES_SYS_TYPE # default value

# if extra_ARCH_LIST is set, add it to HERMES_ARCH_LIST
if ( $?extra_ARCH_LIST != 0 ) then
  setenv HERMES_ARCH_LIST "$HERMES_ARCH_LIST $extra_ARCH_LIST"
endif

#-------------------------------------------------------------------
#
#  General Hermes environmental variables
#
setenv HERMES_BIN $HERMES_ROOT/bin
setenv HERMES_LIB $HERMES_ROOT/lib
setenv HERMES_GLOBAL $HERMES_ROOT/etc/
#
#  Add HERMES_BIN to the users current path:
#
echo ":${PATH}:" | grep ":${HERMES_BIN}:" > /dev/null
if ( $status != 0) then
   set addtopath = $HERMES_BIN/$HERMES_SYS_TYPE
   echo "$HERMES_SYS_TYPE" | grep "_64_" > /dev/null
   if ( $status == 0) then
     set dir32 = `echo "$HERMES_SYS_TYPE" | sed 's/_64_/_/'`
     set addtopath = "$addtopath $HERMES_BIN/$dir32"
   endif
   set path = ($addtopath $HERMES_BIN $path)
endif
#
#  Add $MPI_ROOT/bin to the users current path if it exists
#
if ( $?MPI_ROOT != 0 ) then
  if ( "$MPI_ROOT" != "" ) then
    set mpibin = $MPI_ROOT/bin
    if ( -d $mpibin ) then
      echo "\:$PATH\:" | grep "\:$mpibin\:" > /dev/null
      if ( $status != 0) then
        set path = ($mpibin $path)
      endif
    endif
  endif
endif
#
#  IDL/PFIDL environmental variables
#
if ( $?IDLROOT != 0 ) then
  if ( ! -d "$IDLROOT" ) then
    set idlexe=`which idl | grep '/idl$'`
    if ( $idlexe != "" ) then
      set testroot = `echo $idlexe | sed 's?/idl/.*/idl$??'`
    else
      unset testroot
    endif
    if ( -d "$testroot" ) then
      setenv IDLROOT $testroot
      setenv IDL_DIR $IDLROOT/idl
    else
      unsetenv IDLROOT
      echo "hermesenv.csh: Warning: IDLROOT cannot be located !!"
    endif
  else
    setenv IDL_DIR $IDLROOT/idl
  endif
endif
