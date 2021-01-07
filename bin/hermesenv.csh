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
  cat <<EOF > /dev/stderr
hermesenv.csh: Warning: Not customized for this installation!!
EOF
  exit 1
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
	      setenv HERMES_SYS_TYPE linux_x86_gnu
	      #### setenv HERMES_SYS_TYPE linux_x86_intel
            breaksw
	    case "ia64":
	      setenv HERMES_SYS_TYPE linux_ia64_intel
            breaksw
	    case "x86_64":
	      setenv HERMES_SYS_TYPE linux_x86_64_gnu
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
else if ( $HERMES_SYS_TYPE == "linux_x86_64_gnu" ) then
  # setenv MPI_ROOT /usr/local/apps/mpich/1.2.7p1/p4/pgi
  # setenv MPI_LIB "-L$MPI_ROOT/lib -lmpich"
  set extra_ARCH_LIST = "linux_x86_gnu"
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

# Define list of python executables that will use the Hermes' python tools.
# If you don't plan to use Hermes' python tools, leave HERMES_PYTHON_TARGETS
# undefined.
# WARNING: Do NOT use the target "python" in this list; use only
# version-qualified python executable names.
# NOTE: If multiple python executables are specified, make sure that the first
#       listed is NOT earlier than version 2.6 
### setenv HERMES_PYTHON_TARGETS=python3.7:python2.7

if ( $?HERMES_PYTHON_TARGETS != 0 ) then
  if ( "$HERMES_PYTHON_TARGETS" != "" ) then
    set versions = ""
    set tlist = `echo $HERMES_PYTHON_TARGETS | \
                 awk -F: '{s="";for(i=1; i<=NF; ++i) s = s " " $i; print s}'`
    set pre26ver = 0
    set post25ver = 1
    set bad_exe = ""
    foreach t  ($tlist)
      which $t  >& /dev/null
      if ( $? != 0 ) then
        set bad_exe = "$bad_exe $t"
      else
        set tmp = `$t -V >& /dev/stdout`
        set nv = `echo $tmp | \
                  awk '{split($2,a,"."); print a[3]+100*(a[2]+100*a[1]) }'`
        if ( $nv < 20600 ) then
          set pre26ver = 1
        else if ( $nv >= 20600 ) then
          set post25ver = 1
        endif
        set versions = $versions':'$nv
      endif
    end 

    if ( "$bad_exe" != "" ) then
      cat <<EOF

WARNING: The following Python executables could not be found and will be
         removed from HERMES_PYTHON_TARGETS:
             $bad_exe
             
EOF
      set tlist = ':'$HERMES_PYTHON_TARGETS':'
      foreach bad ($bad_exe)
        set tlist = `echo $tlist | awk '{sub(":" "'$bad'",""); print}'`
      end
      unset bad
      setenv HERMES_PYTHON_TARGETS `echo $tlist | \
                                    awk '{sub("^:","");sub(":$","");print}'`
    endif
    setenv HERMES_PYTHON_VERSIONS `echo $versions | awk '{sub(":",""); print}'`

    set pyexe = `echo "$HERMES_PYTHON_TARGETS" | awk -F: '{print $1}'`

    # This provides the python version that python shell scripts invoked with
    # the the first line of the executable file containing
    # "#!/usr/bin/env python"
    if ( ! -f $HERMES_BIN/python ) then
      setPythonLink $pyexe
    endif

    echo "primary Python executable: $pyexe"

    # Add Hermes python modules to PYTHONPATH
    #
    set pbase = $HERMES_ROOT/python
    set mod_dir = $pbase/modules
    # if the only python version specified is pre-2.6, then need to use
    # Hermes' pre-2.6 modules directory
    if ( $pre26ver == 1 && $post25ver == 0 ) then
      set mod_dir = $pbase/modules2.5
    endif
    set pver = `$pyexe -V |& awk '{split($2,a,".");print a[1] "." a[2]}'`
    set needed = "$pbase/extensions/$HERMES_SYS_TYPE-$pver $mod_dir"
    if ( $?PYTHONPATH == 0 ) then
      setenv PYTHONPATH `echo $needed | \
                  awk '{s=$NF; for(i=NF-1;i>0;--i) s = s ":" $i; print s}'`
    else
      set pypath = $PYTHONPATH
      set mods = 0
      foreach p ($needed)
        echo "\:$pypath\:" | grep "\:$p\:" > /dev/null
        if ( $status != 0) then
          set pypath = $p\:$pypath
          set mods = 1
        endif
      end
      if ( $mods != 0 ) then
        setenv PYTHONPATH $pypath
      endif
    endif

    # If you plan to use both pre-2.6 and post-2.5 versions of python,
    # you need to define the PYTHONSTARTUP variable in hermesenv.sh,
    # if it is not already in your environment. The specified startup
    # file should contain code to make sure the proper hermes module
    # directory is on the python path, following the directions provided
    # in $HERMES_ROOT/README.
    ### setenv PYTHONSTARTUP $HOME/.pythonrc

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

#
#  Set up help environment if files exist
#
set tiohelp = $HERMES_ROOT/doc/Tiolib.pdf
if ( -f $tiohelp ) then
  setenv TIOhelp $tiohelp
endif
# if acroread is not available, set TIO_help_reader to an alternate PDF reader
##setenv TIO_help_reader okular

set bldpffhelp = $HERMES_ROOT/doc/Bldpff.pdf
if ( -f $bldpffhelp ) then
  setenv BLDPFFhelp $bldpffhelp
endif
