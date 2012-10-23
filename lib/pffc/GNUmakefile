#-----------------------------------------------------------------------
#     $Id$
#     
#     Copyright (2008) Sandia Corporation. Under the terms of
#     Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
#     Government retains certain rights in this software.
#     
#     Hermes is free software: you can redistribute it and/or modify
#     it under the terms of the GNU Lesser General Public License as
#     published by the Free Software Foundation, either version 3 of
#     the License, or (at your option) any later version.
#     
#     Hermes is distributed in the hope that it will be useful, but
#     WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU Lesser General Public License for more details.
#     
#     You should have received a copy of the GNU Lesser General
#     Public License along with Hermes.  If not, see
#     <http://www.gnu.org/licenses/>.
#     
#-----------------------------------------------------------------------
#
#     Standard HERMES Library Makefile
#
#-----------------------------------------------------------------------
#
#   This Makefile depends on the following environmental variables:
#
#   variable                      defined in
#   --------                      ----------
#    HERMES_ARCH_LIST   'hermes_root'/bin/hermesenv.[sh|csh]
#    HERMES_GLOBAL      'hermes_root'/bin/hermesenv.[sh|csh]
#
PRODUCT = pffc
PLATFORM = $(HERMES_SYS_TYPE)
MAKEFILE = GNUmakefile
DEBUG =
PRODUCT_EXT = 
CPP_OPTS =
BLD_SRC_EXTS = -sc -scc
BLD_INC_EXTS = -ih -ihh
LGRPFLAGS = 
DBGLIBBASE = 
CFLAGS_OD = $(CFLAGS_OPT)
CXXFLAGS_OD = $(CXXFLAGS_OPT)
CFLAGS_SHR = 
GOOD2GO = yes
C_INCDIRS = 
F_INCDIRS = 
BLDDEPLST_ARGS =

include $(HERMES_GLOBAL)makeopts.$(PLATFORM)

# check for legal cross-compiling on this platform
ifeq ($(strip $(findstring :$(PLATFORM):,$(HU_SYS_LOCAL))),)
  GOOD2GO=no
  ERR_MSG=Need to use system from following list: $(subst :, ,$(HU_SYS_LOCAL))
endif

ifneq ($(strip $(DEBUG)),)
  CFLAGS_OD = $(CFLAGS_DBG)
  CXXFLAGS_OD = $(CXXFLAGS_DBG)
  DBGLIBBASE = /debug
endif

LIST_NAME = $(PRODUCT)
ifneq ($(strip $(MPI)),)
  PRODUCT_EXT = _mpi
  CPP_OPTS = -DUSE_MPI
  CC=$(MPI_CC)
  ifeq ($(strip $(MPI_SYS)),)
    C_INCDIRS += -I$(MPI_ROOT)/include
    F_INCDIRS += -I$(MPI_ROOT)/include
    BLDDEPLST_ARGS = -VMPI_ROOT
    LIST_NAME = $(PRODUCT)_mpi
  endif
endif

PIC_EXT = 
ifneq ($(strip $(PIC)),)
  CFLAGS_SHR = $(SHR_CFLAGS)
  PIC_EXT = _pic
endif

ifneq ($(strip $(SHARED)),)
  CFLAGS_SHR = $(SHR_CFLAGS)
  PIC_EXT = _pic
  # check if this platform supports shared libraries
  ifeq ($(strip $(SHR_EXT)),)
    GOOD2GO = no
    ERR_MSG = Shared library not supported on $(PLATFORM)
  endif
endif

PRODNAME=$(PRODUCT)$(PRODUCT_EXT)

MACRO_LIST_FILE = $(LIST_NAME)_macro_list
DEP_LIST_FILE = $(LIST_NAME)_dep_list

CFLAGS = $(CFLAGS_ALL) $(CFLAGS_OD) $(CFLAGS_SHR) $(CFLAGS_ANSI)
CXXFLAGS += $(CXXFLAGS_ALL) $(CXXFLAGS_OD) $(CFLAGS_SHR) $(C_INCDIRS)
CPPFLAGS = $(CFLAGS_SYS) $(CPP_OPTS)
DEP_INCDIRS = $(C_INCDIRS)

LIBDIRNAME = $(PLATFORM)
LIBBASE = ..$(DBGLIBBASE)
LIBDIR  = $(LIBBASE)/$(LIBDIRNAME)
LIBRARY = $(LIBDIR)/lib$(PRODNAME)$(PIC_EXT).a
SHR_LIBDIR = ../shared$(DBGLIBBASE)/$(LIBDIRNAME)
SHR_LIB = $(SHR_LIBDIR)/lib$(PRODNAME).$(SHR_EXT)

TARGET = $(LIBRARY)
ifneq ($(strip $(SHARED)),)
  TARGET = $(SHR_LIB)
endif

(%.o) : %.c
	$(CC) -c $(CFLAGS) $(C_INCDIRS) $(CPPFLAGS) $<
	$(AR) rv $@ $*.o
	rm -f $*.o

(%.o) : %.cc
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) $<
	$(AR) rv $@ $*.o
	rm -f $*.o

ifneq ($(strip $(wildcard $(MACRO_LIST_FILE))),)
  include $(MACRO_LIST_FILE)
endif

.PHONY : first target DEBUG dbg SHARED shared PIC pic MPI mpi
.PHONY : $(HERMES_ARCH_LIST) help bld_macro_list depend nmakedepend
#
#--------------------------- Target Section -----------------------------
#

ifeq ($(strip $(GOOD2GO)),no)
first target DEBUG dbg SHARED shared PIC pic MPI mpi $(HERMES_ARCH_LIST) :
	@echo $(ERR_MSG)
else
first :
	@$(MAKE) PLATFORM=$(PLATFORM) DEBUG=$(DEBUG) SHARED=$(SHARED) \
	         PIC=$(PIC) MPI=$(MPI) depend $(LIBDIR)
	@$(MAKE) PLATFORM=$(PLATFORM) DEBUG=$(DEBUG) SHARED=$(SHARED) \
	         PIC=$(PIC) MPI=$(MPI) target

target :
	@$(MAKE) -f $(MAKEFILE) DEBUG=$(DEBUG) MPI=$(MPI) SHARED=$(SHARED) \
	         PIC=$(PIC) $(TARGET)

DEBUG dbg :
	@$(MAKE) -f $(MAKEFILE) DEBUG=1 PIC=$(PIC) SHARED=$(SHARED) MPI=$(MPI) \
		  first

MPI mpi :
	@$(MAKE) -f $(MAKEFILE) MPI=1 DEBUG=$(DEBUG) SHARED=$(SHARED) \
		 PIC=$(PIC)  first

PIC pic :
	@$(MAKE) -f $(MAKEFILE) MPI=$(MPI) DEBUG=$(DEBUG) SHARED= PIC=1  first

SHARED shared :
	@$(MAKE) -f $(MAKEFILE) SHARED=1 DEBUG=$(DEBUG) MPI=$(MPI) PIC=  first

$(HERMES_ARCH_LIST) :
	@$(MAKE) -f $(MAKEFILE) PLATFORM=$@ DEBUG=$(DEBUG) MPI=$(MPI) \
	         PIC=$(PIC) SHARED=$(SHARED) first
endif

$(LIBDIR) :
	@if [ ! -d $(LIBDIR) ]; then mkdir -p $(LIBDIR); fi

help :
	@echo ""
	@echo "The following options are available:"
	@echo "  make               Builds optimized version"
	@echo "  make debug         Builds debug version"
	@echo "  make mpi           Builds optimized, mpi version"
	@echo "  make pic           Builds library with position-independent code"
	@echo "  make shared        Builds optimized, shared library"
	@echo "  \"arch\"             Cross-compiles and cross-links for the architecture"
	@echo "                     \"arch\", where \"arch\" is one of the following:"
	@echo "                       $(HERMES_ARCH_LIST)"
	@echo "  make depend        Updates dependency files for non-mpi version"
	@echo "  make MPI=1 depend  Updates dependency files for mpi version"
	@echo "  make nmake            Build tarball for a WIN32 installation"
	@echo "  make nmakedepend      Updates NMAKE-compatable dependency files (WIN32)"
	@echo ""
	@echo "By adding the macro definitions \"DEBUG=1\", \"MPI=1\", \"PIC=1\", and/or \"SHARED=1\""
	@echo "to the first six options above, the corresponding library permutation will be"
	@echo "built."
	@echo ""

##.SILENT : depend bld_macro_list $(MACRO_LIST_FILE) $(DEP_LIST_FILE)

depend :
	@$(MAKE) -f $(MAKEFILE) MPI=$(MPI) bld_macro_list
	@$(MAKE) -f $(MAKEFILE) MPI=$(MPI) $(DEP_LIST_FILE)

$(MACRO_LIST_FILE) bld_macro_list :
	@bld_macro_list $(BLD_SRC_EXTS) $(BLD_INC_EXTS) $(LGRPFLAGS) -ld \
                       -SSRCS -IINCS $(LIST_NAME)

$(DEP_LIST_FILE) : $(SRCS) $(INCS)
	@bld_dep_list $(DEP_INCDIRS) $(FPPFLAGS) $(CPPFLAGS) $(BLD_SRC_EXTS) \
	              $(BLDDEPLST_ARGS) $(LGRPFLAGS) -l -e0 $(LIST_NAME)

nmakedepend :
	@bld_macro_list $(BLD_SRC_EXTS) $(BLD_INC_EXTS) $(LGRPFLAGS) -W \
	      -SSRCS -IINCS $(LIST_NAME) .nmake
	@bld_dep_list $(DEP_INCDIRS) $(FPPFLAGS) $(CPPFLAGS) $(BLD_SRC_EXTS) \
	              $(BLDDEPLST_ARGS) $(LGRPFLAGS) -W -e0 $(LIST_NAME) .nmake

$(LIBRARY) : $(OBJS)
	@echo $(LIBRARY) is now up-to-date

$(SHR_LIB):	$(LIBRARY)
		@if [ ! -d $(SHR_LIBDIR) ]; then mkdir -p $(SHR_LIBDIR); fi
		$(AR) x $(LIBRARY)
		$(CC) $(CC_SHR_LFLAGS) -o $(SHR_LIB) *.o $(LIBS)
		rm -f *.o

#
# ------------------ Include File Dependency Section --------------------
#
ifneq ($(strip $(wildcard $(DEP_LIST_FILE))),)
  include $(DEP_LIST_FILE)
endif

#
