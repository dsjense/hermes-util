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
#     Standard Hermes Library Makefile
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
PRODUCT = nserver
PLATFORM = $(HERMES_SYS_TYPE)
MAKEFILE = GNUmakefile
DEBUG =
CPP_OPTS =
FPP_OPTS =
F_OPTS =
BLD_SRC_EXTS = -sF -sf -sc
BLD_INC_EXTS = -iinc -ih
LGRPFLAGS = 
LIBBASE = ..
FFLAGS_OD = $(FFLAGS_OPT)
CFLAGS_OD = $(CFLAGS_OPT)
FFLAGS_SHR = 
GOOD2GO = yes
C_INCDIRS = -I../../cinclude
F_INCDIRS = 
BLDDEPLST_ARGS =

include $(HERMES_GLOBAL)makeopts.$(PLATFORM)

# check for legal cross-compiling on this platform
ifeq ($(strip $(findstring :$(PLATFORM):,$(HU_SYS_LOCAL))),)
  GOOD2GO=no
  ERR_MSG=Need to use system from following list: $(subst :, ,$(HU_SYS_LOCAL))
endif

ifneq ($(strip $(DEBUG)),)
  FFLAGS_OD = $(FFLAGS_DBG)
  CFLAGS_OD = $(CFLAGS_DBG)
  LIBBASE = ../debug
endif

ifneq ($(strip $(SHARED)),)
  FFLAGS_SHR = $(SHR_FFLAGS)
  CFLAGS_SHR = $(SHR_CFLAGS)
  # check if this platform supports shared libraries
  ifeq ($(strip $(SHR_EXT)),)
    GOOD2GO = no
    ERR_MSG = Shared library not supported on $(PLATFORM)
  endif
endif

PRODNAME=$(PRODUCT)

MACRO_LIST_FILE = $(PRODNAME)_macro_list
DEP_LIST_FILE = $(PRODNAME)_dep_list

CFLAGS = $(CFLAGS_ALL) $(CFLAGS_OD) $(CFLAGS_SHR) $(CFLAGS_ANSI)
FFLAGS = $(FFLAGS_ALL) $(FFLAGS_OD) $(FFLAGS_SHR)
CPPFLAGS = $(CFLAGS_SYS) $(CPP_OPTS)
FPPFLAGS = $(FPPFLAGS_SYS) $(FPP_OPTS)
FFLAGS_FPP = $(FFLAGS_SYS) $(F_OPTS)
DEP_INCDIRS = $(C_INCDIRS) $(F_INCDIRS)

LIBDIRNAME = $(PLATFORM)
LIBDIR  = $(LIBBASE)/$(LIBDIRNAME)
LIBRARY = $(LIBDIR)/lib$(PRODNAME).a
SHR_LIB = $(LIBDIR)/lib$(PRODNAME).$(SHR_EXT)

TARGET = $(LIBRARY)
ifneq ($(strip $(SHARED)),)
  LIBRARY = $(LIBDIR)/lib$(PRODNAME)_shr.a
  TARGET = $(SHR_LIB)
endif

(%.o) : %.F
	$(FC) -c $(FFLAGS) $(F_INCDIRS) $(FFLAGS_FPP) $<
	$(AR) rv $@ $*.o
	rm -f $*.o

(%.o) : %.f
	$(FC) -c $(FFLAGS) $<
	$(AR) rv $@ $*.o
	rm -f $*.o

(%.o) : %.c
	$(CC) -c $(CFLAGS) $(C_INCDIRS) $(CPPFLAGS) $<
	$(AR) rv $@ $*.o
	rm -f $*.o

ifneq ($(strip $(wildcard $(MACRO_LIST_FILE))),)
  include $(MACRO_LIST_FILE)
endif

.PHONY : first target DEBUG dbg SHARED shared $(HERMES_ARCH_LIST)
.PHONY : help bld_macro_list depend nmakedepend
#
#--------------------------- Target Section -----------------------------
#

ifeq ($(strip $(GOOD2GO)),no)
first target DEBUG dbg SHARED shared $(HERMES_ARCH_LIST) :
	@echo $(ERR_MSG)
else
first :
	@$(MAKE) PLATFORM=$(PLATFORM) DEBUG=$(DEBUG) SHARED=$(SHARED) \
	         depend $(LIBDIR)
	@$(MAKE) PLATFORM=$(PLATFORM) DEBUG=$(DEBUG) SHARED=$(SHARED) target

target :
	@$(MAKE) -f $(MAKEFILE) DEBUG=$(DEBUG) SHARED=$(SHARED) $(TARGET)

DEBUG dbg :
	@$(MAKE) -f $(MAKEFILE) DEBUG=1  SHARED=$(SHARED) first

SHARED shared :
	@$(MAKE) -f $(MAKEFILE) SHARED=1 DEBUG=$(DEBUG) first

$(HERMES_ARCH_LIST) :
	@$(MAKE) -f $(MAKEFILE) PLATFORM=$@ DEBUG=$(DEBUG) SHARED=$(SHARED) \
	        first
endif

$(LIBDIR) :
	@if [ ! -d $(LIBDIR) ]; then mkdir -p $(LIBDIR); fi

help :
	@echo ""
	@echo "The following options are available:"
	@echo "  make               Builds optimized version"
	@echo "  make debug         Builds debug version"
	@echo "  make shared        Builds optimized shared library"
	@echo "  \"arch\"             Cross-compiles and cross-links for the architecture"
	@echo "                     \"arch\", where \"arch\" is one of the following:"
	@echo "                       $(HERMES_ARCH_LIST)"
	@echo "  make depend        Updates dependency files"
	@echo "  make nmakedepend   Updates NMAKE-compatable dependency files (WIN32)"
	@echo ""
	@echo "By adding the macro definitions \"DEBUG=1\" and/or \"SHARED=1\" to the first four"
	@echo "options above, the corresponding library permutation will be built."
	@echo ""

##.SILENT : depend bld_macro_list $(MACRO_LIST_FILE) $(DEP_LIST_FILE)

depend :
	@$(MAKE) -f $(MAKEFILE) bld_macro_list
	@$(MAKE) -f $(MAKEFILE) $(DEP_LIST_FILE)

$(MACRO_LIST_FILE) bld_macro_list :
	@bld_macro_list $(BLD_SRC_EXTS) $(BLD_INC_EXTS) $(LGRPFLAGS) -ld \
                        -SSRCS -IINCS $(PRODNAME)

$(DEP_LIST_FILE) : $(SRCS) $(INCS)
	@bld_dep_list $(DEP_INCDIRS) $(FPPFLAGS) $(CPPFLAGS) $(BLD_SRC_EXTS) \
	              $(BLDDEPLST_ARGS) $(LGRPFLAGS) -l -e0 $(PRODNAME)

nmakedepend :
	@bld_macro_list $(BLD_SRC_EXTS) $(BLD_INC_EXTS) $(LGRPFLAGS) -W \
	                -SSRCS -IINCS $(PRODNAME) .nmake
	@bld_dep_list $(DEP_INCDIRS) $(FPPFLAGS) $(CPPFLAGS) $(BLD_SRC_EXTS) \
	              $(BLDDEPLST_ARGS) $(LGRPFLAGS) -W -e0 $(PRODNAME) .nmake

$(LIBRARY) : $(OBJS)
	@echo $(LIBRARY) is now up-to-date

$(SHR_LIB):	$(LIBRARY)
		$(AR) x $(LIBRARY)
		$(FC) $(SHR_LFLAGS) -o $(SHR_LIB) *.o $(LIBS)
		rm -f *.o

#
# ------------------ Include File Dependency Section --------------------
#
ifneq ($(strip $(wildcard $(DEP_LIST_FILE))),)
  include $(DEP_LIST_FILE)
endif

#
