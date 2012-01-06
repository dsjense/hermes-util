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

LIB_LIST = mdutil nserver cidl p2d pff pffc stak tio
MPI_LIB_LIST = pffc tio

PLATFORM = $(HERMES_SYS_TYPE)
MAKEFILE = GNUmakefile
DEBUG =
TYPE =
SUB_TARGET =
GOOD2GO = yes
DIR_LIST = $(LIB_LIST)

include $(HERMES_GLOBAL)makeopts.$(PLATFORM)

# check for legal cross-compiling on this platform
ifeq ($(strip $(findstring :$(PLATFORM):,$(HU_SYS_LOCAL))),)
  GOOD2GO=no
  ERR_MSG=Need to use system from following list: $(subst :, ,$(HU_SYS_LOCAL))
endif

# check if this platform supports shared libraries
ifneq ($(strip $(SHARED)),)
  ifeq ($(strip $(SHR_EXT)),)
    GOOD2GO = no
    ERR_MSG = Shared library not supported on $(PLATFORM)
  endif
endif

ifneq ($(strip $(MPI)),)
  LIB_LIST = $(MPI_LIB_LIST)
endif

ifeq ($(strip $(TYPE)),test)
  DIR_LIST = $(addsuffix /test, $(LIB_LIST))
else
  ifeq ($(strip $(TYPE)),tools)
    DIR_LIST = $(addsuffix /tools, $(LIB_LIST))
  else
    TYPE = LIBS
  endif
endif

.PHONY : dbg $(HERMES_ARCH_LIST) test tools DEBUG dbg SHARED shared MPI mpi
.PHONY : help depend
.PHONY : targets $(DIR_LIST)

#
#--------------------------- Target Section -----------------------------
#

ifeq ($(strip $(GOOD2GO)),no)
targets test tools DEBUG dbg SHARED shared $(HERMES_ARCH_LIST) :
	@echo $(ERR_MSG)
else
targets : $(DIR_LIST)

$(DIR_LIST) :
	@if [ -d $@ ]; then \
	  $(MAKE) -C $@ PLATFORM=$(PLATFORM) DEBUG=$(DEBUG) SHARED=$(SHARED) \
	          $(SUB_TARGET); \
	fi; \

test tools :
	@$(MAKE) DEBUG=$(DEBUG) SHARED=$(SHARED) TYPE=$@ targets

DEBUG dbg :
	@$(MAKE) DEBUG=1  SHARED=$(SHARED) MPI=$(MPI) targets

SHARED shared :
	@$(MAKE) SHARED=1 DEBUG=$(DEBUG) MPI=$(MPI) targets

MPI mpi :
	@$(MAKE) DEBUG=$(DEBUG) SHARED=$(SHARED) MPI=1 targets

$(HERMES_ARCH_LIST) :
	@$(MAKE) PLATFORM=$@ DEBUG=$(DEBUG) SHARED=$(SHARED) MPI=$(MPI) targets

endif
help :
	@echo ""
	@echo "The following options are available:"
	@echo "  make         Builds optimized libraries"
	@echo "  make test    Builds library test programs (optimized)"
	@echo "  make tools   Builds library-associated tools (optimized)"
	@echo "  make debug   Builds debug libraries"
	@echo "  make shared  Builds optimized, shared libraries, if supported"
	@echo "  make mpi     Builds optimized MPI versions of libraries with MPI support"
	@echo "  \"arch\"             Cross-compiles and cross-links for the architecture"
	@echo "                     \"arch\", where \"arch\" is one of the following:"
	@echo "                       $(HERMES_ARCH_LIST)"
	@echo ""
	@echo "By adding the macro definitions \"DEBUG=1\", \"MPI=1\", and/or \"SHARED=1\" to the"
	@echo "first seven options above, the corresponding library permutation will be built."
	@echo "By adding the macro definition \"PLATFORM=arch\" to any of the first six options,"
	@echo "where \"arch\" is on the list of architectures above, cross-compilation/linking"
	@echo "will be performed."
	@echo ""

depend :
	@$(MAKE) DEBUG="" SHARED="" MPI=$(MPI) SUB_TARGET=depend targets


