# PFFC TOOLS makefile
#
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
#
PRODUCT = pffctools
BLD_SRC_EXTS = -sc -scc
BLD_INC_EXTS = -ih
PRODUCT_EXT =
PLATFORM = $(HERMES_SYS_TYPE)
BIN_ROOT = .
CPPFLAGS =
GOOD2GO = yes
EXE = none
DBG_EXT =
BLDDEPLST_ARGS = -VHERMES_LIB

include $(HERMES_GLOBAL)makeopts.$(PLATFORM)

# check for legal cross-compiling on this platform
ifeq ($(strip $(findstring :$(PLATFORM):,$(HU_SYS_LOCAL))),)
  GOOD2GO=no
  ERR_MSG=Need to use system from following list: $(subst :, ,$(HU_SYS_LOCAL))
endif

ifeq ($(strip $(DEBUG)),)
 USE_DBG = no
else
 USE_DBG = yes
endif

MAKEFILE = GNUmakefile
SHELL    = /bin/sh

LIBDEST = ./Build_$(PLATFORM)

ifeq ($(strip $(DEBUG)),libs)
 LIBDIR = $(HERMES_LIB)/debug/$(PLATFORM)
else
 LIBDIR = $(HERMES_LIB)/$(PLATFORM)
endif

PRODNAME=$(PRODUCT)$(PRODUCT_EXT)

MACRO_LIST_FILE = $(PRODNAME)_macro_list
DEP_LIST_FILE = $(PRODNAME)_dep_list

ifeq ($(USE_DBG),yes)
  DEST    = $(LIBDEST)
  DBG_EXT = _dbg
  CXXFLAGS_OD = $(CXXFLAGS_DBG)
  CFLAGS_OD = $(CFLAGS_DBG)
  NEED_DIRS = $(LIBDEST)
else
  DEST    = $(HERMES_BIN)/$(PLATFORM)
  CXXFLAGS_OD = $(CXXFLAGS_OPT)
  CFLAGS_OD = $(CFLAGS_OPT)
  NEED_DIRS = $(LIBDEST) $(DEST)
endif

LIBRARY = $(LIBDEST)/lib$(PRODNAME)$(DBG_EXT).a
LIBRARY_LINK = -L$(LIBDEST) -l$(PRODNAME)$(DBG_EXT) $(SYSLIB)

C_INCDIRS = $(STL_INC) -I$(HERMES_LIB)/pffc
DEP_INCDIRS = -I$(HERMES_LIB)/pffc
LIBS = -L$(LIBDIR) -lpffc -lm

CFLAGS += $(CFLAGS_ALL) $(CFLAGS_OD) $(C_INCDIRS)
CXXFLAGS += $(CXXFLAGS_ALL) $(CXXFLAGS_OD) $(C_INCDIRS)
CPPFLAGS = $(CFLAGS_SYS) $(CPP_OPTS)

%.cc : %.gen
	txt2cwriter $< $@

(%.o) : %.cc
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) $<
	$(AR) rv $@ $*.o
	rm -f $*.o

(%.o) : %.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $<
	$(AR) rv $@ $*.o
	rm -f $*.o

$(DEST)/% : %.cc
	$(CXX) -o $@ $(CXXFLAGS) $(CPPFLAGS) $< $(LIBS)

$(DEST)/% : %.c
	$(CC) -o $@ $(CFLAGS) $(CPPFLAGS) $< $(LIBS)

ifneq ($(strip $(wildcard $(MACRO_LIST_FILE))),)
  include $(MACRO_LIST_FILE)
endif

.INTERMEDIATE : dump_copyright.c

.PHONY : dbg $(HERMES_ARCH_LIST) depend nmakedepend bld_macro_list
.PHONY : newdepend cleandep doxygen pdf help
.PHONY : first targets $(STANDALONE_TARGETS) $(TARGETS)

ifeq ($(strip $(wildcard $(MACRO_LIST_FILE))),)
no_depfile_target :
	$(MAKE) depend
	$(MAKE)
endif

ifeq ($(strip $(GOOD2GO)),no)
first targets  $(STANDALONE_TARGETS) $(TARGETS) dbg $(HERMES_ARCH_LIST) :
	@echo $(ERR_MSG)
else
first :
	@$(MAKE) PLATFORM=$(PLATFORM) DEBUG=$(DEBUG) depend $(DEST) targets

targets : $(STANDALONE_TARGETS) $(TARGETS)

$(TARGETS) : $(LIBRARY)
	@$(MAKE) DEBUG=$(DEBUG) EXE=$(DEST)/$@ LIBS="$(LIBRARY_LINK) $(LIBS)" \
	        PLATFORM=$(PLATFORM) $(DEST)/$@

$(STANDALONE_TARGETS) :
	@$(MAKE) depend $(DEST)
	@$(MAKE) PLATFORM=$(PLATFORM) DEBUG=$(DEBUG) SEXE=$(DEST)/$@ $(DEST)/$@

dbg:
	@$(MAKE) PLATFORM=$(PLATFORM) DEBUG=1

$(HERMES_ARCH_LIST) :
	@$(MAKE) DEBUG=$(DEBUG) PLATFORM=$@
endif

$(EXE) : $(LIBRARY)

$(NEED_DIRS) :
	@if [ ! -d $@ ]; then mkdir -p $@; fi

$(LIBRARY) : $(LIBDEST) $(LIB_OBJS)

newdepend :
	@$(MAKE) -f $(MAKEFILE) cleandep depend

depend :
	@$(MAKE) -f $(MAKEFILE) bld_macro_list
	@$(MAKE) -f $(MAKEFILE) $(DEP_LIST_FILE)

$(MACRO_LIST_FILE) bld_macro_list :
	@bld_macro_list -nnone $(BLD_SRC_EXTS) $(BLD_INC_EXTS) -SSRCS -O "" \
	                -P $(PRODUCT)
	@bld_macro_list -nmain -nstandalone $(BLD_SRC_EXTS) -OLIB_OBJS -laP \
	                $(PRODUCT)
	@bld_macro_list -mmain -nstandalone $(BLD_SRC_EXTS) -OTARGETS -EaP \
	                $(PRODUCT)
	@bld_macro_list -mstandalone $(BLD_SRC_EXTS) -OSTANDALONE_TARGETS \
	                -Ead $(PRODUCT)

$(DEP_LIST_FILE) : $(SRCS) $(INCS)
	@bld_dep_list -nmain -nstandalone -S'# LIBRARY dependencies' -lP -e0 \
	              $(BLD_SRC_EXTS) $(DEP_INCDIRS) $(CPPFLAGS) \
	              $(BLDDEPLST_ARGS) $(PRODUCT)
	@bld_dep_list -mmain -p'$$(DEST)/' -S'# Target dependencies' -Ea -e0 \
	              $(BLD_SRC_EXTS) $(DEP_INCDIRS) $(CPPFLAGS) \
	              $(BLDDEPLST_ARGS) $(PRODUCT)

nmakedepend :
	@bld_macro_list -nnone $(BLD_SRC_EXTS) $(BLD_INC_EXTS) -SSRCS -O "" \
	               -P $(PRODUCT) .nmake
	@bld_macro_list -nmain $(BLD_SRC_EXTS) -WPa $(PRODUCT) .nmake
	@bld_macro_list -mmain $(BLD_SRC_EXTS) -OMAIN_OBJS -WPa \
	               $(PRODUCT) .nmake
	@bld_macro_list -mmain -nstandalone $(BLD_SRC_EXTS) -OTARGETS -EaWP \
	                $(PRODUCT) .nmake
	@bld_macro_list -mstandalone $(BLD_SRC_EXTS) -OSTANDALONE_TARGETS \
	                -EaW $(PRODUCT) .nmake
	@bld_dep_list $(DEP_INCDIRS) $(FPPFLAGS) $(CPPFLAGS) $(BLD_SRC_EXTS) \
	              $(BLDDEPLST_ARGS) -W -e0 $(PRODUCT) .nmake

cleandep:
	@rm -f $(MACRO_LIST_FILE) $(DEP_LIST_FILE)

doxygen:
	@doxygen pffdiff.doxy

pdf :
	@doxygen latex.doxy; cd latex; gmake pdf

help:
	@echo "Valid Targets:"
	@echo "  [default] -- Makes optimized version for current architecture"
	@echo "  dbg       -- Makes debug version for current architecture"
	@echo "  \"arch\"    -- Cross-compiles and cross-links the tools for the architecture"
	@echo "               \"arch\", where \"arch\" is one of the following:"
	@echo "                 $(HERMES_ARCH_LIST)"
	@echo "  doxygen   -- Builds HTML interface documentation"
	@echo "  pdf       -- Builds PDF interface documentation"
	@echo "  depend    -- Builds dependency files that are out of date"
	@echo "               (automatically performed before any executable is built)"
	@echo "  cleandep  -- Removes all dependency files"
	@echo "  help      -- Generates this list"

#
# ------------------ Explicit dependencies -----------------------------
#

$(LIBRARY) : $(LIBRARY)(pffdiff_manpage.o) $(LIBRARY)(dump_copyright.o)

dump_copyright.c : $(HERMES_ROOT)/etc/Hermes_copyright.txt
	txt2cwriter -p " * " $< $@

#
# ------------------ Include File Dependency Section --------------------
#

ifneq ($(strip $(wildcard $(DEP_LIST_FILE))),)
  include $(DEP_LIST_FILE)
endif
