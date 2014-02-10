# Makefile for Hermes Tool library
#
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

PRODUCT = hermes
PLATFORM = $(HERMES_SYS_TYPE)
MAKEFILE = GNUmakefile
DEBUG =
GOOD2GO = yes
CPP_OPTS =
LIBBASE = ../lib
DEST = ../bin/$(PLATFORM)

include $(HERMES_GLOBAL)makeopts.$(PLATFORM)

# check for legal cross-compiling on this platform
ifeq ($(strip $(findstring :$(PLATFORM):,$(HU_SYS_LOCAL))),)
  GOOD2GO=no
  ERR_MSG=Need to use system from following list: $(subst :, ,$(HU_SYS_LOCAL))
endif

CFLAGS_OD = $(CFLAGS_OPT)
ifneq ($(strip $(DEBUG)),)
  FFLAGS_OD = $(FFLAGS_DBG)
  CFLAGS_OD = $(CFLAGS_DBG)
  LIBBASE = ../lib/debug
  DEST = ./$(PLATFORM)
endif

PRODNAME=$(PRODUCT)

MACRO_LIST_FILE = $(PRODNAME)_macro_list
DEP_LIST_FILE = $(PRODNAME)_dep_list

CFLAGS = $(CFLAGS_ALL) $(CFLAGS_OD)
CPPFLAGS = $(CFLAGS_SYS) $(CPP_OPTS)
CXXFLAGS = $(CXXFLAGS_ALL) $(CFLAGS_OD) 

TOOLDIR = $(HERMES_ROOT)/tools

LIBDIRNAME = $(PLATFORM)
LIBDIR  = $(LIBBASE)/$(LIBDIRNAME)
LIBNAME = libhermestools.a
LIBPPNAME = libhermestools++.a
LIBRARY = $(LIBDIR)/$(LIBNAME)
LIBCPP = $(LIBDIR)/$(LIBPPNAME)

CPPFLAGS = #  -DNO_SCANDIR

ifneq ($(strip $(wildcard $(MACRO_LIST_FILE))),)
  include $(MACRO_LIST_FILE)
endif

# These are the targets needed to generate "macro" and "dependency" files
BOOTSTRAP_TARGETS = bld_dep_list bld_macro_list

# "function" variable to test if the file "name" does NOT exist
FindFile = $(if $(findstring $(name),$(wildcard $(name))),no,yes)

# If either are missing, or the macro_list_file is missing,
# need to run in Bootstrap mode to build them first.
NEEDED_FILES = $(addprefix $(DEST)/,$(BOOTSTRAP_TARGETS)) $(MACRO_LIST_FILE)
NEED_BOOTSTRAP = $(findstring yes,$(foreach name,$(NEEDED_FILES),$(FindFile)))

ifeq ($(strip $(NEED_BOOTSTRAP)),yes)
# Bootstrap mode -- after this minimal set is made, make dependencies, then
# make everything
  LIB_OBJS = $(LIBRARY)(cppsetup.o) $(LIBRARY)(ifparser.o) \
	$(LIBRARY)(include.o) $(LIBRARY)(mkdep.o) $(LIBRARY)(parse.o) \
	$(LIBRARY)(pr.o) $(LIBRARY)(ev_macros.o)
  LIBCPP_OBJS = $(LIBCPP)(ext_match_list.o) $(LIBCPP)(file_compare.o) \
	$(LIBCPP)(l_group.o) $(LIBCPP)(line_dumper.o)
  TARGETS = $(BOOTSTRAP_TARGETS)
endif

(%.o) : %.c
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $<
	$(AR) rv $@ $*.o
	rm -f $*.o

(%.o) : %.cc
	$(CXX) -c $(CXXFLAGS) $(CPPFLAGS) $<
	$(AR) rv $@ $*.o
	rm -f $*.o

% : %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $*.c $(LIBRARY) -o $@ 

% : %.cc
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $*.cc $(LIBCPP) $(LIBRARY) -o $@ 

$(DEST)/% : %.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $*.c $(LIBRARY) -o $@ 

$(DEST)/% : %.cc
	$(CXX) $(CXXFLAGS) $(CPPFLAGS) $*.cc $(LIBCPP) $(LIBRARY) -o $@ 

ifeq ($(strip $(NEED_BOOTSTRAP)),yes)
.PHONY : bootstrap
bootstrap:
	@echo "\"Entering BootStrap mode\""
	$(MAKE) $(LIBDIR) $(DEST) targets depend
	$(MAKE) targets
endif

.PHONY : targets $(TARGETS) DEBUG dbg tests clean cleandbg cleantests clobber
.PHONY : newdepend depend cleandep help build_macro_list first

ifeq ($(strip $(GOOD2GO)),no)
first targets $(TARGETS) DEBUG dbg tests $(TEST_TARGETS) $(HERMES_ARCH_LIST) :
	@echo $(ERR_MSG)
else
first :
	@$(MAKE) PLATFORM=$(PLATFORM) DEBUG=$(DEBUG) depend $(DEST) targets

targets : $(TARGETS)

$(TARGETS) : $(LIBRARY) $(LIBCPP)
	@$(MAKE) PLATFORM=$(PLATFORM) DEBUG=$(DEBUG) EXE=$(DEST)/$@ $(DEST)/$@

DEBUG dbg :
	@$(MAKE) -f $(MAKEFILE) DEBUG=1 first

tests : $(TEST_TARGETS)

$(TEST_TARGETS) : $(LIBRARY) $(LIBCPP)

$(HERMES_ARCH_LIST) :
	@$(MAKE) -f $(MAKEFILE) DEBUG=$(DEBUG) PLATFORM=$@ first
endif

$(LIBRARY) : $(LIBDIR) $(LIB_OBJS)
$(LIBCPP) : $(LIBDIR) $(LIBCPP_OBJS)

$(LIBDIR) $(DEST) :
	@if [ ! -d $@ ]; then mkdir -p $@; fi

clean :
	rm -f $(LIBRARY) $(LIBCPP) *.bak $(TEST_TARGETS)
	cd $(DEST); rm -f $(TARGETS)

cleandbg :
	make DEBUG=1 clean

cleantests :
	rm -f $(TEST_TARGETS)

clobber :
	rm -f $(LIBRARY) $(LIBCPP) *.bak $(TEST_TARGETS) $(MACRO_LIST_FILE) \
	      $(DEP_LIST_FILE)
	cd $(DEST); rm -f $(TARGETS)

newdepend :
	@$(MAKE) -f $(MAKEFILE) cleandep depend

depend :
	@$(MAKE) -f $(MAKEFILE) build_macro_list
	@$(MAKE) -f $(MAKEFILE) $(DEP_LIST_FILE)

$(MACRO_LIST_FILE) build_macro_list :
	@bld_macro_list -nnone -sc -scc -ih -SSRCS -O "" -P $(PRODNAME)
	@bld_macro_list -mhermes -nwinrtl -sc -OLIB_OBJS -laP $(PRODNAME)
	@bld_macro_list -mhermes -nwinrtl -scc -OLIBCPP_OBJS -LLIBCPP -aP \
	                $(PRODNAME)
	@bld_macro_list -nhermes -ntest -nwinrtl -sc -scc -OTARGETS -EaP \
	                $(PRODNAME)
	@bld_macro_list -mtest -nwinrtl -sc -scc -OTEST_TARGETS -Ead \
	                $(PRODNAME)

ifneq ($(strip $(NEED_BOOTSTRAP)),yes)
$(DEP_LIST_FILE) : $(SRCS) $(INCS)
	@bld_dep_list -mhermes -nwinrtl -sc -S'# LIBRARY dependencies' -lP \
	              -e0 $(PRODNAME)
	@bld_dep_list -mhermes -nwinrtl -scc -S'# LIBCPP dependencies' \
	              -LLIBCPP -aP -e0 $(PRODNAME)
	@bld_dep_list -nhermes -ntest -nwinrtl -sc -scc -p'$$(DEST)/' \
	              -S'# Tool dependencies' -EaP -e0 $(PRODNAME)
	@bld_dep_list -mtest -nwinrtl -sc -scc -S'# Test dependencies' -Ea \
	              -e0 $(PRODNAME)

nmakedepend :
	@bld_macro_list -mhermes -sc -OLIB_OBJS -W -P $(PRODNAME) .nmake
	@bld_macro_list -mhermes -scc -OLIBCPP_OBJS -W -aP $(PRODNAME) .nmake
	@bld_macro_list -nhermes -ntest -sc -scc -OTARG_OBJS -W -aP \
	                $(PRODNAME) .nmake
	@bld_macro_list -nhermes -ntest -sc -scc -OTARGETS -EW -aP \
	                $(PRODNAME) .nmake
	@bld_macro_list -mtest -sc -scc -OTEST_OBJS -W -aP $(PRODNAME) .nmake
	@bld_macro_list -mtest -sc -scc -OTEST_TARGETS -EW -ad \
	                $(PRODNAME) .nmake
	@bld_dep_list -mhermes -sc -e0 -S"# LIBRARY dependencies" \
	              $(DEP_INCDIRS) $(CPPFLAGS) -W -P -e0 $(PRODNAME) .nmake
	@bld_dep_list -mhermes -scc -S"# LIBCPP dependencies" \
	              $(DEP_INCDIRS) $(CPPFLAGS) -W -aP -e0 $(PRODNAME) .nmake
	@bld_dep_list -nhermes -ntest -sc -scc -S"# Tool dependencies" \
	              $(DEP_INCDIRS) $(CPPFLAGS) -EW -aP -e0 $(PRODNAME) .nmake
	@bld_dep_list -nhermes -ntest -sc -scc -S"# Tool dependencies" \
	              $(DEP_INCDIRS) $(CPPFLAGS) -W -aP -e0 $(PRODNAME) .nmake
	@bld_dep_list -mtest -sc -scc -S"# Test dependencies" -EW -aP \
	              $(DEP_INCDIRS) $(CPPFLAGS) -e0 $(PRODNAME) .nmake
	@bld_dep_list -mtest -sc -scc -S"# Test dependencies" -W -a \
	              $(DEP_INCDIRS) $(CPPFLAGS) -e0 $(PRODNAME) .nmake
endif

cleandep:
	@rm -f $(MACRO_LIST_FILE) $(DEP_LIST_FILE)

help:
	@echo "Valid Targets:"
	@echo "  [default] -- Makes optimized make tools for current architecture"
	@echo "  tests     -- Makes any test programs for current architecture"
	@echo "  dbg       -- Makes debug version for current architecture"
	@echo "  \"arch\"    -- Cross-compiles and cross-links the tools for the architecture"
	@echo "               \"arch\", where \"arch\" is one of the following:"
	@echo "                 $(HERMES_ARCH_LIST)"
	@echo "  dbg       -- Makes debug version for current architecture"

	@echo "  depend    -- Builds dependency files that are out of date"
	@echo "               (automatically performed before any executable is built)"
	@echo "  cleandep  -- Removes all dependency files"
	@echo "  clean     -- Removes libraries and tools for current architecture"
	@echo "  clobber   -- Same as clean and cleandep combined"
	@echo "  help      -- Generates this list"
	@echo ""
	@echo "By adding the macro definition \"DEBUG=1\" to the first three options above,"
	@echo "the code will be compiled and linked in DEBUG mode."
	@echo ""

ifneq ($(strip $(wildcard $(DEP_LIST_FILE))),)
  include $(DEP_LIST_FILE)
endif
