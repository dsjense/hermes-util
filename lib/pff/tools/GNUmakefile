# PFF TOOLS makefile
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
PRODUCT = pfftools
BLD_SRC_EXTS = -sf -sF
BLD_INC_EXTS = -iinc
PRODUCT_EXT =
PLATFORM = $(HERMES_SYS_TYPE)
BIN_ROOT = .
CPPFLAGS =
GOOD2GO = yes
EXE = none
DBG_EXT =
TIOAUXDIR = $(HERMES_LIB)/tio/taux
EXT_OBJS = nulaux.o
BLDDEPLST_ARGS = -VHERMES_LIB -VHERMES_ROOT

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
  FFLAGS_OD = $(FFLAGS_DBG)
  NEED_DIRS = $(LIBDEST)
else
  DEST    = $(HERMES_BIN)/$(PLATFORM)
  FFLAGS_OD = $(FFLAGS_OPT)
  NEED_DIRS = $(LIBDEST) $(DEST)
endif

ifeq ($(strip $(FBD_EXPLICIT_LINK)),yes)
  BD_OBJS = pffbd.o
else
  BD_OBJS =
endif

LIBRARY = $(LIBDEST)/lib$(PRODNAME)$(DBG_EXT).a
LIBRARY_LINK = -L$(LIBDEST) -l$(PRODNAME)$(DBG_EXT)

F_INCDIRS = -I$(HERMES_LIB)/pff -I$(HERMES_LIB)/tio -I$(HERMES_ROOT)/finclude
DEP_INCDIRS =  $(F_INCDIRS)
OTHER_LIBS = pff tio stak mdutil nserver
LIBS = -L$(LIBDIR)
LIBS += $(foreach mac,$(OTHER_LIBS),-l$(mac))
LIB_FILES = $(foreach mac,$(OTHER_LIBS),$(LIBDIR)/lib$(mac).a)

PFFLIB = $(LIBDIR)/libpff$(STATIC_EXT).a

FFLAGS += $(FFLAGS_ALL)  $(FFLAGS_BSLSH) $(FFLAGS_OD) $(F_INCDIRS)
FPPFLAGS = $(FPPFLAGS_SYS) $(FPP_OPTS)
FFLAGS_FPP = $(FFLAGS_SYS) $(F_OPTS)

%.f : %.gen
	txt2cwriter -s $< $@

(%.o) : %.F
	$(FC) -c $(FFLAGS) $(F_INCDIRS) $(FFLAGS_FPP) $<
	$(AR) rv $@ $*.o
	rm -f $*.o

(%.o) : %.f
	$(FC) -c $(FFLAGS) $<
	$(AR) rv $@ $*.o
	rm -f $*.o

$(DEST)/% : %.F
	$(FC) -o $@ $(FFLAGS) $(F_INCDIRS) $(FFLAGS_FPP) $< $(BD_OBJS) \
	      $(EXT_OBJS) $(LIBRARY_LINK) $(LIBS)
	rm -f $(@F).o

(%.o) : $(TIOAUXDIR)/%.f
	$(FC) -c $(FFLAGS) $<
	$(AR) rv $@ $*.o
	rm -f $*.o

$(DEST)/% : %.f
	$(FC) -o $@ $(FFLAGS) $< $(BD_OBJS) $(EXT_OBJS) $(LIBRARY_LINK) $(LIBS)

ifneq ($(strip $(wildcard $(MACRO_LIST_FILE))),)
  include $(MACRO_LIST_FILE)
endif

CLOSE = )
OPEN = (
EXT_LIBOBJS = $(addsuffix $(CLOSE),$(addprefix $(LIBRARY)$(OPEN),$(EXT_OBJS)))

.INTERMEDIATE : $(addsuffix .o,$(STANDALONE_TARGETS) $(TARGETS)) \
	        dump_copyright.f

.PHONY : dbg $(HERMES_ARCH_LIST) depend nmakedepend bld_macro_list
.PHONY : newdepend cleandep help
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

ifneq ($(strip $(TARGETS)),)
$(TARGETS) : $(LIBRARY) $(BD_OBJS) $(EXT_OBJS) $(LIB_FILES)
	@$(MAKE) DEBUG=$(DEBUG) EXE=$(DEST)/$@ PLATFORM=$(PLATFORM) $(DEST)/$@
	rm -f $(BD_OBJS) $(EXT_OBJS)
endif

ifneq ($(strip $(STANDALONE_TARGETS)),)
$(STANDALONE_TARGETS) : $(BD_OBJS) $(EXT_OBJS)
	@$(MAKE) depend $(DEST)
	@$(MAKE) PLATFORM=$(PLATFORM) DEBUG=$(DEBUG) SEXE=$(DEST)/$@ $(DEST)/$@
	rm -f $(BD_OBJS) $(EXT_OBJS)
endif

dbg:
	@$(MAKE) PLATFORM=$(PLATFORM) DEBUG=1

$(HERMES_ARCH_LIST) :
	@$(MAKE) DEBUG=$(DEBUG) PLATFORM=$@
endif

$(EXE) : $(LIBRARY)

$(NEED_DIRS) :
	@if [ ! -d $@ ]; then mkdir -p $@; fi

$(LIBRARY) : $(LIBDEST) $(LIB_OBJS) $(EXT_LIBOBJS)

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
	              $(BLD_SRC_EXTS) $(DEP_INCDIRS) $(FPPFLAGS) \
	              $(BLDDEPLST_ARGS) $(PRODUCT)
	@bld_dep_list -mmain -p'$$(DEST)/' -S'# Target dependencies' -Ea -e0 \
	              $(BLD_SRC_EXTS) $(DEP_INCDIRS) $(FPPFLAGS) \
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

help:
	@echo "Valid Targets:"
	@echo "  [default] -- Makes optimized version for current architecture"
	@echo "  dbg       -- Makes debug version for current architecture"
	@echo "  \"arch\"    -- Cross-compiles and cross-links the tools for the architecture"
	@echo "               \"arch\", where \"arch\" is one of the following:"
	@echo "                 $(HERMES_ARCH_LIST)"
	@echo "  depend    -- Builds dependency files that are out of date"
	@echo "               (automatically performed before any executable is built)"
	@echo "  cleandep  -- Removes all dependency files"
	@echo "  help      -- Generates this list"

#
# ------------------ Explicit dependencies -----------------------------
#

$(LIBRARY) : $(LIBRARY)(dump_copyright.o) $(LIBRARY)(bldpff_usage.o)

dump_copyright.f : $(HERMES_ROOT)/etc/Hermes_copyright.txt
	txt2cwriter -p " * " $< $@

#
# ------------------ Include File Dependency Section --------------------
#

ifneq ($(strip $(wildcard $(DEP_LIST_FILE))),)
  include $(DEP_LIST_FILE)
endif

nulaux.o:
	$(AR) xv $(LIBRARY) $@

ifeq ($(strip $(FBD_EXPLICIT_LINK)),yes)
pffbd.o:
	$(AR) xv $(PFFLIB) $@
endif
