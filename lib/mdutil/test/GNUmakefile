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
#   This Makefile depends on the following environmental variables:
#
#   variable                      defined in
#   --------                      ----------
#    HERMES_ARCH_LIST   'hermes_root'/bin/hermesenv.[sh|csh]
#    HERMES_GLOBAL      'hermes_root'/bin/hermesenv.[sh|csh]
#
#--------------------------- Macro Section -----------------------------
#
PRODUCT = mdutest
PLATFORM = $(HERMES_SYS_TYPE)
MAKEFILE 	= GNUmakefile
SHELL 		= /bin/sh
GOOD2GO = yes

DEBUG =
CPP_OPTS =
FPP_OPTS =
F_OPTS =
BLD_SRC_EXTS = -sF -sf -sc
BLD_INC_EXTS = -iinc
LGRPFLAGS = 
LIBBASE   := ../..
FFLAGS_OD = $(FFLAGS_OPT)
CFLAGS_OD = $(CFLAGS_OPT)
#
include $(HERMES_GLOBAL)makeopts.$(PLATFORM)

# check for legal cross-compiling on this platform
ifeq ($(strip $(findstring :$(PLATFORM):,$(HU_SYS_LOCAL))),)
  GOOD2GO=no
  ERR_MSG=Need to use system from following list: $(subst :, ,$(HU_SYS_LOCAL))
endif

THIS_MAKE := $(PLATFORM)
#
ifneq ($(strip $(DEBUG)),)
  FFLAGS_OD = $(FFLAGS_DBG)
  CFLAGS_OD = $(CFLAGS_DBG)
  LIBBASE := $(LIBBASE)/debug
  THIS_MAKE := $(THIS_MAKE):DEBUG
endif

C_INCDIRS =
F_INCDIRS = -I../../../finclude
BLDDEPLST_ARGS =

LIBDIRNAME      = $(PLATFORM)
LIBDIR          = $(LIBBASE)/$(LIBDIRNAME)

LIBS		= -L$(LIBDIR) -lmdutil

PRODNAME=$(PRODUCT)

MACRO_LIST_FILE = $(PRODUCT)_macro_list
DEP_LIST_FILE = $(PRODUCT)_dep_list

FFLAGS 	   = $(FFLAGS_ALL) $(FFLAGS_OD)
FPPFLAGS   = $(FPPFLAGS_SYS) $(FPP_OPTS)
FFLAGS_FPP = $(FFLAGS_SYS) $(F_OPTS)

DEP_INCDIRS = $(C_INCDIRS) $(F_INCDIRS)

% : %.f
	$(FC) $(FFLAGS) $(F_INCDIRS) $< $(LIBS) -o $@
	rm -f $@.o

% : %.F
	$(FC) $(FFLAGS) $(FFLAGS_FPP) $(F_INCDIRS) $< $(LIBS) -o $@
	rm -f $@.o

ifneq ($(strip $(wildcard $(MACRO_LIST_FILE))),)
  include $(MACRO_LIST_FILE)
endif

.INTERMEDIATE : $(MAIN_OBJS)

.PHONY : first targets DEBUG dbg $(HERMES_ARCH_LIST)
.PHONY : bld_macro_list clean all clobber depend nmakedepend
#
#--------------------------- Target Section -----------------------------
#

ifeq ($(strip $(GOOD2GO)),no)
first targets DEBUG dbg $(HERMES_ARCH_LIST) :
	@echo $(ERR_MSG)
else
first :
	$(MAKE) PLATFORM=$(PLATFORM) DEBUG=$(DEBUG) sys_check
	$(MAKE) PLATFORM=$(PLATFORM) DEBUG=$(DEBUG) targets

targets : $(TARGETS) $(STANDALONE_TARGETS)

ifneq ($(strip $(TARGETS)),)
$(TARGETS) : $(OBJS) $(EXT_OBJS)
endif

DEBUG dbg :
	@$(MAKE) DEBUG=1 first

$(HERMES_ARCH_LIST) :
	@$(MAKE) DEBUG="$(DEBUG)" PLATFORM=$@ first
endif

sys_check :
	@if [ ! -f LAST_MAKE ]; then $(MAKE) clobber; \
	 elif ! grep ^$(THIS_MAKE)$$ LAST_MAKE >/dev/null; then \
	   $(MAKE) clobber; fi
	@$(MAKE) depend
	@echo $(THIS_MAKE) > LAST_MAKE

depend :
	@$(MAKE) -f $(MAKEFILE) bld_macro_list
	@$(MAKE) -f $(MAKEFILE) $(DEP_LIST_FILE)

$(MACRO_LIST_FILE) bld_macro_list :
	@bld_macro_list $(BLD_SRC_EXTS) $(BLD_INC_EXTS) $(LGRPFLAGS) \
                        -SSRCS -IINCS -O "" -P $(PRODNAME)
	@bld_macro_list -nmain $(BLD_SRC_EXTS) $(LGRPFLAGS) -aP $(PRODNAME)
	@bld_macro_list -mmain $(BLD_SRC_EXTS) $(LGRPFLAGS) -OMAIN_OBJS -aP \
                        $(PRODNAME)
	@bld_macro_list -mmain -nstandalone $(BLD_SRC_EXTS) $(LGRPFLAGS) -aEP \
                        -OTARGETS $(PRODNAME)
	@bld_macro_list -mstandalone $(BLD_SRC_EXTS) $(LGRPFLAGS) -aEd \
                        -OSTANDALONE_TARGETS $(PRODNAME)

$(DEP_LIST_FILE) : $(SRCS) $(INCS)
	bld_dep_list $(DEP_INCDIRS) $(FPPFLAGS) $(CPPFLAGS) $(BLD_SRC_EXTS) \
	              $(BLDDEPLST_ARGS) $(LGRPFLAGS) -e0 $(PRODUCT)

nmakedepend :
	@bld_macro_list -nmain $(BLD_SRC_EXTS) $(BLD_INC_EXTS) -WP \
	               $(LGRPFLAGS) -SSRCS -IINCS $(PRODNAME) .nmake
	@bld_macro_list -mmain $(BLD_SRC_EXTS) -OMAIN_OBJS -WPa \
	               $(LGRPFLAGS) $(PRODNAME) .nmake
	@bld_macro_list -mmain -nstandalone $(BLD_SRC_EXTS) -OTARGETS -EaWP \
	                $(LGRPFLAGS) $(PRODUCT) .nmake
	@bld_macro_list -mstandalone $(BLD_SRC_EXTS) -OSTANDALONE_TARGETS \
	                $(LGRPFLAGS) -EaWd $(PRODUCT) .nmake
	@bld_dep_list $(DEP_INCDIRS) $(FPPFLAGS) $(CPPFLAGS) $(BLD_SRC_EXTS) \
	             $(BLDDEPLST_ARGS) $(LGRPFLAGS) -W -e0 $(PRODUCT) .nmake

clean:;	rm -f *.o core

clobber:; rm -f *.o core LAST_MAKE $(OBJS) $(TARGETS) $(STANDALONE_TARGETS)

#
# ------------------ Include File Dependency Section --------------------
#
ifneq ($(strip $(wildcard $(DEP_LIST_FILE))),)
  include $(DEP_LIST_FILE)
endif
