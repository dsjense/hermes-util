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
PFFINC		= ..
PRODUCT = pffctest
PLATFORM = $(HERMES_SYS_TYPE)
MAKEFILE 	= GNUmakefile
SHELL 		= /bin/sh
GOOD2GO = yes

DEBUG =
BLD_SRC_EXTS = -sc
BLD_INC_EXTS = -ih 
LGRPFLAGS = 
WINLGRPFLAGS = -nmpi
LIBBASE   := ../..
LIB_EXT =
CPP_OPTS =
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
  CFLAGS_OD = $(CFLAGS_DBG)
  LIBBASE := $(LIBBASE)/debug
  THIS_MAKE := $(THIS_MAKE):DEBUG
endif

C_INCDIRS = -I..
F_INCDIRS =
BLDDEPLST_ARGS =
PRODUCT_EXT =

#
DEBUG           =
DEBUG_PATH      =

LIBDIRNAME      = $(PLATFORM)
LIBDIR          = $(LIBBASE)/$(LIBDIRNAME)

OTHER_LIBS      = 

LIST_NAME = $(PRODUCT)
ifneq ($(strip $(MPI)),)
  PRODUCT_EXT = _mpi
  CPP_OPTS = -DUSE_MPI
  CC=$(MPI_CC)
  ifeq ($(strip $(MPI_SYS)),)
    C_INCDIRS += -I$(MPI_ROOT)/include
    F_INCDIRS += -I$(MPI_ROOT)/include
    BLDDEPLST_ARGS = -VMPI_ROOT
    OTHER_LIBS += $(MPI_LIB)
    LIST_NAME = $(PRODUCT)_mpi
  endif
endif

PRODNAME=$(PRODUCT)$(PRODUCT_EXT)

MACRO_LIST_FILE = $(LIST_NAME)_macro_list
DEP_LIST_FILE = $(LIST_NAME)_dep_list

CFLAGS = $(CFLAGS_ALL) $(CFLAGS_OD) $(CFLAGS_ANSI)
CPPFLAGS = $(CFLAGS_SYS) $(CPP_OPTS)

DEP_INCDIRS = $(C_INCDIRS) $(F_INCDIRS)

LIBS = -L$(LIBDIR) -lpffc$(PRODUCT_EXT) $(OTHER_LIBS)

% :%.c
	$(CC) $(CFLAGS) $(C_INCDIRS) $(CPPFLAGS) $< $(LIBS) -lm -o $@

%.o :%.c
	$(CC) -c $(CFLAGS) $(C_INCDIRS) $(CPPFLAGS) $<

% : %.o
	$(CC) $(CFLAGS) $< $(LIBS) -lm -o $@

ifneq ($(strip $(wildcard $(MACRO_LIST_FILE))),)
  include $(MACRO_LIST_FILE)
endif

FMPI_TARGETS = $(addprefix m,$(MPI_TARGETS))

MPI_INTERMEDIATE = $(addsuffix .o,$(MPI_TARGETS))

.INTERMEDIATE : $(MAIN_OBJS) $(MPI_INTERMEDIATE)

.PHONY : first targets DEBUG dbg mpi MPI $(HERMES_ARCH_LIST)
.PHONY : mpi_targets sys_check bld_macro_list clean clobber depend nmakedepend
#
#--------------------------- Target Section -----------------------------
#

ifeq ($(strip $(GOOD2GO)),no)
first targets DEBUG dbg mpi MPI $(HERMES_ARCH_LIST) :
	@echo $(ERR_MSG)
else
first :
	@$(MAKE) PLATFORM=$(PLATFORM) MPI=$(MPI) DEBUG=$(DEBUG) sys_check
ifeq ($(strip $(MPI)),)
	@$(MAKE) PLATFORM=$(PLATFORM) DEBUG=$(DEBUG) targets
else
	@$(MAKE) PLATFORM=$(PLATFORM) MPI=1 DEBUG=$(DEBUG) mpi_targets
	@$(MAKE) $(FMPI_TARGETS)
	@sleep 1
	@$(MAKE) PLATFORM=$(PLATFORM) MPI="" DEBUG=$(DEBUG) mpi_targets
	@for x in $(MPI_TARGETS); do \
	   if [ $$x -nt m$$x ]; then touch m$$x; fi \
	 done
endif

targets : $(TARGETS) $(STANDALONE_TARGETS)

ifneq ($(strip $(TARGETS)),)
$(TARGETS) : $(OBJS) $(EXT_OBJS)
endif

DEBUG dbg :
	@$(MAKE) MPI=$(MPI) DEBUG=1 first

mpi MPI :
	@$(MAKE) MPI=1 DEBUG=$(DEBUG) first

$(HERMES_ARCH_LIST) :
	@$(MAKE) MPI=$(MPI) DEBUG="$(DEBUG)" PLATFORM=$@ first

mpi_targets : $(MPI_TARGETS)

ifneq ($(strip $(MPI_TARGETS)),)
$(MPI_TARGETS) : $(MPI_OBJS) $(EXT_OBJS)

$(FMPI_TARGETS) : $(MPI_TARGETS)
	mv $(shell echo $@|sed 's/^m//') $@
endif

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
                        -SSRCS -IINCS -O "" -P $(LIST_NAME)
	@bld_macro_list -nmain $(BLD_SRC_EXTS) $(LGRPFLAGS) -aP $(LIST_NAME)
	@bld_macro_list -mmain -nmpi $(BLD_SRC_EXTS) $(LGRPFLAGS) -OMAIN_OBJS \
                        -aP $(LIST_NAME)
	@bld_macro_list -mmpi $(BLD_SRC_EXTS) $(LGRPFLAGS) -OMPI_OBJS -aP \
                        $(LIST_NAME)
	@bld_macro_list -mmain -nstandalone $(BLD_SRC_EXTS) $(LGRPFLAGS) \
                        -nmpi -OTARGETS -aEP $(LIST_NAME)
	@bld_macro_list -mstandalone -nmpi $(BLD_SRC_EXTS) $(LGRPFLAGS) -aEP \
                        -OSTANDALONE_TARGETS $(LIST_NAME)
	@bld_macro_list -mmain -mmpi $(BLD_SRC_EXTS) $(LGRPFLAGS) -aEd \
                        -OMPI_TARGETS $(LIST_NAME)

$(DEP_LIST_FILE) : $(SRCS) $(INCS)
	@bld_dep_list $(DEP_INCDIRS) $(FPPFLAGS) $(CPPFLAGS) $(BLD_SRC_EXTS) \
	              $(BLDDEPLST_ARGS) $(LGRPFLAGS) -E -e0 $(LIST_NAME)

nmakedepend :
	@bld_macro_list -nmain $(BLD_SRC_EXTS) $(BLD_INC_EXTS) -WP \
	               $(WINLGRPFLAGS) -SSRCS -IINCS $(PRODNAME) .nmake
	@bld_macro_list -mmain $(BLD_SRC_EXTS) -OMAIN_OBJS -WPa \
	               $(WINLGRPFLAGS) $(PRODNAME) .nmake
	@bld_macro_list -mmain -nstandalone $(BLD_SRC_EXTS) -OTARGETS -EaWP \
	                $(WINLGRPFLAGS) $(PRODUCT) .nmake
	@bld_macro_list -mstandalone $(BLD_SRC_EXTS) -OSTANDALONE_TARGETS \
	                $(WINLGRPFLAGS) -EaWd $(PRODUCT) .nmake
	@bld_dep_list $(DEP_INCDIRS) $(FPPFLAGS) $(CPPFLAGS) $(BLD_SRC_EXTS) \
	             $(BLDDEPLST_ARGS) $(WINLGRPFLAGS) -W -e0 $(PRODUCT) .nmake

clean:;	rm -f *.o core

clobber:; rm -f *.o core LAST_MAKE $(TARGETS) $(STANDALONE_TARGETS) \
	        $(MPI_TARGETS) $(FMPI_TARGETS)

#
# ------------------ Include File Dependency Section --------------------
#
ifneq ($(strip $(wildcard $(DEP_LIST_FILE))),)
  include $(DEP_LIST_FILE)
endif
