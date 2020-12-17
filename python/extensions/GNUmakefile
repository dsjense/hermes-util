#-----------------------------------------------------------------------
#     $Id$
#     
#     Copyright (2014) David Seidel.
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
PRODUCT = pythonext
PYTHON_EXE = python
PLATFORM = $(HERMES_SYS_TYPE)
MAKEFILE = GNUmakefile
BLD_SRC_EXTS = -scc
BLD_INC_EXTS = -ih -ihh
DEP_INCDIRS = -I../../lib/pffc
LGRPFLAGS = 
LIST_NAME = $(PRODUCT)
MACRO_LIST_FILE = $(LIST_NAME)_macro_list
DEP_LIST_FILE = $(LIST_NAME)_dep_list
PY_VER = $(basename $(word 2,$(shell $(PYTHON_EXE) -V 2>&1)))
BLDDIR = $(HERMES_SYS_TYPE)-$(PY_VER)
HTMLDIR = ../html
TOOLDIR = ../tools
GENERATED_INCLUDE = pff_ext.h

RMBLDDIRS = $(TOOLDIR)/rmBldDirs

include $(HERMES_GLOBAL)makeopts.$(PLATFORM)

ifneq ($(strip $(wildcard $(MACRO_LIST_FILE))),)
  include $(MACRO_LIST_FILE)
endif

FULL_TARGETS = $(foreach targ,$(TARGETS),$(BLDDIR)/$(targ).$(SHR_EXT))

%.h : %.docgen
	txt2pydoc $< $@

$(BLDDIR)/%.$(SHR_EXT) : %.cc
	@if [ ! -d $(BLDDIR) ]; then mkdir $(BLDDIR); fi
	$(PYTHON_EXE) $(basename $(notdir $<))_setup.py build \
	       --build-lib $(BLDDIR) --build-temp=$(BLDDIR)/tmp -g

start :
	@$(MAKE) -f $(MAKEFILE) depend all

all :
	@$(MAKE) -f $(MAKEFILE) $(FULL_TARGETS)

depend :
	@$(MAKE) -f $(MAKEFILE) bld_macro_list
	@$(MAKE) -f $(MAKEFILE) $(DEP_LIST_FILE)

html :
	@$(MAKE) -f $(MAKEFILE).htmlgen HTMLDIR=$(HTMLDIR)

clean :
	@$(MAKE) htmlclean
	rm -rf $(MACRO_LIST_FILE)* $(DEP_LIST_FILE)* $(BLDDIR)/*.$(SHR_EXT) \
	       $(BLDDIR)/tmp $(GENERATED_INCLUDE)

htmlclean :
	@$(MAKE) -f $(MAKEFILE).htmlgen HTMLDIR=$(HTMLDIR) clean

distclean :
	@$(MAKE) -f $(MAKEFILE) clean
	$(RMBLDDIRS)

$(MACRO_LIST_FILE) bld_macro_list :
	@bld_macro_list $(BLD_SRC_EXTS) $(BLD_INC_EXTS) $(LGRPFLAGS) -Ed \
                       -SSRCS -IINCS -OTARGETS $(LIST_NAME)

$(DEP_LIST_FILE) : $(SRCS) $(INCS)
	bld_dep_list $(DEP_INCDIRS) $(FPPFLAGS) $(CPPFLAGS) $(BLD_SRC_EXTS) \
	              $(BLDDEPLST_ARGS) $(LGRPFLAGS) \
	              -p'$$(BLDDIR)/:.$(SHR_EXT)' -e0 $(LIST_NAME)

ifneq ($(strip $(wildcard $(DEP_LIST_FILE))),)
  include $(DEP_LIST_FILE)
endif

# Bootstrap generated file dependency
$(BLDDIR)/pff_ext.$(SHR_EXT) : $(GENERATED_INCLUDE)
