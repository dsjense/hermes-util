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
PRODUCT = python
PLATFORM = $(HERMES_SYS_TYPE)
MAKEFILE = GNUmakefile

PYC_LIST = $(shell find . -name '*.pyc') 

ifneq ($(HERMES_PYTHON_TARGETS),)
  PY_TARGS = $(subst :, ,$(HERMES_PYTHON_TARGETS))
else
  PY_TARGS = python
endif

.PHONY : all extensions ext html clean htmlclean distclean $(PY_TARGS)

all:
	@$(MAKE) $(PY_TARGS)
	@cd extensions; $(MAKE) html

ext extensions:
	@$(MAKE) $(PY_TARGS)

$(PY_TARGS):
	@cd extensions; $(MAKE) PYTHON_EXE=$@

html:
	@cd extensions; $(MAKE) html

clean:
	@cd extensions; $(MAKE) clean

htmlclean:
	@cd extensions; $(MAKE) htmlclean

distclean:
	@cd extensions; $(MAKE) distclean
	rm -f $(PYC_LIST)
