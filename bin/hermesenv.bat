@rem $Id$
@rem 
@rem Copyright (2008) Sandia Corporation. Under the terms of
@rem Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
@rem Government retains certain rights in this software.
@rem 
@rem Hermes is free software: you can redistribute it and/or modify
@rem it under the terms of the GNU Lesser General Public License as
@rem published by the Free Software Foundation, either version 3 of
@rem the License, or (at your option) any later version.
@rem 
@rem Hermes is distributed in the hope that it will be useful, but
@rem WITHOUT ANY WARRANTY; without even the implied warranty of
@rem MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
@rem GNU Lesser General Public License for more details.
@rem 
@rem You should have received a copy of the GNU Lesser General
@rem Public License along with Hermes.  If not, see
@rem <http://www.gnu.org/licenses/>.
@rem 

@rem Set HERMES_ROOT to be root directory for HERMES on Windows in next line
@rem set HERMES_ROOT=C:\hermes

@set HERMES_SYS_TYPE=win32_intel_ms

@path %HERMES_ROOT%\bin;%HERMES_ROOT%\bin\%HERMES_SYS_TYPE%;%PATH%

@path | findstr /C:"%IDLBIN%" >null
@if %ERRORLEVEL% GEQ 1 (
  path %IDLBIN%;%PATH%
)

@set HERMES_LIB=%HERMES_ROOT%\lib
@set HERMES_BIN=%HERMES_ROOT%\bin

@set TIOhelp=%HERMES_ROOT%\doc\Tiolib.pdf
@set BLDPFFhelp=%HERMES_ROOT%\doc\Bldpff.pdf

@rem Set TIO_help_reader to be the path to the Adobe Acrobat Reader
@rem Note that non-8dot3 directory and file names must be replaced with
@rem   the appropriate generated short 8dot3 names (see help for dir /x)
@rem if not defined TIO_help_reader (
  @rem set TIO_help_reader=C:\progra~1\Adobe\Acroba~1.0\Reader\AcroRd32.exe
@rem )
