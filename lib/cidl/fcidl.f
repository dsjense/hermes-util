c **********************************************************************
c     Fortran wrappers for CIDL interface library
c     $Id$
c     
c     Copyright (2008) Sandia Corporation. Under the terms of
c     Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
c     Government retains certain rights in this software.
c     
c     Hermes is free software: you can redistribute it and/or modify
c     it under the terms of the GNU Lesser General Public License as
c     published by the Free Software Foundation, either version 3 of
c     the License, or (at your option) any later version.
c     
c     Hermes is distributed in the hope that it will be useful, but
c     WITHOUT ANY WARRANTY; without even the implied warranty of
c     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c     GNU Lesser General Public License for more details.
c     
c     You should have received a copy of the GNU Lesser General
c     Public License along with Hermes.  If not, see
c     <http://www.gnu.org/licenses/>.
c     
c **********************************************************************


c **********************************************************************
      subroutine send_command(comand, err)
c **********************************************************************
c
c     Summary:
c       Sends a command to callable IDL.
c
c     Input:
c       comand  -  IDL command to be executed via callable IDL
c
c     Output:
c       err     -  Error Flag.
c                   0  No Errors
c                   1  Callable IDL not initialized
c                  <0  non-zero IDL error code from command's execution
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      character*(*) comand
      integer  err
c 
c     External functions
c
      integer c_send_command
c
c ----------------------------------------------------------------------
c
c ... Call C interface routine, passing lengths of any character variables
      err = c_send_command(comand,len(comand))

      return
      end


c **********************************************************************
      subroutine send_real_var (name, var, err)
c **********************************************************************
c
c     Summary:
c       Sends a real scalar variable to callable IDL.
c
c     Input:
c       name    -  name of resulting IDL variable
c       var     -  value to be assigned to resulting IDL variable
c
c     Output:
c       err     -  Error Flag.
c                   0  No Errors
c                  -1  Callable IDL not initialized
c                   1  Empty variable name
c                   2  IDL could not create the variable
c                   3  Illegal type
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      character*(*) name
      real var
      integer err
c
c     Parameters
      integer IDL_TYP_FLOAT
      parameter (IDL_TYP_FLOAT = 4)
c 
c     External functions
      integer put_idl_variable
c
c ----------------------------------------------------------------------

c ... Call C interface routine, passing lengths of any character variables
      err = put_idl_variable(name,len(name),IDL_TYP_FLOAT,var,0)

      return
      end


c **********************************************************************
      subroutine send_int_var (name, var, err)
c **********************************************************************
c
c     Summary:
c       Sends an integer scalar variable to callable IDL.
c
c     Input:
c       name    -  name of resulting IDL variable
c       var     -  value to be assigned to resulting IDL variable
c
c     Output:
c       err     -  Error Flag.
c                   0  No Errors
c                  -1  Callable IDL not initialized
c                   1  Empty variable name
c                   2  IDL could not create the variable
c                   3  Illegal type
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      character*(*) name
      integer var
      integer err
c
c     Parameters
      integer IDL_TYP_LONG
      parameter (IDL_TYP_LONG = 3)
c 
c     External functions
      integer put_idl_variable
c
c ----------------------------------------------------------------------

c ... Call C interface routine, passing lengths of any character variables
      err = put_idl_variable(name,len(name),IDL_TYP_LONG,var,0)

      return
      end


c **********************************************************************
      subroutine send_str_var (name, var, err)
c **********************************************************************
c
c     Summary:
c       Sends a string scalar variable to callable IDL.
c
c     Input:
c       name    -  name of resulting IDL variable
c       var     -  value to be assigned to resulting IDL variable
c
c     Output:
c       err     -  Error Flag.
c                   0  No Errors
c                  -1  Callable IDL not initialized
c                   1  Empty variable name
c                   2  IDL could not create the variable
c                   3  Illegal type
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      character name*(*), var*(*)
      integer err
c
c     Parameters
      integer IDL_TYP_STRING
      parameter (IDL_TYP_STRING = 7)
c 
c     External functions
      integer put_idl_variable
c
c ----------------------------------------------------------------------

c ... Call C interface routine, passing lengths of any character variables
      err = put_idl_variable(name,len(name),IDL_TYP_STRING,var,len(var))

      return
      end


c **********************************************************************
      subroutine send_byte_array (name, array, ndim, idim, err)
c **********************************************************************
c
c     Summary:
c       Sends a byte array variable to callable IDL.
c
c     Input:
c       name    -  name of resulting IDL array
c       array   -  values to be assigned to resulting IDL array
c       ndim    -  number of array dimensions
c       idim    -  array of dimension sizes (1:ndim)
c
c     Output:
c       err     -  Error Flag.
c                   0  No Errors
c                  -1  Callable IDL not initialized
c                   1  NDIM exceeds IDL's maximum number of array
c                      dimensions (8)
c                   2  Empty variable name
c                   3  Illegal type
c                   4  IDL could not create the variable
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      integer err, ndim, idim(*)
      character*(*) name
      real array(*)
c
c     Parameters
      integer IDL_TYP_BYTE
      parameter (IDL_TYP_BYTE = 1)
c 
c     External functions
      integer put_idl_array
c
c ----------------------------------------------------------------------

c ... Call C interface routine, passing lengths of any character variables
      err = put_idl_array(name,len(name),IDL_TYP_BYTE,ndim,idim,array)

      return
      end


c **********************************************************************
      subroutine send_real_array (name, array, ndim, idim, err)
c **********************************************************************
c
c     Summary:
c       Sends a real array variable to callable IDL.
c
c     Input:
c       name    -  name of resulting IDL array
c       array   -  values to be assigned to resulting IDL array
c       ndim    -  number of array dimensions
c       idim    -  array of dimension sizes (1:ndim)
c
c     Output:
c       err     -  Error Flag.
c                   0  No Errors
c                  -1  Callable IDL not initialized
c                   1  NDIM exceeds IDL's maximum number of array
c                      dimensions (8)
c                   2  Empty variable name
c                   3  Illegal type
c                   4  IDL could not create the variable
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      integer ndim, err, idim(*)
      character*(*) name
      real array(*)
c
c     Parameters
      integer IDL_TYP_FLOAT
      parameter (IDL_TYP_FLOAT = 4)
c 
c     External functions
      integer put_idl_array
c
c ----------------------------------------------------------------------

c ... Call C interface routine, passing lengths of any character variables
      err = put_idl_array(name,len(name),IDL_TYP_FLOAT,ndim,idim,array)

      return
      end


c **********************************************************************
      subroutine send_integer_array (name, array, ndim, idim, err)
c **********************************************************************
c
c     Summary:
c       Sends an integer array variable to callable IDL.
c
c     Input:
c       name    -  name of resulting IDL array
c       array   -  values to be assigned to resulting IDL array
c       ndim    -  number of array dimensions
c       idim    -  array of dimension sizes (1:ndim)
c
c     Output:
c       err     -  Error Flag.
c                   0  No Errors
c                  -1  Callable IDL not initialized
c                   1  NDIM exceeds IDL's maximum number of array
c                      dimensions (8)
c                   2  Empty variable name
c                   3  Illegal type
c                   4  IDL could not create the variable
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      integer err, ndim, idim(*)
      character*(*) name
      integer array(*)
c
c     Parameters
      integer IDL_TYP_LONG
      parameter (IDL_TYP_LONG = 3)
c 
c     External functions
      integer put_idl_array
c
c ----------------------------------------------------------------------

c ... Call C interface routine, passing lengths of any character variables
      err = put_idl_array(name,len(name),IDL_TYP_LONG,ndim,idim,array)

      return
      end


c **********************************************************************
      subroutine get_real_array(name, array, ndim, idim, narray, npass,
     &                          err)
c **********************************************************************
c
c     Summary:
c        Return a real array of from callable IDL. 
c
c     Input:
c       name    -  name of IDL array to be retrieved
c       narray  -  Maximum linear size of ARRAY
c                  NOTE: NARRAY can be of size=1
c
c     Output:
c       array   -  array to receive data (Data will be packed into ARRAY)
c       ndim    -  Number of dimensions in IDL array NAME
c       idim    -  Array of NDIM elements containing number of elements
c                  per dimension (IDIM(8) is always safe)
c       npass   -  Total number of elements in IDL array NAME
c       err     -  Error Flag
c                   0  No Errors
c                  -1 Callable IDL not started.
c                  -2 IDL variable is not defined.
c                  -3 Name is an empty string
c                  -4 Insufficient storage for data available (NPASS>NARRAY)
c                 1-9 IDL type mismatch. Error code is actual type of the
c                     IDL variable (1, Byte; 2, Int; 3, Long; 4, Float;
c                     5, Double; 6, Complex; 7, String; 8, Structure;
c                     9, DComplex
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      integer npass, narray, err, ndim, idim(8)
      character name*(*)
      real array(narray)
c
c     Parameters
      integer IDL_TYP_FLOAT
      parameter (IDL_TYP_FLOAT = 4)
c 
c     External functions
      integer get_idl_variable
c
c ----------------------------------------------------------------------

c ... Call C interface routine, passing lengths of any character variables
      err = get_idl_variable(name, len(name), IDL_TYP_FLOAT, narray,
     &                       ndim, idim, npass, array)

      return
      end


c **********************************************************************
      subroutine get_integer_array(name, array, ndim, idim, narray,
     &                             npass, err)
c **********************************************************************
c
c     Summary:
c        Return an integer array of from callable IDL. 
c
c     Input:
c       name    -  name of IDL array to be retrieved
c       narray  -  Maximum linear size of ARRAY
c                  NOTE: NARRAY can be of size=1
c
c     Output:
c       array   -  array to receive data (Data will be packed into ARRAY)
c       ndim    -  Number of dimensions in IDL array NAME
c       idim    -  Array of NDIM elements containing number of elements
c                  per dimension (IDIM(8) is always safe)
c       npass   -  Total number of elements in IDL array NAME
c       err     -  Error Flag
c                   0  No Errors
c                  -1 Callable IDL not started.
c                  -2 IDL variable is not defined.
c                  -3 Name is an empty string
c                  -4 Insufficient storage for data available (NPASS>NARRAY)
c                 1-9 IDL type mismatch. Error code is actual type of the
c                     IDL variable (1, Byte; 2, Int; 3, Long; 4, Float;
c                     5, Double; 6, Complex; 7, String; 8, Structure;
c                     9, DComplex
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      integer npass, narray, err, ndim, idim(8)
      character name*(*)
      integer array(narray)
c
c     Parameters
      integer IDL_TYP_LONG
      parameter (IDL_TYP_LONG = 3)
c 
c     External functions
      integer get_idl_variable
c
c ----------------------------------------------------------------------

c ... Call C interface routine, passing lengths of any character variables
      err = get_idl_variable(name, len(name), IDL_TYP_LONG, narray,
     &                       ndim, idim, npass, array)

      return
      end


c **********************************************************************
      subroutine get_byte_array(name, array, ndim, idim, narray,
     &                          npass, err)
c **********************************************************************
c
c     Summary:
c        Return a byte array of from callable IDL. 
c
c     Input:
c       name    -  name of IDL array to be retrieved
c       narray  -  Maximum linear size of ARRAY
c                  NOTE: NARRAY can be of size=1
c
c     Output:
c       array   -  array to receive data (Data will be packed into ARRAY)
c       ndim    -  Number of dimensions in IDL array NAME
c       idim    -  Array of NDIM elements containing number of elements
c                  per dimension (IDIM(8) is always safe)
c       npass   -  Total number of elements in IDL array NAME
c       err     -  Error Flag
c                   0  No Errors
c                  -1 Callable IDL not started.
c                  -2 IDL variable is not defined.
c                  -3 Name is an empty string
c                  -4 Insufficient storage for data available (NPASS>NARRAY)
c                 1-9 IDL type mismatch. Error code is actual type of the
c                     IDL variable (1, Byte; 2, Int; 3, Long; 4, Float;
c                     5, Double; 6, Complex; 7, String; 8, Structure;
c                     9, DComplex
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      integer npass, narray, err, ndim, idim(8)
      character name*(*)
      integer array(narray)
c
c     Parameters
      integer IDL_TYP_BYTE
      parameter (IDL_TYP_BYTE = 1)
c 
c     External functions
      integer get_idl_variable
c
c ----------------------------------------------------------------------

c ... Call C interface routine, passing lengths of any character variables
      err = get_idl_variable(name, len(name), IDL_TYP_BYTE, narray,
     &                       ndim, idim, npass, array)

      return
      end


c **********************************************************************
      subroutine get_real_var (name, var, npass, err)
c **********************************************************************
c
c     Summary:
c        Return a real scalar variable from callable IDL. 
c
c     Input:
c       name    -  name of IDL array to be retrieved
c
c     Output:
c       var     -  array to receive data (Data will be packed into ARRAY)
c       npass   -  Number of elements in IDL variable NAME (this should
c                  always be one unless error code -4 is returned)
c       err     -  Error Flag
c                   0  No Errors
c                  -1 Callable IDL not started.
c                  -2 IDL variable is not defined.
c                  -3 Name is an empty string
c                  -4 IDL variable is not a scalar
c                 1-9 IDL type mismatch. Error code is actual type of the
c                     IDL variable (1, Byte; 2, Int; 3, Long; 4, Float;
c                     5, Double; 6, Complex; 7, String; 8, Structure;
c                     9, DComplex
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      integer npass, err
      character name*(*)
      real var
c
c     Parameters
      integer IDL_TYP_FLOAT
      parameter (IDL_TYP_FLOAT = 4)
c
c     Internal
      integer ndim, dims(8)
c 
c     External functions
      integer get_idl_variable
c
c ----------------------------------------------------------------------

c ... Call C interface routine, passing lengths of any character variables
c ... Use storage size of zero to indicate request for scalar quantity
      err = get_idl_variable(name, len(name), IDL_TYP_FLOAT, 0, ndim,
     &                       dims, npass, var)

      return
      end


c **********************************************************************
      subroutine get_integer_var (name, var, npass, err)
c **********************************************************************
c
c     Summary:
c        Return an integer scalar variable from callable IDL. 
c
c     Input:
c       name    -  name of IDL array to be retrieved
c
c     Output:
c       var     -  array to receive data (Data will be packed into ARRAY)
c       npass   -  Number of elements in IDL variable NAME (this should
c                  always be one unless error code -4 is returned)
c       err     -  Error Flag
c                   0  No Errors
c                  -1 Callable IDL not started.
c                  -2 IDL variable is not defined.
c                  -3 Name is an empty string
c                  -4 IDL variable is not a scalar
c                 1-9 IDL type mismatch. Error code is actual type of the
c                     IDL variable (1, Byte; 2, Int; 3, Long; 4, Float;
c                     5, Double; 6, Complex; 7, String; 8, Structure;
c                     9, DComplex
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      integer npass, err
      character name*(*)
      integer var
c
c     Parameters
      integer IDL_TYP_LONG
      parameter (IDL_TYP_LONG = 3)
c
c     Internal
      integer ndim, dims(8)
c 
c     External functions
      integer get_idl_variable
c
c ----------------------------------------------------------------------

c ... Call C interface routine, passing lengths of any character variables
c ... Use storage size of zero to indicate request for scalar quantity
      err = get_idl_variable(name, len(name), IDL_TYP_LONG, 0, ndim,
     &                       dims, npass, var)

      return
      end


c **********************************************************************
      subroutine get_str_var (name, var, npass, err)
c **********************************************************************
c
c     Summary:
c        Return an integer scalar variable from callable IDL. 
c
c     Input:
c       name    -  name of IDL array to be retrieved
c
c     Output:
c       var     -  array to receive data (Data will be packed into ARRAY)
c       npass   -  Number of elements in IDL variable NAME (this should
c                  always be one unless error code -4 is returned)
c       err     -  Error Flag
c                   0  No Errors
c                  -1 Callable IDL not started.
c                  -2 IDL variable is not defined.
c                  -3 Name is an empty string
c                  -4 IDL variable is not a scalar
c                 1-9 IDL type mismatch. Error code is actual type of the
c                     IDL variable (1, Byte; 2, Int; 3, Long; 4, Float;
c                     5, Double; 6, Complex; 7, String; 8, Structure;
c                     9, DComplex
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      integer npass, err
      character name*(*)
      character var*(*)
c
c     Parameters
      integer IDL_TYP_STRING
      parameter (IDL_TYP_STRING = 7)
c
c     Internal
      integer ndim, dims(8)
c 
c     External functions
      integer get_idl_variable
c
c ----------------------------------------------------------------------

c ... Call C interface routine, passing lengths of any character variables
      err = get_idl_variable(name, len(name), IDL_TYP_STRING,
     &                       len(var), ndim, dims, npass, var)

      return
      end


c **********************************************************************
      subroutine idl_write(input, flag)
c **********************************************************************
c
c     Summary:
c
c        This routine writes a string to an IDL display 
c
c     Input:
c       input   -  string to be written
c       flag    -  If flag is zero, no leading characters will be printed.
c                  If flag is not zero, then the value of the IDL system
c                  variable !MSG_PREFIX will be printed. (Default = '% ')
c
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      character input*(*)
      integer flag
c
c ----------------------------------------------------------------------

c ... Call C interface routine, passing lengths of any character variables
      call c_idl_write(input,len(input),flag)

      return
      end


c **********************************************************************
      subroutine idl_main (lcterm, mode, prompt, xsize, ysize,
     &   n_hist, cmd_out, err)
c **********************************************************************
c
c     Summary:
c
c       This routine provides a means of getting strings from IDL.
c
c     Input:
c       lcterm  -  flag for terminal rather than GUI interface
c                   0 - Use GUI interface to read strings
c                   1 - read strings from terminal
c       mode    -  flag indicating whether strings should be passed to
c                  IDL as commands
c                   0 - pass to IDL until "EXIT" (or "QUIT") is encountered
c                   1 - return string to calling routine (as cmd_out)
c       prompt  -  String used for prompting for input.
c       xsize   -  GUI window width in characters (Default = 50)
c       ysize   -  GUI window height in lines (Default = 20)
c       n_hist  -  Total number of commands available from recall stack
c
c
c     Output:
c       cmd_out -  string retrieved (mode = 1 only)
c       err     -  Error Flag
c                   0  No Errors
c                  -1 Callable IDL not started.
c                  -5 Input string exceeds CIDL's internal buffer
c               other CIDL internal errors
c  
c ----------------------------------------------------------------------
c
c     For mode  = 0, commands will be processed in idl until a EXIT (or
c     QUIT) command is entered (this is case-insensitive). When EXIT is
c     entered, the GUI's widget will be destroyed (if using the GUI).
c
c     For mode = 1, commands will be returned to the calling program. If
c     there is a widget, the the widget will stay active.
c
c     If PROMPT is blank, the default prompt is dependent on LCTERM:
c        LCTERM = 0 ==> default prompt: "Enter Command Below"
c        LCTERM = 1 ==> default prompt: "IDL"
c     
c ----------------------------------------------------------------------
c
c     Declare variables:
c
c     Passed
      integer  lcterm, xsize, ysize, mode, n_hist, err
      character *(*) prompt, cmd_out
c 
c     External functions
      integer c_idl_main
c     
c ----------------------------------------------------------------------

c ... Call C interface routine, passing lengths of any character variables
      err = c_idl_main(lcterm, mode, prompt, len(prompt), xsize, ysize,
     &                 n_hist, cmd_out, len(cmd_out))

      return
      end
