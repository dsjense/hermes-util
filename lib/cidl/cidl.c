/*************************************************************************
 *     C portion for CIDL interface library
 *       Only START_CALL_IDL, END_CALL_IDL, and GET_CIDL_STATE are
 *       intended to be called from Fortran
 *
 *     $Id$
 *     
 *     Copyright (2008) Sandia Corporation. Under the terms of
 *     Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
 *     Government retains certain rights in this software.
 *     
 *     Hermes is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU Lesser General Public License as
 *     published by the Free Software Foundation, either version 3 of
 *     the License, or (at your option) any later version.
 *     
 *     Hermes is distributed in the hope that it will be useful, but
 *     WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU Lesser General Public License for more details.
 *     
 *     You should have received a copy of the GNU Lesser General
 *     Public License along with Hermes.  If not, see
 *     <http://www.gnu.org/licenses/>.
 *     
 ************************************************************************/

#ifdef WIN32sys
/* IDL's export.h keys off the macro MSWIN */
# define MSWIN
#endif

/* function name mangling for F77 linkage */
#include "mdf77mangle.h"
#if !defined(HU_F77_MANGLING_L00)
# define end_call_idl      HU_F77_FUNC_( end_call_idl, END_CALL_IDL )
# define start_call_idl    HU_F77_FUNC_( start_call_idl, START_CALL_IDL )
# define get_cidl_state    HU_F77_FUNC_( get_cidl_state, GET_CIDL_STATE )
# define c_send_command    HU_F77_FUNC_( c_send_command, C_SEND_COMMAND )
# define c_idl_write       HU_F77_FUNC_( c_idl_write, C_IDL_WRITE )
# define put_idl_variable  HU_F77_FUNC_( put_idl_variable, PUT_IDL_VARIABLE )
# define put_idl_array     HU_F77_FUNC_( put_idl_array, PUT_IDL_ARRAY )
# define get_idl_variable  HU_F77_FUNC_( get_idl_variable, GET_IDL_VARIABLE )
# define c_idl_main        HU_F77_FUNC_( c_idl_main, C_IDL_MAIN )
/*# define     HU_F77_FUNC_( ,  )*/
#endif

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "export.h"

#define MXCMD 256

/* prototypes for internal-only routines */
char *strpack(const char* str, int maxlen, int keepcase );
int parse_uint(const char* str);
void cidl_free(UCHAR *p);
#ifdef WIN32sys
void CIDL_OutF(int flags, char *buf, int n);
#endif

/* DEBUGGING stuff 
 *    Set with "debug" argument of start_call_idl or from environment
 *    variable CIDL_DEBUG 
 */
#define DBG_START_CIDL      1   /* debug output for start_call_idl */
#define DBG_SND_CMD         2   /* debug output for c_send_command */
#define DBG_IDL_WRITE       4   /* debug output for c_idl_write */
#define DBG_PUT_VAR         8   /* debug output for put_idl_variable */
#define DBG_PUT_ARR        16   /* debug output for put_idl_array */
#define DBG_GET_VAR        32   /* debug output for get_idl_variable */
#define DBG_IDL_MAIN       64   /* debug output for idl_main */
#define DBG_FREE          128   /* debug output for cidl_free */

static FILE *dbgfile = 0;
static int   dbgflg  = 0;

/* CIDL state indicator */
static int IDL_STATE = 0; /* 0: not started; 1: active; -1: ended */


/* Routine to start CIDL (callable IDL interface)
 *
 *  Input:
 *    dbg -  Debug flag
 *             0 - no debug output
 *             bit-field for debugging specific routines
 *
 *  Output:
 *    ier -  Error flag
 *            0 - No errors
 *           -1 - IDL has been stopped and cannot be restarted.
 *           -2 - IDL is already running.
 *            1 - Unable to start IDL. Perhaps a license problem.
 */
/************************************************************************/
void start_call_idl (int *debug, int *ierr)
/************************************************************************/
{
  int retval, tmp = -1;
  char *str, *cmd;
  IDL_INIT_DATA init_data;  
  int dbg;

  dbgflg = *debug;
  /* Check to see if debug flag is overridden via environment variable */
  if ( (str = getenv("CIDL_DEBUG")) != 0 ) {
    tmp = parse_uint(str);
    if ( tmp >= 0 ) {
      dbgflg = tmp;
    }
  }
  if (dbgflg) dbgfile = fopen("debug.out","w");

  dbg = dbgflg & DBG_START_CIDL;
  if (dbg) {
    fprintf(dbgfile,"start_call_idl:\n"
	    " debug:    %d",dbgflg);
    if ( dbgflg != *debug) fprintf(dbgfile," (originally %d)\n",*debug);
    else fprintf(dbgfile,"\n");
  }

  if ( IDL_STATE == 0 ) {
    /* CIDL has not yet been opened, so open it now */
    init_data.options = 0;
#ifdef WIN32sys
    /* turn off command line interface on Windows */   
    init_data.options |= IDL_INIT_NOCMDLINE;
#endif
    retval = IDL_Initialize(&init_data);
    *ierr = 1;
    if (dbg) fprintf(dbgfile," IDL_Initialize returned: %d\n",retval);
    if ( retval ) {
      *ierr = 0;
      IDL_STATE = 1;
#ifdef WIN32sys
    /* On Windows, provide callback for IDL output, so it goes to the
       terminal rather than a GUI log window (which may not exist) */   
      IDL_ToutPush(&CIDL_OutF);
#endif
      /* Send NOP command, since 1st command seems to be ignored on Windows */
      IDL_ExecuteStr("wait,0");
      /* Provide read-only IDL system variable that can be tested by IDL
       * procedures to determine if running under CIDL environment */
      IDL_ExecuteStr("defsysv, '!CALLABLE_IDL', 1, 1");
      /* If an IDL startup file has been set in the environment, run it */
      if ( (str = getenv("IDL_STARTUP")) != 0 ) {
        cmd = (char *) malloc(strlen(str)+2);
        cmd[0] = '@';
        strcpy(cmd+1,str);
        tmp = IDL_ExecuteStr(cmd);
        if (dbg) fprintf(dbgfile," IDL_STARTUP cmd: %s err: %d\n",cmd,tmp);
        free(cmd);
      }
    }
    else *ierr = 1; /* IDL did not initialize properly */
  }
  else if ( IDL_STATE == 1 ) {
    /* CIDL is already open */
    *ierr = -2;
  }
  else if ( IDL_STATE == -1 ) {
    /* CIDL has been opened and then closed */
    *ierr = -1;
  }
}

/* Routine to end callable IDL
 *
 *  Input: None
 *
 *  Output:
 *    ier -  Error flag
 *            0 - No errors
 *           -1 - IDL has already been stopped. No action taken.
 *            1 - IDL has not been started. No action taken.
 */
/************************************************************************/
void end_call_idl (int *ierr)
/************************************************************************/
{
  if ( IDL_STATE == 1 ) {
    /* CIDL is open, so close it now */
    IDL_Cleanup(1);
    IDL_STATE = -1;
    *ierr = 0;
  }
  else if ( IDL_STATE == 0 ) {
    /* CIDL has not yet been opened */
    *ierr = 1;
  }
  else if ( IDL_STATE == -1 ) {
    /* CIDL has already been opened and then closed */
    *ierr = -1;
  }
  if (dbgfile) fclose(dbgfile);
}

/* Integer function to return the current state of CIDL
 *
 *  Return value:
 *            0 - IDL has not been started.
 *            1 - IDL has been started and is currently active.
 *           -1 - IDL has already been stopped.
 */
/************************************************************************/
int get_cidl_state()
/************************************************************************/
{
  return IDL_STATE;
}


/************************************************************************/
/* END of FORTRAN-CALLABLE ROUTINES */
/************************************************************************/

/* Function to send a command to IDL. It is designed to be called by the
 * Fortran subroutine SEND_COMMAND.
 *
 *  Input:
 *    cmd -  IDL command to be executed via callable IDL (note that this
 *           string is a Fortran string, NOT a C-delimited string)
 *    len -  Pointer to the length of the Fortran string.
 *
 *  Return value:
 *            0 - No errors
 *            1 - Callable IDL not initialized
 *           <0 - non-zero IDL error code from command's execution
 */
/************************************************************************/
int c_send_command ( char *cmd, int *len )
/************************************************************************/
{
  char *tcmd;
  int err, dbg, cnt = 0;

  if ( IDL_STATE != 1 ) return 1;

  dbg = dbgflg & DBG_SND_CMD;

  /* pack provided string into a new (dynamically allocated) string */
  tcmd = strpack(cmd,*len,0);

  if ( dbg) fprintf(dbgfile,"c_send_command:\n"
                    " len: %d  cmd: '%s'\n",*len,tcmd);

  /* Clear IDL's !ERROR_STATE variable, in case last command had an error */
  err = IDL_ExecuteStr("message,/reset");
  while ( err  && cnt < 5 ) {
    err = IDL_ExecuteStr("message,/reset");
    ++cnt;
  }
  /* execute provided command -- then free allocated temporary string */
  err = IDL_ExecuteStr(tcmd);
  free(tcmd);

  return err;
}

/* Function to write a string to an IDL display. It is designed to be called
 * by the Fortran subroutine IDL_WRITE.
 *
 *  Input:
 *    str  - The string to display (note that this string is a Fortran
 *           string, NOT a C-delimited string)
 *    len  - Pointer to the length of the Fortran string.
 *    flag - If flag is zero, no leading characters will be printed.
 *           If flag is not zero, then the value of the IDL system
 *           variable !MSG_PREFIX will be printed. (Default = '% ')
 */
/************************************************************************/
void c_idl_write ( char *str, int *len, int *flag )
/************************************************************************/
{
  char *tstr;
  int action = IDL_MSG_INFO;
  int dbg;

  dbg = dbgflg & DBG_IDL_WRITE;

  /* pack provided string into a new (dynamically allocated) string */
  tstr = strpack(str,*len,0);
  if ( ! *flag ) action |= IDL_MSG_ATTR_NOPREFIX;

  if (dbg) fprintf(dbgfile,"c_idl_write:\n"
                   " len:    %d  str: '%s'\n"
                   " action: 0x%08x\n",*len,tstr,action);

  /* send provided message to IDL -- then free allocated temporary string */
  IDL_Message(IDL_M_GENERIC, action, tstr);
  free(tstr);
}

/* Function to assign a value to an IDL variable. It is designed to be called
 * by various Fortran subroutines: SEND_REAL_VAR, SEND_INT_VAR, SEND_STR_VAR
 *
 *  Input:
 *    name - Name of IDL variable. (note that this string is a Fortran
 *           string, NOT a C-delimited string)
 *    nlen - Pointer to the length of the Fortran string "name".
 *    type - Pointer to the IDL variable type.
 *    data - Pointer to the data to be assigned to the IDL variable
 *    slen - Pointer to the length of the Fortran string "data". This is only
 *           used if *type = IDL_TYP_STRING.
 *
 *  Return value:
 *            0 - No errors
 *           -1 - Callable IDL not initialized
 *            1 - Empty variable name
 *            2 - IDL could not create the variable
 *            3 - unsupported or illegal type
 */
/************************************************************************/
int put_idl_variable(const char *name, int *nlen, int *type, void *data, 
                     int *slen)
/************************************************************************/
{
  char *tname;
  IDL_VPTR vloc;
  IDL_STRING strobj;
  void *dptr;
  int dbg;

  dbg = dbgflg & DBG_PUT_VAR;

  if ( IDL_STATE != 1 ) return -1;

  /* pack provided string into a (dynamically allocated) uppercase string */
  tname = strpack(name, *nlen, 1);
  if ( strlen(tname) == 0 ) return 1; /* blank varible name provided?? */

  if (dbg) fprintf(dbgfile,"put_idl_variable:\n"
                   " type: %d\n"
                   " nlen: %d  tname: '%s'\n",*type,*nlen,tname);

  /* get a pointer to a named variable object (this call will delete any
   * dynamic portions of the variable if it already existed) */
  vloc = IDL_GetVarAddr1(tname,1);
  free(tname); /* don't need this string anymore */
  tname = 0;

  if ( vloc == 0 ) return 2; /* IDL couldn't give me a variable pointer */

  switch ( *type ) {
    case IDL_TYP_STRING:
      /* pack provided string into a new (dynamically allocated) string */
      tname = strpack( (char *) data, *slen, 0);
      /* set appropriate members of string variable object */
      strobj.slen = (IDL_STRING_SLEN_T) strlen(tname);
      strobj.stype = 1;
      strobj.s = tname;
      dptr = &strobj;
      if (dbg) fprintf(dbgfile," slen: '%d'  tname: '%s'\n",*slen,tname);
      break;
    case IDL_TYP_LONG:
    case IDL_TYP_FLOAT:
      /* set data member of arithmetic variable object */
      dptr = data;
      break;
    default:
      return 3; /* unsupported or illegal type */
  }
  /* Now store the constructed variable */
  IDL_StoreScalar(vloc, *type, dptr );
  free(tname); /* if a string variable, need to free the temporary */
  return 0;
}

/* Function to assign values to an IDL array. It is designed to be called
 * by various Fortran subroutines:
 *   SEND_REAL_ARRAY, SEND_INT_ARRAY, SEND_BYTE_ARRAY
 * Note that IDL string arrays are not currently supported.
 *
 *  Input:
 *    name - Name of IDL array variable. (note that this string is a
 *           Fortran string, NOT a C-delimited string)
 *    nlen - Pointer to the length of the Fortran string "name".
 *    type - Pointer to the IDL variable type.
 *    ndim - Pointer to the # of dimensions in the array.
 *    dims - Array of dimension lengths for the "ndim" dimensions.
 *    data - Pointer to the data to be assigned to the IDL variable
 *
 *  Return value:
 *            0 - No errors
 *           -1 - Callable IDL not initialized
 *            1 - NDIM exceeds IDL's maximum number of array dimensions
 *                (IDL_MAX_ARRAY_DIM)
 *            2 - Empty variable name
 *            3 - Unsupported or illegal type
 *            4 - IDL could not create the variable
 */
/************************************************************************/
int put_idl_array(const char *name, int *nlen, int *type, int *ndim,
                  int *dims, void *data)
/************************************************************************/
{
  int i, tot;
  int *bdat;
  char *tname;
  UCHAR *bytes = 0;
  IDL_VPTR vloc;
  int dbg;
  IDL_ARRAY_DIM tdims;

  dbg = dbgflg & DBG_PUT_ARR;

  if ( IDL_STATE != 1 ) return -1;

  if ( *ndim > IDL_MAX_ARRAY_DIM ) return 1;

  for(i=0;i<*ndim;++i) tdims[i]=dims[i];

  /* pack provided string into a (dynamically allocated) uppercase string */
  tname = strpack(name, *nlen, 1);
  if ( strlen(tname) == 0 ) return 2; /* blank varible name provided?? */

  if (dbg) {
    fprintf(dbgfile,"put_idl_array:\n"
                    " type: %d\n"
                     " nlen: %d  tname: '%s'\n"
                     " ndim: %d  dims:",*type,*nlen,tname,*ndim);
    for(i=0;i<*ndim;++i) fprintf(dbgfile," %d",dims[i]);
    fprintf(dbgfile,"\n");
  }

  switch ( *type ) {
    case IDL_TYP_BYTE:
      /* for byte array, need to pack bytes into allocated UCHAR array */
      tot = 1;
      for(i=0;i<*ndim;++i) tot *= dims[i];
      bytes = (UCHAR *) malloc(tot);
      bdat = (int *) data;
      for(i=0;i<tot;++i) bytes[i] = *bdat++;
      /* Send array variable to IDL, provide callback to free UCHAR array */
      vloc = IDL_ImportNamedArray(tname, *ndim, tdims, *type, bytes, cidl_free,
                                  0);
      if (dbg) fprintf(dbgfile," size: %d\n ptr:  %p\n",tot,bytes);
      break;
    case IDL_TYP_LONG:
    case IDL_TYP_FLOAT:
      /* Send array variable to IDL, */
      vloc = IDL_ImportNamedArray(tname, *ndim, tdims, *type, data, 0, 0);
      break;
    default:
      return 3; /* unsupported or illegal type */
  }

  free(tname);

  if ( vloc == 0 ) return 4; /* IDL could not create the variable */

  return 0;
}

/* Function to retrieve values from an IDL scalar or array variable. It is
 * designed to be called by various Fortran subroutines:
 *   GET_REAL_VAR, GET_INT_VAR, GET_STR_VAR,
 *   GET_REAL_ARRAY, GET_INT_ARRAY, GET_BYTE_ARRAY
 * Note that IDL string arrays are not currently supported.
 *
 *  Input:
 *    name   - Name of IDL variable. (note that this string is a Fortran
 *             string, NOT a C-delimited string)
 *    nlen   - Pointer to the length of the Fortran string "name".
 *    type   - Pointer to the IDL variable type.
 *    mxsize - Pointer to maximum linear size of an array, or zero for
 *             a scalar
 *
 *  Output:
 *    ndim   - Pointer to the # of dimensions in the array.
 *    dims   - Array of dimension lengths for the "ndim" dimensions.
 *    nel    - Pointer to the # of elements in the IDL variable accessed.
 *             (for scalar strings, it is the length of the string)
 *    data   - Pointer to the data to be assigned to the IDL variable
 *
 *  Return value:
 *            0 - No errors
 *           -1 - Callable IDL not initialized
 *           -2 - IDL variable is not defined.
 *           -3 - Name is an empty string
 *           -4 - Insufficient storage for data available:
 *                  For arrays and string scalars, this  means *nel > *mxsize
 *                  For other scalars, this  means *nel > 1
 *          1-9 - IDL type mismatch. Error code is actual type of the
 *              - IDL variable
 */
/************************************************************************/
int get_idl_variable(const char *name, int *nlen, int *type, int *mxsize,
                     int *ndim, int *dims, int *nel, void *data)
/************************************************************************/
{
  int i, rval;
  char *tname;
  int *dat = 0;
  UCHAR *bdata = 0;
  int is_array = 0;
  int mxbytes, bpel, avail_size;
  IDL_VPTR vloc;
  IDL_ARRAY *arptr;
  IDL_STRING *sptr;
  void *dptr;
  int dbg;

  dbg = dbgflg & DBG_GET_VAR;

  *nel = 0;

  if ( IDL_STATE != 1 ) return -1;

  /* pack provided string into a (dynamically allocated) uppercase string */
  tname = strpack(name, *nlen, 1);
  if ( strlen(tname) == 0 ) return -3; /* blank varible name provided?? */

  if (dbg) fprintf(dbgfile,"get_idl_var:\n"
                           " type: %d\n"
                           " nlen: %d  tname: '%s'\n"
                           " mxsize: %d",*type,*nlen,tname,*mxsize);

  /* Get the pointer to the IDL variable -- then free temporary string */
  vloc = IDL_FindNamedVariable(tname,0);
  free(tname);
  if ( ! vloc ) return -2; /* variable doesn't exist */
  if (dbg)fprintf(dbgfile," type: %d  flags: 0x%08x\n",vloc->type,vloc->flags);

  if ( *type != vloc->type ) return vloc->type; /* variable type isn't right */

  is_array = vloc->flags & IDL_V_ARR; /* Is this an array? */

  rval = 0;
  if ( is_array ) {
    if ( *type == IDL_TYP_STRING ) return -5; /* string arrays not supported */
    if ( *mxsize == 0 ) rval = -4; /* 0 size indicates request for scalar */
    /* get array dimensions, etc. */
    arptr = vloc->value.arr;
    *ndim = arptr->n_dim;
    for(i=0;i<*ndim;++i) dims[i] = arptr->dim[i];
    *nel = arptr->n_elts;
    dptr = arptr->data; /* get pointer to the actual data */
  }
  else {
    /* Not an array, set dimensions and get pointer to the data */
    *ndim = 1;
    dims[0] = 1;
    *nel = 1;
    dptr = &vloc->value;
  }

  bdata = data; /* for all but byte data, data can be received directly */
  /* For scalars, there is space for 1 value */
  avail_size = *mxsize > 0 ? *mxsize : 1;
  switch ( *type ) {
    case IDL_TYP_BYTE:
      /* For byte data, need a temporary UCHAR array to receive it */
      bdata = (UCHAR *) malloc(*nel);
      dat = (int *) data;
      mxbytes = (*nel < avail_size) ? *nel : avail_size;
      break;
    case IDL_TYP_STRING:
      /* for a string, get byte length and a pointer to the data */
      sptr = dptr;
      *nel = sptr->slen;
      mxbytes = (*nel < avail_size) ? *nel : avail_size;
      dptr = sptr->s;
      break;
    case IDL_TYP_LONG:
    case IDL_TYP_FLOAT:
      /* for aritemetic data, get byte length */
      if ( *type == IDL_TYP_LONG ) bpel = sizeof(IDL_LONG);
      else bpel = sizeof(float);
      mxbytes = bpel*((*nel < avail_size) ? *nel : avail_size);
      break;
    default:
      return vloc->type; /* unsupported type */
  }
  if (dbg) {
    fprintf(dbgfile," nel: %d  ndim: %d  dims:",*nel, *ndim);
    for(i=0;i<*ndim;++i) fprintf(dbgfile," %d",dims[i]);
    fprintf(dbgfile,"\n");
  }

  /* now copy data from IDL variable into receiving location */
  memcpy(bdata,dptr,mxbytes);
  if ( *type == IDL_TYP_BYTE ) {
    /* for byte data, copy data into integer array & delete temp byte array */
    for(i=0;i<*nel;++i) dat[i] = bdata[i];
    free(bdata);
  }
  else if ( *type == IDL_TYP_STRING ) {
    /* for string data, fill rest of Fortran string with blank characters */
    tname = (char *) data;
    for(i=*nel;i<avail_size;++i) tname[i] = ' ';
  }
  /* if no other errors, check to see if there was enough space */
  if ( rval == 0 && *nel > avail_size ) rval = -4;

  return rval;
}

/* Function to retrieve strings from an IDL input interface. It is
 * designed to be called by the Fortran subroutine IDL_MAIN.
 *
 *  Input:
 *    pis_term - Pointer to integer flag indicating interface type:
 *                0 - GUI interface
 *                1 - terminal interface
 *    pmode    - Pointer to integer flag indicating whether strings should
 *               be passed to IDL as commands:
 *                0 - pass to IDL until "EXIT" (or "QUIT") is encountered
 *                1 - return string to calling routine (as cmd_out)
 *    prompt   - Prompt string. (note that this string is a Fortran string,
 *               NOT a C-delimited string)
 *    plen     - Pointer to the length of the Fortran string "prompt".
 *    pxs      - Pointer to the GUI window width in characters
 *    pys      - Pointer to the GUI window heights in lines
 *    phistsiz - Pointer to the number of commands available from recall stack
 *    clen     - Pointer to the length of the Fortran string "cmd_out".
 *
 *  Output:
 *    cmd_out  - string retrieved (mode = 1 only). Note this is a Fortran
 *               string.
 *
 *  Return value:
 *            0 - No errors
 *           -1 - Callable IDL not initialized
 *           -5 - Input string exceeds CIDL's internal buffer
 *        other - errors returned by get_idl_variable for a scalar string
 */
/************************************************************************/
int c_idl_main(int *pis_term, int *pmode, char *prompt, int *plen, int *pxs,
               int *pys, int *phistsiz, char *cmd_out, int *clen)
/************************************************************************/
{
  int mode, xs , ys, histsiz;
  int is_term = *pis_term;
  int rval, is_exit;
  int i, alen, len, ndim, nel;
  int dims[IDL_MAX_ARRAY_DIM];
  int type = IDL_TYP_STRING, zero = 0, mxsiz = MXCMD;
  char *prmt;
  char def_prompt[] = "Enter Command Below";
  char *cmd_string;
  char ans[] = "CIDL_CMDSTRING";
  char cmd[MXCMD+1];
  char *tcmd;
  int dbg;

  if ( IDL_STATE != 1 ) return -1;

  dbg = dbgflg & DBG_IDL_MAIN;

  rval = 0;
  mode = (*pmode) ? 1:0;
  /* pack provided string into a new (dynamically allocated) string */
  prmt = strpack(prompt, *plen, 0);
  alen = (int) strlen(ans);

  if (dbg) fprintf(dbgfile,"idl_main:\n"
                           " is_term: %d  mode: %d\n"
                           " plen: %d  prompt: '%s'\n"
                           " xs: %d  ys: %d  hsiz: %d\n",
                   is_term,*pmode,*plen,prmt,*pxs, *pys, *phistsiz);

  if ( is_term ) { /* Terminal Interface */
#ifndef WIN32sys
    if ( strlen(prmt) == 0 ) { /* need to use default prompt */
      free(prmt);
      prmt = (char *) malloc(5);
      strcpy(prmt,"IDL>");
    }
    /* allocate and build terminal-based command string */
    len = 2*alen + (int) strlen(prmt) + 28;
    cmd_string = (char *) malloc(len);
    /*                     12345678901234567890123  4567  8 */
    sprintf(cmd_string, "%s = ' ' & read, prompt='%s ', %s", ans, prmt, ans );
  }
  else {
#else
    /* For Windows, can't use terminal interface, so revert to GUI interface */
    IDL_Message(IDL_M_GENERIC, IDL_MSG_INFO | IDL_MSG_ATTR_NOPREFIX,
                "Terminal interface not available under Windows");
    IDL_Message(IDL_M_GENERIC, IDL_MSG_INFO | IDL_MSG_ATTR_NOPREFIX,
                "Using widget interface instead");
  }
#endif
    /* GUI Interface */
    xs = (*pxs) % 10000;
    ys = (*pys) % 10000;
    histsiz = (*phistsiz) % 100000;
    if ( strlen(prmt) == 0 ) { /* need to use default prompt */
      free(prmt);
      prmt = def_prompt;
    }
    /* allocate and build GUI-based command string */
    len = alen + (int) strlen(prmt) + 79;
    cmd_string = (char *) malloc(len);
    /*                   12345678901234567  890123456  012345678  234 */
    sprintf(cmd_string, "GET_IDL_COMMAND, %s, xsize =%4d, ysize=%4d, "
            /*           56789  0123456789012  78901234567  89 */
                        "mode=%1d, max_stack=%5d, prompt='%s'",
            ans, xs, ys, mode, histsiz, prmt);
#ifndef WIN32sys
# if 0
  {    /* kluge for auto-indenting editors */
# endif
  }
#endif
  if (dbg) fprintf(dbgfile," cmd_string:\n  '%s'\n", cmd_string);

  while (1) { /* Loop until an exit event is encountered */
    /* Send command to IDL for execution */
    IDL_ExecuteStr(cmd_string);
    /* Retrieve the string returned by IDL */
    rval = get_idl_variable(ans, &zero, &type, &mxsiz, &ndim, dims, &nel, cmd);
    if ( rval ) break; /* error retrieving string */

    if ( nel > mxsiz ) { /* Is returned string too long for our buffer? */
      rval = -5;
      break;
    }
    else cmd[nel] = '\0'; /* Need to terminate as a C string */

    if (dbg) fprintf(dbgfile," cmd: '%s'\n", cmd);
    if ( mode ) {
      /* If "return" mode, copy string (w/ blank fill) into Fortran string,
       * then return */
      len = (*clen < nel ) ? *clen : nel;
      strncpy(cmd_out,cmd,len);
      for(i=nel;i<*clen;++i) cmd_out[i] = ' ';
      break; /* Exit loop */
    }

    /* pack provided string into a (dynamically allocated) uppercase string */
    tcmd = strpack(cmd, 0, 1 );
    /* check to see if an "exit" condition has been encountered */
    is_exit = strcmp("EXIT",tcmd) == 0 || strcmp("QUIT",tcmd) == 0;
    free(tcmd);
    if ( is_exit ) break; /* EXIT command has been processed */
    IDL_ExecuteStr(cmd);  /* Otherwise, send to IDL for execution */
  }

  /* Free prompt string only if it was previously allocated */
  if ( prmt && prmt != def_prompt ) free(prmt);
  free(cmd_string); /* Free command string */
  /* If there's an error in "return" mode, return a blank string */
  if ( rval && mode ) for(i=0;i<*clen;++i) cmd_out[i] = ' ';
  return rval;
}

/************************************************************************/
/* END of Fortran INTERFACE functions */
/************************************************************************/

/* Function that returns a "packed" C-string from another string. It removes
 * any leading of trailing blank characters.
 *
 *  Input:
 *    str    - String to be packed
 *    maxlen - If str is a standard C-string, maxlen should be zero.
 *             If str points to a Fortran string, maxlen is the length of
 *             the string
 *    upcase - If nonzero, the string is also converted to upper case.
 *
 *  Returns:
 *    A pointer to the packed string, which has been dynamically allocated.
 *    The calling function is responsible for freeing the pointer when no
 *    longer needed.
 */
/************************************************************************/
char *strpack(const char* str, int maxlen, int upcase )
/************************************************************************/
{
  int len, l1=0, l2=0, l;
  char *nstr;

  /* maxlen not positive indicates provided string is a C string; get length */
  if ( maxlen <= 0 ) maxlen = (int) strlen(str);

  /* find 1st non-blank character (l1) */
  for(l1=0;l1<maxlen;++l1) if ( str[l1] != ' ' ) break;

  if ( l1 == maxlen ) len = 0;
  else {
    /* find last non-blank character (l2) */
    for(l2=maxlen-1;l2>l1;--l2) if ( str[l2] != ' ' ) break;
    len = l2 - l1 + 1;
  }

  nstr = (char *) malloc(len+1); /* Allocate the string to be returned */
  strncpy(nstr,str+l1,len);      /* copy the residual string */
  nstr[len] = '\0';              /* and terminate properly */

  /* if requested, convert to uppercase */
  if ( upcase ) {
    for(l=0; l<len; ++l) nstr[l] = toupper(nstr[l]);
  }

  return nstr;
}

/* Function that parses a string as an unsigned integer, in standard
 * decimal, octal (0 prefix) or hexidecimal (0x or 0X prefix) notation.
 *
 *  Input:
 *    str    - String to be parsed
 *
 *  Returns:
 *    Parsed integer, or -1 if string cannot be interpreted as an unsigned
 *    integer
 */
/************************************************************************/
int parse_uint(const char* str)
/************************************************************************/
{
  int i, istart, len, val, mult;

  len = (int) strlen(str);
  if ( len == 0 ) return -1;

  val = 0;
  istart = 0;
  mult = 10;

  if ( str[0] == '0' ) {
    /* is this octal or hexadecimal */
    if ( len == 1 ) return 0; /* It's simply zero "0" */
    ++istart; /* 1st digit to process is now one */
    if ( isdigit(str[1]) ) mult = 8;  /* Try to interpret as octal */

    else if ( toupper(str[1]) == 'X' ) {
      /* interpret as hexidecimal */
      ++istart;/* 1st digit to process is now two */
      mult = 16;
    }
    else return -1; /* cannot interpret as a number */ 
  }

  for(i=istart;i<len;++i) {
    if ( mult == 16 ) {
      /* if hex, look for legal hex digit */
      if ( isxdigit(str[i]) ) {
        if ( str[i] <= '9' ) { 
          val = mult*val + str[i] - '0';
        }
        else {
          val = mult*val + toupper(str[i]) - 'A' + 10;
        }
      }
      else return -1; /* cannot interpret as a number */
    }
    else {  /* processing decimal or octal */
      if ( isdigit(str[i]) ) {/* first check for legal decimal digit */
        if ( mult == 8 ) {
          /* if octal, check for legal octal digit */
          if ( str[i] >= '8' ) {
            /* if not legal octal digit, go back to the beginning and try to
             * reinterpret as a decimal number */
            i = 0;
            val = 0;
            mult = 10;
          }
          else val = mult*val + str[i] - '0';
        }
        else val = mult*val + str[i] - '0';
      }
      else return -1; /* cannot interpret as a number */
    }
  }

  return val; /* if we've gotten this far, val should be the number */
}


/* Callback function that IDL calls when it is no longer referencing
   dynamically-allocated data. This allows CIDL to free memory that it
   allocates when creating an IDL array variable.
 *
 *  Input:
 *    p    - pointer to dynamically-allocated data
 */
/************************************************************************/
void cidl_free(UCHAR *p)
/************************************************************************/
{
  if (dbgflg & DBG_FREE) fprintf(dbgfile,"cidl_free called for: %p\n",p);
  free(p);
}

#ifdef WIN32sys
/* Callback function that IDL calls when it needs to write data to the screen.
 * This is needed for Windows systems since Windows IDL will not write to the
 * terminal on its own.
 *
 *  Input:
 *    flags - Bitmask of flag values that specify how the text should be output
 *    buf   - The text to be output
 *    n     - The number of characters in buf to be output
 */
/************************************************************************/
void CIDL_OutF(int flags, char *buf, int n)
/************************************************************************/
{
  char *tbuf = 0;
  FILE *ofile;

  /* build a temporary C string */
  tbuf = (char *) malloc(n+1);
  strncpy(tbuf,buf,n);
  tbuf[n] = '\0';

  /* chect to see whether output goes to stdout or stderr */
  if ( flags & IDL_TOUT_F_STDERR ) ofile = stderr;
  else ofile = stdout;

  fprintf(ofile,"%s",tbuf); /* print string, then free allocated string */
  free(tbuf);
  if  ( flags & IDL_TOUT_F_NLPOST ) fprintf(ofile,"\n"); /* write newline? */
}
#endif
