/*
 * $Id$
 * 
 * Copyright (2008) Sandia Corporation. Under the terms of
 * Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
 * Government retains certain rights in this software.
 * 
 * Hermes is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 * 
 * Hermes is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General
 * Public License along with Hermes.  If not, see
 * <http://www.gnu.org/licenses/>.
 * 
 * C_Groups
 */

/* This file must be C-compatible. */

#ifndef token_enumH
#define token_enumH

/*! Enumeraton of Token types */
typedef enum
{
  /*! Indicates that an \b End-of-File was encountered while extracting this
   *  token. */
  TK_EXIT       =   0,
  /*! Indicates an \b empty token. */
  TK_NONE,
  /*! Indicates the token contains an \b identifier, which is a special form of
   *  string that is composed entirely of alphanumeric characters and
   *  underscores ('_'), with the additional restriction that the first
   *  character cannot be a  numeral (0-9). */
  TK_IDENTIFIER,
  /*! Indicates the token contains an \b integer value. */
  TK_INTEGER,
  /*! Indicates the token contains a \b real value. */
  TK_REAL,
  /*! Indicates the token contains a \b character \b string that cannot be
   *  interpreted as an identifier, integer, or real value. */
  TK_STRING
}
Token_Type;

#endif

