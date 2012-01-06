/*
 * Copyright 1992 Network Computing Devices, Inc.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Network Computing Devices may not be
 * used in advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  Network Computing Devices makes
 * no representations about the suitability of this software for any purpose.
 * It is provided ``as is'' without express or implied warranty.
 * 
 * NETWORK COMPUTING DEVICES DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 * SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
 * IN NO EVENT SHALL NETWORK COMPUTING DEVICES BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author:  Jim Fulton
 *          Network Computing Devices, Inc.
 * 
 * Simple if statement processor
 *
 * This module can be used to evaluate string representations of C language
 * if constructs.  It accepts the following grammar:
 * 
 *     EXPRESSION	:=	VALUE
 * 			 |	VALUE  BINOP	EXPRESSION
 *			 |	VALUE	'?'	EXPRESSION ':'	EXPRESSION
 * 
 *     VALUE		:=	'('  EXPRESSION  ')'
 * 			 |	'!'  VALUE
 * 			 |	'-'  VALUE
 *			 |	'~'  VALUE
 * 			 |	'defined'  '('  variable  ')'
 * 			 |	variable
 * 			 |	number
 * 
 *     BINOP		:=	'*'	|  '/'	|  '%'
 * 			 |	'+'	|  '-'
 * 			 |	'<<'	|  '>>'
 * 			 |	'<'	|  '>'	|  '<='  |  '>='
 * 			 |	'=='	|  '!='
 * 			 |	'&'	|  '^'  |  '|'
 * 			 |	'&&'	|  '||'
 * 
 * The normal C order of precedence is supported.
 * 
 * 
 * External Entry Points:
 * 
 *     ParseIfExpression		parse a string for #if
 */

/* C_Groups hermes
   $Id$
   
   Copyright (2008) Sandia Corporation. Under the terms of
   Contract DE-AC04-94AL85000 with Sandia Corporation, the U.S.
   Government retains certain rights in this software.
   
   Hermes is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation, either version 3 of
   the License, or (at your option) any later version.
   
   Hermes is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.
   
   You should have received a copy of the GNU Lesser General
   Public License along with Hermes.  If not, see
   <http://www.gnu.org/licenses/>.
   
   Hermes uses ifparser.h without modification.

*/

#include <stdio.h>

typedef int Bool;
#define False 0
#define True 1

typedef struct _if_parser {
    struct {				/* functions */
	const char *(*handle_error) (struct _if_parser *, const char *,
				     const char *);
	long (*eval_variable) (struct _if_parser *, const char *, int);
	int (*eval_defined) (struct _if_parser *, const char *, int);
    } funcs;
    char *data;
} IfParser;

const char *ParseIfExpression (
    IfParser *, 
    const char *, 
    long *
);

