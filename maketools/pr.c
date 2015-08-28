/*

Copyright (c) 1993, 1994, 1998 The Open Group

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of The Open Group shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from The Open Group.

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
   
   Hermes uses pr.c with some modification:

     1. Instead of redirecting stdout to the specified output file,
        simply open it as another file, and use an extern FILE pointer
        for functions that write to it (now with fprintf rather than
        printf, etc.). This allows a calling program to still be able
        to write to stdout after calling this function.
     2. Changed makedepend's default behavior to use a continuation
        character (\) on multiple-line dependencies rather than
        repeating the object and ":" for each line. A "-nocontinue"
        option was added to restore the default behavior.
     3. Add "bool list_src_dep" to force inclusion source file as a
        dependency (this is needed, for example, by NMAKE on WIN32
        systems).
     4. Modify to optionally write path names in which user-specified
        environment variable names are substituted for for their
        values anywhere they occur in a dependency specification.
     5. Add "bool list_extra_deps" and char *extra_deps" to force
        inclusion of other items in all dependencies (e.g., this could
        be used to add a makefile to the list).

*/

#include "def.h"

extern struct	inclist	inclist[ MAXFILES ],
			*inclistp;
extern char	*objprefix;
extern char	*objsuffix;
extern char	*cont_string;
extern char	*extra_deps;
extern int	width;
extern boolean	printed;
extern boolean	verbose;
extern boolean	show_where_not;
extern boolean  list_src_dep;
extern boolean  list_extra_deps;
extern FILE    *fdout;

void
add_include(struct filepointer *filep, struct inclist *file,
	    struct inclist *file_red, char *include, int type,
	    boolean failOK)
{
	register struct inclist	*newfile;
	register struct filepointer	*content;

	/*
	 * First decide what the pathname of this include file really is.
	 */
	newfile = inc_path(file->i_file, include, type);
	if (newfile == NULL) {
		if (failOK)
		    return;
		if (file != file_red)
			warning("%s (reading %s, line %d): ",
				file_red->i_file, file->i_file, filep->f_line);
		else
			warning("%s, line %d: ", file->i_file, filep->f_line);
		warning1("cannot find include file \"%s\"\n", include);
		show_where_not = TRUE;
		newfile = inc_path(file->i_file, include, type);
		show_where_not = FALSE;
	}

	if (newfile) {
		included_by(file, newfile);
		if (!(newfile->i_flags & SEARCHED)) {
			newfile->i_flags |= SEARCHED;
			content = getfile(newfile->i_file);
			find_includes(content, newfile, file_red, 0, failOK);
			freefile(content);
		}
	}
}

static void
pr(struct inclist *ip, char *file, char *base)
{
	static char	*lastfile;
	static int	current_len, cont_len;
	register int	len, i;
	char	buf[ BUFSIZ ];

        if ( !printed ) {
          printed = TRUE;
          cont_len = strlen(cont_string);
          current_len = cont_len;
        }
	if (ip) len = strlen(ip->m_file)+1; else len = 0;
	if (current_len + len > width || file != lastfile) {
          if ( !cont_len || file != lastfile ) {
            if ( file != lastfile && (list_src_dep || list_extra_deps) ) {
              sprintf(buf, "\n%s%s%s:", objprefix, base, objsuffix);
              if ( list_extra_deps )
                sprintf(buf+strlen(buf), " %s", extra_deps); 
              if ( list_src_dep )
                sprintf(buf+strlen(buf), " %s", file); 
              if (ip ) sprintf(buf+strlen(buf), " %s", ip->m_file); 
            }
            else {
              sprintf(buf, "\n%s%s%s: %s", objprefix, base, objsuffix,
                      ip->m_file);
            }
            len = strlen(buf);
            current_len = len + cont_len - 1;
            lastfile = file;
          }
          else {
            sprintf(buf, "%s\n\t%s", cont_string, ip->m_file);
            len = strlen(buf);
            current_len =  len + 7;
          }
	}
	else {
		buf[0] = ' ';
		strcpy(buf+1, ip->m_file);
		current_len += len;
	}
	fwrite(buf, len, 1, fdout);

	/*
	 * If verbose is set, then print out what this file includes.
	 */
	if (! verbose || ip->i_list == NULL || ip->i_flags & NOTIFIED)
		return;
	ip->i_flags |= NOTIFIED;
	lastfile = NULL;
	fprintf(fdout,"\n# %s includes:", ip->m_file);
	for (i=0; i<ip->i_listlen; i++)
		fprintf(fdout,"\n#\t%s", ip->i_list[ i ]->i_incstring);
}

void
recursive_pr_include(struct inclist *head, char *file, char *base)
{
	int	i;

	if (head->i_flags & MARKED)
		return;
	head->i_flags |= MARKED;
        if (head->m_file != file)
		pr(head, file, base);
        else if ( list_src_dep || list_extra_deps )
		pr(0, file, base);
	for (i=0; i<head->i_listlen; i++)
		recursive_pr_include(head->i_list[ i ], file, base);
}
