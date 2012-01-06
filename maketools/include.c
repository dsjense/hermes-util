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
   
   Hermes uses include.c with some modification:
     1. Path delimiters now properly handled for Unix-like and WIN32-like
        file systems.
     2. Proper handling of filenames with spaces (for WIN32) added.
     3. Macro substitution into filenames in dependency lists implemented.

*/


#include "def.h"
#include "ev_macros.h"

#ifdef WIN32
  char path_delim = '\\';
#else
  char path_delim = '/';
#endif

	extern struct	inclist	inclist[ MAXFILES ],
	*inclistp, *inclistnext;
extern char	*includedirs[ ],
	**includedirsnext;
extern char	*notdotdot[ ];
extern boolean show_where_not;
extern boolean warn_multiple;

static boolean
isdot(char *p)
{
	if(p && *p++ == '.' && *p++ == '\0')
		return(TRUE);
	return(FALSE);
}

static boolean
isdotdot(char *p)
{
	if(p && *p++ == '.' && *p++ == '.' && *p++ == '\0')
		return(TRUE);
	return(FALSE);
}

static boolean
issymbolic(char *dir, char *component)
{
#ifdef S_IFLNK
	struct stat	st;
	char	buf[ BUFSIZ ], **pp;

	sprintf(buf, "%s%s%s", dir, *dir ? "/" : "", component);
	for (pp=notdotdot; *pp; pp++)
		if (strcmp(*pp, buf) == 0)
			return (TRUE);
	if (lstat(buf, &st) == 0
	    && (st.st_mode & S_IFMT) == S_IFLNK) {
		*pp++ = copy(buf);
		if (pp >= &notdotdot[ MAXDIRS ])
			fatalerr("out of .. dirs, increase MAXDIRS\n");
		return(TRUE);
	}
#endif
	return(FALSE);
}

/*
 * Occasionally, pathnames are created that look like .../x/../y
 * Any of the 'x/..' sequences within the name can be eliminated.
 * (but only if 'x' is not a symbolic link!!)
 */
static void
remove_dotdot(char *path)
{
	register char	*end, *from, *to, **cp;
	char		*components[ MAXFILES ],
		newpath[ BUFSIZ ];
	boolean		component_copied;

	/*
	 * slice path up into components.
	 */
	to = newpath;
	if (*path == '/')
		*to++ = '/';
#ifdef WIN32
	if (*path == '\\')
		*to++ = '\\';
#endif
	*to = '\0';
	cp = components;
	for (from=end=path; *end; end++)
	{
		if (*end == '/') {
			while (*end == '/')
				*end++ = '\0';
			if (*from)
				*cp++ = from;
			from = end;
		}
#ifdef WIN32
		if (*end == '\\') {
			while (*end == '\\')
				*end++ = '\0';
			if (*from)
				*cp++ = from;
			from = end;
		}
#endif
	}
	*cp++ = from;
	*cp = NULL;

	/*
	 * Recursively remove all 'x/..' component pairs.
	 */
	cp = components;
	while(*cp) {
		if (!isdot(*cp) && !isdotdot(*cp) && isdotdot(*(cp+1))
		    && !issymbolic(newpath, *cp))
		{
			char **fp = cp + 2;
			char **tp = cp;

			do
				*tp++ = *fp; /* move all the pointers down */
			while (*fp++);
			if (cp != components)
				cp--;	/* go back and check for nested ".." */
		} else {
			cp++;
		}
	}
	/*
	 * Concatenate the remaining path elements.
	 */
	cp = components;
	component_copied = FALSE;
	while(*cp) {
		if (component_copied)
			*to++ = path_delim;
		component_copied = TRUE;
		for (from = *cp; *from; )
			*to++ = *from++;
		*to = '\0';
		cp++;
	}
	*to++ = '\0';

	/*
	 * copy the reconstituted path back to our pointer.
	 */
	strcpy(path, newpath);
}

/*
 * Add an include file to the list of those included by 'file'.
 */
struct inclist *
newinclude(char *newfile, char *incstring)
{
	register struct inclist	*ip;
#ifdef WIN32
        int nlen;
	char *p;
#endif

	/*
	 * First, put this file on the global list of include files.
	 */
	ip = inclistp++;
	if (inclistp == inclist + MAXFILES - 1)
		fatalerr("out of space: increase MAXFILES\n");
#ifdef WIN32
	if ( strchr(newfile,' ') ) {
		nlen = strlen(newfile);
		ip->i_file = (char *) malloc(nlen+3);
                ip->i_file[0] = '"';
		strcpy(ip->i_file+1,newfile);
		p = ip->i_file + nlen + 1;
		*p++ = '"';
		*p = '\0';
	}
	else
#endif
        ip->i_file = copy(newfile);

        ip->m_file = EV_Subst_Macro(ip->i_file);

	if (incstring == NULL)
		ip->i_incstring = ip->i_file;
	else
		ip->i_incstring = copy(incstring);

	inclistnext = inclistp;
	return(ip);
}

void
included_by(struct inclist *ip, struct inclist *newfile)
{
	register int i;

	if (ip == NULL)
		return;
	/*
	 * Put this include file (newfile) on the list of files included
	 * by 'file'.  If 'file' is NULL, then it is not an include
	 * file itself (i.e. was probably mentioned on the command line).
	 * If it is already on the list, don't stick it on again.
	 */
	if (ip->i_list == NULL) {
		ip->i_list = (struct inclist **)
			malloc(sizeof(struct inclist *) * ++ip->i_listlen);
		ip->i_merged = (boolean *)
			malloc(sizeof(boolean) * ip->i_listlen);
	} else {
		for (i=0; i<ip->i_listlen; i++)
			if (ip->i_list[ i ] == newfile) {
				i = strlen(newfile->i_file);
				if (!(ip->i_flags & INCLUDED_SYM) &&
				    !(i > 2 &&
				      newfile->i_file[i-1] == 'c' &&
				      newfile->i_file[i-2] == '.'))
				{
					/* only bitch if ip has */
					/* no #include SYMBOL lines  */
					/* and is not a .c file */
					if (warn_multiple)
					{
						warning("%s includes %s more than once!\n",
							ip->i_file, newfile->i_file);
						warning1("Already have\n");
						for (i=0; i<ip->i_listlen; i++)
							warning1("\t%s\n", ip->i_list[i]->i_file);
					}
				}
				return;
			}
		ip->i_list = (struct inclist **) realloc(ip->i_list,
							 sizeof(struct inclist *) * ++ip->i_listlen);
		ip->i_merged = (boolean *)
			realloc(ip->i_merged, sizeof(boolean) * ip->i_listlen);
	}
	ip->i_list[ ip->i_listlen-1 ] = newfile;
	ip->i_merged[ ip->i_listlen-1 ] = FALSE;
}

void
inc_clean (void)
{
	register struct inclist *ip;

	for (ip = inclist; ip < inclistp; ip++) {
		ip->i_flags &= ~MARKED;
	}
}

struct inclist *
inc_path(char *file, char *include, int type)
{
	static char		path[ BUFSIZ ];
	register char		**pp, *p;
	register struct inclist	*ip;
	struct stat		st;

	/*
	 * Check all previously found include files for a path that
	 * has already been expanded.
	 */
	if ((type == INCLUDE) || (type == INCLUDEDOT))
		inclistnext = inclist;
	ip = inclistnext;

	for (; ip->i_file; ip++) {
		if ((strcmp(ip->i_incstring, include) == 0) &&
		    !(ip->i_flags & INCLUDED_SYM)) {
			inclistnext = ip + 1;
			return ip;
		}
	}

	if (inclistnext == inclist) {
		/*
		 * If the path was surrounded by "" or is an absolute path,
		 * then check the exact path provided.
		 */
		if ((type == INCLUDEDOT) ||
		    (type == INCLUDENEXTDOT) ||
		    (*include == '/')
#ifdef WIN32
		    || (*include == '\\')
#endif
			) {
			if (stat(include, &st) == 0)
				return newinclude(include, include);
			if (show_where_not)
				warning1("\tnot in %s\n", include);
		}

		/*
		 * If the path was surrounded by "" see if this include file is
		 * in the directory of the file being parsed.
		 */
		if ((type == INCLUDEDOT) || (type == INCLUDENEXTDOT)) {
			for (p=file+strlen(file); p>file; p--)
				if (*p == '/'
#ifdef WIN32
				    || *p == '\\'
#endif
					)

					break;
			if (p == file) {
				strcpy(path, include);
			} else {
				strncpy(path, file, (p-file) + 1);
				path[ (p-file) + 1 ] = '\0';
				strcpy(path + (p-file) + 1, include);
			}
			remove_dotdot(path);
			if (stat(path, &st) == 0)
				return newinclude(path, include);
			if (show_where_not)
				warning1("\tnot in %s\n", path);
		}
	}

	/*
	 * Check the include directories specified.  Standard include dirs
	 * should be at the end.
	 */
	if ((type == INCLUDE) || (type == INCLUDEDOT))
		includedirsnext = includedirs;
	pp = includedirsnext;

	for (; *pp; pp++) {
		sprintf(path, "%s%c%s", *pp, path_delim, include);
		remove_dotdot(path);
		if (stat(path, &st) == 0) {
			includedirsnext = pp + 1;
			return newinclude(path, include);
		}
		if (show_where_not)
			warning1("\tnot in %s\n", path);
	}

	return NULL;
}
