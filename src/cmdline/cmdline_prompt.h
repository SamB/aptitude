// cmdline-prompt.h                     -*-c++-*-
//
//  Copyright 2004 Daniel Burrows

#ifndef CMDLINE_PROMPT_H
#define CMDLINE_PROMPT_H

#include "cmdline_common.h"

#include <cwidget/generic/util/exception.h>

/** Thrown when we get EOF on stdin.  Should never be thrown
 *  to the cwidget::toplevel.
 */
class StdinEOFException : public cwidget::util::Exception
{
public:
  std::string errmsg() const;
};

/** The main preview-prompt-adjust-preview loop for the command-line
 *  interface.  Displays a preview of what will happen, then allows
 *  the user to confirm, cancel, or perform a number of other actions.
 *  to_install and friends are meant to be the sets the user
 *  explicitly selected (so the prompt can be displayed only when
 *  extra stuff is added or removed).
 *
 *  \param as_upgrade currently ignored; meant to control how the
 *                    preview and prompting are handled.
 *  \param to_install a set of packages to install.
 *  \param to_hold a set of packages to hold.
 *  \param to_remove a set of packages to remove.
 *  \param to_purge a set of packages to purge.
 *  \param showvers \b true to show version information in the preview.
 *  \param showdeps \b true to show dependency information in the preview.
 *  \param showsize \b true to show size information in the preview.
 *  \param verbose the current verbosity level
 *  \param assume_yes if \b true, assume the user entered "yes"
 *                    at the prompt.
 *  \param force_no_change if \b true, try extra-hard to preserve
 *                         the user's explicit requests (as
 *                         specified in to_install et al)
 *
 *  \throws StdinEOFException
 */
bool cmdline_do_prompt(bool as_upgrade,
		       pkgset &to_install,
		       pkgset &to_hold,
		       pkgset &to_remove,
		       pkgset &to_purge,
		       bool showvers,
		       bool showdeps,
		       bool showsize,
		       bool always_prompt,
		       int verbose,
		       bool assume_yes,
		       bool force_no_change);

/** Prompt for a single line of input from the user.
 *
 *  \param prompt a message to display before reading input.
 *  \return the text the user entered
 *
 *  \throws StdinEOFException
 */
string prompt_string(const string &prompt);


#endif // CMDLINE_PROMPT_H
