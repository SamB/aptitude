// cmdline_show.h                                      -*-c++-*-
//
//  Copyright 2004 Daniel Burrows

#ifndef CMDLINE_SHOW_H
#define CMDLINE_SHOW_H

#include <iosfwd>
#include <string>

namespace cwidget
{
  class fragment_contents;
}

/** Run the "show" operation on a single argument, presented as a string. */
bool do_cmdline_show(std::string s, int verbose);

/** The "show" user command. */
int cmdline_show(int argc, char *argv[], int verbose);

std::ostream &operator<<(std::ostream &out, const cwidget::fragment_contents &contents);

#endif // CMDLINE_SHOW_H
