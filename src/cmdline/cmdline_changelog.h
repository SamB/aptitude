// cmdline_changelog.h                        -*-c++-*-
//
// Copyright (C) 2004, 2010 Daniel Burrows

#ifndef CMDLINE_CHANGELOG_H
#define CMDLINE_CHANGELOG_H

#include <boost/shared_ptr.hpp>

#include <string>
#include <vector>

/** \file cmdline_changelog.h
 */

namespace aptitude
{
  namespace cmdline
  {
    class terminal;
  }
}

/** \brief Display the changelog of each of the given package specifiers.
 *
 *  The specifiers are literal package names, with optional version/archive
 *  descriptors.  DumpErrors() is called after each changelog is displayed.
 */
void do_cmdline_changelog(const std::vector<std::string> &packages,
                          const boost::shared_ptr<aptitude::cmdline::terminal> &term);

int cmdline_changelog(int argc, char *argv[]);

#endif // CMDLINE_CHANGELOG_H
