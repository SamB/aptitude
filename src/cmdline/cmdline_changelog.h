// cmdline_changelog.h                        -*-c++-*-
//
//   Copyright 2004 Daniel Burrows

#ifndef CMDLINE_CHANGELOG_H
#define CMDLINE_CHANGELOG_H

#include <string>
#include <vector>

/** \brief Display the changelog of each of the given package specifiers.
 *
 *  The specifiers are literal package names, with optional version/archive
 *  descriptors.  DumpErrors() is called after each changelog is displayed.
 */
bool do_cmdline_changelog(const std::vector<std::string> &packages);

int cmdline_changelog(int argc, char *argv[]);

#endif // CMDLINE_CHANGELOG_H
