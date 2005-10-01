// cmdline_changelog.h                        -*-c++-*-
//
//   Copyright 2004 Daniel Burrows

#ifndef CMDLINE_CHANGELOG_H
#define CMDLINE_CHANGELOG_H

#include <string>
#include <vector>

bool do_cmdline_changelog(const std::vector<std::string> &packages);

int cmdline_changelog(int argc, char *argv[]);

#endif // CMDLINE_CHANGELOG_H
