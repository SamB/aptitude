// cmdline_search.h                         -*-c++-*-
//
//   Copyright 2004 Daniel Burrows

#ifndef CMDLINE_SEARCH_H
#define CMDLINE_SEARCH_H

#include <string>

/** \file cmdline_search.h
 */

int cmdline_search(int argc, char *argv[], const char *status_fname,
		   std::string display_format, std::string width, std::string sort,
		   bool disable_columns, bool debug,
                   bool search_versions, bool group_by_package);

#endif // CMDLINE_SEARCH_H
