// cmdline_search.h                         -*-c++-*-
//
//   Copyright 2004 Daniel Burrows

#ifndef CMDLINE_SEARCH_H
#define CMDLINE_SEARCH_H

#include <string>

/** \file cmdline_search.h
 */

int cmdline_search(int argc, char *argv[], const char *status_fname,
		   std::string display_format, std::string width, std::string sort);

#endif // CMDLINE_SEARCH_H
