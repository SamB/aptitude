// cmdline_check_resolver.cc
//
//   Copyright (C) 2005, 2007 Daniel Burrows

//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.

//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.

//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.
//
// Just print out the current resolver state (debugging tool)

#include "cmdline_check_resolver.h"

#include <generic/apt/aptitude_resolver_universe.h>
#include <generic/problemresolver/sanity_check_universe.h>

#include <apt-pkg/error.h>

using namespace std;

int cmdline_check_resolver(int argc, char *argv[],
			  const char *status_fname)
{
  _error->DumpErrors();

  OpProgress progress;

  apt_init(&progress, true, status_fname);

  if(_error->PendingError())
    {
      _error->DumpErrors();
      return -1;
    }

  aptitude_universe u(*apt_cache_file);

  sanity_check_universe(u);

  return 0;
}
