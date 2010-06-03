// cmdline_search.cc
//
//
// Copyright (C) 2004, 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include "cmdline_search.h"

#include "cmdline_common.h"
#include "cmdline_util.h"
#include "terminal.h"

#include <aptitude.h>
#include <load_sortpolicy.h>
#include <loggers.h>
#include <pkg_columnizer.h>
#include <pkg_item.h>
#include <pkg_sortpolicy.h>

#include <generic/apt/apt.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>

#include <cwidget/config/column_definition.h>
#include <cwidget/generic/util/transcode.h>

#include <apt-pkg/error.h>
#include <apt-pkg/strutl.h>

#include <algorithm>

#include <boost/scoped_ptr.hpp>

using namespace std;
namespace cw = cwidget;
using aptitude::Loggers;
using aptitude::cmdline::create_terminal;
using aptitude::cmdline::terminal;
using boost::shared_ptr;
using cwidget::util::ref_ptr;
using cwidget::util::transcode;
using namespace aptitude::matching;
using namespace cwidget::config;

namespace
{
  int do_search_packages(const std::vector<ref_ptr<pattern> > &patterns,
                         pkg_sortpolicy *sort_policy,
                         const column_definition_list &columns,
                         int format_width,
                         const unsigned int screen_width,
                         bool disable_columns,
                         bool debug)
  {
    typedef std::vector<std::pair<pkgCache::PkgIterator, ref_ptr<structural_match> > >
      results_list;

    results_list output;
    ref_ptr<search_cache> search_info(search_cache::create());
    for(std::vector<ref_ptr<pattern> >::const_iterator pIt = patterns.begin();
        pIt != patterns.end(); ++pIt)
      {
        // Q: should I just wrap an ?or around them all?
        aptitude::matching::search(*pIt,
                                   search_info,
                                   output,
                                   *apt_cache_file,
                                   *apt_package_records,
                                   debug);
      }

    _error->DumpErrors();

    std::sort(output.begin(), output.end(),
              aptitude::cmdline::package_results_lt(sort_policy));
    output.erase(std::unique(output.begin(), output.end(),
                             aptitude::cmdline::package_results_eq(sort_policy)),
                 output.end());

    for(results_list::const_iterator it = output.begin(); it != output.end(); ++it)
      {
        column_parameters *p =
          new aptitude::cmdline::search_result_column_parameters(it->second);
        pkg_item::pkg_columnizer columnizer(it->first,
                                            it->first.VersionList(),
                                            columns,
                                            0);
        if(disable_columns)
          printf("%ls\n", aptitude::cmdline::de_columnize(columns, columnizer, *p).c_str());
        else
          printf("%ls\n",
                 columnizer.layout_columns(format_width == -1 ? screen_width : format_width,
                                           *p).c_str());

        // Note that this deletes the whole result, so we can't re-use
        // the list.
        delete p;
      }

    return 0;
  }
}

// FIXME: apt-cache does lots of tricks to make this fast.  Should I?
int cmdline_search(int argc, char *argv[], const char *status_fname,
		   string display_format, string width, string sort,
		   bool disable_columns, bool debug)
{
  shared_ptr<terminal> term = create_terminal();

  int real_width=-1;

  pkg_item::pkg_columnizer::setup_columns();

  pkg_sortpolicy *s=parse_sortpolicy(sort);

  if(!s)
    {
      _error->DumpErrors();
      return -1;
    }

  _error->DumpErrors();

  const unsigned int screen_width = term->get_screen_width();
  if(!width.empty())
    {
      unsigned long tmp=screen_width;
      StrToNum(width.c_str(), tmp, width.size());
      real_width=tmp;
    }

  wstring wdisplay_format;

  if(!cw::util::transcode(display_format.c_str(), wdisplay_format))
    {
      _error->DumpErrors();
      fprintf(stderr, _("iconv of %s failed.\n"), display_format.c_str());
      return -1;
    }

  boost::scoped_ptr<column_definition_list> columns;
  columns.reset(parse_columns(wdisplay_format,
                              pkg_item::pkg_columnizer::parse_column_type,
                              pkg_item::pkg_columnizer::defaults));

  if(columns.get() == NULL)
    {
      _error->DumpErrors();
      return -1;
    }

  if(argc<=1)
    {
      fprintf(stderr, _("search: You must provide at least one search term\n"));
      return -1;
    }

  OpProgress progress;

  apt_init(&progress, true, status_fname);

  if(_error->PendingError())
    {
      _error->DumpErrors();
      return -1;
    }

  vector<ref_ptr<pattern> > matchers;

  for(int i=1; i<argc; ++i)
    {
      const char * const arg = argv[i];

      ref_ptr<pattern> m = parse(arg);
      if(!m.valid())
        {
          _error->DumpErrors();

          return -1;
        }

      matchers.push_back(m);
    }

  return do_search_packages(matchers,
                            s,
                            *columns,
                            real_width,
                            screen_width,
                            disable_columns,
                            debug);
}
