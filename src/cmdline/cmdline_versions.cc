// cmdline_versions.cc
//
// Copyright (C) 2010 Daniel Burrows
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

#include "cmdline_versions.h"

#include "cmdline_util.h"

#include <aptitude.h>
#include <pkg_ver_item.h>
#include <load_sortpolicy.h>

#include <cwidget/generic/util/ref_ptr.h>

#include <generic/apt/matching/parse.h>

#include <boost/make_shared.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/unordered_map.hpp>

#include <vector>

#include <apt-pkg/error.h>

namespace cw = cwidget;
namespace m = aptitude::matching;

using aptitude::cmdline::package_results_lt;
using aptitude::cmdline::search_result_column_parameters;
using aptitude::cmdline::version_results_eq;
using aptitude::cmdline::version_results_lt;

namespace
{
  // Print the matches against a group of versions.
  void show_version_match_list(const std::vector<std::pair<pkgCache::VerIterator, cw::util::ref_ptr<m::structural_match> > > &output,
                               const cw::config::column_definition_list &columns,
                               int format_width,
                               bool disable_columns)
  {
    for(std::vector<std::pair<pkgCache::VerIterator, cw::util::ref_ptr<m::structural_match> > >::const_iterator it = output.begin();
        it != output.end(); ++it)
      {
        boost::scoped_ptr<cw::config::column_parameters> p(new search_result_column_parameters(it->second));
        pkg_ver_columnizer columnizer(it->first,
                                      false, // Could set this to true to show the package's name.
                                      columns,
                                      0);
        if(disable_columns)
          printf("%ls\n", aptitude::cmdline::de_columnize(columns, columnizer, *p).c_str());
        else
          printf("%ls\n",
                 columnizer.layout_columns(format_width == -1 ? screen_width : format_width,
                                           *p).c_str());
      }
  }

  int do_search_versions(const std::vector<cw::util::ref_ptr<m::pattern> > &patterns,
                         pkg_sortpolicy *sort_policy,
                         const cw::config::column_definition_list &columns,
                         int format_width,
                         bool disable_columns,
                         group_by_package_option group_by_package,
                         bool debug)
  {
    typedef std::vector<std::pair<pkgCache::VerIterator, cw::util::ref_ptr<m::structural_match> > >
      results_list;

    results_list output;
    cw::util::ref_ptr<m::search_cache> search_info(m::search_cache::create());
    for(std::vector<cw::util::ref_ptr<m::pattern> >::const_iterator pIt = patterns.begin();
        pIt != patterns.end(); ++pIt)
      {
        // Q: should I just wrap an ?or around them all?
        aptitude::matching::search_versions(*pIt,
                                            search_info,
                                            output,
                                            *apt_cache_file,
                                            *apt_package_records,
                                            debug);
      }

    const bool do_group_by_package =
      group_by_package == group_by_package_always ||
      (group_by_package == group_by_package_auto &&
       !(patterns.size() == 1 &&
         patterns[0]->get_type() == m::pattern::exact_name));

    _error->DumpErrors();

    // NB: sort the big list of results by version first so that we
    // don't have to sort lots of little lists later.  The code below
    // very carefully builds a list of the versions of each package in
    // a stable way, so the versions will continue to be in order.
    std::sort(output.begin(), output.end(), version_results_lt(sort_policy));
    output.erase(std::unique(output.begin(), output.end(), version_results_eq(sort_policy)),
                 output.end());

    if(do_group_by_package)
      {
        typedef boost::unordered_map<pkgCache::PkgIterator, boost::shared_ptr<results_list>,
                                     aptitude::cmdline::hash_pkgiterator>
          results_by_package_map;

        results_by_package_map by_packages;

        for(results_list::const_iterator it = output.begin();
            it != output.end(); ++it)
          {
            pkgCache::VerIterator ver = it->first;
            pkgCache::PkgIterator pkg = ver.ParentPkg();

            results_by_package_map::iterator found = by_packages.find(pkg);
            if(found == by_packages.end())
              {
                boost::shared_ptr<results_list> cell = boost::make_shared<results_list>();

                by_packages[pkg] = cell;
                cell->push_back(*it);
              }
            else
              found->second->push_back(*it);
          }

        typedef std::vector<std::pair<pkgCache::PkgIterator, boost::shared_ptr<results_list> > >
          results_by_package_list;

        results_by_package_list by_packages_list(by_packages.begin(), by_packages.end());
        std::sort(by_packages_list.begin(), by_packages_list.end(),
                  package_results_lt(sort_policy));

        for(results_by_package_list::const_iterator it = by_packages_list.begin();
            it != by_packages_list.end(); ++it)
          {
            if(it != by_packages_list.begin())
              printf("\n");
            printf("Package %s:\n", it->first.Name());
            // No need to sort the versions in this list since we
            // sorted them above.
            show_version_match_list(*it->second, columns, format_width, disable_columns);
          }
      }
    else
      show_version_match_list(output, columns, format_width, disable_columns);

    return 0;
  }
}

int cmdline_versions(int argc, char *argv[], const char *status_fname,
                     std::string display_format, std::string width,
                     std::string sort, bool disable_columns, bool debug,
                     group_by_package_option group_by_package)
{
  int real_width=-1;

  pkg_item::pkg_columnizer::setup_columns();

  pkg_sortpolicy *sort_policy = parse_sortpolicy(sort);

  if(!sort_policy)
    {
      _error->DumpErrors();
      return -1;
    }

  _error->DumpErrors();

  if(!width.empty())
    {
      unsigned long tmp = screen_width;
      StrToNum(width.c_str(), tmp, width.size());
      real_width = tmp;
    }

  std::wstring wdisplay_format;

  if(!cw::util::transcode(display_format.c_str(), wdisplay_format))
    {
      _error->DumpErrors();
      fprintf(stderr, _("iconv of %s failed.\n"), display_format.c_str());
      return -1;
    }

  boost::scoped_ptr<cw::config::column_definition_list> columns;
  columns.reset(parse_columns(wdisplay_format,
                              pkg_item::pkg_columnizer::parse_column_type,
                              pkg_item::pkg_columnizer::defaults));

  if(columns.get() == NULL)
    {
      _error->DumpErrors();
      return -1;
    }

  if(argc <= 1)
    {
      fprintf(stderr, _("versions: You must provide at least one package selector\n"));
      return -1;
    }

  OpProgress progress;

  apt_init(&progress, true, status_fname);

  if(_error->PendingError())
    {
      _error->DumpErrors();
      return -1;
    }

  std::vector<cw::util::ref_ptr<m::pattern> > matchers;

  for(int i = 1; i < argc; ++i)
    {
      const char * const arg = argv[i];
      const bool treat_as_exact_name =
        !aptitude::matching::is_pattern(arg);

      if(treat_as_exact_name)
        matchers.push_back(m::pattern::make_exact_name(arg));
      else
        {
          cw::util::ref_ptr<m::pattern> m = m::parse(arg);
          if(!m.valid())
            {
              _error->DumpErrors();

              return -1;
            }

          matchers.push_back(m);
        }
    }

  return do_search_versions(matchers,
                            sort_policy,
                            *columns,
                            real_width,
                            disable_columns,
                            group_by_package,
                            debug);
}
