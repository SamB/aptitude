// cmdline_search.cc
//
//   Copyright 2004 Daniel Burrows

#include "cmdline_search.h"

#include "cmdline_common.h"
#include "cmdline_util.h"

#include <aptitude.h>
#include <load_sortpolicy.h>
#include <loggers.h>
#include <pkg_columnizer.h>
#include <pkg_item.h>
#include <pkg_sortpolicy.h>
#include <pkg_ver_item.h>

#include <generic/apt/apt.h>
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>

#include <cwidget/config/column_definition.h>
#include <cwidget/generic/util/transcode.h>

#include <apt-pkg/error.h>
#include <apt-pkg/strutl.h>

#include <algorithm>

#include <boost/make_shared.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/unordered_map.hpp>

using namespace std;
namespace cw = cwidget;
using aptitude::Loggers;
using cwidget::util::ref_ptr;
using cwidget::util::transcode;
using namespace aptitude::matching;
using namespace cwidget::config;

/// \todo search_result_parameters currently acts as an empty list; it
/// should parse the match result and return the groups it contains.
class search_result_parameters : public column_parameters
{
  ref_ptr<structural_match> r;

public:
  search_result_parameters(const ref_ptr<structural_match> &_r)
    :r(_r)
  {
  }

  int param_count()
  {
    return 0;
  }

  wstring get_param(int n)
  {
    return wstring();
  }
};

/** Compare pairs whose first elements are package iterators,
 *  according to a sorting policy.
 *
 *  When a version is needed to do a comparison, I arbitrarily decided
 *  to use the candidate version.
 */
class package_results_lt
{
  pkg_sortpolicy *s;
public:
  package_results_lt(pkg_sortpolicy *_s):s(_s) {}

  template<typename T, typename U>
  bool operator()(const pair<pkgCache::PkgIterator, T> &a,
		  const pair<pkgCache::PkgIterator, U> &b)
  {
    pkgCache::VerIterator av=(*apt_cache_file)[a.first].CandidateVerIter(*apt_cache_file);
    pkgCache::VerIterator bv=(*apt_cache_file)[b.first].CandidateVerIter(*apt_cache_file);

    return s->compare(a.first, av, b.first, bv)<0;
  }
};

/** \brief operator== for match results. */
class result_equality
{
  pkg_sortpolicy *s;
public:
  result_equality(pkg_sortpolicy *_s):s(_s) {}

  bool operator()(const pair<pkgCache::PkgIterator, ref_ptr<structural_match> > &a,
		  const pair<pkgCache::PkgIterator, ref_ptr<structural_match> > &b)
  {
    pkgCache::VerIterator av =
      (*apt_cache_file)[a.first].CandidateVerIter(*apt_cache_file);
    pkgCache::VerIterator bv =
      (*apt_cache_file)[b.first].CandidateVerIter(*apt_cache_file);

    return s->compare(a.first, av, b.first, bv) == 0;
  }
};

/** \brief 3-way compare of version match results. */
class compare_version_results3
{
  pkg_sortpolicy *sortpolicy;

public:
  compare_version_results3(pkg_sortpolicy *_sortpolicy)
    : sortpolicy(_sortpolicy)
  {
  }

  int operator()(const pair<pkgCache::VerIterator, ref_ptr<structural_match> > &a,
                 const pair<pkgCache::VerIterator, ref_ptr<structural_match> > &b)
  {
    const pkgCache::VerIterator &av = a.first;
    const pkgCache::VerIterator &bv = b.first;

    return sortpolicy->compare(av.ParentPkg(), av, bv.ParentPkg(), bv);
  }
};

/** \brief less-than over version match results. */
class version_results_lt
{
  compare_version_results3 comparer;

public:
  version_results_lt(pkg_sortpolicy *sortpolicy)
    : comparer(sortpolicy)
  {
  }

  bool operator()(const pair<pkgCache::VerIterator, ref_ptr<structural_match> > &a,
                  const pair<pkgCache::VerIterator, ref_ptr<structural_match> > &b)
  {
    return comparer(a, b) < 0;
  }
};

/** \brief equality over version match results.
 *
 *  Returns \b true if two match results are equivalent under the
 *  given sort order.
 */
class version_results_eq
{
  compare_version_results3 comparer;

public:
  version_results_eq(pkg_sortpolicy *sortpolicy)
    : comparer(sortpolicy)
  {
  }

  bool operator()(const pair<pkgCache::VerIterator, ref_ptr<structural_match> > &a,
                  const pair<pkgCache::VerIterator, ref_ptr<structural_match> > &b)
  {
    return comparer(a, b) == 0;
  }
};

/** \brief Custom hash on packages. */
class hash_pkgiterator
{
public:
  std::size_t operator()(const pkgCache::PkgIterator &pkg) const
  {
    return pkg->ID;
  }
};

namespace
{
  int do_search_packages(const std::vector<ref_ptr<pattern> > &patterns,
                         pkg_sortpolicy *sort_policy,
                         const column_definition_list &columns,
                         int format_width,
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

    std::sort(output.begin(), output.end(), package_results_lt(sort_policy));
    output.erase(std::unique(output.begin(), output.end(), result_equality(sort_policy)),
                 output.end());

    for(results_list::const_iterator it = output.begin(); it != output.end(); ++it)
      {
        column_parameters *p =
          new search_result_parameters(it->second);
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

  // Print the matches against a group of versions.
  void show_version_match_list(const std::vector<std::pair<pkgCache::VerIterator, ref_ptr<structural_match> > > &output,
                               const column_definition_list &columns,
                               int format_width,
                               bool disable_columns)
  {
    for(std::vector<std::pair<pkgCache::VerIterator, ref_ptr<structural_match> > >::const_iterator it = output.begin();
        it != output.end(); ++it)
      {
        boost::scoped_ptr<column_parameters> p(new search_result_parameters(it->second));
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

  int do_search_versions(const std::vector<ref_ptr<pattern> > &patterns,
                         pkg_sortpolicy *sort_policy,
                         const column_definition_list &columns,
                         int format_width,
                         bool disable_columns,
                         group_by_package_option group_by_package,
                         bool debug)
  {
    typedef std::vector<std::pair<pkgCache::VerIterator, ref_ptr<structural_match> > >
      results_list;

    results_list output;
    ref_ptr<search_cache> search_info(search_cache::create());
    for(std::vector<ref_ptr<pattern> >::const_iterator pIt = patterns.begin();
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
         patterns[0]->get_type() == pattern::exact_name));

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
                                     hash_pkgiterator>
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

// FIXME: apt-cache does lots of tricks to make this fast.  Should I?
int cmdline_search(int argc, char *argv[], const char *status_fname,
		   string display_format, string width, string sort,
		   bool disable_columns, bool debug,
                   bool search_versions, group_by_package_option group_by_package)
{
  // This is essentially two routines folded into one to avoid having
  // to share a bunch of initialization.  This is a really bad idea,
  // but so is having a lot of identical initialization code in two
  // places.  A principled solution is needed, maybe extracting the
  // common initialization into common code.  Well, either that, or a
  // way of better parameterizing this routine on the search strategy.
  //
  // The cores of the two routines are do_version_search() and
  // do_package_search() above.


  int real_width=-1;

  pkg_item::pkg_columnizer::setup_columns();

  pkg_sortpolicy *s=parse_sortpolicy(sort);

  if(!s)
    {
      _error->DumpErrors();
      return -1;
    }

  _error->DumpErrors();

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

      // If we are in "versions" mode, bare strings are treated as
      // package names to find.  \todo Check the name up-front to give
      // nicer error messages if it doesn't exist.
      const bool treat_as_exact_name =
        search_versions && !aptitude::matching::is_pattern(arg);

      if(treat_as_exact_name)
        matchers.push_back(aptitude::matching::pattern::make_exact_name(arg));
      else
        {
          ref_ptr<pattern> m = parse(argv[i]);
          if(!m.valid())
            {
              _error->DumpErrors();

              return -1;
            }

          matchers.push_back(m);
        }
    }

  if(!search_versions)
    return do_search_packages(matchers,
                              s,
                              *columns,
                              real_width,
                              disable_columns,
                              debug);
  else
    return do_search_versions(matchers,
                              s,
                              *columns,
                              real_width,
                              disable_columns,
                              group_by_package,
                              debug);
}
