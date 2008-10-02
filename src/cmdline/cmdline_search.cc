// cmdline_search.cc
//
//   Copyright 2004 Daniel Burrows

#include "cmdline_search.h"

#include "cmdline_common.h"
#include "cmdline_util.h"

#include <aptitude.h>
#include <load_sortpolicy.h>
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

using namespace std;
namespace cw = cwidget;
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

// compare...uses the candidate version of each package.
class compare
{
  pkg_sortpolicy *s;
public:
  compare(pkg_sortpolicy *_s):s(_s) {}

  bool operator()(const pair<pkgCache::PkgIterator, ref_ptr<structural_match> > &a,
		  const pair<pkgCache::PkgIterator, ref_ptr<structural_match> > &b)
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

// FIXME: apt-cache does lots of tricks to make this fast.  Should I?
int cmdline_search(int argc, char *argv[], const char *status_fname,
		   string display_format, string width, string sort,
		   bool disable_columns, bool debug)
{
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

  column_definition_list *columns =
    parse_columns(wdisplay_format,
		  pkg_item::pkg_columnizer::parse_column_type,
		  pkg_item::pkg_columnizer::defaults);

  if(!columns)
    {
      _error->DumpErrors();
      return -1;
    }

  if(argc<=1)
    {
      fprintf(stderr, _("search: You must provide at least one search term\n"));
      delete columns;
      return -1;
    }

  OpProgress progress;

  apt_init(&progress, true, status_fname);

  if(_error->PendingError())
    {
      _error->DumpErrors();
      delete columns;
      return -1;
    }

  vector<ref_ptr<pattern> > matchers;

  for(int i=1; i<argc; ++i)
    {
      ref_ptr<pattern> m = parse(argv[i]);
      if(!m.valid())
	{
	  _error->DumpErrors();

	  delete columns;
	  return -1;
	}

      matchers.push_back(m);
    }

  vector<pair<pkgCache::PkgIterator, ref_ptr<structural_match> > > output;
  ref_ptr<search_cache> search_info(search_cache::create());
  for(vector<ref_ptr<pattern> >::iterator m=matchers.begin();
      m!=matchers.end(); ++m)
    {
      // Q: should I just wrap an ?or around them all?
      aptitude::matching::search(*m,
				 search_info,
				 output,
				 *apt_cache_file,
				 *apt_package_records,
				 debug);
    }

  std::sort(output.begin(), output.end(), compare(s));
  output.erase(std::unique(output.begin(), output.end(), result_equality(s)),
	       output.end());

  for(vector<pair<pkgCache::PkgIterator, ref_ptr<structural_match> > >::iterator i=output.begin();
      i!=output.end(); ++i)
    {
      column_parameters *p =
	new search_result_parameters(i->second);
      pkg_item::pkg_columnizer columnizer(i->first,
					  i->first.VersionList(),
					  *columns,
					  0);
      if(disable_columns)
	printf("%ls\n", aptitude::cmdline::de_columnize(*columns, columnizer, *p).c_str());
      else
	printf("%ls\n",
	       columnizer.layout_columns(real_width==-1?screen_width:real_width,
					 *p).c_str());

      // Note that this deletes the whole result, so we can't re-use
      // the list.
      delete p;
    }

  delete columns;

  return 0;
}
