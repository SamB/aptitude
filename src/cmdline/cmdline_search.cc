// cmdline_search.cc
//
//   Copyright 2004 Daniel Burrows

#include "cmdline_search.h"

#include "cmdline_common.h"

#include <aptitude.h>
#include <load_sortpolicy.h>
#include <pkg_columnizer.h>
#include <pkg_item.h>
#include <pkg_sortpolicy.h>

#include <generic/apt/apt.h>
#include <generic/apt/matchers.h>

#include <vscreen/config/column_definition.h>
#include <vscreen/transcode.h>

#include <apt-pkg/error.h>
#include <apt-pkg/strutl.h>

#include <algorithm>

using namespace std;

class search_result_parameters : public column_parameters
{
  pkg_match_result *r;
public:
  search_result_parameters(pkg_match_result *_r)
    :r(_r)
  {
  }

  ~search_result_parameters()
  {
    delete r;
  }

  int param_count()
  {
    return r->num_groups();
  }

  wstring get_param(int n)
  {
    return transcode(r->group(n));
  }
};

// compare...uses the candidate version of each package.
class compare
{
  pkg_sortpolicy *s;
public:
  compare(pkg_sortpolicy *_s):s(_s) {}

  bool operator()(const pair<pkgCache::PkgIterator, pkg_match_result *> &a,
		  const pair<pkgCache::PkgIterator, pkg_match_result *> &b)
  {
    pkgCache::VerIterator av=(*apt_cache_file)[a.first].CandidateVerIter(*apt_cache_file);
    pkgCache::VerIterator bv=(*apt_cache_file)[b.first].CandidateVerIter(*apt_cache_file);

    return s->compare(a.first, av, b.first, bv)<0;
  }
};

// FIXME: apt-cache does lots of tricks to make this fast.  Should I?
int cmdline_search(int argc, char *argv[], const char *status_fname,
		   string display_format, string width, string sort)
{
  int real_width=-1;

  pkg_item::pkg_columnizer::setup_columns();

  pkg_sortpolicy *s=parse_sortpolicy(sort);

  if(!s)
    {
      _error->DumpErrors();
      return -1;
    }

  compare comp(s);

  _error->DumpErrors();

  if(!width.empty())
    {
      unsigned long tmp=screen_width;
      StrToNum(width.c_str(), tmp, width.size());
      real_width=tmp;
    }

  wstring wdisplay_format;

  if(!transcode(display_format.c_str(), wdisplay_format))
    {
      _error->DumpErrors();
      fprintf(stderr, _("iconv of %s failed.\n"), display_format.c_str());
      return -1;
    }

  column_definition_list *columns=parse_columns(wdisplay_format,
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

  vector<pkg_matcher *> matchers;

  for(int i=1; i<argc; ++i)
    {
      pkg_matcher *m=parse_pattern(argv[i]);
      if(!m)
	{
	  while(!matchers.empty())
	    {
	      delete matchers.back();
	      matchers.pop_back();
	    }

	  _error->DumpErrors();

	  delete columns;
	  return -1;
	}

      matchers.push_back(m);
    }

  vector<pair<pkgCache::PkgIterator, pkg_match_result *> > output;
  for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin();
      !pkg.end(); ++pkg)
    {
      // Ignore packages that exist only due to dependencies.
      if(pkg.VersionList().end() && pkg.ProvidesList().end())
	continue;

      for(vector<pkg_matcher *>::iterator m=matchers.begin();
	  m!=matchers.end(); ++m)
	{
	  pkg_match_result *r=(*m)->get_match(pkg);

	  if(r != NULL)
	    output.push_back(pair<pkgCache::PkgIterator, pkg_match_result *>(pkg, r));
	}
    }

  std::sort(output.begin(), output.end(), comp);

  for(vector<pair<pkgCache::PkgIterator, pkg_match_result *> >::iterator i=output.begin();
      i!=output.end(); ++i)
    {
      column_parameters *p=new search_result_parameters(i->second);

      printf("%ls\n",
	     pkg_item::pkg_columnizer(i->first,
				      i->first.VersionList(),
				      *columns,
				      0).layout_columns(real_width==-1?screen_width:real_width,
							*p).c_str());

      // Note that this deletes the whole result, so we can't re-use
      // the list.
      delete p;
    }

  delete columns;

  return 0;
}
