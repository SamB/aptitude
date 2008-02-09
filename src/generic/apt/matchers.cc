// matchers.cc
//
//  Copyright 2000-2005, 2007-2008 Daniel Burrows
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; see the file COPYING.  If not, write to
//  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//  Boston, MA 02111-1307, USA.
//
//  Grammer for the condition language.  (TODO: this is what the
//  grammar *will* be, not what it is)
//
//  CONDITION := CONDITION-LIST
//  CONDITION-LIST := CONDITION-AND-GROUP '|' CONDITION-LIST
//                 |  CONDITION-AND-GROUP
//  CONDITION-AND-GROUP := CONDITION-ATOM CONDITION-AND-GROUP
//                      := CONDITION-ATOM
//  CONDITION-ATOM := '(' CONDITION-LIST ')'
//                 |  '!' CONDITION-ATOM
//                 |  '?' condition-name '(' arguments... ')'
//                 |  '~'field-id <string>
//                 |  <string>
//
// The (arguments...) to a ?function-style matcher are parsed
// according to their expected type.  This is unfortunate but
// necessary: since arbitrary strings not containing metacharacters
// are legal condition values, distinguishing conditions from other
// argument types would require the user to type extra punctuation in,
// e.g., ?broken(Depends, ?name(apt.*)).

#include "matchers.h"

#include "apt.h"
#include "aptcache.h"
#include "tags.h"
#include "tasks.h"

#include <aptitude.h>

#include <generic/util/util.h>

#include <cwidget/generic/util/ssprintf.h>
#include <cwidget/generic/util/transcode.h>

#include <set>

#include <stdarg.h>

#include <apt-pkg/error.h>
#include <apt-pkg/pkgcache.h>
#include <apt-pkg/pkgrecords.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>

#include <cwidget/generic/util/eassert.h>
#include <ctype.h>
#include <stdio.h>
#include <regex.h>
#include <sys/types.h>

using namespace std;
namespace cw = cwidget;

pkg_matcher *parse_atom(string::const_iterator &start,
			const string::const_iterator &end,
			const vector<const char *> &terminators,
			bool search_descriptions);

pkg_matcher *parse_condition_list(string::const_iterator &start,
				  const string::const_iterator &end,
				  const vector<const char *> &terminators,
				  bool search_descriptions);

/** Used to cleanly abort without having to contort the code. */
class CompilationException
{
  string reason;
public:
  CompilationException(const char *format,
		       ...)
#ifdef __GNUG__
    __attribute__ ((format (printf, 2, 3)))
#endif
  {
    va_list args;

    va_start(args, format);

    reason = cw::util::vssprintf(format, args);
  }

  const string &get_msg() {return reason;}
};

namespace
{
  /** \brief Enumeration containing the known types of
   *  matchers.
   *
   *  I want to have a table mapping matcher names to matcher types,
   *  and so the matcher type has to be POD.  Well, I could use some
   *  indirection pattern like grouping policies do, but IMNSHO the
   *  payoff in grouping-policy land has not made up for the syntactic
   *  clutter and semantic overhead.  I think that if anything it
   *  would be less valuable here.
   *
   *  Note that matchers for dependencies and for broken dependencies
   *  are parsed separately below.
   */
  enum matcher_type
    {
      matcher_type_action,
      matcher_type_and,
      matcher_type_archive,
      matcher_type_automatic,
      matcher_type_broken,
      matcher_type_configfiles,
      matcher_type_description,
      matcher_type_essential,
      matcher_type_false,
      matcher_type_garbage,
      matcher_type_installed,
      matcher_type_maintainer,
      matcher_type_narrow,
      matcher_type_name,
      matcher_type_new,
      matcher_type_not,
      matcher_type_obsolete,
      matcher_type_or,
      matcher_type_origin,
      matcher_type_provides,
      matcher_type_section,
      matcher_type_tag,
      matcher_type_true,
      matcher_type_version,
      matcher_type_widen
    };

  struct matcher_info
  {
    /** \brief The string used to pick the matcher.
     */
    const char *name;

    /** \brief The matcher type indicated by this struct. */
    matcher_type type;
  };

  // ForTranslators: these strings can be translated to allow your
  // users to specify match terms in their native tongue.  However,
  // the English words are the canonical names for match terms and
  // will always be accepted *in preference to* any translation, in
  // order to ensure that scripts and user instructions can use the
  // English names without fear that the meanings will change with the
  // locale.
  //
  // That is: if you translate "false" to "description", "description"
  // would still produce a description matcher, not a false matcher,
  // because the English meaning takes precedence.
  const matcher_info matcher_types[] =
  {
    { N_("Matcher Type|action"), matcher_type_action },
    { N_("Matcher Type|and"), matcher_type_and },
    { N_("Matcher Type|archive"), matcher_type_archive },
    { N_("Matcher Type|automatic"), matcher_type_automatic },
    { N_("Matcher Type|broken"), matcher_type_broken },
    { N_("Matcher Type|configfiles"), matcher_type_configfiles },
    { N_("Matcher Type|description"), matcher_type_description },
    { N_("Matcher Type|essential"), matcher_type_essential },
    { N_("Matcher Type|false"), matcher_type_false },
    { N_("Matcher Type|garbage"), matcher_type_garbage },
    { N_("Matcher Type|installed"), matcher_type_installed },
    { N_("Matcher Type|maintainer"), matcher_type_maintainer },
    { N_("Matcher Type|name"), matcher_type_name },
    { N_("Matcher Type|narrow"), matcher_type_narrow },
    { N_("Matcher Type|new"), matcher_type_new },
    { N_("Matcher Type|not"), matcher_type_not },
    { N_("Matcher Type|obsolete"), matcher_type_obsolete },
    { N_("Matcher Type|or"), matcher_type_or },
    { N_("Matcher Type|origin"), matcher_type_origin },
    { N_("Matcher Type|provides"), matcher_type_provides },
    { N_("Matcher Type|section"), matcher_type_section },
    { N_("Matcher Type|tag"), matcher_type_tag },
    { N_("Matcher Type|true"), matcher_type_true },
    { N_("Matcher Type|version"), matcher_type_version },
    { N_("Matcher Type|widen"), matcher_type_widen }
  };
}

pkg_match_result::~pkg_match_result()
{
}

bool pkg_matcher::matches(const pkgCache::PkgIterator &pkg,
			  aptitudeDepCache &cache,
			  pkgRecords &records)
{
  for(pkgCache::VerIterator v = pkg.VersionList();
      !v.end(); ++v)
    if(matches(pkg, v, cache, records))
      return true;

  if(pkg.VersionList().end())
    return matches(pkg, pkgCache::VerIterator(cache, 0),
		   cache, records);
  else
    return false;
}

pkg_match_result *pkg_matcher::get_match(const pkgCache::PkgIterator &pkg,
					 aptitudeDepCache &cache,
					 pkgRecords &records)
{
  pkg_match_result *rval = NULL;

  for(pkgCache::VerIterator v = pkg.VersionList();
      rval == NULL && !v.end(); ++v)
    rval = get_match(pkg, v, cache, records);

  if(pkg.VersionList().end())
    rval = get_match(pkg, pkgCache::VerIterator(cache, 0),
		     cache, records);

  return rval;
}

pkg_matcher::~pkg_matcher()
{
}

/** A common class to use when there's no interesting result.  This is
 *  distinct from a match failure: an example would be the
 *  auto_matcher.
 */
class empty_match_result : public pkg_match_result
{
public:
  unsigned int num_groups() {return 0;}

  const string &group(unsigned int n) {abort();}
};

class pkg_nonstring_matcher : public pkg_matcher
{
public:
  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    return matches(pkg, ver, cache, records) ? new empty_match_result : NULL;
  }
};

class unitary_result : public pkg_match_result
{
  string s;
public:
  unitary_result(const string &_s):s(_s) {}

  unsigned int num_groups() {return 1;}

  const string &group(unsigned int n)
  {
    if(n != 0)
      abort();

    return s;
  }
};

class result_pair : public pkg_match_result
{
  pkg_match_result *r1, *r2;
public:
  result_pair(pkg_match_result *_r1,
	      pkg_match_result *_r2)
    :r1(_r1), r2(_r2)
  {
  }

  ~result_pair()
  {
    delete r1;
    delete r2;
  }

  unsigned int num_groups()
  {
    return r1->num_groups() + r2->num_groups();
  }

  const string &group(unsigned int n)
  {
    unsigned int num_groups_r1 = r1->num_groups();

    if(n < num_groups_r1)
      return r1->group(n);
    else
      return r2->group(n - num_groups_r1);
  }
};

class pkg_string_matcher : public pkg_matcher
{
  regex_t pattern_nogroup;
  regex_t pattern_group;
  bool pattern_nogroup_initialized:1;
  bool pattern_group_initialized:1;

  pkg_string_matcher()
  {
  }

  void do_compile(const string &_pattern,
		  regex_t &pattern,
		  int cflags)
  {
    int err=regcomp(&pattern, _pattern.c_str(), cflags);
    if(err!=0)
      {
	size_t needed=regerror(err, &pattern, NULL, 0);

	auto_ptr<char> buf(new char[needed+1]);

	regerror(err, &pattern, buf.get(), needed+1);

	throw CompilationException("Regex compilation error: %s", buf.get());
      }
  }

  void compile(const string &_pattern)
  {
    do_compile(_pattern, pattern_nogroup, REG_ICASE|REG_EXTENDED|REG_NOSUB);
    pattern_nogroup_initialized=true;

    do_compile(_pattern, pattern_group, REG_ICASE|REG_EXTENDED);
    pattern_group_initialized=true;
  }

  class match_result : public pkg_match_result
  {
    // well, it's pretty much necessary to copy all the groups anyway
    // :(
    vector<string> matches;
  public:
    match_result(const char *s, regmatch_t *pmatch, int matches_len)
    {
      for(int i=0; i<matches_len && pmatch[i].rm_so>=0; ++i)
	{
	  matches.push_back(string());
	  matches.back().assign(s, pmatch[i].rm_so,
				pmatch[i].rm_eo-pmatch[i].rm_so);
	}
    }

    unsigned int num_groups() {return matches.size();}
    const string &group(unsigned int n) {return matches[n];}
  };
public:
  pkg_string_matcher (const string &_pattern)
    :pattern_nogroup_initialized(false),
     pattern_group_initialized(false)
  {
    // By convention, empty patterns match anything. (anything, you
    // hear me??)  That allows you to put "~m" into the pattern
    // grouping policy and get a by-maintainer grouping out.
    if(_pattern.empty())
      compile(".*");
    else
      compile(_pattern);
  }

  ~pkg_string_matcher()
  {
    if(pattern_nogroup_initialized)
      regfree(&pattern_nogroup);
    if(pattern_group_initialized)
      regfree(&pattern_group);
  }

  bool string_matches(const char *s)
  {
    return !regexec(&pattern_nogroup, s, 0, NULL, 0);
  }

  pkg_match_result *get_string_match(const char *s)
  {
    // ew.  You need a hard limit here.
    regmatch_t matches[30];

    bool matched = (regexec(&pattern_group, s,
			    sizeof(matches)/sizeof(regmatch_t),
			    matches, 0) == 0);

    if(!matched)
      return NULL;
    else
      return new match_result(s, matches,
			      sizeof(matches)/sizeof(regmatch_t));
  }
};

typedef pair<bool, std::string> match_target;

class pkg_trivial_string_matcher : public pkg_string_matcher
{
public:
  pkg_trivial_string_matcher (const string &s) : pkg_string_matcher(s)
  {
  }

  // If the first element is false, the match fails; otherwise, it
  // proceeds using the second element.
  virtual match_target val(const pkgCache::PkgIterator &pkg,
			   const pkgCache::VerIterator &ver,
			   aptitudeDepCache &cache,
			   pkgRecords &records)=0;

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    match_target v = val(pkg, ver, cache, records);

    if(!v.first)
      return false;
    else
      return string_matches(v.second.c_str());
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    match_target v = val(pkg, ver, cache, records);

    if(!v.first)
      return NULL;
    else
      return get_string_match(v.second.c_str());
  }
};

class pkg_name_matcher:public pkg_trivial_string_matcher
{
public:
  pkg_name_matcher(const string &s):pkg_trivial_string_matcher(s) {}

  match_target val(const pkgCache::PkgIterator &pkg,
		   const pkgCache::VerIterator &ver,
		   aptitudeDepCache &cache,
		   pkgRecords &records)
  {
    return match_target(true, pkg.Name());
  }
};

class pkg_description_matcher:public pkg_trivial_string_matcher
{
public:
  pkg_description_matcher(const string &s):pkg_trivial_string_matcher(s) {}

  match_target val(const pkgCache::PkgIterator &pkg,
		   const pkgCache::VerIterator &ver,
		   aptitudeDepCache &cache,
		   pkgRecords &records)
  {
    if(ver.end())
      return match_target(false, "");
    else
      return match_target(true, cw::util::transcode(get_long_description(ver, &records).c_str()));
  }
};

class pkg_maintainer_matcher : public pkg_trivial_string_matcher
{
public:
  pkg_maintainer_matcher(const string &s):pkg_trivial_string_matcher(s)
  {
  }

  match_target val(const pkgCache::PkgIterator &pkg,
		   const pkgCache::VerIterator &ver,
		   aptitudeDepCache &cache,
		   pkgRecords &records)
  {
    if(ver.end())
      return match_target(false, "");
    else
      return match_target(true, records.Lookup(ver.FileList()).Maintainer().c_str());
  }
};

class pkg_section_matcher : public pkg_trivial_string_matcher
{
public:
  pkg_section_matcher(const string &s):pkg_trivial_string_matcher(s)
  {
  }

  match_target val(const pkgCache::PkgIterator &pkg,
		   const pkgCache::VerIterator &ver,
		   aptitudeDepCache &cache,
		   pkgRecords &records)
  {
    if(ver.end())
      return match_target(false, "");

    const char *s=ver.Section();

    if(!s)
      return match_target(false, "");

    return match_target(true, s);
  }
};

class pkg_version_matcher : public pkg_trivial_string_matcher
{
public:
  pkg_version_matcher(const string &s) : pkg_trivial_string_matcher(s)
  {
  }

  match_target val(const pkgCache::PkgIterator &pkg,
		   const pkgCache::VerIterator &ver,
		   aptitudeDepCache &cache,
		   pkgRecords &records)
  {
    if(ver.end())
      return match_target(false, "");

    const char *s=ver.VerStr();

    if(!s)
      return match_target(false, "");

    return match_target(true, s);
  }
};

// NOTE: pkg_current_version_matcher, pkg_inst_version_matcher, and
// pkg_cand_version_matcher are all a bit inefficient since they loop
// over all versions when they only match one; if they become a
// performance problem (unlikely), you could (carefully!!) implement
// the version-agnostic match variants to speed things up.
class pkg_curr_version_matcher : public pkg_matcher
{
public:
  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return !ver.end() && ver == pkg.CurrentVer();
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(matches(pkg, ver, cache, records))
      return new unitary_result(ver.VerStr());
    else
      return NULL;
  }
};

class pkg_cand_version_matcher : public pkg_matcher
{
public:
  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return !ver.end() &&
      ver == cache[pkg].CandidateVerIter(cache);
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(matches(pkg, ver, cache, records))
      return new unitary_result(ver.VerStr());
    else
      return NULL;
  }
};

class pkg_inst_version_matcher : public pkg_matcher
{
public:
  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return !ver.end() &&
      ver == cache[pkg].InstVerIter(cache);
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(matches(pkg, ver, cache, records))
      return new unitary_result(ver.VerStr());
    else
      return NULL;
  }
};

pkg_matcher *make_package_version_matcher(const string &substr)
{
  if(substr == "CURRENT")
    return new pkg_curr_version_matcher;
  else if(substr == "TARGET")
    return new pkg_inst_version_matcher;
  else if(substr == "CANDIDATE")
    return new pkg_cand_version_matcher;
  else
    return new pkg_version_matcher(substr);
}

class pkg_task_matcher : public pkg_string_matcher
{
public:
  pkg_task_matcher(const string &s) : pkg_string_matcher(s)
  {
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &v,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    set<string> *l = get_tasks(pkg);

    if(!l)
      return false;

    for(set<string>::iterator i=l->begin();
	i!=l->end();
	++i)
      if(string_matches(i->c_str()))
	return true;

    return false;
  }

  // Uses the fact that the result returns NULL <=> nothing matched
  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    set<string> *l=get_tasks(pkg);

    if(!l)
      return NULL;

    for(set<string>::iterator i=l->begin();
	i!=l->end();
	++i)
      {
	pkg_match_result *r=get_string_match(i->c_str());

	if(r != NULL)
	  return r;
      }

    return NULL;
  }
};

class pkg_tag_matcher : public pkg_string_matcher
{
public:
  pkg_tag_matcher(const string &s)
    : pkg_string_matcher(s)
  {
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
#ifdef HAVE_EPT
    typedef ept::debtags::Tag tag;
    using aptitude::apt::get_tags;
#endif

#ifdef HAVE_EPT
    const std::set<tag> realTags(get_tags(pkg));
    const std::set<tag> * const tags(&realTags);
#else
    const std::set<tag> * const tags(get_tags(pkg));
#endif

    if(tags == NULL)
      return false;

    for(std::set<tag>::const_iterator i=tags->begin(); i!=tags->end(); ++i)
      {
#ifdef HAVE_EPT
	std::string name(i->fullname());
#else
	const std::string name = i->str().c_str();
#endif
	if(string_matches(name.c_str()))
	  return true;
      }

    return false;
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
#ifdef HAVE_EPT
    typedef ept::debtags::Tag tag;
    using aptitude::apt::get_tags;
#endif

#ifdef HAVE_EPT
    const set<tag> realTags(get_tags(pkg));
    const set<tag> * const tags(&realTags);
#else
    const set<tag> * const tags(get_tags(pkg));
#endif

    if(tags == NULL)
      return NULL;

    for(set<tag>::const_iterator i=tags->begin(); i!=tags->end(); ++i)
      {
#ifdef HAVE_EPT
	std::string name(i->fullname());
#else
	const std::string name = i->str().c_str();
#endif

	pkg_match_result *res = get_string_match(name.c_str());
	if(res != NULL)
	  return res;
      }

    return NULL;
  }
};

//  Package-file info matchers.  Match a package if any of its
// available files (for all versions) match the given criteria.
//
//  Should I use templates?
class pkg_origin_matcher : public pkg_string_matcher
{
public:
  pkg_origin_matcher(const string &s) : pkg_string_matcher(s)
  {
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    if(ver.end())
      return false;

    for(pkgCache::VerFileIterator f = ver.FileList(); !f.end(); ++f)
      {
	pkgCache::PkgFileIterator cur = f.File();

	if(!cur.end() && cur.Origin() && string_matches(cur.Origin()))
	  return true;
      }

    return false;
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(ver.end())
      return NULL;

    for(pkgCache::VerFileIterator f = ver.FileList(); !f.end(); ++f)
      {
	pkgCache::PkgFileIterator cur = f.File();

	if(!cur.end() && cur.Origin())
	  {
	    pkg_match_result *r = get_string_match(cur.Origin());

	    if(r != NULL)
	      return r;
	  }
      }

    return NULL;
  }
};

class pkg_archive_matcher : public pkg_string_matcher
{
public:
  pkg_archive_matcher(const string &s) : pkg_string_matcher(s)
  {
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    if(ver.end() || ver.FileList().end())
      return false;

    for(pkgCache::VerFileIterator f = ver.FileList(); !f.end(); ++f)
      {
	pkgCache::PkgFileIterator cur = f.File();

	if(!cur.end() && cur.Archive() && string_matches(cur.Archive()))
	  return true;
      }

    return false;
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(ver.end() || ver.FileList().end())
      return NULL;

    for(pkgCache::VerFileIterator f = ver.FileList(); !f.end(); ++f)
      {
	pkgCache::PkgFileIterator cur = f.File();

	if(!cur.end() && cur.Archive())
	  {
	    pkg_match_result *r = get_string_match(cur.Archive());

	    if(r != NULL)
	      return r;
	  }
      }

    return NULL;
  }
};

class pkg_auto_matcher:public pkg_matcher
{
public:
  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return
      (!pkg.CurrentVer().end() || cache[pkg].Install()) &&
      (cache[pkg].Flags & pkgCache::Flag::Auto);
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    return matches(pkg, ver, cache, records)
      ? new unitary_result(_("Automatically Installed")) : NULL;
  }
};

class pkg_broken_matcher:public pkg_matcher
{
public:
  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    if(ver.end())
      return false;
    else
      {
	aptitudeDepCache::StateCache &state = cache[pkg];
	return state.NowBroken() || state.InstBroken();
      }
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    return matches(pkg, ver, cache, records) ? new unitary_result(_("Broken")) : NULL;
  }
};

class pkg_priority_matcher:public pkg_matcher
{
  pkgCache::State::VerPriority type;
public:
  pkg_priority_matcher(pkgCache::State::VerPriority _type)
    :type(_type) {}

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    if(ver.end())
      return false;
    else
      return ver->Priority == type;
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(ver.end())
      return NULL;
    else if(ver->Priority != type)
      return NULL;
    else
      return new unitary_result(const_cast<pkgCache::VerIterator &>(ver).PriorityType());
  }
};

pkg_match_result *dep_match(const pkgCache::DepIterator &dep)
{
  string realization;

  pkgCache::DepIterator start, end;

  surrounding_or(dep, start, end);

  bool first = true;

  while(start != end)
    {
      if(!first)
	realization += " | ";

      first = false;

      realization += start.TargetPkg().Name();

      if(start.TargetVer())
	{
	  realization += " (";
	  realization += start.CompType();
	  realization += " ";
	  realization += start.TargetVer();
	  realization += ")";
	}

      ++start;
    }

  // erm...
  return new result_pair(new unitary_result(const_cast<pkgCache::DepIterator &>(dep).DepType()),
			 new unitary_result(realization));
}

// Matches packages with unmet dependencies of a particular type.
class pkg_broken_type_matcher:public pkg_matcher
{
  pkgCache::Dep::DepType type; // Which type to match
public:
  pkg_broken_type_matcher(pkgCache::Dep::DepType _type)
    :type(_type) {}

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    if(ver.end())
      return false;
    else
      {
	pkgCache::DepIterator dep=ver.DependsList();

	while(!dep.end())
	  {
	    // Skip to the end of the Or group to check GInstall
	    while(dep->CompareOp & pkgCache::Dep::Or)
	      ++dep;

	    if(dep->Type==type &&
	       !(cache[dep] & pkgDepCache::DepGInstall))
	      // Oops, it's broken..
	      return true;

	    ++dep;
	  }
      }
    return false;
  }

  pkg_match_result * get_match(const pkgCache::PkgIterator &pkg,
			       const pkgCache::VerIterator &ver,
			       aptitudeDepCache &cache,
			       pkgRecords &records)
  {
    if(ver.end())
      return NULL;
    else
      {
	pkgCache::DepIterator dep=ver.DependsList();

	while(!dep.end())
	  {
	    // Skip to the end of the Or group to check GInstall
	    while(dep->CompareOp & pkgCache::Dep::Or)
	      ++dep;

	    if(dep->Type==type &&
	       !(cache[dep] & pkgDepCache::DepGInstall))
	      // Oops, it's broken..
	      return dep_match(dep);

	    ++dep;
	  }
      }

    return NULL;
  }
};

// This matches packages based on the action that will be taken with them.
//
// It will treat a request for a non-auto type as also being a request
// for the auto type.
class pkg_action_matcher:public pkg_matcher
{
  pkg_action_state type;
  bool require_purge;
public:
  pkg_action_matcher(pkg_action_state _type, bool _require_purge)
    :type(_type), require_purge(_require_purge)
  {
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    if(require_purge &&
       (cache[pkg].iFlags & pkgDepCache::Purge) == 0)
      return false;
    else
      {
	switch(type)
	  {
	  case pkg_install:
	    {
	      pkg_action_state thetype = find_pkg_state(pkg, cache);
	      return thetype==pkg_install || thetype==pkg_auto_install;
	    }
	  case pkg_hold:
	    return !pkg.CurrentVer().end() && cache.get_ext_state(pkg).selection_state == pkgCache::State::Hold;
	  case pkg_remove:
	    {
	      pkg_action_state thetype = find_pkg_state(pkg, cache);

	      return thetype==pkg_remove || thetype==pkg_auto_remove ||
		thetype==pkg_unused_remove;
	    }
	  default:
	    {
	      pkg_action_state thetype = find_pkg_state(pkg, cache);

	      return thetype==type;
	    }
	  }
      }
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(!matches(pkg, ver, cache, records))
      return NULL;
    else
      switch(type)
	{
	case pkg_unchanged: // shouldn't happen (?)
	  return new unitary_result(_("Unchanged"));
	case pkg_broken:
	  return new unitary_result(_("Broken"));
	case pkg_unused_remove:
	  return new unitary_result(_("Remove [unused]"));
	case pkg_auto_hold:
	  return new unitary_result(_("Hold [auto]"));
	case pkg_auto_install:
	  return new unitary_result(_("Install [auto]"));
	case pkg_auto_remove:
	  return new unitary_result(_("Remove [auto]"));
	case pkg_downgrade:
	  return new unitary_result(_("Downgrade"));
	case pkg_hold:
	  return new unitary_result(_("Hold"));
	case pkg_reinstall:
	  return new unitary_result(_("Reinstall"));
	case pkg_install:
	  return new unitary_result(_("Install"));
	case pkg_remove:
	  return new unitary_result(_("Remove"));
	case pkg_upgrade:
	  return new unitary_result(_("Upgrade"));
	default:
	  // should never happen.
	  abort();
	}
  }
};

class pkg_keep_matcher:public pkg_matcher
{
  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return cache[pkg].Keep();
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(cache[pkg].Keep())
      return new unitary_result(_("Keep"));
    else
      return NULL;
  }
};

/** Matches package versions that are not associated with a 'real'
 *  package.  Applied to a whole package, this matches virtual
 *  packages; it also matches package versions corresponding to
 *  removing a package.
 */
class pkg_virtual_matcher:public pkg_matcher
{
public:
  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return ver.end();
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(!ver.end())
      return NULL;
    else
      return new unitary_result(_("Virtual"));
  }
};

/** Matches the currently installed version of a package.
 */
class pkg_installed_matcher:public pkg_matcher
{
public:
  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return !pkg.CurrentVer().end() && ver == pkg.CurrentVer();
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(pkg.CurrentVer().end() || ver != pkg.CurrentVer())
      return NULL;
    else
      return new unitary_result(_("Installed"));
  }
};

class pkg_essential_matcher:public pkg_matcher
// Matches essential packages
{
public:
  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return
      (pkg->Flags&pkgCache::Flag::Essential)==pkgCache::Flag::Essential ||
      (pkg->Flags&pkgCache::Flag::Important)==pkgCache::Flag::Important;
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(!matches(pkg, ver, cache, records))
      return NULL;
    else
      return new unitary_result(_("Essential"));
  }
};

class pkg_configfiles_matcher:public pkg_matcher
// Matches a package which was removed but has config files remaining
{
public:
  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return pkg->CurrentState==pkgCache::State::ConfigFiles;
  }

  // ???
  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(pkg->CurrentState == pkgCache::State::ConfigFiles)
      return new unitary_result(_("Config Files Remain"));
    else
      return NULL;
  }
};

// Matches packages with a dependency on the given pattern.
class pkg_dep_matcher:public pkg_matcher
{
private:
  pkg_matcher *pattern;
  pkgCache::Dep::DepType type;

  /** If \b true, only broken dependencies will be matched. */
  bool broken;

public:
  pkg_dep_matcher(pkgCache::Dep::DepType _type,
		  pkg_matcher *_pattern,
		  bool _broken)
    :pattern(_pattern), type(_type), broken(_broken)
  {
  }
  ~pkg_dep_matcher() {delete pattern;}

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    eassert(!pkg.end());
    if(ver.end())
      return false;

    for(pkgCache::DepIterator dep=ver.DependsList(); !dep.end(); ++dep)
      {
	if( (type == dep->Type) ||
	    (type == pkgCache::Dep::Depends &&
	     dep->Type == pkgCache::Dep::PreDepends))
	  {
	    if(broken)
	      {
		pkgCache::DepIterator d2(cache, &*dep);
		while(d2->CompareOp & pkgCache::Dep::Or)
		  ++d2;
		if(cache[d2] & pkgDepCache::DepGInstall)
		  continue;
	      }

	    // See if a versionless match works,.
	    if(dep.TargetPkg().VersionList().end() &&
	       pattern->matches(dep.TargetPkg(), dep.TargetPkg().VersionList(), cache, records))
	      return true;

	    for(pkgCache::VerIterator i=dep.TargetPkg().VersionList(); !i.end(); i++)
	      if(_system->VS->CheckDep(i.VerStr(), dep->CompareOp, dep.TargetVer()))
		{
		  if(pattern->matches(dep.TargetPkg(), i, cache, records))
		    return true;
		}
	  }
      }

    return false;
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    eassert(!pkg.end());
    if(ver.end())
      return NULL;

    for(pkgCache::DepIterator dep=ver.DependsList(); !dep.end(); ++dep)
      {
	if( (type == dep->Type) ||
	    (type == pkgCache::Dep::Depends &&
	     dep->Type == pkgCache::Dep::PreDepends))
	  {
	    if(broken)
	      {
		pkgCache::DepIterator d2(cache, &*dep);
		while(d2->CompareOp & pkgCache::Dep::Or)
		  ++d2;
		if(cache[d2] & pkgDepCache::DepGInstall)
		  continue;
	      }

	    // See if a versionless match works,.
	    if(dep.TargetPkg().VersionList().end())
	      {
		pkg_match_result *r = pattern->get_match(dep.TargetPkg(), dep.TargetPkg().VersionList(), cache, records);

		if(r)
		  return new result_pair(r, dep_match(dep));
	      }

	    for(pkgCache::VerIterator i=dep.TargetPkg().VersionList(); !i.end(); i++)
	      if(_system->VS->CheckDep(i.VerStr(), dep->CompareOp, dep.TargetVer()))
		{
		  pkg_match_result *r = pattern->get_match(dep.TargetPkg(), i, cache, records);

		  if(r)
		    return new result_pair(r, dep_match(dep));
		}
	  }
      }

    return false;
  }
};

class pkg_or_matcher:public pkg_matcher
{
  pkg_matcher *left,*right;
public:
  pkg_or_matcher(pkg_matcher *_left, pkg_matcher *_right)
    :left(_left),right(_right)
  {
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return left->matches(pkg, ver, cache, records) ||
      right->matches(pkg, ver, cache, records);
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return left->matches(pkg, cache, records) ||
      right->matches(pkg, cache, records);
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    pkg_match_result *lr = left->get_match(pkg, ver, cache, records);

    if(lr != NULL)
      return lr;
    else
      return right->get_match(pkg, ver, cache, records);
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    pkg_match_result *lr = left->get_match(pkg, cache, records);

    if(lr != NULL)
      return lr;
    else
      return right->get_match(pkg, cache, records);
  }

  ~pkg_or_matcher()
  {
    delete left;
    delete right;
  }
};

class pkg_and_matcher:public pkg_matcher
{
  pkg_matcher *left,*right;
public:
  pkg_and_matcher(pkg_matcher *_left, pkg_matcher *_right)
    :left(_left),right(_right)
  {
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return left->matches(pkg, ver, cache, records) &&
      right->matches(pkg, ver, cache, records);
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return left->matches(pkg, cache, records) &&
      right->matches(pkg, cache, records);
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    pkg_match_result *r1 = left->get_match(pkg, ver, cache, records);

    if(r1 == NULL)
      return NULL;

    pkg_match_result *r2 = right->get_match(pkg, ver, cache, records);

    if(r2 == NULL)
      {
	delete r1;
	return NULL;
      }

    return new result_pair(r1, r2);
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    pkg_match_result *r1 = left->get_match(pkg, cache, records);

    if(r1 == NULL)
      return NULL;

    pkg_match_result *r2 = right->get_match(pkg, cache, records);

    if(r2 == NULL)
      {
	delete r1;
	return NULL;
      }

    return new result_pair(r1, r2);
  }

  ~pkg_and_matcher()
  {
    delete left;
    delete right;
  }
};

class pkg_not_matcher:public pkg_matcher
{
  pkg_matcher *child;
public:
  pkg_not_matcher(pkg_matcher *_child)
    :child(_child)
  {
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return !child->matches(pkg, ver, cache, records);
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return !child->matches(pkg, cache, records);
  }

  // Eh, there isn't really a good choice about what to return here...
  // just return an empty result if the child doesn't match.
  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    pkg_match_result *child_match = child->get_match(pkg, ver, cache, records);

    if(child_match == NULL)
      return new empty_match_result;
    else
      {
	delete child_match;
	return NULL;
      }
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    pkg_match_result *child_match = child->get_match(pkg, cache, records);

    if(child_match == NULL)
      return new empty_match_result;
    else
      {
	delete child_match;
	return NULL;
      }
  }

  ~pkg_not_matcher() {delete child;}
};

/** Widen the search to include all versions of every package. */
class pkg_widen_matcher : public pkg_matcher
{
  pkg_matcher *pattern;
public:
  pkg_widen_matcher(pkg_matcher *_pattern)
    : pattern(_pattern)
  {
  }

  ~pkg_widen_matcher()
  {
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return pattern->matches(pkg, cache, records);
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return pattern->matches(pkg, cache, records);
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    return pattern->get_match(pkg, cache, records);
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    return pattern->get_match(pkg, cache, records);
  }
};

/** Narrow the search to versions that match a pattern. */
class pkg_select_matcher : public pkg_matcher
{
  pkg_matcher *filter;
  pkg_matcher *pattern;
public:
  pkg_select_matcher(pkg_matcher *_filter,
		     pkg_matcher *_pattern)
    : filter(_filter), pattern(_pattern)
  {
  }

  ~pkg_select_matcher()
  {
    delete filter;
    delete pattern;
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return filter->matches(pkg, ver, cache, records) &&
      pattern->matches(pkg, ver, cache, records);
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(filter->matches(pkg, ver, cache, records))
      return pattern->get_match(pkg, ver, cache, records);
    else
      return NULL;
  }
};

// Matches packages that were garbage-collected.
class pkg_garbage_matcher:public pkg_matcher
{
public:
  pkg_garbage_matcher() {}

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    if(ver.end())
      return false;
    else
      return cache[pkg].Garbage;
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(!matches(pkg, ver, cache, records))
      return NULL;
    else
      return new unitary_result(_("Garbage"));
  }
};

// A dummy matcher that matches any package.
class pkg_true_matcher:public pkg_matcher
{
public:
  pkg_true_matcher() {}

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return true;
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    return new empty_match_result;
  }
};

// A dummy matcher that matches no packages.
class pkg_false_matcher:public pkg_matcher
{
public:
  pkg_false_matcher() {}

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return false;
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    return NULL;
  }
};

// Matches packages which have a dependency of the given type declared
// on them by a package matching a given pattern.
//
// Traces through Provided packages as well.
class pkg_revdep_matcher:public pkg_matcher
{
  pkgCache::Dep::DepType type;
  pkg_matcher *pattern;

  /** If \b true, only install-broken dependencies will cause a
   *  match.
   */
  bool broken;

public:
  pkg_revdep_matcher(pkgCache::Dep::DepType _type,
		     pkg_matcher *_pattern,
		     bool _broken)
    :type(_type), pattern(_pattern), broken(_broken)
  {
  }

  ~pkg_revdep_matcher()
  {
    delete pattern;
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    // Check direct dependencies.
    for(pkgCache::DepIterator d=pkg.RevDependsList(); !d.end(); ++d)
      {
	if(broken)
	  {
	    pkgCache::DepIterator d2(cache, &*d);
	    while(d2->CompareOp & pkgCache::Dep::Or)
	      ++d2;
	    if(cache[d2] & pkgDepCache::DepGInstall)
	      continue;
	  }

	if((d->Type==type ||
	    (type==pkgCache::Dep::Depends && d->Type==pkgCache::Dep::PreDepends)) &&
	   (!d.TargetVer() || (!ver.end() &&
			       _system->VS->CheckDep(ver.VerStr(), d->CompareOp, d.TargetVer()))) &&
	   pattern->matches(d.ParentPkg(), d.ParentVer(),
			    cache, records))
	  return true;
      }

    // Check dependencies through virtual packages.  ie, things that Depend
    // on stuff this package [version] Provides.
    if(!ver.end())
      for(pkgCache::PrvIterator p=ver.ProvidesList(); !p.end(); ++p)
	{
	  for(pkgCache::DepIterator d=p.ParentPkg().RevDependsList();
	      !d.end(); ++d)
	    {
	      if(broken)
		{
		  pkgCache::DepIterator d2(cache, &*d);
		  while(d2->CompareOp & pkgCache::Dep::Or)
		    ++d2;
		  if(cache[d2] & pkgDepCache::DepGInstall)
		    continue;
		}

	      // Only unversioned dependencies can match here.
	      if(d->Type==type && !d.TargetVer() &&
		 pattern->matches(d.ParentPkg(), d.ParentVer(),
				  cache, records))
		return true;
	    }
	}

    return false;
  }

  // Too much duplication, can I factor out the common stuff here?
  // C++ doesn't make it easy..
  //
  // Maybe I should just forget trying to be efficient and base
  // everything on match results..
  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    // Check direct dependencies.
    for(pkgCache::DepIterator d=pkg.RevDependsList(); !d.end(); ++d)
      {
	if(broken)
	  {
	    pkgCache::DepIterator d2(cache, &*d);
	    while(d2->CompareOp & pkgCache::Dep::Or)
	      ++d2;
	    if(cache[d2] & pkgDepCache::DepGInstall)
	      continue;
	  }

	if((d->Type==type ||
	    (type==pkgCache::Dep::Depends && d->Type==pkgCache::Dep::PreDepends)) &&
	   (!d.TargetVer() || (!ver.end() &&
			       _system->VS->CheckDep(ver.VerStr(), d->CompareOp, d.TargetVer()))))
	  {
	    pkg_match_result *r = pattern->get_match(d.ParentPkg(),
						     d.ParentVer(),
						     cache, records);

	    if(r != NULL)
	      return new result_pair(r, dep_match(d));
	  }
      }

    // Check dependencies through virtual packages.  ie, things that Depend
    // on stuff this package [version] Provides.
    if(!ver.end())
      for(pkgCache::PrvIterator p=ver.ProvidesList(); !p.end(); ++p)
	{
	  for(pkgCache::DepIterator d=p.ParentPkg().RevDependsList();
	      !d.end(); ++d)
	    {
	      // Only unversioned dependencies can match here.
	      if(d->Type==type && !d.TargetVer())
		{
		  if(broken)
		    {
		      pkgCache::DepIterator d2(cache, &*d);
		      while(d2->CompareOp & pkgCache::Dep::Or)
			++d2;
		      if(cache[d2] & pkgDepCache::DepGInstall)
			continue;
		    }

		  pkg_match_result *r = pattern->get_match(d.ParentPkg(),
							   d.ParentVer(),
							   cache, records);
		  if(r != NULL)
		    return new result_pair(r, dep_match(d));
		}
	    }
	}

    return NULL;
  }
};


/** Matches packages that provide a package that matches the given
 *  pattern.
 */
class pkg_provides_matcher:public pkg_matcher
{
  pkg_matcher *pattern;
public:
  pkg_provides_matcher(pkg_matcher *_pattern)
    :pattern(_pattern)
  {
  }

  ~pkg_provides_matcher() 
  {
    delete pattern;
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    if(ver.end())
      return false;

    for(pkgCache::PrvIterator p=ver.ProvidesList(); !p.end(); ++p)
      {
	// Assumes no provided version.
	if(pattern->matches(p.ParentPkg(), cache, records))
	  return true;
      }

    return false;
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(ver.end())
      return NULL;

    for(pkgCache::PrvIterator p=ver.ProvidesList(); !p.end(); ++p)
      {
	pkg_match_result *r = pattern->get_match(p.ParentPkg(), pkgCache::VerIterator(cache),
						 cache, records);

	if(r != NULL)
	  return new result_pair(r, new unitary_result(_("Provides")));
      }

    return false;
  }
};

/** Matches packages which are provided by a package that fits the
 *  given pattern.
 */
class pkg_revprv_matcher:public pkg_matcher
{
  pkg_matcher *pattern;
public:
  pkg_revprv_matcher(pkg_matcher *_pattern)
    :pattern(_pattern) 
  {
  }

  ~pkg_revprv_matcher() 
  {
    delete pattern;
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    for(pkgCache::PrvIterator p=pkg.ProvidesList(); !p.end(); ++p)
      {
	if(pattern->matches(p.OwnerPkg(), p.OwnerVer(), cache, records))
	  return true;
      }

    return false;
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    for(pkgCache::PrvIterator p=pkg.ProvidesList(); !p.end(); ++p)
      {
	pkg_match_result *r = pattern->get_match(p.OwnerPkg(),
						 p.OwnerVer(),
						 cache, records);

	if(r != NULL)
	  return new result_pair(r,
				 new unitary_result(_("Provided by")));
      }

    return false;
  }
};

//  Now back from the dead..it seems some people were actually using it ;-)
//
// Matches (non-virtual) packages which no installed package declares
// an "important" dependency on.
//
// Note that the notion of "importantness" is affected by the current
// settings!
class pkg_norevdep_matcher:public pkg_matcher
{
public:
  pkg_norevdep_matcher()
  {
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    if(ver.end())
      return false;
    else
      {
        pkgCache::DepIterator dep=pkg.RevDependsList();

        while(!dep.end())
          {
            if(cache.GetPolicy().IsImportantDep(dep) &&
               !dep.ParentVer().ParentPkg().CurrentVer().end())
              return false;

            ++dep;
          }

        return true;
      }
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(!matches(pkg, ver, cache, records))
      return NULL;
    else
      return new unitary_result(_("No reverse dependencies"));
  }
};

// Matches (non-virtual) packages which no installed package declares
// a dependency of the given type on.
class pkg_norevdep_type_matcher:public pkg_matcher
{
  pkgCache::Dep::DepType type; // Which type to match
public:
  pkg_norevdep_type_matcher(pkgCache::Dep::DepType _type)
    :type(_type)
  {
  }

  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    if(ver.end())
      return false;
    else
      {
	pkgCache::DepIterator dep=pkg.RevDependsList();

	while(!dep.end())
	  {
	    // Return false if the depender is installed.
	    if(dep->Type==type &&
	       !dep.ParentVer().ParentPkg().CurrentVer().end())
	      return false;

	    ++dep;
	  }
      }
    return true;
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(!matches(pkg, ver, cache, records))
      return NULL;
    else
      return new unitary_result(pkgCache::DepType(type));
  }
};

class pkg_new_matcher:public pkg_matcher
{
public:
  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    // Don't match virtual packages.
    if(pkg.VersionList().end())
      return false;
    else
      return cache.get_ext_state(pkg).new_package;
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(!matches(pkg, ver, cache, records))
      return NULL;
    else
      return new unitary_result(_("New Package"));
  }
};

class pkg_upgradable_matcher:public pkg_matcher
{
public:
  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return !pkg.CurrentVer().end() && cache[pkg].Upgradable();
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(!matches(pkg, ver, cache, records))
      return NULL;
    else
      return new unitary_result(_("Upgradable"));
  }
};

class pkg_obsolete_matcher : public pkg_matcher
{
public:
  bool matches(const pkgCache::PkgIterator &pkg,
	       const pkgCache::VerIterator &ver,
	       aptitudeDepCache &cache,
	       pkgRecords &records)
  {
    return pkg_obsolete(pkg);
  }

  pkg_match_result *get_match(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      aptitudeDepCache &cache,
			      pkgRecords &records)
  {
    if(!matches(pkg, ver, cache, records))
      return NULL;
    else
      return new unitary_result(_("Obsolete"));
  }
};

// Check for terminators.  Not terribly efficient, but I expect under
// 3 terminators in any interesting usage (more than that and I really
// should force yacc to do my bidding)
bool terminate(const string::const_iterator &start,
	       const string::const_iterator &end,
	       const vector<const char *> &terminators)
{
  for(vector<const char *>::const_iterator i = terminators.begin();
      i != terminators.end(); ++i)
    {
      string::const_iterator j1 = start;
      const char *j2 = *i;
      bool matches = true;

      while(j1 != end && *j2 != 0 && matches)
	{
	  if(*j1 != *j2)
	    matches=false;

	  ++j1;
	  ++j2;
	}

      if(matches)
	return true;
    }

  return false;
}

// Parses a dependency type.  Returns (ick) -1 if the type is not
// recognized.
pkgCache::Dep::DepType parse_deptype(const string &s)
{
  if(!strcasecmp(s.c_str(), "depends"))
    return pkgCache::Dep::Depends;
  if(!strcasecmp(s.c_str(), "predepends"))
    return pkgCache::Dep::PreDepends;
  if(!strcasecmp(s.c_str(), "recommends"))
    return pkgCache::Dep::Recommends;
  else if(!strcasecmp(s.c_str(), "suggests"))
    return pkgCache::Dep::Suggests;
  else if(!strcasecmp(s.c_str(), "conflicts"))
    return pkgCache::Dep::Conflicts;
  else if(!strcasecmp(s.c_str(), "breaks"))
    return pkgCache::Dep::DpkgBreaks;
  else if(!strcasecmp(s.c_str(), "replaces"))
    return pkgCache::Dep::Replaces;
  else // ewww.
    return (pkgCache::Dep::DepType) -1;
}

// Ideally this would parse the string and return an action type, but
// purging isn't a separate type to the matcher.  Maybe instead there
// should be a separate enum for the action matcher's modes?
pkg_matcher *make_action_matcher(const std::string &action_str)
{
  // Match packages to be installed
  if(!strcasecmp(action_str.c_str(), "install"))
    return new pkg_action_matcher(pkg_install, false);

  // Match packages to be upgraded
  else if(!strcasecmp(action_str.c_str(), "upgrade"))
    return new pkg_action_matcher(pkg_upgrade, false);

  else if(!strcasecmp(action_str.c_str(), "downgrade"))
    return new pkg_action_matcher(pkg_downgrade, false);

  // Match packages to be removed OR purged
  else if(!strcasecmp(action_str.c_str(), "remove"))
    return new pkg_action_matcher(pkg_remove, false);

  // Match packages to be purged
  else if(!strcasecmp(action_str.c_str(), "purge"))
    return new pkg_action_matcher(pkg_remove, true);

  // Match packages to be reinstalled
  else if(!strcasecmp(action_str.c_str(), "reinstall"))
    return new pkg_action_matcher(pkg_reinstall, false);

  // Match held packages
  else if(!strcasecmp(action_str.c_str(), "hold"))
    return new pkg_action_matcher(pkg_hold, false);
  else if(!strcasecmp(action_str.c_str(), "keep"))
    return new pkg_keep_matcher;

  else
    throw CompilationException(_("Unknown action type: %s"),
			       action_str.c_str());
}


pkg_matcher *parse_condition_list(string::const_iterator &start,
				  const string::const_iterator &end,
				  const vector<const char *> &terminators,
				  bool match_descriptions);

static
std::string parse_literal_string_tail(string::const_iterator &start,
				      const string::const_iterator end)
{
  std::string rval;

  while(start != end && *start != '"')
    {
      if(*start == '\\')
	{
	  ++start;
	  if(start != end)
	    {
	      switch(*start)
		{
		case 'n':
		  rval += '\n';
		  break;
		case 't':
		  rval += '\t';
		  break;
		default:
		  rval += *start;
		  break;
		}
	      ++start;
	    }
	}
      else
	{
	  rval += *start;
	  ++start;
	}
    }

  if(start == end)
    throw CompilationException(_("Unterminated literal string after %s"), rval.c_str());

  eassert(*start == '"');
  ++start;

  return rval;
}

// Returns a substring up to the first metacharacter, including escaped
// metacharacters (parentheses, ~, |, and !)
//
// Advances loc to the first character of 's' following the escaped string.
std::string parse_substr(string::const_iterator &start,
			 const string::const_iterator &end,
			 const vector<const char *> &terminators,
			 bool whitespace_breaks)
{
  std::string rval;
  bool done=false;

  // Strip leading whitespace.
  while(start != end && isspace(*start))
    ++start;

  do
    {
      while(start != end &&
	    *start != '(' &&
	    *start != ')' &&
	    *start != '!' &&
	    *start != '~' &&
	    *start != '|' &&
	    *start != '"' &&
	    (!whitespace_breaks || !isspace(*start)) &&
	    !terminate(start, end, terminators))
	{
	  rval += *start;
	  ++start;
	}

      if(start != end && *start == '"')
	{
	  ++start;

	  rval += parse_literal_string_tail(start, end);
	}

      // We quit because we ran off the end of the string or saw a
      // metacharacter.  If the latter case and it was a tilde-escape,
      // add the escaped character to the string and continue.
      if(start != end && start+1 != end && *start == '~')
	{
	  const char next = *(start+1);

	  if(next == '(' || next == ')' ||
	     next == '!' || next == '~' ||
	     next == '|' || next == '"' ||
	     (whitespace_breaks && isspace(next)))
	    {
	      rval += next;
	      start += 2;
	    }
	  else
	    done = true;
	}
      else
	done = true;
    } while(!done);

  return rval;
}


void parse_required_character(string::const_iterator &start,
			      const string::const_iterator &end,
			      char c)
{
  while(start != end && isspace(*start))
    ++start;

  if(start == end)
    throw CompilationException(_("Unexpected end of match pattern (expected '%c')."),
			       c);
  else if(*start != c)
    throw CompilationException(_("Expected '%c', got '%c'."),
			       c, *start);

  ++start;
}

template<typename arg>
struct parse_method;

void parse_open_paren(string::const_iterator &start,
		      const string::const_iterator &end)
{
  parse_required_character(start, end, '(');
}

void parse_close_paren(string::const_iterator &start,
		       const string::const_iterator &end)
{
  parse_required_character(start, end, ')');
}

void parse_comma(string::const_iterator &start,
		 const string::const_iterator &end)
{
  parse_required_character(start, end, ',');
}

template<>
struct parse_method<string>
{
  string operator()(string::const_iterator &start,
		    const string::const_iterator &end,
		    const std::vector<const char *> &terminators,
		    bool search_descriptions) const
  {
    return parse_substr(start, end, std::vector<const char *>(), false);
  }
};

template<>
struct parse_method<pkg_matcher *>
{
  pkg_matcher *operator()(string::const_iterator &start,
			  const string::const_iterator &end,
			  const std::vector<const char *> &terminators,
			  bool search_descriptions) const
  {
    return parse_condition_list(start, end, terminators, search_descriptions);
  }
};

template<typename T, typename A1>
T *parse_unary_matcher(string::const_iterator &start,
		       const string::const_iterator &end,
		       const std::vector<const char *> &terminators,
		       bool search_descriptions,
		       const parse_method<A1> &parse1 = parse_method<A1>())
{
  parse_open_paren(start, end);
  A1 a1(parse1(start, end, terminators, search_descriptions));
  parse_close_paren(start, end);

  return new T(a1);
}

void add_new_terminator(const char *new_terminator,
			std::vector<const char *> &terminators)
{
  for(std::vector<const char*>::const_iterator it = terminators.begin();
      it != terminators.end(); ++it)
    {
      if(strcmp(new_terminator, *it) == 0)
	return;
    }

  terminators.push_back(new_terminator);
}

template<typename T, typename A1, typename A2>
T *parse_binary_matcher(string::const_iterator &start,
			const string::const_iterator &end,
			const std::vector<const char *> &terminators,
			bool search_descriptions,
			const parse_method<A1> &parse1 = parse_method<A1>(),
			const parse_method<A2> &parse2 = parse_method<A2>())
{
  std::vector<const char *> terminators_plus_comma(terminators);
  add_new_terminator(",", terminators_plus_comma);

  parse_open_paren(start, end);
  A1 a1(parse1(start, end, terminators_plus_comma, search_descriptions));
  parse_comma(start, end);
  A2 a2(parse2(start, end, terminators, search_descriptions));
  parse_close_paren(start, end);

  return new T(a1, a2);
}

string parse_string_match_args(string::const_iterator &start,
			       const string::const_iterator &end)
{
  parse_open_paren(start, end);
  string substr(parse_substr(start, end, std::vector<const char *>(), false));
  parse_close_paren(start, end);

  return substr;
}

pkg_matcher *parse_pkg_matcher_args(string::const_iterator &start,
				    const string::const_iterator &end,
				    const std::vector<const char *> &terminators,
				    bool search_descriptions)
{
  parse_open_paren(start, end);
  auto_ptr<pkg_matcher> m(parse_condition_list(start, end, terminators, search_descriptions));
  parse_close_paren(start, end);

  return m.release();
}

pkg_matcher *parse_optional_pkg_matcher_args(string::const_iterator &start,
					     const string::const_iterator &end,
					     const std::vector<const char *> terminators,
					     bool search_descriptions)
{
  while(start != end && isspace(*start))
    ++start;

  if(start != end && *start == '(')
    return parse_pkg_matcher_args(start, end, terminators, search_descriptions);
  else
    return NULL;
}

pkg_matcher *parse_function_style_matcher_tail(string::const_iterator &start,
					       const string::const_iterator &end,
					       const vector<const char *> &terminators,
					       bool search_descriptions)
{
  // The name is considered to be the next sequence of non-whitespace
  // characters that are not an open paren.

  while(start != end && isspace(*start))
    ++start;

  string raw_name;
  string lower_case_name;
  while(start != end && *start != '(' && *start != '!' &&
	*start != '|' && *start != ')' && *start != '?' &&
	*start != '~' && !isspace(*start) &&
	!terminate(start, end, terminators))
    {
      raw_name += *start;
      lower_case_name += tolower(*start);
      ++start;
    }

  {
    // This block parses the following forms:
    //
    // ?TYPE(term)
    // ?broken-TYPE
    // ?broken-TYPE(term)
    // ?reverse-TYPE(term)
    // ?broken-reverse-TYPE(term)
    // ?reverse-broken-TYPE(term)
    const std::string broken_prefix("broken-");
    const std::string reverse_prefix("reverse-");

    bool broken = false;
    bool reverse = false;
    std::string suffix;

    if(std::string(lower_case_name, 0, broken_prefix.size()) == broken_prefix)
      {
	broken = true;

	if(std::string(lower_case_name, broken_prefix.size(), reverse_prefix.size()) == reverse_prefix)
	  {
	    reverse = true;
	    suffix = std::string(lower_case_name, broken_prefix.size() + reverse_prefix.size());
	  }
	else
	  suffix = std::string(lower_case_name, broken_prefix.size());
      }
    else if(std::string(lower_case_name, 0, reverse_prefix.size()) == reverse_prefix)
      {
	reverse = true;

	if(std::string(lower_case_name, reverse_prefix.size(), broken_prefix.size()) == broken_prefix)
	  {
	    broken = true;
	    suffix = std::string(lower_case_name, broken_prefix.size() + reverse_prefix.size());
	  }
	else
	  suffix = std::string(lower_case_name, reverse_prefix.size());
      }
    else
      suffix = lower_case_name;

    const pkgCache::Dep::DepType deptype = parse_deptype(suffix);

    while(start != end && isspace(*start) &&
	  !terminate(start, end, terminators))
      ++start;

    if(deptype == -1)
      {
	// Handle the special case of reverse-provides.
	if(reverse && suffix == "provides")
	  return new pkg_revprv_matcher(parse_pkg_matcher_args(start, end,
							       terminators,
							       search_descriptions));
	else if(broken || reverse)
	  throw CompilationException(_("Unknown dependency type: %s"),
				     suffix.c_str());

	// Otherwise what we have isn't a dep-matcher at all, so just
	// don't do anything and try other options.
      }
    else
      {
	if(reverse)
	  {
	    // broken-reverse-TYPE(term) and reverse-broken-TYPE(term)
	    pkg_matcher *m(parse_pkg_matcher_args(start, end,
						  terminators,
						  search_descriptions));

	    return new pkg_revdep_matcher(deptype, m, broken);
	  }
	else
	  {
	    // broken-TYPE and broken-TYPE(term) in the first branch,
	    // TYPE(term) in the second.
	    auto_ptr<pkg_matcher> m(broken
				    ? parse_optional_pkg_matcher_args(start, end, terminators, search_descriptions)
				    : parse_pkg_matcher_args(start, end, terminators, search_descriptions));

	    if(m.get() != NULL)
	      return new pkg_dep_matcher(deptype, m.release(), broken);
	    else
	      return new pkg_broken_type_matcher(deptype);
	  }
      }
  }

  matcher_type type;
  bool found = false;

  // Hokey sequential scan.  Why?  Allocating a static map and
  // populating it raises icky issues of thread-safety, when the
  // initializer runs, etc...I'd rather just accept some (hopefully
  // minor) inefficiency.
  for(const matcher_info *it = matcher_types;
      !found && (unsigned)(it - matcher_types) < (sizeof(matcher_types) / sizeof(matcher_types[0]));
      ++it)
    {
      string test_name = it->name;
      string::size_type found = test_name.find('|');
      if(found != string::npos)
	test_name.erase(0, found + 1);

      if(lower_case_name == test_name)
	{
	  type = it->type;
	  found = true;
	}
    }

  for(const matcher_info *it = matcher_types;
      !found && (unsigned)(it - matcher_types) < (sizeof(matcher_types) / sizeof(matcher_types[0]));
      ++it)
    {
      string name_to_check(P_(it->name));
      for(std::string::iterator name_to_check_it = name_to_check.begin();
	  name_to_check_it != name_to_check.end(); ++name_to_check_it)
	{
	  *name_to_check_it = tolower(*name_to_check_it);
	}

      if(lower_case_name == name_to_check)
	{
	  type = it->type;
	  found = true;
	}
    }

  if(!found)
    throw CompilationException(_("Unknown matcher type \"%s\"."),
			       raw_name.c_str());

  switch(type)
    {
    case matcher_type_action:
      return make_action_matcher(parse_string_match_args(start, end));
    case matcher_type_and:
      return parse_binary_matcher<pkg_and_matcher, pkg_matcher*, pkg_matcher*>(start, end, terminators, search_descriptions);
    case matcher_type_archive:
      return new pkg_archive_matcher(parse_string_match_args(start, end));
    case matcher_type_automatic:
      return new pkg_auto_matcher;
    case matcher_type_broken:
      return new pkg_broken_matcher;
    case matcher_type_configfiles:
      return new pkg_configfiles_matcher;
    case matcher_type_description:
      return new pkg_description_matcher(parse_string_match_args(start, end));
    case matcher_type_essential:
      return new pkg_essential_matcher;
    case matcher_type_false:
      return new pkg_false_matcher;
    case matcher_type_garbage:
      return new pkg_garbage_matcher;
    case matcher_type_installed:
      return new pkg_installed_matcher;
    case matcher_type_maintainer:
      return new pkg_maintainer_matcher(parse_string_match_args(start, end));
    case matcher_type_name:
      return new pkg_name_matcher(parse_string_match_args(start, end));
    case matcher_type_narrow:
      return parse_binary_matcher<pkg_select_matcher, pkg_matcher*, pkg_matcher*>(start, end, terminators, search_descriptions);
    case matcher_type_new:
      return new pkg_new_matcher;
    case matcher_type_not:
      return new pkg_not_matcher(parse_pkg_matcher_args(start, end, terminators, search_descriptions));
    case matcher_type_obsolete:
      return new pkg_obsolete_matcher;
    case matcher_type_or:
      return parse_binary_matcher<pkg_or_matcher, pkg_matcher*, pkg_matcher*>(start, end, terminators, search_descriptions);
    case matcher_type_origin:
      return new pkg_origin_matcher(parse_string_match_args(start, end));
    case matcher_type_provides:
      return parse_unary_matcher<pkg_provides_matcher, pkg_matcher*>(start, end, terminators, search_descriptions);
    case matcher_type_section:
      return new pkg_section_matcher(parse_string_match_args(start, end));
    case matcher_type_tag:
      return new pkg_tag_matcher(parse_string_match_args(start, end));
    case matcher_type_true:
      return new pkg_true_matcher;
    case matcher_type_version:
      return make_package_version_matcher(parse_string_match_args(start, end));
    case matcher_type_widen:
      return new pkg_widen_matcher(parse_pkg_matcher_args(start, end, terminators, search_descriptions));
    default:
      throw CompilationException(_("Unexpected matcher type %d encountered."),
				 type);
    }
}

pkg_matcher *parse_atom(string::const_iterator &start,
			const string::const_iterator &end,
			const vector<const char *> &terminators,
			bool search_descriptions)
{
  std::string substr;

  while(start != end && isspace(*start))
    ++start;

  while(start != end && *start != '|' && *start != ')' &&
	!terminate(start, end, terminators))
    {
      if(*start == '!')
	{
	  ++start;
	  return new pkg_not_matcher(parse_atom(start, end, terminators,
						search_descriptions));
	}
      else if(*start == '(')
	// Recur into the list, losing the extra terminators (they are
	// treated normally until the closing paren)
	{
	  ++start;
	  auto_ptr<pkg_matcher> lst(parse_condition_list(start, end,
							 vector<const char *>(),
							 search_descriptions));

	  if(!(start != end && *start == ')'))
	    throw CompilationException(_("Unmatched '('"));
	  else
	    {
	      ++start;
	      return lst.release();
	    }
	}
      else if(*start == '?')
	{
	  ++start;
	  return parse_function_style_matcher_tail(start, end, terminators, search_descriptions);
	}
      else if(*start == '~')
	{
	  ++start;
	  while(start != end && isspace(*start))
	    ++start;

	  if(start == end)
	    {
	      if(!search_descriptions)
		return new pkg_name_matcher("~");
	      else
		{
		  auto_ptr<pkg_matcher> name(new pkg_name_matcher("~"));
		  auto_ptr<pkg_matcher> desc(new pkg_description_matcher(substr));

		  return new pkg_or_matcher(name.release(),
					    desc.release());
		}
	    }
	  else
	    {
	      const char search_flag = *start;

	      ++start;

	      while(start != end && isspace(*start))
		++start;

	      switch(search_flag)
		// Nested switch statements, mmmm...
		// Ok, there really is a reason here.  For all of the match
		// types that need a string argument, some prefix code (see
		// below) is needed to find the string's end.  But this would
		// be worse than unnecessary for the others.  So I have this
		// double check -- first test for anything that doesn't need
		// the prefix code, then work out which of the other forms
		// we have.
		{
		case 'v':
		  return new pkg_virtual_matcher;
		case 'b':
		  return new pkg_broken_matcher;
		case 'g':
		  return new pkg_garbage_matcher;
		case 'c':
		  return new pkg_configfiles_matcher;
		case 'i':
		  return new pkg_installed_matcher;
		case 'E':
		  return new pkg_essential_matcher;
		case 'M':
		  return new pkg_auto_matcher;
		case 'N':
		  return new pkg_new_matcher;
		case 'U':
		  return new pkg_upgradable_matcher;
		case 'o':
		  return new pkg_obsolete_matcher;
		case 'P':
		case 'C':
		case 'W':
		  {
		    auto_ptr<pkg_matcher> m(parse_atom(start,
						       end,
						       terminators,
						       search_descriptions));
		    
		    switch(search_flag)
		      {
		      case 'C':
			return new pkg_dep_matcher(pkgCache::Dep::Conflicts, m.release(), false);
		      case 'P':
			return new pkg_provides_matcher(m.release());
		      case 'W':
			return new pkg_widen_matcher(m.release());
		      }
		  }
		case 'S':
		  {
		    auto_ptr<pkg_matcher> filter(parse_atom(start,
							    end,
							    terminators,
							    search_descriptions));

		    auto_ptr<pkg_matcher> pattern(parse_atom(start,
							     end,
							     terminators,
							     search_descriptions));

		    return new pkg_select_matcher(filter.release(), pattern.release());
		  }
		case 'D':
		case 'R':
		  {
		    bool do_provides = false;
		    bool broken = false;
		    pkgCache::Dep::DepType type=pkgCache::Dep::Depends;

		    if(start != end && *start == 'B')
		      {
			broken = true;
			++start;
		      }

		    string::const_iterator nextstart = start;

		    while(nextstart != end && isalpha(*nextstart) &&
			  !terminate(nextstart, end, terminators))
		      ++nextstart;

		    while(nextstart != end && isspace(*nextstart))
		      ++nextstart;

		    if(nextstart != end && *nextstart == ':')
		      {
			string tname(start, nextstart);
			stripws(tname);

			start = nextstart;
			++start;

			if(!strcasecmp(tname.c_str(), "provides"))
			  do_provides=true;
			else
			  {
			    type=parse_deptype(tname.c_str());

			    if(type==-1)
			      throw CompilationException(_("Unknown dependency type: %s"),
							 tname.c_str());
			  }
		      }

		    if(do_provides && broken)
		      throw CompilationException(_("Provides: cannot be broken"));

		    auto_ptr<pkg_matcher> m(parse_atom(start, end, terminators,
						       search_descriptions));

		    switch(search_flag)
		      {
		      case 'D':
			if(do_provides)
			  return new pkg_provides_matcher(m.release());
			else
			  return new pkg_dep_matcher(type, m.release(), broken);
		      case 'R':
			if(do_provides)
			  return new pkg_revprv_matcher(m.release());
			else
			  return new pkg_revdep_matcher(type, m.release(), broken);
		      }
		  }
		default:
		  substr = parse_substr(start, end, terminators, true);
		  switch(search_flag)
		    {
		    case 'a':
		      {
			return make_action_matcher(substr.c_str());
		      }
		    case 'A':
		      return new pkg_archive_matcher(substr);
		    case 'B':
		      {
			pkgCache::Dep::DepType ptype=parse_deptype(substr);

			if(ptype!=-1)
			  return new pkg_broken_type_matcher(ptype);
			else
			  throw CompilationException(_("Unknown dependency type: %s"),
						     substr.c_str());
		      }
		    case 'd':
		      return new pkg_description_matcher(substr);
		    case 'G':
		      return new pkg_tag_matcher(substr);
		    case 'F':
		      return new pkg_false_matcher;
		    case 'm':
		      return new pkg_maintainer_matcher(substr);
		    case 'n':
		      return new pkg_name_matcher(substr);
		    case 'O':
		      return new pkg_origin_matcher(substr);
		    case 'p':
		      {
			pkgCache::State::VerPriority type;

			const char *s=substr.c_str();

			if(strcasecmp(s, "important") == 0 ||
			   (apt_cache_file &&
			    strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Important)) == 0))
			  type=pkgCache::State::Important;
			else if(strcasecmp(s, "required") == 0 ||
				(apt_cache_file &&
				 strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Required)) == 0))
			  type=pkgCache::State::Required;
			else if(strcasecmp(s, "standard") == 0 ||
				(apt_cache_file &&
				 strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Standard)) == 0))
			  type=pkgCache::State::Standard;
			else if(strcasecmp(s, "optional") == 0 ||
				(apt_cache_file &&
				 strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Optional)) == 0))
			  type=pkgCache::State::Optional;
			else if(strcasecmp(s, "extra") == 0 ||
				(apt_cache_file &&
				 strcasecmp(s, (*apt_cache_file)->GetCache().Priority(pkgCache::State::Extra)) == 0))
			  type=pkgCache::State::Extra;
			else
			  throw CompilationException(_("Unknown priority %s"),
						     substr.c_str());

			return new pkg_priority_matcher(type);
		      }
		    case 's':
		      return new pkg_section_matcher(substr);
		    case 't':
		      return new pkg_task_matcher(substr);
		    case 'T':
		      return new pkg_true_matcher;
		    case 'V':
		      return make_package_version_matcher(substr);
		    default:
		      throw CompilationException(_("Unknown pattern type: %c"), search_flag);
		    }
		}
	    }
	}
      else
	{
	  if(!search_descriptions)
	    return new pkg_name_matcher(parse_substr(start, end,
						     terminators, true));
	  else
	    {
	      substr = parse_substr(start, end, terminators, true);
	      auto_ptr<pkg_matcher> name(new pkg_name_matcher(substr));
	      auto_ptr<pkg_matcher> desc(new pkg_description_matcher(substr));

	      return new pkg_or_matcher(name.release(),
					desc.release());
	    }
	}
    }

  // If we get here, the string was empty.
  throw CompilationException(_("Can't search for \"\""));
}

pkg_matcher *parse_and_group(string::const_iterator &start,
			     const string::const_iterator &end,
			     const vector<const char *> &terminators,
			     bool search_descriptions)
{
  auto_ptr<pkg_matcher> rval(NULL);
  while(start != end && isspace(*start))
    ++start;

  while(start != end && *start != '|' && *start != ')' &&
	!terminate(start, end, terminators))
    {
      auto_ptr<pkg_matcher> atom(parse_atom(start, end, terminators,
					    search_descriptions));

      if(rval.get() == NULL)
	rval = atom;
      else
	rval = auto_ptr<pkg_matcher>(new pkg_and_matcher(rval.release(), atom.release()));

      while(start != end && isspace(*start))
	++start;
    }

  if(rval.get() == NULL)
    throw CompilationException(_("Unexpected empty expression"));

  return rval.release();
}

pkg_matcher *parse_condition_list(string::const_iterator &start,
				  const string::const_iterator &end,
				  const vector<const char *> &terminators,
				  bool search_descriptions)
{
  auto_ptr<pkg_matcher> grp(parse_and_group(start, end, terminators,
					    search_descriptions));

  while(start != end && isspace(*start))
    ++start;

  while(start != end && *start != ')' && !terminate(start, end, terminators))
    {
      if(start != end && *start == '|')
	{
	  ++start;
	  auto_ptr<pkg_matcher> grp2(parse_condition_list(start, end, terminators,
							  search_descriptions));

	  return new pkg_or_matcher(grp.release(), grp2.release());
	}
      else
	throw CompilationException(_("Badly formed expression"));

      // Note that this code should never execute:
      while(start != end && isspace(*start))
	++start;
    }

  // If there's no second element in the condition list, return its
  // head.
  return grp.release();
}

pkg_matcher *parse_pattern(string::const_iterator &start,
			   const string::const_iterator &end,
			   const std::vector<const char *> &terminators,
			   bool search_descriptions,
			   bool flag_errors,
			   bool require_full_parse)
{
  // Just filter blank strings out immediately.
  while(start != end && isspace(*start) && !terminate(start, end, terminators))
    ++start;

  if(start == end)
    return NULL;

  try
    {
      auto_ptr<pkg_matcher> rval(parse_condition_list(start, end, terminators,
						      search_descriptions));

      while(start != end && isspace(*start))
	++start;

      if(require_full_parse && start != end)
	throw CompilationException(_("Unexpected ')'"));
      else
	return rval.release();
    }
  catch(CompilationException e)
    {
      if(flag_errors)
	_error->Error("%s", e.get_msg().c_str());

      return NULL;
    }
}
