// tags.cc
//
//   Copyright (C) 2005 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#include "tags.h"

#include <aptitude.h>

#include "apt.h"
#include "config_signal.h"

#include <algorithm>
#include <map>
#include <utility>

#include <ctype.h>
#include <string.h>

#include <sigc++/functors/mem_fun.h>

#include <apt-pkg/error.h>
#include <apt-pkg/pkgrecords.h>
#include <apt-pkg/tagfile.h>

#include <generic/util/eassert.h>

using namespace std;

tag::tag(std::string::const_iterator start,
	 std::string::const_iterator finish)
{
  while(start != finish && isspace(*start))
    ++start;

  while(start != finish && isspace(*(finish-1)))
    --finish;

  s.assign(start, finish);
}

tag::const_iterator &tag::const_iterator::operator++()
{
  start = finish;
  while(start != limit && (*start)==':')
    ++start;

  if(start == limit)
    finish = limit;
  else
    {
      finish = start+1;
      while(finish != limit && (*finish) != ':')
	++finish;
    }

  return *this;
}

tag::const_iterator tag::begin() const
{
  tag::const_iterator rval(s.begin(), s.begin(), s.end());

  ++rval;

  return rval;
}

int tag::cmp(const tag &other) const
{
  const_iterator myT=begin(), otherT=other.begin();

  while(myT != end() && otherT != other.end())
    {
      // ew, rather slow
      if(lexicographical_compare(myT.start, myT.finish,
				 otherT.start, otherT.finish))
	return -1;
      else if(lexicographical_compare(otherT.start, otherT.finish,
				      myT.start, myT.finish))
	return 1;

      ++myT;
      ++otherT;
    }

  if(otherT != other.end())
    return -1;
  else if(myT != end())
    return 1;
  else
    return 0;
}

tag_list::const_iterator &tag_list::const_iterator::operator++()
{
  start = finish;

  while(start != limit && (*start) != ',')
    ++start;

  if(start != limit) // Push past the comma.
    ++start;

  if(start == limit)
    finish = limit;
  else
    {
      // Eat everything up to the next comma.
      finish = start+1;
      while(finish != limit && (*finish) != ',')
	++finish;
    }

  return *this;
}

tag_list::const_iterator tag_list::begin() const
{
  std::string::const_iterator endfirst=s.begin();
  while(endfirst != s.end() && (*endfirst) != ',')
    ++endfirst;

  const_iterator rval(s.begin(), endfirst, s.end());
  return rval;
}

typedef set<tag> db_entry;

// The database is built eagerly, since the common use case is
// to scan everything in sight right away and this makes it easy
// to provide a progress bar to the user.
db_entry *tagDB;

static void insert_tags(const pkgCache::VerIterator &ver,
			const pkgCache::VerFileIterator &vf)
{
  set<tag> *tags = tagDB + ver.ParentPkg()->ID;

  const char *recstart=0, *recend=0;
  const char *tagstart, *tagend;
  pkgTagSection sec;

  eassert(apt_package_records);
  eassert(tagDB);

  apt_package_records->Lookup(vf).GetRec(recstart, recend);
  if(!recstart || !recend)
    return;
  if(!sec.Scan(recstart, recend-recstart+1))
    return;

  if(!sec.Find("Tag", tagstart, tagend))
    return;

  tag_list lst(tagstart, tagend);

  for(tag_list::const_iterator t=lst.begin(); t!=lst.end(); ++t)
    tags->insert(*t);
}

static void reset_tags()
{
  delete[] tagDB;
  tagDB = NULL;
}

const set<tag> *get_tags(const pkgCache::PkgIterator &pkg)
{
  if(!apt_cache_file || !tagDB)
    return NULL;

  return tagDB + pkg->ID;
}

bool initialized_reset_signal;
void load_tags(OpProgress &progress)
{
  eassert(apt_cache_file && apt_package_records);

  if(!initialized_reset_signal)
    {
      cache_closed.connect(sigc::ptr_fun(reset_tags));
      cache_reload_failed.connect(sigc::ptr_fun(reset_tags));
      initialized_reset_signal = true;
    }

  tagDB = new db_entry[(*apt_cache_file)->Head().PackageCount];

  std::vector<loc_pair> verfiles;

  for(pkgCache::PkgIterator p = (*apt_cache_file)->PkgBegin();
      !p.end(); ++p)
    for(pkgCache::VerIterator v = p.VersionList(); !v.end(); ++v)
      for(pkgCache::VerFileIterator vf = v.FileList();
	  !vf.end(); ++vf)
	verfiles.push_back(loc_pair(v, vf));

  sort(verfiles.begin(), verfiles.end(), location_compare());

  progress.OverallProgress(0, verfiles.size(), 1,
			   _("Building tag database"));
  size_t n=0;
  for(std::vector<loc_pair>::iterator i=verfiles.begin();
      i!=verfiles.end(); ++i)
    {
      insert_tags(i->first, i->second);
      ++n;
      progress.OverallProgress(n, verfiles.size(), 1, _("Building tag database"));
    }

  progress.Done();
}




// TAG VOCABULARY FILE
typedef map<string, string> facet_description_map;
typedef map<string, string> tag_description_map;

facet_description_map *facet_descriptions;
tag_description_map *tag_descriptions;

static void init_vocabulary()
{
  if(facet_descriptions != NULL)
    {
      eassert(tag_descriptions != NULL);
      return;
    }

  facet_descriptions = new facet_description_map;
  tag_descriptions = new tag_description_map;

  FileFd F(aptcfg->FindFile("DebTags::Vocabulary", "/var/lib/debtags/vocabulary"),
	   FileFd::ReadOnly);

  if(!F.IsOpen())
    {
      _error->Warning(_("Unable to load debtags vocabulary, perhaps debtags is not installed?"));
      // Fail silently; debtags need not be installed.
      return;
    }

  pkgTagFile tagfile(&F);

  pkgTagSection sec;

  while(tagfile.Step(sec))
    {
      string facet;
      const char *start, *end;

      if(sec.Find("Facet", start, end))
	{
	  facet.assign(start, end-start);

	  if(sec.Find("Description", start, end))
	    (*facet_descriptions)[facet] = string(start, end-start);
	}
      else if(sec.Find("Tag", start, end))
	{
	  string tag(start, end-start);

	  if(sec.Find("Description", start, end))
	    (*tag_descriptions)[tag] = string(start, end-start);
	}
    }
}

string facet_description(const std::string &facet)
{
  init_vocabulary();

  facet_description_map::const_iterator found =
    facet_descriptions->find(facet);

  if(found == facet_descriptions->end())
    return string();
  else
    return found->second;
}

string tag_description(const std::string &tag)
{
  init_vocabulary();

  tag_description_map::const_iterator found =
    tag_descriptions->find(tag);

  if(found == tag_descriptions->end())
    return string();
  else
    return found->second;
}
