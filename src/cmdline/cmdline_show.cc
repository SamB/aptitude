// cmdline_show.cc                               -*-c++-*-
//
//  Copyright 2004 Daniel Burrows

#include "cmdline_show.h"

#include <aptitude.h>
#include <desc_parse.h>

#include "cmdline_common.h"
#include "cmdline_util.h"

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/matchers.h>

#include <vscreen/fragment.h>
#include <vscreen/transcode.h>

#include <apt-pkg/error.h>
#include <apt-pkg/init.h>
#include <apt-pkg/pkgcache.h>
#include <apt-pkg/pkgrecords.h>
#include <apt-pkg/progress.h>
#include <apt-pkg/strutl.h>
#include <apt-pkg/version.h>

#include <iostream>

using namespace std;

ostream &operator<<(ostream &out, const fragment_contents &contents)
{
  for(fragment_contents::const_iterator i=contents.begin();
      i!=contents.end(); ++i)
    {
      wstring s;
      // Drop the attributes.
      for(fragment_line::const_iterator j=i->begin(); j!=i->end(); ++j)
	s.push_back((*j).ch);

      out << transcode(s) << endl;
    }

  return out;
}

static fragment *dep_lst_frag(pkgCache::DepIterator dep,
			      string title, pkgCache::Dep::DepType T)
{
  vector<fragment *> fragments;

  while(!dep.end())
    {
      pkgCache::DepIterator start, end;
      dep.GlobOr(start, end);

      vector<fragment *> or_fragments;

      if(start->Type==T)
	do
	  {
	    fragment *verfrag;

	    if(start.TargetVer())
	      verfrag=fragf(" (%s %s)", start.CompType(), start.TargetVer());
	    else
	      verfrag=fragf("");

	    or_fragments.push_back(fragf("%s%F",
					 start.TargetPkg().Name(),
					 verfrag));

	    if(start==end)
	      break;
	    ++start;
	  } while(1);

      if(!or_fragments.empty())
	fragments.push_back(join_fragments(or_fragments, L" | "));
    }

  if(fragments.size()==0)
    return fragf("");
  else
    return fragf("%s: %F",
		 title.c_str(),
		 indentbox(0, title.size()+2,
			   flowbox(join_fragments(fragments, L", "))));
}

typedef std::pair<std::string, std::string> pkgverpair;
struct package_version_pair_cmp
{
  bool operator()(const pkgverpair &x, const pkgverpair &y) const
  {
    return x.first < y.first
      || _system->VS->CmpVersion(x.second, y.second) < 0;
  }
};

static fragment *prv_lst_frag(pkgCache::PrvIterator prv,
			      bool reverse,
			      int verbose,
			      const std::string &title)
{
  vector<fragment *> fragments;

  if(reverse && verbose >= 1)
    {
      std::set<pkgverpair, package_version_pair_cmp> packagevers;

      for( ; !prv.end(); ++prv)
	{
	  string name         = prv.OwnerPkg().Name();
	  const char *version = prv.OwnerVer().VerStr();

	  if(version != NULL)
	    packagevers.insert(pkgverpair(name, version));
	  else
	    packagevers.insert(pkgverpair(name, ""));
	}

      for(std::set<pkgverpair>::const_iterator it = packagevers.begin();
	  it != packagevers.end(); ++it)
	fragments.push_back(fragf("%s (%s)", it->first.c_str(), it->second.c_str()));
    }
  else
    {
      std::set<std::string> packages;

      for( ; !prv.end(); ++prv)
	{
	  string name = reverse ? prv.OwnerPkg().Name() : prv.ParentPkg().Name();

	  packages.insert(name);
	}

      for(std::set<std::string>::const_iterator it = packages.begin();
	  it != packages.end(); ++it)
	fragments.push_back(text_fragment(*it));
    }

  if(fragments.size()==0)
    return fragf("");
  else
    return fragf("%s: %F",
		 title.c_str(),
		 indentbox(0, title.size()+2,
			   flowbox(join_fragments(fragments, L", "))));
}

static fragment *archive_lst_frag(pkgCache::VerFileIterator vf,
				  const std::string &title)
{
  vector<fragment *> fragments;

  for( ; !vf.end(); ++vf)
    {
      if(vf.File().Archive() == 0)
	fragments.push_back(text_fragment(_("<NULL>")));
      else
	fragments.push_back(text_fragment(vf.File().Archive()));
    }

  if(fragments.size()==0)
    return fragf("");
  else
    return fragf("%s: %F",
		 title.c_str(),
		 indentbox(0, title.size()+2,
			   flowbox(join_fragments(fragments, L", "))));
}

static const char *current_state_string(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
{
  if(!ver.end() && ver != pkg.CurrentVer())
    return _("not installed");

  switch(pkg->CurrentState)
    {
    case pkgCache::State::NotInstalled:
      return _("not installed");
    case pkgCache::State::UnPacked:
      return _("unpacked");
    case pkgCache::State::HalfConfigured:
      return _("partially configured");
    case pkgCache::State::HalfInstalled:
      return _("partially installed");
    case pkgCache::State::ConfigFiles:
      return _("not installed (configuration files remain)");
    case pkgCache::State::Installed:
      return _("installed");
    default:
      return "Internal error: bad state flag!";
    }
}

/** \brief Return a fragment describing the reason that the package
 *  with the given state is being deleted.
 *
 *  statestr is passed to avoid screwing up translations (otherwise I'd
 *  have to break the format string into two pieces).
 */
static fragment *deletion_fragment(const char *statestr,
				   const pkgDepCache::StateCache &state,
				   const aptitudeDepCache::aptitude_state &estate)
{
  bool unused_delete=(estate.remove_reason!=aptitudeDepCache::manual);

  if(!state.Delete())
    return fragf("Internal error: no InstVer for a non-deleted package");
  else if(state.iFlags&pkgDepCache::Purge)
    {
      if(unused_delete)
	return fragf(_("%s; will be purged because nothing depends on it"), statestr);
      else
	return fragf(_("%s; will be purged"));
    }
  else
    {
      if(unused_delete)
	return fragf(_("%s; will be removed because nothing depends on it"), statestr);
      else
	return fragf(_("%s; will be removed"));
    }
}

static fragment *version_change_fragment(const char *statestr,
					 const char *holdstr,
					 pkgCache::VerIterator curver,
					 pkgCache::VerIterator instver)
{
  const char *curverstr=curver.VerStr();
  const char *instverstr=instver.VerStr();

  int vercmp=_system->VS->DoCmpVersion(curverstr,
				       curverstr+strlen(curverstr),
				       instverstr,
				       instverstr+strlen(instverstr));

  if(vercmp>0)
    return fragf(_("%s%s; will be downgraded [%s -> %s]"),
		 statestr, holdstr, curverstr, instverstr);
  else if(vercmp<0)
    return fragf(_("%s%s; will be upgraded [%s -> %s]"),
		 statestr, holdstr, curverstr, instverstr);
  else
    return fragf("%s%s", statestr, holdstr);
}

static fragment *state_fragment(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
{
  if(pkg.end() || pkg.VersionList().end())
    return fragf(_("not a real package"));

  pkgCache::VerIterator curver=pkg.CurrentVer();
  pkgCache::VerIterator instver=(*apt_cache_file)[pkg].InstVerIter(*apt_cache_file);

  pkgDepCache::StateCache &state=(*apt_cache_file)[pkg];
  aptitudeDepCache::aptitude_state &estate=(*apt_cache_file)->get_ext_state(pkg);

  const char *statestr=current_state_string(pkg, ver);

  const char *holdstr="";

  if(ver == curver && estate.selection_state==pkgCache::State::Hold)
    holdstr=_(" [held]");

  if(!ver.end())
    {
      if(instver != ver && curver != ver)
	return fragf("%s", statestr);
      else if(instver == ver)
	{
	  if(curver != instver)
	    {
	      if(pkg->CurrentState == pkgCache::State::NotInstalled ||
		 pkg->CurrentState == pkgCache::State::ConfigFiles)
		{
		  if(estate.install_reason == aptitudeDepCache::manual)
		    return fragf(_("%s; will be installed"), statestr);
		  else
		    return fragf(_("%s; will be installed automatically"), statestr);
		}
	      else
		return version_change_fragment(statestr, holdstr, curver, instver);
	    }
	  else
	    return fragf("%s", statestr);
	}
      else if(instver.end()) // Now curver is ver.
	return deletion_fragment(statestr, state, estate);
      else
	return version_change_fragment(statestr, holdstr, curver, instver);
    }
  else if(curver.end())
    {
      if(state.Install() &&
	 (pkg->CurrentState==pkgCache::State::NotInstalled ||
	  pkg->CurrentState==pkgCache::State::ConfigFiles))
	{
	  if(estate.install_reason == aptitudeDepCache::manual)
	    return fragf(_("%s; version %s will be installed"),
			 statestr,
			 instver.VerStr());
	  else
	    return fragf(_("%s; version %s will be installed automatically"),
			 statestr,
			 instver.VerStr());
	}
      else if(state.Delete() && (state.iFlags&pkgDepCache::Purge) &&
	      pkg->CurrentState!=pkgCache::State::NotInstalled)
	return fragf(_("%s; will be purged"), statestr);
      else
	return fragf("%s", statestr);
    }
  else if(instver.end())
    {
      return deletion_fragment(statestr, state, estate);
    }
  else
    {
      return version_change_fragment(statestr, holdstr, curver, instver);
    }
}

/** \brief Shows information about a package. */
static void show_package(pkgCache::PkgIterator pkg, int verbose)
{
  vector<fragment *> fragments;

  fragments.push_back(fragf("%s%s%n", _("Package: "), pkg.Name()));
  fragments.push_back(fragf("%s: %F%n", _("State"), state_fragment(pkg, pkgCache::VerIterator())));
  fragments.push_back(prv_lst_frag(pkg.ProvidesList(), true, verbose, _("Provided by")));

  fragment *f=sequence_fragment(fragments);

  cout << f->layout(screen_width, screen_width, style());

  delete f;
}

static fragment *version_file_fragment(pkgCache::VerIterator ver,
				       pkgCache::VerFileIterator vf,
				       int verbose)
{
  vector<fragment *> fragments;

  pkgCache::PkgIterator pkg=ver.ParentPkg();
  pkgRecords::Parser &rec=apt_package_records->Lookup(vf);
  aptitudeDepCache::aptitude_state &estate=(*apt_cache_file)->get_ext_state(pkg);

  fragments.push_back(fragf("%s%s%n", _("Package: "), pkg.Name()));
  if((pkg->Flags & pkgCache::Flag::Essential)==pkgCache::Flag::Essential)
    fragments.push_back(fragf("%s%s%n", _("Essential: "),  _("yes")));

  if(estate.new_package)
    fragments.push_back(fragf("%s: %s%n",
			      _("New"), _("yes")));

  fragments.push_back(fragf("%s: %F%n", _("State"), state_fragment(pkg, ver)));
  if(!estate.forbidver.empty())
    fragments.push_back(fragf("%s: %s%n",
			      _("Forbidden version"),
			      estate.forbidver.c_str()));

  if(!pkg.CurrentVer().end())
    fragments.push_back(fragf("%s: %s%n", _("Automatically installed"),
			      estate.install_reason == aptitudeDepCache::manual
			      ? _("no") : _("yes")));

  fragments.push_back(fragf("%s%s%n", _("Version: "), ver.VerStr()));
  fragments.push_back(fragf("%s%s%n", _("Priority: "),
			    ver.PriorityType()?ver.PriorityType():_("N/A")));
  fragments.push_back(fragf("%s%s%n", _("Section: "),
			    ver.Section()?ver.Section():_("N/A")));
  fragments.push_back(fragf("%s%s%n", _("Maintainer: "),
			    rec.Maintainer().c_str()));

  fragments.push_back(fragf("%s%s%n", _("Uncompressed Size: "),
			    SizeToStr(ver->InstalledSize).c_str()));
  if(verbose>0)
    {
      fragments.push_back(fragf("%s%s%n", _("Architecture: "),
				ver.Arch()?ver.Arch():_("N/A")));
      fragments.push_back(fragf("%s%s%n", _("Compressed Size: "),
				SizeToStr(ver->Size).c_str()));
      fragments.push_back(fragf("%s%s%n", _("Filename: "),
				rec.FileName().c_str()));
      fragments.push_back(fragf("%s%s%n", _("MD5sum: "),
				rec.MD5Hash().c_str()));

      if(verbose<2) // Show all archives in a list.
	fragments.push_back(archive_lst_frag(ver.FileList(), _("Archive")));
      else
	{
	  fragments.push_back(fragf("%s: %s%n", _("Archive"), vf.File().Archive()?vf.File().Archive():_("<NULL>")));
	}
    }

  fragments.push_back(dep_lst_frag(ver.DependsList(),
				   _("Depends"), pkgCache::Dep::Depends));
  fragments.push_back(dep_lst_frag(ver.DependsList(),
				   _("PreDepends"), pkgCache::Dep::PreDepends));
  fragments.push_back(dep_lst_frag(ver.DependsList(),
				   _("Recommends"), pkgCache::Dep::Recommends));
  fragments.push_back(dep_lst_frag(ver.DependsList(),
				   _("Suggests"), pkgCache::Dep::Suggests));
  fragments.push_back(dep_lst_frag(ver.DependsList(),
				   _("Conflicts"), pkgCache::Dep::Conflicts));
  fragments.push_back(dep_lst_frag(ver.DependsList(),
				   _("Replaces"), pkgCache::Dep::Replaces));
  fragments.push_back(dep_lst_frag(ver.DependsList(),
				   _("Obsoletes"), pkgCache::Dep::Obsoletes));

  fragments.push_back(prv_lst_frag(ver.ProvidesList(), false, verbose, _("Provides")));
  fragments.push_back(prv_lst_frag(ver.ParentPkg().ProvidesList(), true, verbose, _("Provided by")));

  fragments.push_back(fragf("%s%ls%n",
			    _("Description: "),
			    get_short_description(ver).c_str()));
  fragments.push_back(indentbox(1, 1, make_desc_fragment(get_long_description(ver))));

  fragment *tags = make_tags_fragment(pkg);
  if(tags)
    fragments.push_back(fragf("%n%F", tags));

  return sequence_fragment(fragments);
}

static void show_version(pkgCache::VerIterator ver, int verbose)
{
  if(ver.FileList().end())
    {
      fragment *f=version_file_fragment(ver, ver.FileList(), verbose);

      cout << f->layout(screen_width, screen_width, style());

      delete f;
    }
  else
    {
      for(pkgCache::VerFileIterator vf=ver.FileList(); !vf.end(); ++vf)
	{
	  fragment *f=version_file_fragment(ver, vf, verbose);

	  cout << f->layout(screen_width, screen_width, style()) << endl;

	  delete f;

	  // If verbose<2, only show the first file.
	  if(verbose<2)
	    break;
	}
    }
}

static
bool do_cmdline_show_target(const pkgCache::PkgIterator &pkg,
			    cmdline_version_source source,
			    const string &sourcestr,
			    int verbose,
			    bool has_explicit_source)
{
  if(verbose == 0 || has_explicit_source)
    {
      // HACK: default to current-or-candidate behavior.  This should be
      // done in a more up-front way :-(.
      if(source == cmdline_version_cand)
	source = cmdline_version_curr_or_cand;
      pkgCache::VerIterator ver = cmdline_find_ver(pkg, source, sourcestr);

      if(ver.end())
	ver = pkg.VersionList();

      if(!ver.end())
	show_version(ver, verbose);
      else
	show_package(pkg, verbose);
    }
  else if(!pkg.VersionList().end())
    for(pkgCache::VerIterator ver=pkg.VersionList(); !ver.end(); ++ver)
      show_version(ver, verbose);
  else
    show_package(pkg, verbose);

  return true;
}

bool do_cmdline_show(string s, int verbose)
{
  cmdline_version_source source;
  string name, sourcestr;
  string default_release = aptcfg->Find("APT::Default-Release");
  bool has_explicit_source = false;

  if(!cmdline_parse_source(s, source, name, sourcestr))
    return false;

  has_explicit_source = (source != cmdline_version_cand);

  if(source == cmdline_version_cand && !default_release.empty())
    {
      source    = cmdline_version_archive;
      sourcestr = default_release;
    }

  bool is_pattern=(name.find('~')!=name.npos);
  pkgCache::PkgIterator pkg;

  if(!is_pattern)
    {
      pkg=(*apt_cache_file)->FindPkg(name);

      if(pkg.end())
	{
	  _error->Error(_("Unable to locate package %s"), s.c_str());
	  return false;
	}
    }

  if(!is_pattern && !pkg.end())
    return do_cmdline_show_target(pkg, source, sourcestr, verbose, has_explicit_source);
  else if(is_pattern)
    {
      std::auto_ptr<pkg_matcher> m(parse_pattern(name));

      if(m.get() == NULL)
	{
	  _error->Error(_("Unable to parse pattern %s"), name.c_str());
	  return false;
	}

      for(pkgCache::PkgIterator P=(*apt_cache_file)->PkgBegin();
	  !P.end(); ++P)
	if(m->matches(P))
	  return do_cmdline_show_target(P, source, sourcestr, verbose, has_explicit_source);

      delete m;
    }
  else
    ; // TODO: print an error message -- Christian will kill me if I
      // make the pofile bigger right now.

  return true;
}

int cmdline_show(int argc, char *argv[], int verbose)
{
  _error->DumpErrors();

  OpProgress progress;
  apt_init(&progress, false);

  if(_error->PendingError())
    {
      _error->DumpErrors();
      return -1;
    }

  for(int i=1; i<argc; ++i)
    if(!do_cmdline_show(argv[i], verbose))
      {
	_error->DumpErrors();
	return -1;
      }

  if(_error->PendingError())
    {
      _error->DumpErrors();
      return -1;
    }

  return 0;
}
