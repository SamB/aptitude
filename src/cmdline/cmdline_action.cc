// cmdline_action.cc
//
//  Copyright 2004 Daniel Burrows

#include "cmdline_action.h"
#include "cmdline_util.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/matchers.h>
#include <generic/apt/tasks.h>

#include <apt-pkg/algorithms.h>
#include <apt-pkg/error.h>
#include <apt-pkg/pkgcache.h>

bool cmdline_applyaction(cmdline_pkgaction_type action,
			 pkgCache::PkgIterator pkg,
			 pkgset &to_install, pkgset &to_hold,
			 pkgset &to_remove, pkgset &to_purge,
			 int verbose,
			 cmdline_version_source source,
			 const string &sourcestr,
			 bool allow_auto)
{
  // Handle virtual packages.
  if(!pkg.ProvidesList().end())
    {
      if(pkg.VersionList().end())
	{
	  for(pkgCache::PrvIterator prv=pkg.ProvidesList(); !prv.end(); ++prv)
	    {
	      if(prv.OwnerPkg().CurrentVer()==prv.OwnerVer())
		{
		  if(verbose>0)
		    printf(_("Note: \"%s\", providing the virtual package\n      \"%s\", is already installed.\n"),
			   prv.OwnerPkg().Name(), pkg.Name());
		  return true;
		}
	      else if((*apt_cache_file)[prv.OwnerPkg()].InstVerIter(*apt_cache_file)==prv.OwnerVer())
		{
		  if(verbose>0)
		    printf(_("Note: \"%s\", providing the virtual package\n      \"%s\", is already going to be installed.\n"),
			   prv.OwnerPkg().Name(), pkg.Name());
		  return true;
		}
	    }

	  // See if there's only one possible package to install.
	  pkgvector cands;

	  for(pkgCache::PrvIterator prv=pkg.ProvidesList();
	      !prv.end(); ++prv)
	    {
	      if((*apt_cache_file)[prv.OwnerPkg()].CandidateVerIter(*apt_cache_file)==prv.OwnerVer())
		cands.push_back(prv.OwnerPkg());
	    }

	  if(cands.size()==0)
	    {
	      printf(_("\"%s\" exists in the package database, but it is not a\nreal package and no package provides it.\n"),
		     pkg.Name());
	      return false;
	    }
	  else if(cands.size()>1)
	    {
	      printf(_("\"%s\" is a virtual package provided by:\n"),
		     pkg.Name());
	      cmdline_show_pkglist(cands);
	      printf(_("You must choose one to install.\n"));
	      return false;
	    }
	  else if(cands.size()==1)
	      {
		printf(_("Note: selecting \"%s\" instead of the\n      virtual package \"%s\"\n"),
		       cands[0].Name(), pkg.Name());
		pkg=cands[0];
	      }
 	}
    }

  pkgCache::VerIterator ver=pkg.CurrentVer();
  if(action==cmdline_install)
    ver=cmdline_find_ver(pkg, source, sourcestr);

  switch(action)
    {
    case cmdline_installauto:
    case cmdline_install:
      if(pkg.CurrentVer()!=ver || pkg->CurrentState!=pkgCache::State::Installed)
	to_install.insert(pkg);
      else if((*apt_cache_file)[pkg].Keep() && verbose>0)
	printf(_("%s is already installed at the requested version (%s)\n"),
	       pkg.Name(),
	       ver.VerStr());
      break;
    case cmdline_reinstall:
      if(pkg.CurrentVer().end())
	printf(_("%s is not currently installed, so it will not be reinstalled.\n"), pkg.Name());

      break;
    case cmdline_remove:
      if(!pkg.CurrentVer().end())
	to_remove.insert(pkg);
      else if((*apt_cache_file)[pkg].Keep() && verbose>0)
	printf(_("Package %s is not installed, so it will not be removed\n"), pkg.Name());
      break;
    case cmdline_purge:
      if(!pkg.CurrentVer().end() || pkg->CurrentState!=pkgCache::State::ConfigFiles)
	to_purge.insert(pkg);
      else if((*apt_cache_file)[pkg].Keep() && verbose>0)
	printf(_("Package %s is not installed, so it will not be removed\n"), pkg.Name());
      break;
    case cmdline_hold:
      to_hold.insert(pkg);
      break;
    case cmdline_keep:
      to_install.erase(pkg);
      to_remove.erase(pkg);
      to_purge.erase(pkg);
      to_hold.erase(pkg);
      break;
    case cmdline_unhold:
      to_hold.erase(pkg);
      break;
    case cmdline_forbid_version:
      if(source==cmdline_version_cand)
	{
	  if(pkg.CurrentVer().end())
	    printf(_("Package %s is not installed, cannot forbid an upgrade\n"), pkg.Name());
	  else if(!(*apt_cache_file)[pkg].Upgradable())
	    printf(_("Package %s is not upgradable, cannot forbid an upgrade\n"), pkg.Name());
	}
    default:
      break;
    }

  switch(action)
    {
    case cmdline_installauto:
    case cmdline_install:
    case cmdline_reinstall:
      if(action==cmdline_reinstall && pkg.CurrentVer().end())
	break;

      (*apt_cache_file)->set_candidate_version(ver, NULL);
      (*apt_cache_file)->mark_install(pkg, allow_auto && aptcfg->FindB(PACKAGE "::Auto-Install", true),
				      action == cmdline_reinstall, NULL);
      if(action == cmdline_installauto)
	(*apt_cache_file)->mark_auto_installed(pkg, true, NULL);
      break;
    case cmdline_remove:
      (*apt_cache_file)->mark_delete(pkg, false, false, NULL);
      break;
    case cmdline_purge:
      (*apt_cache_file)->mark_delete(pkg, true, false, NULL);
      break;
    case cmdline_hold:
      (*apt_cache_file)->mark_keep(pkg, false, true, NULL);
      break;
    case cmdline_keep:
      (*apt_cache_file)->mark_keep(pkg, false, false, NULL);
      break;
    case cmdline_unhold:
      if(pkg->CurrentState==pkgCache::State::Installed)
	(*apt_cache_file)->mark_install(pkg, allow_auto && aptcfg->FindB(PACKAGE "::Auto-Install", true),
					false, NULL);
      else
	(*apt_cache_file)->mark_keep(pkg, false, false, NULL);
      break;
    case cmdline_markauto:
      (*apt_cache_file)->mark_auto_installed(pkg, true, NULL);
      break;
    case cmdline_unmarkauto:
      (*apt_cache_file)->mark_auto_installed(pkg, false, NULL);
      break;
    case cmdline_forbid_version:
      if(source!=cmdline_version_cand)
	(*apt_cache_file)->forbid_upgrade(pkg, sourcestr, NULL);
      else
	{
	  pkgCache::VerIterator curver=pkg.CurrentVer();
	  pkgCache::VerIterator candver=(*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);
	  if(!curver.end() && !candver.end() && curver!=candver)
	    (*apt_cache_file)->forbid_upgrade(pkg, candver.VerStr(), NULL);
	}
      break;
    default:
      fprintf(stderr, "Internal error: impossible pkgaction type\n");
      abort();
    }

  return true;
}

bool cmdline_applyaction(string s,
			 cmdline_pkgaction_type action,
			 pkgset &to_install, pkgset &to_hold,
			 pkgset &to_remove, pkgset &to_purge,
			 int verbose,
			 bool allow_auto)
{
  bool rval=true;

  cmdline_version_source source=cmdline_version_cand;

  string sourcestr, package;

  // Handle task installation.  Won't work if tasksel isn't installed.
  if(task_list->find(s)!=task_list->end())
    {
      task t=(*task_list)[s];

      printf(_("Note: selecting the task \"%s: %s\" for installation\n"),
	     s.c_str(), t.shortdesc.c_str());

      for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin();
	  !pkg.end(); ++pkg)
	{
	  std::list<std::string> *tasks=get_tasks(pkg);

	  for(std::list<std::string>::iterator i=tasks->begin();
	      i!=tasks->end(); ++i)
	    if(*i==s)
	      rval=cmdline_applyaction(action, pkg,
				       to_install, to_hold, to_remove, to_purge,
				       verbose, source,
				       sourcestr, allow_auto) && rval;
	}

      // break out.
      return rval;
    }

  if(!cmdline_parse_source(s, source, package, sourcestr))
    return false;

  // This is harmless for other commands, but it won't make sense.
  if(source == cmdline_version_version &&
     action != cmdline_install &&
     action != cmdline_forbid_version &&
     action != cmdline_installauto)
    {
      printf(_("You can only specify a package version with an 'install' command or a 'forbid-version' command.\n"));
      return false;
    }

  if(source == cmdline_version_archive &&
     action != cmdline_install &&
     action != cmdline_installauto)
    {
      printf(_("You can only specify a package archive with an 'install' command.\n"));
      return false;
    }

  if(package.find('~')==package.npos)
    {
      pkgCache::PkgIterator pkg=(*apt_cache_file)->FindPkg(package.c_str());
      if(pkg.end())
	{
	  // Maybe they misspelled the package name?
	  pkgvector possible;
	  pkg_matcher *m=parse_pattern(package);

	  if(!m)
	    {
	      _error->Error("Badly formed pattern %s", package.c_str());
	      _error->DumpErrors();

	      return false;
	    }

	  for(pkgCache::PkgIterator j=(*apt_cache_file)->PkgBegin();
	      !j.end(); ++j)
	    {
	      if(!(j.VersionList().end() && j.ProvidesList().end()) && m->matches(j))
		possible.push_back(j);
	    }

	  delete m;

	  if(!possible.empty())
	    {
	      // Don't overwhelm the user.
	      if(possible.size()>40)
		printf(_("Couldn't find package \"%s\", and more than 40\npackages contain \"%s\" in their name.\n"), package.c_str(), package.c_str());
	      else
		{
		  printf(_("Couldn't find package \"%s\".  However, the following\npackages contain \"%s\" in their name:\n"), package.c_str(), package.c_str());
		  cmdline_show_pkglist(possible);
		}
	    }
	  else
	    {
	      m=parse_pattern("~d"+s);

	      if(!m)
		{
		  _error->DumpErrors();
		  return false;
		}

	      for(pkgCache::PkgIterator j=(*apt_cache_file)->PkgBegin();
		  !j.end(); ++j)
		{
		  if(m->matches(j))
		    possible.push_back(j);
		}

	      delete m;

	      if(possible.empty())
		printf(_("Couldn't find any package whose name or description matched \"%s\"\n"), package.c_str());
	      else if(possible.size()>40)
		printf(_("Couldn't find any package matching \"%s\", and more than 40\npackages contain \"%s\" in their description.\n"), package.c_str(), package.c_str());
	      else
		{
		  printf(_("Couldn't find any package matching \"%s\".  However, the following\npackages contain \"%s\" in their description:\n"), package.c_str(), package.c_str());
		  cmdline_show_pkglist(possible);
		}
	    }

	  return false;
	}

      rval=cmdline_applyaction(action, pkg,
			       to_install, to_hold, to_remove, to_purge,
			       verbose, source,
			       sourcestr, allow_auto);
    }
  else
    {
      pkg_matcher *m=parse_pattern(package.c_str());
      if(!m)
	{
	  _error->DumpErrors();
	  return false;
	}

      for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin();
	  !pkg.end(); ++pkg)
	{
	  pkgCache::VerIterator testver;

	  if(m->matches(pkg))
	    rval=cmdline_applyaction(action, pkg,
				     to_install, to_hold, to_remove, to_purge,
				     verbose, source,
				     sourcestr, allow_auto) && rval;
	}

      delete m;
    }

  return rval;
}

/** \return \b false iff the beginning of s doesn't look like an
 *  action specifier.
 */
static bool parse_action_str(const string &s,
			     string::size_type &loc,
			     cmdline_pkgaction_type &action)
{
  if(loc<s.size())
    {
      switch(s[loc])
	{
	case '_':
	  action=cmdline_purge;
	  ++loc;
	  break;
	case '-':
	  action=cmdline_remove;
	  ++loc;
	  break;
	case '=':
	  action=cmdline_hold;
	  ++loc;
	  break;
	case '+':
	  action=cmdline_install;
	  ++loc;
	  if(loc<s.size() && s[loc]=='M')
	    {
	      action=cmdline_installauto;
	      ++loc;
	    }
	  break;
	case ':':
	  action=cmdline_keep;
	  ++loc;
	  break;
	case '&':
	  ++loc;
	  if(loc<s.size())
	    switch(s[loc])
	      {
	      case 'M':
		action=cmdline_markauto;
		++loc;
		break;
	      case 'm':
		action=cmdline_unmarkauto;
		++loc;
		break;
	      default:
		return false;
	      }
	  break;
	default:
	  return false;
	}
      return true;
    }
  else
    return false;
}

void cmdline_parse_action(string s,
			  pkgset &to_install, pkgset &to_hold,
			  pkgset &to_remove, pkgset &to_purge,
			  int verbose,
			  bool allow_auto)
{
  string::size_type loc=0;

  while(loc<s.size() && isspace(s[loc]))
    ++loc;

  if(loc==s.size())
    return;

  cmdline_pkgaction_type action;

  if(!parse_action_str(s, loc, action))
    {
      printf(_("Bad action character '%c'\n"), s[0]);
      return;
    }

  while(loc<s.size())
    {
      while(loc<s.size() && isspace(s[loc]))
	++loc;

      if(loc<s.size())
	{
	  if(!parse_action_str(s, loc, action))
	    {
	      string pkgname;
	      while(loc<s.size() && !isspace(s[loc]))
		pkgname+=s[loc++];

	      if(!cmdline_applyaction(pkgname, action,
				      to_install, to_hold,
				      to_remove, to_purge,
				      verbose, allow_auto))
		return;
	    }
	}
    }
}
