// aptcache.cc
//
//  Copyright 1999-2007 Daniel Burrows
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

#include "aptcache.h"

#include <aptitude.h>

#include "apt.h"
#include "aptitude_resolver_universe.h"
#include "aptitudepolicy.h"
#include "config_signal.h"
#include "matchers.h"
#include <generic/problemresolver/solution.h>
#include <generic/util/undo.h>

#include <apt-pkg/error.h>
#include <apt-pkg/sourcelist.h>
#include <apt-pkg/pkgcachegen.h>
#include <apt-pkg/configuration.h>
#include <apt-pkg/tagfile.h>
#include <apt-pkg/fileutl.h>
#include <apt-pkg/algorithms.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/policy.h>
#include <apt-pkg/version.h>

#include <vector>

#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>

#include <generic/util/eassert.h>

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>

using namespace std;

class aptitudeDepCache::apt_undoer:public undoable
// Allows an action performed on the package cache to be undone.  My first
// thought was to just snapshot the package cache before the action and then
// copy over the state, but this works better (IMO :) ) -- the other method
// had the drawback of not telling what was actually happening, and potentially
// leaving bits of the cache in weird states.
//
// Of course, there's always the danger that this won't properly restore the
// cache state, so I'll have to revert to the original method..
{
  PkgIterator pkg;
  int prev_mode;  // One of Delete,Keep,Install
  int prev_iflags, prev_flags;
  changed_reason prev_removereason;
  pkgCache::State::PkgSelectedState prev_selection_state;

  string prev_forbidver;

  aptitudeDepCache *owner;
public:
  apt_undoer(PkgIterator _pkg, int _prev_mode, int _prev_flags, int _prev_iflags,
	     changed_reason _prev_removereason,
	     pkgCache::State::PkgSelectedState _prev_selection_state,
	     string _prev_forbidver,
	     aptitudeDepCache *_owner)
    :pkg(_pkg), prev_mode(_prev_mode), prev_iflags(_prev_iflags), prev_flags(_prev_flags),
     prev_removereason(_prev_removereason),
     prev_selection_state(_prev_selection_state),
     prev_forbidver(_prev_forbidver),
     owner(_owner)
  {
  }

  void undo()
  {
    aptitudeDepCache::action_group group(*owner);

    if(prev_iflags&ReInstall)
      owner->internal_mark_install(pkg, false, true);
    else switch(prev_mode)
      {
      case ModeDelete:
	// the unused_delete parameter isn't that important..
	owner->internal_mark_delete(pkg, prev_iflags & Purge, prev_removereason == unused);
	break;
      case ModeKeep:
	owner->internal_mark_keep(pkg, prev_iflags & AutoKept, prev_selection_state == pkgCache::State::Hold);
	break;
      case ModeInstall:
	owner->internal_mark_install(pkg, false, false);
	break;
      }

    // make sure that everything is really set.
    owner->MarkAuto(pkg, (prev_flags & Flag::Auto));
    owner->get_ext_state(pkg).remove_reason=prev_removereason;
    owner->get_ext_state(pkg).forbidver=prev_forbidver;
  }
};

extern sigc::signal0<void> cache_reloaded;

class aptitudeDepCache::forget_undoer:public undoable
// Undoes a "forget_new" command
{
  vector<pkgCache::PkgIterator> packages;

  aptitudeDepCache *owner;
public:
  forget_undoer(aptitudeDepCache *_owner):owner(_owner) {}

  void add_item(pkgCache::PkgIterator item)
  {
    packages.push_back(item);
  }

  bool empty()
  {
    return packages.empty();
  }

  void undo()
  {
    for(vector<pkgCache::PkgIterator>::iterator i=packages.begin(); i!=packages.end(); i++)
      owner->set_new_flag(*i, true);

    // Hack to make all the trees rebuild themselves.
    cache_reloaded();
  }
};

class aptitudeDepCache::candver_undoer:public undoable
// Undoes a "set candidate version" command.
{
  pkgCache::VerIterator oldver;

  aptitudeDepCache *owner;
public:
  candver_undoer(pkgCache::VerIterator _oldver,
		 aptitudeDepCache *_owner)
    :oldver(_oldver), owner(_owner)
  {
  }

  void undo()
  {
    owner->set_candidate_version(oldver, NULL);
  }
};

aptitudeDepCache::action_group::action_group(aptitudeDepCache &cache, undo_group *group)
  : parent_group(new pkgDepCache::ActionGroup(cache)),
    cache(cache), group(group)
{
  cache.begin_action_group();
}

aptitudeDepCache::action_group::~action_group()
{
  // Force the parent to mark-and-sweep first.
  delete parent_group;

  cache.end_action_group(group);
}

aptitudeDepCache::aptitudeDepCache(pkgCache *Cache, Policy *Plcy)
  :pkgDepCache(Cache, Plcy), dirty(false), read_only(true),
   package_states(NULL), lock(-1), group_level(0)
{
  // When the "install recommended packages" flag changes, collect garbage.
#if 0
  aptcfg->connect(PACKAGE "::Recommends-Important",
		  sigc::bind(sigc::mem_fun(*this,
					   &pkgDepCache::MarkAndSweep),
			     (undo_group *) NULL));

  aptcfg->connect(PACKAGE "::Keep-Recommends",
		  sigc::bind(sigc::mem_fun(*this,
					   &pkgDepCache::MarkAndSweep),
			     (undo_group *) NULL));

  // sim.
  aptcfg->connect(PACKAGE "::Keep-Suggests",
		  sigc::bind(sigc::mem_fun(*this,
					   &pkgDepCache::MarkAndSweep),
			     (undo_group *) NULL));
#endif
}

bool aptitudeDepCache::Init(OpProgress *Prog, bool WithLock, bool do_initselections, const char *status_fname)
{
  return build_selection_list(*Prog, WithLock, do_initselections, status_fname);
}

aptitudeDepCache::~aptitudeDepCache()
{
  delete[] package_states;

  if(lock!=-1)
    close(lock);
}

void aptitudeDepCache::set_read_only(bool new_read_only)
{
  read_only = new_read_only;
}

bool aptitudeDepCache::build_selection_list(OpProgress &Prog, bool WithLock,
					    bool do_initselections,
					    const char *status_fname)
{
  action_group group(*this);

  bool initial_open=false;
  // This will be set to true if the state file does not exist.

  if(!pkgDepCache::Init(&Prog))
    return false;

  string statedir=aptcfg->FindDir("Dir::Aptitude::state", STATEDIR);
  // Should this not go under Dir:: ?  I'm not sure..
  delete package_states;
  package_states=new aptitude_state[Head().PackageCount];
  for(unsigned int i=0; i<Head().PackageCount; i++)
    {
      package_states[i].new_package=true;
      package_states[i].reinstall=false;
      package_states[i].remove_reason=manual;
      package_states[i].selection_state = pkgCache::State::Unknown;
    }

  if(WithLock && lock==-1)
    {
      lock = GetLock(aptcfg->Find("Aptitude::LockFile", LOCKFILE));

      if(_error->PendingError())
	{
	  if(lock!=-1)
	    close(lock);
	  lock=-1;
	  read_only = true;
	  return false;
	}
    }
  // Errrrr, I think we need to do this first in case the stuff below
  // manages to trigger a mark operation.
  duplicate_cache(&backup_state);

  FileFd state_file;

  // Read in the states that we saved
  if(status_fname==NULL)
    state_file.Open(statedir+"pkgstates", FileFd::ReadOnly);
  else
    state_file.Open(status_fname, FileFd::ReadOnly);

  // Have to make the file NOT read-only to set up the initial state.
  read_only = false;

  if(!state_file.IsOpen())
    {
      _error->Discard();
      if(errno!=ENOENT)
	_error->Warning(_("Can't open Aptitude extended state file"));
      else
	initial_open=true;
    }
  else
    {
      int file_size=state_file.Size();
      Prog.OverallProgress(0, file_size, 1, _("Reading extended state information"));

      pkgTagFile tagfile(&state_file);
      pkgTagSection section;
      int amt=0;
      bool do_dselect=aptcfg->FindB(PACKAGE "::Track-Dselect-State", true);
      while(tagfile.Step(section))
	{
	  PkgIterator pkg=FindPkg(section.FindS("Package"));
	  if(!pkg.end() && !pkg.VersionList().end())
	    // Silently ignore unknown packages and packages with no actual
	    // version.
	    {
	      unsigned long tmp=0;
	      string candver;

	      aptitude_state &pkg_state=get_ext_state(pkg);

	      section.FindFlag("Unseen", tmp, 1);

	      pkg_state.new_package=(tmp==1);

	      tmp=0;
	      section.FindFlag("Upgrade", tmp, 1);
	      pkg_state.upgrade=(tmp==1);

	      // The install reason is much more important to preserve
	      // from previous versions, so support the outdated name
	      // for it.
	      changed_reason install_reason=(changed_reason)
		section.FindI("Install-Reason",
			      section.FindI("Last-Change", manual));

	      if(install_reason != manual)
		{
		  MarkAuto(pkg, true);
		  dirty = true;
		}

	      pkg_state.remove_reason=(changed_reason)
		section.FindI("Remove-Reason", manual);

	      candver=section.FindS("Version");

	      pkg_state.selection_state=(pkgCache::State::PkgSelectedState) section.FindI("State", pkgCache::State::Unknown);
	      pkgCache::State::PkgSelectedState last_dselect_state
		= (pkgCache::State::PkgSelectedState)
		    section.FindI("Dselect-State", pkg->SelectedState);
	      pkg_state.candver=candver;
	      pkg_state.forbidver=section.FindS("ForbidVer");

	      if(do_dselect && pkg->SelectedState != last_dselect_state)
		{
		  MarkFromDselect(pkg);
		  // dirty should be set to "true" so that we update
		  // the on-disk dselect state ASAP, even if no
		  // package states change as a result.
		  dirty=true;

		  // We need to update the package state from the
		  // dselect state regardless of whether we're doing
		  // initselections.  This is so that, e.g., if the
		  // user installed a package outside aptitude (so the
		  // dselect state says to install it), our internal
		  // state isn't left at "remove".  But if we aren't
		  // supposed to set up stored installs/removals, we
		  // should cancel this at the apt-get level (so the
		  // package doesn't get changed if dselect said to
		  // install it, but this isn't stored in our database
		  // for future runs).
		  //
		  // In the past, we skipped doing MarkFromDselect in
		  // this case.  BAD.
		  if(!do_initselections)
		    MarkKeep(pkg, false);
		}
	    }
	  amt+=section.size();
	  Prog.OverallProgress(amt, file_size, 1, _("Reading extended state information"));
	}
      Prog.OverallProgress(file_size, file_size, 1, _("Reading extended state information"));
    }

  int num=0;

  Prog.OverallProgress(0, Head().PackageCount, 1, _("Initializing package states"));

  new_package_count=0;

  pre_package_state_changed();

  // Act on them
  for(pkgCache::PkgIterator i=PkgBegin(); !i.end(); i++)
    {
      StateCache &state=(*this)[i];
      aptitude_state &estate=get_ext_state(i);

      if(initial_open) // Don't make everything "new".
	estate.new_package=false;
      else if(!i.VersionList().end() && estate.new_package)
	++new_package_count;

      switch(estate.selection_state)
	{
	case pkgCache::State::Unknown:
	  if(i.CurrentVer().end())
	    estate.selection_state=pkgCache::State::DeInstall;
	  else
	    estate.selection_state=pkgCache::State::Install;
	  break;
	case pkgCache::State::Install:
	  if(!do_initselections)
	    break;

	  // FIXME: should I check this for "unknown" packages as well?
	  // Does that even make sense??
	  if(!estate.candver.empty())
	    {
	      for(pkgCache::VerIterator ver=i.VersionList(); !ver.end(); ++ver)
		if(ver.VerStr()==estate.candver &&
		   (ver.Downloadable() ||
		    (ver == ver.ParentPkg().CurrentVer() &&
		     ver.ParentPkg()->CurrentState != pkgCache::State::ConfigFiles)))
		  SetCandidateVersion(ver);

	      MarkInstall(i, false);
	    }
	  else
	    if(i.CurrentVer().end())
	      MarkInstall(i, false);
	    else
	      {
		SetReInstall(i, estate.reinstall);

		if(estate.upgrade && state.Upgradable())
		  MarkInstall(i, false);
	      }
	  break;
	case pkgCache::State::Hold:
	  if(!do_initselections)
	    break;

	  MarkKeep(i, false);
	  break;
	case pkgCache::State::DeInstall:
	  if(!do_initselections)
	    break;

	  if(!i.CurrentVer().end())
	    MarkDelete(i, false);
	  break;
	case pkgCache::State::Purge:
	  if(!do_initselections)
	    break;

	  if(!i.CurrentVer().end())
	    MarkDelete(i, true);
	  break;
	}

      ++num;
      Prog.OverallProgress(num, Head().PackageCount, 1, _("Initializing package states"));
    }

  Prog.OverallProgress(Head().PackageCount, Head().PackageCount, 1, _("Initializing package states"));

  duplicate_cache(&backup_state);

  if(aptcfg->FindB(PACKAGE "::Auto-Upgrade", false) && do_initselections)
    mark_all_upgradable(aptcfg->FindB(PACKAGE "::Auto-Install", true),
			true, NULL);
  Prog.Done();

  read_only = (lock == -1);

  return true;
}

void aptitudeDepCache::mark_all_upgradable(bool with_autoinst,
					   bool ignore_removed,
					   undo_group *undo)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return;
    }

  action_group group(*this, undo);

  for(int iter=0; iter==0 || (iter==1 && with_autoinst); ++iter)
    {
      // Do this twice, only turning auto-install on the second time.
      // A reason for this is the following scenario:
      //
      // Packages A and B are installed at 1.0.  Package C is not installed.
      // Version 2.0 of each package is available.
      //
      // Version 2.0 of A depends on "C (= 2.0) | B (= 2.0)".
      //
      // Upgrading A if B is not upgraded will cause this dependency to
      // break.  Auto-install will then cheerfully fulfill it by installing
      // C.
      //
      // A real-life example of this is xemacs21, xemacs21-mule, and
      // xemacs21-nomule; aptitude would keep trying to install the mule
      // version on upgrades.
      bool do_autoinstall=(iter==1);

      for(pkgCache::PkgIterator i=PkgBegin(); !i.end(); i++)
	{
	  StateCache &state=(*this)[i];
	  aptitude_state &estate=get_ext_state(i);

	  if(i.CurrentVer().end())
	    continue;

	  bool do_upgrade = false;

	  if(!ignore_removed)
	    do_upgrade = state.Status > 0 && !is_held(i);
	  else
	    {
	      switch(estate.selection_state)
		{
		  // This case shouldn't really happen:
		case pkgCache::State::Unknown:
		  estate.selection_state=pkgCache::State::Install;

		  // Fall through
		case pkgCache::State::Install:
		  if(state.Status > 0 && !is_held(i))
		    do_upgrade = true;
		  break;
		default:
		  break;
		}
	    }

	  if(do_upgrade)
	    {
	      pre_package_state_changed();
	      dirty = true;

	      MarkInstall(i, do_autoinstall);
	    }
	}
    }
}

// If fd is -1, just write unconditionally to the given fd.
//
//  FIXME: clean up the logic by having an internal "write to this fd"
// routine and an exported "ok, set up for the write and then clean up"
// routine.
bool aptitudeDepCache::save_selection_list(OpProgress &prog,
					   const char *status_fname)
{
  // Refuse to write to disk if nothing changed and we aren't writing
  // to an unusual file
  if(!dirty && !status_fname)
    return true;

  if(lock==-1 && !status_fname)
    return true;

  writeStateFile(&prog);

  string statefile=_config->FindDir("Dir::Aptitude::state", STATEDIR)+"pkgstates";

  FileFd newstate;

  if(!status_fname)
    newstate.Open(statefile+".new", FileFd::WriteEmpty, 0644);
  else
    newstate.Open(status_fname, FileFd::WriteEmpty, 0644);

  // The user might have a restrictive umask -- make sure we get a
  // mode 644 file.
  fchmod(newstate.Fd(), 0644);

  if(!newstate.IsOpen())
    _error->Error(_("Cannot open Aptitude state file"));
  else
    {
      int num=0;
      prog.OverallProgress(0, Head().PackageCount, 1, _("Writing extended state information"));

      for(PkgIterator i=PkgBegin(); !i.end(); i++)
	if(!i.VersionList().end())
	  {
	    StateCache &state=(*this)[i];
	    aptitude_state &estate=get_ext_state(i);
	    char buf[400];
	    int len;

	    string forbidstr=!estate.forbidver.empty()
	      ? "ForbidVer: "+estate.forbidver+"\n":"";

	    bool upgrade=(!i.CurrentVer().end()) && state.Install();
	    string upgradestr=upgrade ? "Upgrade: yes\n" : "";

	    string tailstr;

	    if(state.Install() &&
	       !estate.candver.empty() &&
	       (GetCandidateVer(i).end() ||
		GetCandidateVer(i).VerStr() != estate.candver))
	      tailstr = "Version: " + estate.candver + "\n";

	    len=snprintf(buf,
			 400,
			 "Package: %s\nUnseen: %s\nState: %i\nDselect-State: %i\nRemove-Reason: %i\n%s%s%s\n",
			 i.Name(),
			 estate.new_package?"yes":"no",
			 estate.selection_state,
			 i->SelectedState,
			 estate.remove_reason,
			 upgradestr.c_str(),
			 forbidstr.c_str(),
			 tailstr.c_str());
	    if(len>=399)
	      {
		_error->Error(_("Internal buffer overflow on package \"%s\" while writing state file"), i.Name());
		newstate.Close();

		if(!status_fname)
		  unlink((statefile+".new").c_str());
		return false;
	      }
	    if(newstate.Failed() || !newstate.Write(buf, len))
	      {
		_error->Error(_("Couldn't write state file"));
		newstate.Close();

		if(!status_fname)
		  unlink((statefile+".new").c_str());
		return false;
	      }

	    num++;
	    prog.OverallProgress(num, Head().PackageCount, 1, _("Writing extended state information"));
	  }

      prog.OverallProgress(Head().PackageCount, Head().PackageCount, 1, _("Writing extended state information"));

      if(newstate.Failed())
	// This is /probably/ redundant, but paranoia never hurts.
	{
	  _error->Error(_("Error writing state file"));
	  newstate.Close();

	  if(!status_fname)
	    unlink((statefile+".new").c_str());

	  prog.Done();
	  return false;
	}
      newstate.Close();
      // FIXME!  This potentially breaks badly on NFS.. (?) -- actually, it
      //       wouldn't be harmful; you'd just get gratuitous errors..
      if(!status_fname)
	{
	  string oldstr(statefile + ".old"), newstr(statefile + ".new");

	  if(unlink(oldstr.c_str()) != 0 && errno != ENOENT)
	    {
	      _error->Errno("save_selection_list", _("failed to remove %s"), oldstr.c_str());
	      prog.Done();
	      return false;
	    }

	  if(link(statefile.c_str(), oldstr.c_str()) != 0 && errno != ENOENT)
	    {
	      _error->Errno("save_selection_list", _("failed to rename %s to %s"),
			    statefile.c_str(), (statefile + ".old").c_str());
	      prog.Done();
	      return false;
	    }

	  if(rename(newstr.c_str(), statefile.c_str()) != 0)
	    {
	      _error->Errno("save_selection_list", _("couldn't replace %s with %s"), statefile.c_str(), newstr.c_str());
	      prog.Done();
	      return false;
	    }
	}
    }

  prog.Done();
  return true;
}

void aptitudeDepCache::set_new_flag(const pkgCache::PkgIterator &pkg,
				    bool is_new)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return;
    }

  aptitude_state &estate=get_ext_state(pkg);

  if(estate.new_package && !is_new)
    {
      --new_package_count;
      estate.new_package=is_new;
    }
  else if(!estate.new_package && is_new)
    {
      ++new_package_count;
      estate.new_package=is_new;
    }
}

void aptitudeDepCache::forget_new(undoable **undoer)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return;
    }

  forget_undoer *undo=undoer?new forget_undoer(this):NULL;

  for(pkgCache::PkgIterator i=PkgBegin(); !i.end(); i++)
    if(package_states[i->ID].new_package)
      {
	dirty=true;
	package_states[i->ID].new_package=false;
	if(undo)
	  undo->add_item(i);
      }

  new_package_count=0;

  if(undoer && undo && !undo->empty())
    *undoer=undo;
  else
    delete undo;

  duplicate_cache(&backup_state);

  // Umm, is this a hack? dunno.
  cache_reloaded();
}

undoable *aptitudeDepCache::state_restorer(PkgIterator pkg, StateCache &state, aptitude_state &ext_state)
{
  return new apt_undoer(pkg, state.Mode, state.Flags, state.iFlags,
			ext_state.remove_reason,
			ext_state.selection_state,
			ext_state.forbidver, this);
}

void aptitudeDepCache::cleanup_after_change(undo_group *undo, bool alter_stickies)
  // Finds any packages whose states have changed and: (a) updates the
  // selected_state if it's not already updated; (b) adds an item to the
  // undo group.
{
  for(pkgCache::PkgIterator pkg=PkgBegin(); !pkg.end(); pkg++)
    {
      if(PkgState[pkg->ID].Mode!=backup_state.PkgState[pkg->ID].Mode ||
	 (PkgState[pkg->ID].Flags & pkgCache::Flag::Auto) != (backup_state.PkgState[pkg->ID].Flags & pkgCache::Flag::Auto) ||
	 package_states[pkg->ID].selection_state!=backup_state.AptitudeState[pkg->ID].selection_state ||
	 package_states[pkg->ID].reinstall!=backup_state.AptitudeState[pkg->ID].reinstall ||
	 package_states[pkg->ID].remove_reason!=backup_state.AptitudeState[pkg->ID].remove_reason ||
	 package_states[pkg->ID].forbidver!=backup_state.AptitudeState[pkg->ID].forbidver)
	{
	  int n=pkg->ID;
	  n=n;
	  char curM=PkgState[pkg->ID].Mode;
	  curM=curM;
	  char oldM=backup_state.PkgState[pkg->ID].Mode;
	  oldM=oldM;

	  if(alter_stickies &&
	     PkgState[pkg->ID].Mode!=backup_state.PkgState[pkg->ID].Mode &&
	     package_states[pkg->ID].selection_state==backup_state.AptitudeState[pkg->ID].selection_state)
	    // Catch packages which switched without altering their Aptitude
	    // selection mode
	    {
	      switch(PkgState[pkg->ID].Mode)
		{
		case ModeDelete:
		  if(package_states[pkg->ID].selection_state!=pkgCache::State::DeInstall)
		    {
		      if(!pkg.CurrentVer().end())
			package_states[pkg->ID].remove_reason=libapt;

		      package_states[pkg->ID].selection_state=pkgCache::State::DeInstall;
		    }
		  break;
		case ModeKeep:
		  if(!pkg.CurrentVer().end())
		    package_states[pkg->ID].selection_state=pkgCache::State::Install;
		  else if(pkg->CurrentState==pkgCache::State::NotInstalled)
		    package_states[pkg->ID].selection_state=pkgCache::State::Purge;
		  else
		    package_states[pkg->ID].selection_state=pkgCache::State::DeInstall;
		  break;
		case ModeInstall:
		  if(package_states[pkg->ID].selection_state!=pkgCache::State::Install)
		    package_states[pkg->ID].selection_state=pkgCache::State::Install;
		  break;
		}
	    }

	  if(undo)
	    undo->add_item(state_restorer(pkg,
					  backup_state.PkgState[pkg->ID],
					  backup_state.AptitudeState[pkg->ID]));
	}
    }
}

void aptitudeDepCache::mark_install(const PkgIterator &Pkg,
				    bool AutoInst,
				    bool ReInstall,
				    undo_group *undo)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return;
    }

  action_group group(*this, undo);

  pre_package_state_changed();

  internal_mark_install(Pkg, AutoInst, ReInstall);
}

void aptitudeDepCache::internal_mark_install(const PkgIterator &Pkg,
					     bool AutoInst,
					     bool ReInstall)
{
  dirty=true;


  bool set_to_manual =
    ((Pkg.CurrentVer().end()  && !(*this)[Pkg].Install()) ||
     (!Pkg.CurrentVer().end() && (*this)[Pkg].Delete() && get_ext_state(Pkg).remove_reason == unused));

  if(set_to_manual)
    MarkAuto(Pkg, false);


  if(!ReInstall)
    {
      pkgDepCache::MarkInstall(Pkg, AutoInst);
    }
  else
    pkgDepCache::MarkKeep(Pkg, AutoInst);

  pkgDepCache::SetReInstall(Pkg, ReInstall);

  get_ext_state(Pkg).selection_state=pkgCache::State::Install;
  get_ext_state(Pkg).reinstall=ReInstall;
  get_ext_state(Pkg).forbidver="";
}

void aptitudeDepCache::mark_delete(const PkgIterator &Pkg,
				   bool Purge,
				   bool unused_delete,
				   undo_group *undo)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return;
    }

  action_group group(*this, undo);

  pre_package_state_changed();

  internal_mark_delete(Pkg, Purge, unused_delete);
}

void aptitudeDepCache::internal_mark_delete(const PkgIterator &Pkg,
					    bool Purge,
					    bool unused_delete)
{
  dirty=true;

  bool previously_to_delete=(*this)[Pkg].Delete();

  pkgDepCache::MarkDelete(Pkg, Purge);
  pkgDepCache::SetReInstall(Pkg, false);

  get_ext_state(Pkg).selection_state=(Purge?pkgCache::State::Purge:pkgCache::State::DeInstall);
  get_ext_state(Pkg).reinstall=false;

  if(!previously_to_delete)
    {
      if(unused_delete)
	get_ext_state(Pkg).remove_reason=unused;
      else
	get_ext_state(Pkg).remove_reason=manual;
    }
}

void aptitudeDepCache::mark_keep(const PkgIterator &Pkg, bool Automatic, bool SetHold, undo_group *undo)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return;
    }

  action_group group(*this, undo);

  pre_package_state_changed();

  internal_mark_keep(Pkg, Automatic, SetHold);
}

void aptitudeDepCache::internal_mark_keep(const PkgIterator &Pkg, bool Automatic, bool SetHold)
{
  dirty=true;


  // If the package is currently installed and is being garbage
  // collected, switch it to manual mode.  We check
  // pkg.CurrentVer().end() to avoid fiddling with packages that are
  // in the ConfigFiles state and are being purged.
  bool was_garbage_removed =
    (*this)[Pkg].Delete() &&
    !Pkg.CurrentVer().end() &&
    get_ext_state(Pkg).remove_reason == unused;

  if(was_garbage_removed)
    MarkAuto(Pkg, false);


  pkgDepCache::MarkKeep(Pkg, false, !Automatic);
  pkgDepCache::SetReInstall(Pkg, false);
  get_ext_state(Pkg).reinstall=false;

  if(Pkg.CurrentVer().end())
    {
      if((*this)[Pkg].iFlags&Purge)
	get_ext_state(Pkg).selection_state=pkgCache::State::Purge;
      else
	get_ext_state(Pkg).selection_state=pkgCache::State::DeInstall;
    }
  else if(SetHold)
    get_ext_state(Pkg).selection_state=pkgCache::State::Hold;
  else
    get_ext_state(Pkg).selection_state=pkgCache::State::Install;
}

void aptitudeDepCache::set_candidate_version(const VerIterator &ver,
					     undo_group *undo)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return;
    }

  dirty=true;

  if(!ver.end() &&
     (ver.Downloadable() ||
      (ver == ver.ParentPkg().CurrentVer() &&
       ver.ParentPkg()->CurrentState != pkgCache::State::ConfigFiles)))
    {
      pre_package_state_changed();


      // Make the package manually installed if it was being
      // garbage-collected.
      bool set_to_manual =
	(ver.ParentPkg().CurrentVer().end() && !(*this)[ver.ParentPkg()].Install()) ||
	(!ver.ParentPkg().CurrentVer().end() &&
	 (*this)[ver.ParentPkg()].Delete() &&
	 get_ext_state(ver.ParentPkg()).remove_reason == unused);

      if(set_to_manual)
	MarkAuto(ver.ParentPkg(), false);



      // Use the InstVerIter instead of GetCandidateVersion, since
      // that seems to store the currently to-be-installed version.
      VerIterator prev=(*this)[(ver.ParentPkg())].InstVerIter(GetCache());

      aptitude_state &estate = get_ext_state(ver.ParentPkg());

      if(ver!=GetCandidateVer(ver.ParentPkg()))
	estate.candver=ver.VerStr();
      else
	estate.candver="";

      estate.selection_state = pkgCache::State::Install;

      SetCandidateVersion(ver);

      if(group_level == 0)
	{
	  if(undo)
	    undo->add_item(new candver_undoer(prev, this));

	  MarkAndSweep();

	  //if(BrokenCount()>0)
	  //create_resolver();
	  //
	  // EW - rely on the fact that mark_and_sweep implicitly calls
	  // begin/end_action_group(), which in turn does just this.

	  package_state_changed();
	}
    }
}

void aptitudeDepCache::forbid_upgrade(const PkgIterator &pkg,
				      string verstr, undo_group *undo)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return;
    }

  aptitude_state &estate=get_ext_state(pkg);

  if(verstr!=estate.forbidver)
    {
      action_group group(*this, undo);

      pre_package_state_changed();

      pkgCache::VerIterator candver=(*this)[pkg].CandidateVerIter(*this);

      dirty=true;

      estate.forbidver=verstr;
      if(!candver.end() && candver.VerStr()==verstr && (*this)[pkg].Install())
	MarkKeep(pkg, false);
    }
}

void aptitudeDepCache::mark_single_install(const PkgIterator &Pkg, undo_group *undo)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return;
    }

  action_group group(*this, undo);

  pre_package_state_changed();
  dirty=true;

  for(PkgIterator i=PkgBegin(); !i.end(); i++)
    pkgDepCache::MarkKeep(i, true);


  bool set_to_manual =
    ((Pkg.CurrentVer().end()  && !(*this)[Pkg].Install()) ||
     (!Pkg.CurrentVer().end() && (*this)[Pkg].Delete() && get_ext_state(Pkg).remove_reason == unused));

  if(set_to_manual)
    MarkAuto(Pkg, false);

  pkgDepCache::MarkInstall(Pkg, true);
}

void aptitudeDepCache::mark_auto_installed(const PkgIterator &Pkg,
					   bool set_auto,
					   undo_group *undo)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return;
    }

  action_group group(*this, undo);

  pre_package_state_changed();
  dirty=true;

  MarkAuto(Pkg, set_auto);
}

bool aptitudeDepCache::all_upgrade(bool with_autoinst, undo_group *undo)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return false;
    }

  action_group group(*this, undo);

  pre_package_state_changed();

  pkgProblemResolver fixer(this);

  if(BrokenCount()!=0)
    return false;

  for(pkgCache::PkgIterator pkg=PkgBegin(); !pkg.end(); ++pkg)
    {
      if((*this)[pkg].Install())
	fixer.Protect(pkg);

      if(!is_held(pkg) &&
	 !pkg.CurrentVer().end() && !(*this)[pkg].Install())
	MarkInstall(pkg, with_autoinst);
    }

  bool rval=fixer.ResolveByKeep();

  return rval;
}

bool aptitudeDepCache::try_fix_broken(pkgProblemResolver &fixer, undo_group *undo)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return false;
    }

  action_group group(*this, undo);

  pre_package_state_changed();
  dirty=true;
  bool founderr=false;
  if(!fixer.Resolve(true))
    founderr=true;

  if(founderr)
    _error->Error(_("Unable to correct dependencies, some packages cannot be installed"));

  return !founderr;
}

bool aptitudeDepCache::try_fix_broken(undo_group *undo)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return false;
    }

  pkgProblemResolver fixer(this);
  pre_package_state_changed();
  for(pkgCache::PkgIterator i=PkgBegin(); !i.end(); i++)
    {
      fixer.Clear(i);
      if(!i.CurrentVer().end() &&
	 get_ext_state(i).selection_state==pkgCache::pkgCache::State::Hold)
	fixer.Protect(i);
      else
	{
	  pkgDepCache::StateCache &state=(*this)[i];
	  if(state.InstBroken() || state.NowBroken())
	    MarkInstall(i,true);
	  else if(state.Delete())
	    fixer.Remove(i);
	}
    }

  return try_fix_broken(fixer, undo);
}

/** Update the given package's aptitude state based on its state
 *  according to dpkg/dselect.
 *
 *  \param Pkg the package to modify.
 */
void aptitudeDepCache::MarkFromDselect(const PkgIterator &Pkg)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return;
    }

  aptitude_state &state=get_ext_state(Pkg);

  if(Pkg->SelectedState!=state.selection_state)
    {
      switch(Pkg->SelectedState)
	{
	case pkgCache::State::Unknown:
	  break;
	case pkgCache::State::Purge:
	  if( (!Pkg.CurrentVer().end()) || !((*this)[Pkg].iFlags&Purge) )
	    mark_delete(Pkg, true, false, NULL);
	  else
	    mark_keep(Pkg, false, false, NULL);
	  break;
	case pkgCache::State::DeInstall:
	  if(!Pkg.CurrentVer().end())
	    mark_delete(Pkg, false, false, NULL);
	  else
	    mark_keep(Pkg, false, false, NULL);
	  break;
	case pkgCache::State::Hold:
	  if(!Pkg.CurrentVer().end())
	    mark_keep(Pkg, false, true, NULL);
	  break;
	case pkgCache::State::Install:
	  if(Pkg.CurrentVer().end())
	    mark_install(Pkg, false, false, NULL);
	  else
	    mark_keep(Pkg, false, false, NULL);
	  break;
	}
    }
}

void aptitudeDepCache::duplicate_cache(apt_state_snapshot *target)
  // Remember: the tables in the target have to be correctly sized!
{
  if(!target->PkgState)
    target->PkgState=new StateCache[Head().PackageCount];
  if(!target->DepState)
    target->DepState=new unsigned char[Head().DependsCount];
  if(!target->AptitudeState)
    target->AptitudeState=new aptitude_state[Head().PackageCount];

  memcpy(target->PkgState, PkgState, sizeof(StateCache)*Head().PackageCount);
  memcpy(target->DepState, DepState, sizeof(char)*Head().DependsCount);
  // memcpy doesn't work here because the aptitude_state structure
  // contains a std::string.  (would it be worthwhile/possible to
  // change things so that it doesn't?)
  for(unsigned int i=0; i<Head().PackageCount; ++i)
    target->AptitudeState[i]=package_states[i];

  target->iUsrSize=iUsrSize;
  target->iDownloadSize=iDownloadSize;
  target->iInstCount=iInstCount;
  target->iDelCount=iDelCount;
  target->iKeepCount=iKeepCount;
  target->iBrokenCount=iBrokenCount;
  target->iBadCount=iBadCount;
}

void aptitudeDepCache::sweep()
{
  // Suppress intermediate removals.
  //
  // \todo this may cause problems if we do undo tracking via ActionGroups.
  pkgDepCache::ActionGroup group(*this);

  bool purge_unused = aptcfg->FindB(PACKAGE "::Purge-Unused", false);

  for(pkgCache::PkgIterator pkg = PkgBegin(); !pkg.end(); ++pkg)
    {
      if(PkgState[pkg->ID].Garbage)
	{
	  if(pkg.CurrentVer() != 0 && pkg->CurrentState != pkgCache::State::ConfigFiles)
	    {
	      MarkDelete(pkg, purge_unused);
	      package_states[pkg->ID].selection_state =
		(purge_unused ? pkgCache::State::Purge : pkgCache::State::DeInstall);
	      package_states[pkg->ID].remove_reason = unused;
	    }
	  else
	    {
	      if(pkg.CurrentVer().end())
		{
		  if((*this)[pkg].iFlags & Purge)
		    package_states[pkg->ID].selection_state = pkgCache::State::Purge;
		  else
		    package_states[pkg->ID].selection_state = pkgCache::State::DeInstall;
		}
	      else
		package_states[pkg->ID].selection_state = pkgCache::State::Install;
	      MarkKeep(pkg, false, false);
	    }
	}
      else if(PkgState[pkg->ID].Delete() && package_states[pkg->ID].remove_reason == unused)
	{
	  MarkKeep(pkg, false, false);
	}
    }
}

void aptitudeDepCache::begin_action_group()
{
  group_level++;
}

void aptitudeDepCache::end_action_group(undo_group *undo)
{
  eassert(group_level>0);

  if(group_level==1)
    {
      if(read_only && !read_only_permission())
	{
	  if(group_level == 0)
	    read_only_fail();

	  group_level--;
	  return;
	}

      sweep();

      cleanup_after_change(undo);

      duplicate_cache(&backup_state);

      package_state_changed();
    }

  group_level--;
}

const aptitudeDepCache::apt_state_snapshot *aptitudeDepCache::snapshot_apt_state()
{
  apt_state_snapshot *rval=new apt_state_snapshot;
  duplicate_cache(rval);

  return rval;
}

void aptitudeDepCache::restore_apt_state(const apt_state_snapshot *snapshot)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return;
    }

  memcpy(PkgState, snapshot->PkgState, sizeof(StateCache)*Head().PackageCount);
  memcpy(DepState, snapshot->DepState, sizeof(char)*Head().DependsCount);
  // memcpy doesn't work here because the aptitude_state structure
  // contains a std::string.  (would it be worthwhile/possible to
  // change things so that it doesn't?)
  for(unsigned int i=0; i<Head().PackageCount; ++i)
    package_states[i]=snapshot->AptitudeState[i];

  iUsrSize=snapshot->iUsrSize;
  iDownloadSize=snapshot->iDownloadSize;
  iInstCount=snapshot->iInstCount;
  iDelCount=snapshot->iDelCount;
  iKeepCount=snapshot->iKeepCount;
  iBrokenCount=snapshot->iBrokenCount;
  iBadCount=snapshot->iBadCount;
}

void aptitudeDepCache::apply_solution(const generic_solution<aptitude_universe> &sol,
				      undo_group *undo)
{
  if(read_only && !read_only_permission())
    {
      if(group_level == 0)
	read_only_fail();
      return;
    }

  action_group group(*this, undo);

  pre_package_state_changed();

  for(imm::map<aptitude_resolver_package, generic_solution<aptitude_universe>::action>::const_iterator
	i = sol.get_actions().begin();
      i != sol.get_actions().end(); ++i)
    {
      pkgCache::PkgIterator pkg=i->first.get_pkg();
      pkgCache::VerIterator curver=pkg.CurrentVer();
      pkgCache::VerIterator actionver=i->second.ver.get_ver();

      // Check what type of action it is.
      if(actionver.end())
	{
	  // removal.
	  internal_mark_delete(pkg, false, false);
	  if(!curver.end())
	    get_ext_state(pkg).remove_reason = from_resolver;
	}
      else if(actionver == curver)
	internal_mark_keep(pkg, true, false);
      else
	// install a particular version that's not the current one.
	{
	  set_candidate_version(actionver, NULL);
	  internal_mark_install(pkg, false, false);
	  MarkAuto(pkg, true);
	}
    }
}

aptitudeCacheFile::aptitudeCacheFile()
  :Map(NULL), Cache(NULL), DCache(NULL), have_system_lock(false), Policy(NULL)
{
}

aptitudeCacheFile::~aptitudeCacheFile()
{
  delete Cache;
  delete Map;
  ReleaseLock();

  delete DCache;
  delete Policy;
}

bool aptitudeCacheFile::Open(OpProgress &Progress, bool do_initselections,
			     bool WithLock, const char *status_fname)
{
  if(WithLock)
    {
      if(!_system->Lock())
	return false;

      have_system_lock=true;
    }

  if(_error->PendingError())
    return false;

  pkgSourceList List;
  if(!List.ReadMainList())
    return _error->Error(_("The list of sources could not be read."));

  // Read the caches:
  bool Res=pkgMakeStatusCache(List, Progress, &Map, !WithLock);
  Progress.Done();

  if(!Res)
    return _error->Error(_("The package lists or status file could not be parsed or opened."));

  if(!_error->empty())
    _error->Warning(_("You may want to update the package lists to correct these missing files"));

  Cache=new pkgCache(Map);
  if(_error->PendingError())
    return false;

  Policy=new aptitudePolicy(Cache);
  if(_error->PendingError())
    return false;
  if(!ReadPinFile(*Policy))
    return false;

  DCache=new aptitudeDepCache(Cache, Policy);
  if(_error->PendingError())
    return false;

  DCache->Init(&Progress, WithLock, do_initselections, status_fname);
  Progress.Done();

  if(_error->PendingError())
    return false;

  return true;
}

void aptitudeCacheFile::ReleaseLock()
{
  if(have_system_lock)
    {
      _system->UnLock();
      have_system_lock=false;
    }
}

bool aptitudeCacheFile::GainLock()
{
  if(have_system_lock)
    return true;

  if(!_system->Lock())
    return false;

  have_system_lock=true;
  return true;
}

bool aptitudeDepCache::is_held(const PkgIterator &pkg)
{
  aptitude_state state=get_ext_state(pkg);

  pkgCache::VerIterator candver=(*this)[pkg].CandidateVerIter(*this);

  return !pkg.CurrentVer().end() &&
    (state.selection_state == pkgCache::State::Hold ||
     (!candver.end() && candver.VerStr() == state.forbidver));
}

bool aptitudeDepCache::MarkFollowsRecommends()
{
  return pkgDepCache::MarkFollowsRecommends() ||
    aptcfg->FindB(PACKAGE "::Recommends-Important", true) ||
    aptcfg->FindB(PACKAGE "::Keep-Recommends", false);
}

bool aptitudeDepCache::MarkFollowsSuggests()
{
  return pkgDepCache::MarkFollowsSuggests() ||
    aptcfg->FindB(PACKAGE "::Keep-Suggests", false) ||
    aptcfg->FindB(PACKAGE "::Suggests-Important", false);
}

class AptitudeInRootSetFunc : public pkgDepCache::InRootSetFunc
{
  /** A matcher if one could be created; otherwise NULL. */
  pkg_matcher *m;

  /** \b true if the package was successfully constructed. */
  bool constructedSuccessfully;

  /** The secondary test to apply.  Only set if creating the
   *  matcher succeeds.
   */
  pkgDepCache::InRootSetFunc *chain;
public:
  AptitudeInRootSetFunc(pkgDepCache::InRootSetFunc *_chain)
    : m(NULL), constructedSuccessfully(false), chain(NULL)
  {
    std::string matchterm = aptcfg->Find(PACKAGE "::Keep-Unused-Pattern", "~nlinux-image-.*");
    if(matchterm.empty()) // Bug-compatibility with old versions.
      matchterm = aptcfg->Find(PACKAGE "::Delete-Unused-Pattern");

    if(matchterm.empty())
      constructedSuccessfully = true;
    else
      {
	m = parse_pattern(matchterm);
	if(m != NULL)
	  constructedSuccessfully = true;
      }

    if(constructedSuccessfully)
      chain = _chain;
  }

  bool wasConstructedSuccessfully() const
  {
    return constructedSuccessfully;
  }

  bool InRootSet(const pkgCache::PkgIterator &pkg)
  {
    if(m != NULL && m->matches(pkg))
      return true;
    else
      return chain != NULL && chain->InRootSet(pkg);
  }

  ~AptitudeInRootSetFunc()
  {
    delete m;
    delete chain;
  }
};

pkgDepCache::InRootSetFunc *aptitudeDepCache::GetRootSetFunc()
{
  InRootSetFunc *superFunc = pkgDepCache::GetRootSetFunc();

  AptitudeInRootSetFunc *f = new AptitudeInRootSetFunc(superFunc);

  if(f->wasConstructedSuccessfully())
    return f;
  else
    {
      delete f;
      return superFunc;
    }
}
