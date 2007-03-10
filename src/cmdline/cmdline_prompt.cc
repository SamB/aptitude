// cmdline_prompt.cc
//
//   Handles the preview and prompt that's displayed from the command-line.

#include "cmdline_prompt.h"

#include "cmdline_action.h"
#include "cmdline_changelog.h"
#include "cmdline_resolver.h"
#include "cmdline_show.h"
#include "cmdline_util.h"

#include <ui.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_signal_log.h>
#include <generic/apt/infer_reason.h>

#include <vscreen/fragment.h>
#include <vscreen/vscreen.h>
#include <vscreen/transcode.h>

#include <apt-pkg/algorithms.h>
#include <apt-pkg/dpkgpm.h>
#include <apt-pkg/error.h>
#include <apt-pkg/sourcelist.h>
#include <apt-pkg/strutl.h>

using namespace std;

string StdinEOFException::errmsg() const
{
  return _("Unexpected end-of-file on standard input");
}

struct fetchinfo
{
  double FetchBytes, FetchPBytes, DebBytes;
};

static bool pkg_byname_compare(const pkgCache::PkgIterator &a, const pkgCache::PkgIterator &b)
{
  return strcmp(a.Name(), b.Name())<0;
}

static bool get_fetchinfo(fetchinfo &f)
{
  download_signal_log m;
  pkgAcquire fetcher(&m);
  pkgSourceList l;
  if(!l.ReadMainList())
    return _error->Error(_("Couldn't read list of sources"));

  pkgDPkgPM pm(*apt_cache_file);
  pm.GetArchives(&fetcher, &l, apt_package_records);

  f.FetchBytes=fetcher.FetchNeeded();
  f.FetchPBytes=fetcher.PartialPresent();
  f.DebBytes=fetcher.TotalNeeded();

  return true;
}

static string reason_string_list(set<reason> &reasons)
{
  set<reason>::iterator prev=reasons.end();
  string s;

  bool first=true;
  for(set<reason>::iterator why=reasons.begin();
      why!=reasons.end(); prev=why++)
    {
      // Filter duplicates.
      if(prev!=reasons.end() &&
	 prev->pkg==why->pkg && prev->dep->Type==why->dep->Type)
	continue;

      if(!first)
	s+=", ";
      else
	{
	  s+=" (";
	  first=false;
	}

      string dep_type = const_cast<pkgCache::DepIterator &>(why->dep).DepType();
      s += transcode(transcode(dep_type).substr(0, 1));
      s+=": ";
      s+=why->pkg.Name();
    }
  if(!first)
    s+=")";

  return s;
}

/** Prints a description of a list of packages, with annotations
 *  reflecting how or why they will be changed.
 *
 *  Tries to infer the dependencies that caused a package to be installed,
 *  removed, or held.
 *
 *  \param items the set of items to examine
 *  \param showvers if true, display version numbers as appropriate
 *  \param showsize if true, display the change in each package's size
 */
static void cmdline_show_instinfo(pkgvector &items,
				  bool showvers,
				  bool showdeps,
				  bool showsize,
				  bool showpurge)
{
  sort(items.begin(), items.end(), pkg_byname_compare);
  strvector output;

  for(pkgvector::iterator i=items.begin(); i!=items.end(); ++i)
    {
      string s=i->Name();

      pkgDepCache::StateCache &state=(*apt_cache_file)[*i];
      //aptitudeDepCache::aptitude_state &extstate=(*apt_cache_file)->get_ext_state(*i);
      pkgCache::VerIterator instver=state.InstVerIter(*apt_cache_file);

      if(showpurge)
	{
	  if(state.Delete() && state.iFlags&pkgDepCache::Purge)
	    s += "{p}";
	}

      // Display version numbers.
      if(showvers)
	{
	  pkgCache::VerIterator cur=i->CurrentVer();
	  pkgCache::VerIterator cand=state.CandidateVerIter(*apt_cache_file);

	  // Display x -> y for upgraded, held, and downgraded packages.
	  if( (state.Status==1 || state.Downgrade()) &&
	      i->CurrentVer()!=cand)
	    {
	      s+=" [";
	      s+=cur.VerStr();
	      s+=" -> ";
	      s+=cand.VerStr();
	      s+="]";
	    }
	  else
	    {
	      s+=" [";
	      s+=cand.VerStr();
	      s+="]";
	    }
	}

      // Show the change in size between the versions.
      if(showsize)
	{
	  int dsize=(instver.end()?0:instver->InstalledSize)
	    -(i->CurrentVer().end()?0:i->CurrentVer()->InstalledSize);

	  if(dsize>0)
	    s+=" <+"+SizeToStr(dsize)+"B>";
	  else if(dsize<0)
	    s+=" <-"+SizeToStr(dsize)+"B>";
	}

      if(showdeps)
	{
	  set<reason> reasons;
	  infer_reason(*i, reasons);
	  s+=reason_string_list(reasons);
	}

      output.push_back(s);
    }

  cmdline_show_stringlist(output);
}

// Shows broken dependencies for a single package

static void show_broken_deps(pkgCache::PkgIterator pkg)
{
  unsigned int indent=strlen(pkg.Name())+3;
  bool is_first_dep=true;
  pkgCache::VerIterator ver=(*apt_cache_file)[pkg].InstVerIter(*apt_cache_file);

  printf("  %s:", pkg.Name());
  for(pkgCache::DepIterator dep=ver.DependsList(); !dep.end(); ++dep)
    {
      pkgCache::DepIterator first=dep, prev=dep;

      while(dep->CompareOp & pkgCache::Dep::Or)
	++dep;

      // Yep, it's broken.
      if(dep.IsCritical() &&
	 !((*apt_cache_file)[dep]&pkgDepCache::DepGInstall))
	{		  
	  bool is_first_of_or=true;
	  // Iterate over the OR group, print out the information.

	  do
	    {
	      if(!is_first_dep)
		for(unsigned int i=0; i<indent; ++i)
		  printf(" ");

	      is_first_dep=false;

	      if(!is_first_of_or)
		for(unsigned int i=0; i<strlen(dep.DepType())+3; ++i)
		  printf(" ");
	      else
		printf(" %s: ", first.DepType());

	      is_first_of_or=false;

	      if(first.TargetVer())
		printf("%s (%s %s)", first.TargetPkg().Name(), first.CompType(), first.TargetVer());
	      else
		printf("%s", first.TargetPkg().Name());

	      // FIXME: handle virtual packages sanely.
	      pkgCache::PkgIterator target=first.TargetPkg();
	      // Don't skip real packages which are provided.
	      if(!target.VersionList().end())
		{
		  printf(" ");

		  pkgCache::VerIterator ver=(*apt_cache_file)[target].InstVerIter(*apt_cache_file);

		  if(!ver.end()) // ok, it's installable.
		    {
		      if((*apt_cache_file)[target].Install())
			printf(_("but %s is to be installed."),
			       ver.VerStr());
		      else if((*apt_cache_file)[target].Upgradable())
			printf(_("but %s is installed and it is kept back."),
			       target.CurrentVer().VerStr());
		      else
			printf(_("but %s is installed."),
			       target.CurrentVer().VerStr());
		    }
		  else
		    printf(_("but it is not installable"));
		}
	      else
		// FIXME: do something sensible here!
		printf(_(" which is a virtual package."));

	      if(first!=dep)
		printf(_(" or"));

	      printf("\n");

	      prev=first;
	      ++first;
	    } while(prev!=dep);
	}
    }
}

static const char *cmdline_action_descriptions[num_pkg_action_states]={
  N_("The following packages are BROKEN:"),
  N_("The following packages are unused and will be REMOVED:"),
  N_("The following packages have been automatically kept back:"),
  N_("The following NEW packages will be automatically installed:"),
  N_("The following packages will be automatically REMOVED:"),
  N_("The following packages will be DOWNGRADED:"),
  N_("The following packages have been kept back:"),
  N_("The following packages will be REINSTALLED:"),
  N_("The following NEW packages will be installed:"),
  N_("The following packages will be REMOVED:"),
  N_("The following packages will be upgraded:"),
};

// Probably something like cin.getline() would work, but I don't trust that
// for interactive use.
string prompt_string(const string &prompt)
{
  printf("%s", prompt.c_str());
  fflush(stdout);

  string rval;
  char buf[1024];
  cin.getline(buf, 1023);
  rval+=buf;

  while(!cin && !cin.eof())
    {
      cin.getline(buf, 1023);
      rval+=buf;
    }

  if(!cin)
    throw StdinEOFException();

  return rval;
}

/** Checks for broken/deleted essential packages and displays a big
 *  fat warning message about them.  Returns false if the user doesn't
 *  want to continue.
 */
static bool prompt_essential()
{
  pkgvector todelete, whatsbroken;
  bool ok=true;

  for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin();
      !pkg.end(); ++pkg)
    {
      if( (pkg->Flags & pkgCache::Flag::Essential) ||
	  (pkg->Flags & pkgCache::Flag::Important))
	{
	  // Eek!
	  if((*apt_cache_file)[pkg].Delete())
	    todelete.push_back(pkg);

	  if((*apt_cache_file)[pkg].InstBroken())
	    whatsbroken.push_back(pkg);
	}
    }

  if(!todelete.empty())
    {
      ok=false;

      printf(_("The following ESSENTIAL packages will be REMOVED!\n"));
      cmdline_show_pkglist(todelete);
      printf("\n");
    }

  if(!whatsbroken.empty())
    {
      ok=false;

      printf(_("The following ESSENTIAL packages will be BROKEN by this action:\n"));

      for(pkgvector::iterator i=whatsbroken.begin(); i!=whatsbroken.end(); ++i)
	show_broken_deps(*i);

      printf("\n");
    }

  if(!ok)
    {
      printf(_("WARNING: Performing this action will probably cause your system to break!\n         Do NOT continue unless you know EXACTLY what you are doing!\n"));

      string prompt=_("I am aware that this is a very bad idea");
      char buf[1024];

      printf(_("To continue, type the phrase \"%s\":\n"), prompt.c_str());
      cin.getline(buf, 1023);
      bool rval=(prompt==buf);

      while(!cin && !cin.eof())
	cin.getline(buf, 1023);

      if(!cin)
	throw StdinEOFException();

      return rval;
    }

  return true;
}

/** Checks for trust violations and displays a big fat warning if any exist.
 *
 *  \return true if everything is OK or the user overrode the warning.
 */
static bool prompt_trust()
{
  pkgvector untrusted;

  for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin();
      !pkg.end(); ++pkg)
    {
      pkgDepCache::StateCache &state=(*apt_cache_file)[pkg];

      if(state.Install())
	{
	  pkgCache::VerIterator curr=pkg.CurrentVer();
	  pkgCache::VerIterator cand=state.InstVerIter(*apt_cache_file);

	  if((curr.end() || package_trusted(curr)) &&
	     !package_trusted(cand))
	    untrusted.push_back(pkg);
	}
    }

  if(!untrusted.empty())
    {
      printf(_("WARNING: untrusted versions of the following packages will be installed!\n\n"
	       "Untrusted packages could compromise your system's security.\n"
	       "You should only proceed with the installation if you are certain that\n"
	       "this is what you want to do.\n\n"));

      cmdline_show_pkglist(untrusted);

      printf("\n");


      if(aptcfg->FindB(PACKAGE "::CmdLine::Ignore-Trust-Violations", false))
	{
	  printf(_("*** WARNING ***   Ignoring these trust violations because\n"
		   "                  %s::CmdLine::Ignore-Trust-Violations is 'true'!\n"),
		 PACKAGE);
	  return true;
	}

      if(aptcfg->FindB("Apt::Get::AllowUnauthenticated", false))
	{
	  printf("%s",
		 _("*** WARNING ***   Ignoring these trust violations because\n"
		   "                  Apt::Get::AllowUnauthenticated is 'true'!\n"));
	  return true;
	}


      const string okstr=_("Yes"), abortstr=_("No");

      while(1)
	{
	  printf(_("Do you want to ignore this warning and proceed anyway?\n"));
	  printf(_("To continue, enter \"%s\"; to abort, enter \"%s\": "), okstr.c_str(), abortstr.c_str());
	  char buf[1024];
	  cin.getline(buf, 1023);
	  buf[1023]='\0';

	  if(cin.eof())
	    throw StdinEOFException();

	  const bool is_ok=(strncasecmp(okstr.c_str(), buf, okstr.size())==0);
	  const bool is_abort=(strncasecmp(abortstr.c_str(), buf, abortstr.size())==0);

	  const bool rval=is_ok;

	  if(!is_ok && !is_abort)
	    printf(_("Unrecognized input.  Enter either \"%s\" or \"%s\".\n"), okstr.c_str(), abortstr.c_str());
	  else
	    return rval;
	}
    }

  return true;
}

/** Displays a preview of the stuff to be done -- like apt-get, it collects
 *  all the "stuff to install" in one place.
 *
 *  The arguments and return value are for when this is used for a targeted
 *  install/remove; it can figure out whether any packages not requested by the
 *  user are being installed/removed (eg, because of sticky states) and
 *  tell the caller to pause for confirmation.
 */
static bool cmdline_show_preview(bool as_upgrade, pkgset &to_install,
				 pkgset &to_hold, pkgset &to_remove,
				 bool showvers, bool showdeps, bool showsize,
				 int verbose)
{
  bool all_empty=true;

  pkgvector lists[num_pkg_action_states];
  pkgvector recommended, suggested;
  pkgvector extra_install, extra_remove;
  unsigned long Upgrade=0, Downgrade=0, Install=0, ReInstall=0;

  for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin();
      !pkg.end(); ++pkg)
    {
      if((*apt_cache_file)[pkg].NewInstall())
	++Install;
      else if((*apt_cache_file)[pkg].Upgrade())
	++Upgrade;
      else if((*apt_cache_file)[pkg].Downgrade())
	++Downgrade;
      else if(!(*apt_cache_file)[pkg].Delete() &&
	      ((*apt_cache_file)[pkg].iFlags & pkgDepCache::ReInstall))
	++ReInstall;

      pkg_action_state state=find_pkg_state(pkg);

      switch(state)
	{
	case pkg_auto_install:
	case pkg_install:
	case pkg_upgrade:
	  if(to_install.find(pkg)==to_install.end())
	    extra_install.push_back(pkg);
	  break;
	case pkg_auto_remove:
	case pkg_unused_remove:
	case pkg_remove:
	  if(to_remove.find(pkg)==to_remove.end())
	    extra_remove.push_back(pkg);
	  break;
	case pkg_unchanged:
	  if(pkg.CurrentVer().end())
	    {
	      if(package_recommended(pkg))
		recommended.push_back(pkg);
	      else if(package_suggested(pkg))
		suggested.push_back(pkg);
	    }
	default:
	  break;
	}

      if(state!=pkg_unchanged &&
	 !((state==pkg_auto_hold || state==pkg_hold) && !as_upgrade))
	lists[state].push_back(pkg);

      if(state==pkg_auto_install)
	lists[pkg_install].push_back(pkg);
      else if(state==pkg_auto_remove)
	lists[pkg_remove].push_back(pkg);
    }

  for(int i=0; i<num_pkg_action_states; ++i)
    {
      if(!lists[i].empty())
	{
	  all_empty=false;

	  printf("%s\n", _(cmdline_action_descriptions[i]));
	  if(i==pkg_auto_install || i==pkg_auto_remove || i==pkg_unused_remove ||
	     i==pkg_auto_hold || i==pkg_broken)
	    cmdline_show_instinfo(lists[i],
				  showvers, showdeps, showsize,
				  (i == pkg_auto_remove ||
				   i == pkg_unused_remove));
	  else
	    cmdline_show_instinfo(lists[i],
				  showvers, false, showsize,
				  i == pkg_remove);
	}
    }

  if(!recommended.empty())
    {
      printf(_("The following packages are RECOMMENDED but will NOT be installed:\n"));
      cmdline_show_instinfo(recommended, showvers, showdeps, showsize, false);
    }

  if(verbose>0 && !suggested.empty())
    {
      printf(_("The following packages are SUGGESTED but will NOT be installed:\n"));
      cmdline_show_instinfo(suggested, showvers, showdeps, showsize, false);
    }

  if(all_empty)
    printf(_("No packages will be installed, upgraded, or removed.\n"));

  printf(_("%lu packages upgraded, %lu newly installed, "),
	 Upgrade, Install);

  if(ReInstall!=0)
    printf(_("%lu reinstalled, "), ReInstall);
  if(Downgrade!=0)
    printf(_("%lu downgraded, "), Downgrade);

  printf(_("%lu to remove and %lu not upgraded.\n"),
	 (*apt_cache_file)->DelCount(),(*apt_cache_file)->KeepCount());

  fetchinfo f;
  if(get_fetchinfo(f))
    {
      if(f.DebBytes!=f.FetchBytes)
	printf(_("Need to get %sB/%sB of archives. "),
	       SizeToStr(f.FetchBytes).c_str(), SizeToStr(f.DebBytes).c_str());
      else
	printf(_("Need to get %sB of archives. "),
	       SizeToStr(f.DebBytes).c_str());
    }
  else
    _error->DumpErrors();

  if((*apt_cache_file)->UsrSize() >=0)
    printf(_("After unpacking %sB will be used.\n"),
	   SizeToStr((*apt_cache_file)->UsrSize()).c_str());
  else
    printf(_("After unpacking %sB will be freed.\n"),
	   SizeToStr(-(*apt_cache_file)->UsrSize()).c_str());

  // If I return directly below, g++ complains about control reaching the
  // end of a non-void function!
  bool rval;

  rval=((as_upgrade && !lists[pkg_upgrade].empty()) ||
	!(extra_install.empty() && extra_remove.empty()));

  return rval;
}

static void cmdline_parse_show(string response,
			       int verbose)
{
  bool one_shown=false;
  // assume response[0]=='i'
  string::size_type i=1;

  while(i<response.size())
    {
      while(i<response.size() && isspace(response[i]))
	++i;

      string pkgname;
      // Could support quoting, etc?
      while(i<response.size() && !isspace(response[i]))
	pkgname+=response[i++];

      if(!pkgname.empty())
	{
	  one_shown=true;
	  do_cmdline_show(pkgname, verbose);
	}
    }

  if(!one_shown)
    printf(_("No packages to show -- enter the package names on the line after 'i'.\n"));

  prompt_string(_("Press Return to continue."));
}

// Erm.  Merge w/ above?
static void cmdline_parse_changelog(string response)
{
  // assume response[0]=='i'
  string::size_type i=1;

  vector<string> packages;

  while(i<response.size())
    {
      while(i<response.size() && isspace(response[i]))
	++i;

      string pkgname;
      // Could support quoting, etc?
      while(i<response.size() && !isspace(response[i]))
	pkgname+=response[i++];

      if(!pkgname.empty())
	packages.push_back(pkgname);
    }

  if(packages.empty())
    printf(_("No packages found -- enter the package names on the line after 'c'.\n"));
  else
    do_cmdline_changelog(packages);

  prompt_string(_("Press Return to continue"));
}

static inline fragment *flowindentbox(int i1, int irest, fragment *f)
{
  return indentbox(i1, irest, flowbox(f));
}

static void prompt_help(ostream &out)
{
  fragment *f=indentbox(2, 2,
			fragf(_("y: %F"
				"n: %F"
				"i: %F"
				"c: %F"
				"d: %F"
				"s: %F"
				"v: %F"
				"e: %F"
				"%n"
				"%F"
				"%n"
				"%F"
				"%F"
				"%F"
				"%F"
				"%F"
				"%F"
				"%F"
				"%F"),
			      flowindentbox(0, 3,
					    fragf(_("continue with the installation"))),
			      flowindentbox(0, 3,
					    fragf(_("abort and quit"))),
			      flowindentbox(0, 3,
					    fragf(_("show information about one or more packages; the package names should follow the 'i'"))),
			      flowindentbox(0, 3,
					    fragf(_("show the Debian changelogs of one or more packages; the package names should follow the 'c'"))),
			      flowindentbox(0, 3,
					    fragf(_("toggle the display of dependency information"))),
			      flowindentbox(0, 3,
					    fragf(_("toggle the display of changes in package sizes"))),
			      flowindentbox(0, 3,
					    fragf(_("toggle the display of version numbers"))),
			      flowindentbox(0, 3,
					    fragf(_("enter the full visual interface"))),
			      flowbox(fragf(_("You may also specify modification to the actions which will be taken.  To do so, type an action character followed by one or more package names (or patterns).  The action will be applied to all the packages that you list.  The following actions are available:"))),
			      // FIXME: copied from
			      // cmdline_resolver.cc, maybe this
			      // should be placed in a common file?
			      flowindentbox(0, 4,
					    fragf(_("'+' to install packages"))),
			      flowindentbox(0, 5,
					    fragf(_("'+M' to install packages and immediately flag them as automatically installed"))),
			      flowindentbox(0, 4,
					    fragf(_("'-' to remove packages"))),
			      flowindentbox(0, 4,
					    fragf(_("'_' to purge packages"))),
			      flowindentbox(0, 4,
					    fragf(_("'=' to place packages on hold"))),
			      flowindentbox(0, 4,
					    fragf(_("':' to keep packages in their current state without placing them on hold"))),
			      flowindentbox(0, 4,
					    fragf(_("'&M' to mark packages as automatically installed"))),
			      flowindentbox(0, 4,
					    fragf(_("'&m' to mark packages as manually installed")))));

  out << _("Commands:") << endl;
  out << f->layout(screen_width, screen_width, style());
  delete f;
}

bool cmdline_do_prompt(bool as_upgrade,
		       pkgset &to_install,
		       pkgset &to_hold,
		       pkgset &to_remove,
		       pkgset &to_purge,
		       bool showvers,
		       bool showdeps,
		       bool showsize,
		       bool always_prompt,
		       int verbose,
		       bool assume_yes,
		       bool force_no_change)
{
  bool cont=false;
  bool rval=true;
  bool first=true;

  while(!cont)
    {
      if(!cmdline_show_preview(true, to_install, to_hold, to_remove,
			       showvers, showdeps, showsize, verbose) &&
	 first &&
	 !always_prompt &&
	 (*apt_cache_file)->BrokenCount()==0)
	cont=true;
      else if((*apt_cache_file)->BrokenCount() > 0)
	{
	  if(!cmdline_resolve_deps(to_install,
				   to_hold,
				   to_remove,
				   to_purge,
				   assume_yes,
				   force_no_change,
				   verbose))
	    {
	      cont=true;
	      rval=false;
	    }
	}
      else if(assume_yes)
	cont=true;
      else
	{
	  bool valid_response=false;

	  while(!valid_response)
	    {
	      valid_response=true;
	      fflush(stdout);

	      string response=prompt_string(_("Do you want to continue? [Y/n/?] "));
	      string::size_type loc=0;

	      while(loc<response.size() && isspace(response[loc]))
		++loc;

	      if(loc==response.size())
		{
		  response='y';
		  loc=0;
		}

	      switch(toupper(response[loc]))
		{
		case 'Y':
		  rval=true;
		  cont=true;
		  break;
		case 'N':
		  rval=false;
		  cont=true;
		  break;
		case 'D':
		  showdeps=!showdeps;
		  if(showdeps)
		    printf(_("\nDependency information will be shown.\n\n"));
		  else
		    printf(_("\nDependency information will not be shown.\n\n"));
		  break;
		case 'V':
		  showvers=!showvers;

		  if(showvers)
		    printf(_("\nVersions will be shown.\n\n"));
		  else
		    printf(_("\nVersions will not be shown.\n\n"));
		  break;
		case 'S':
		  showsize=!showsize;
		  if(showsize)
		    printf(_("\nSize changes will be shown.\n\n"));
		  else
		    printf(_("\nSize changes will not be shown.\n\n"));
		  break;
		case 'I':
		  cmdline_parse_show(response, verbose);
		  break;
		case 'C':
		  cmdline_parse_changelog(response);
		  break;
		case '+':
		case '-':
		case '=':
		case '_':
		case ':':
		  cmdline_parse_action(response, to_install, to_hold,
				       to_remove, to_purge, verbose, true);
		  break;
		case 'E':
		  ui_preview();
		case '?':
		  valid_response=false;
		  prompt_help(cout);
		  break;
		default:
		  printf(_("Invalid response.  Please enter a valid command or '?' for help.\n"));
		  valid_response=false;
		  break;
		}
	    }
	}

      // Note: only show the prompt if we're planning to continue.
      if(rval && (!prompt_essential() || !prompt_trust()))
	{
	  rval=false;
	  cont=true;
	}

      first=false;
    }

  return rval;
}
