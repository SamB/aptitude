// main.cc  (neé testscr.cc)
//
//  Copyright 1999-2008 Daniel Burrows
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
//  Tests the various screen-output mechanisms

#ifndef _GNU_SOURCE
#define _GNU_SOURCE // For getopt.h
#endif

#include <getopt.h>
#include <signal.h>

#include "aptitude.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/matchers.h>

#include <generic/problemresolver/exceptions.h>

#include <cwidget/config/keybindings.h>
#include <cwidget/generic/util/transcode.h>
#include <cwidget/toplevel.h>
#include <cwidget/dialogs.h>

#include <cmdline/cmdline_changelog.h>
#include <cmdline/cmdline_check_resolver.h>
#include <cmdline/cmdline_clean.h>
#include <cmdline/cmdline_common.h>
#include <cmdline/cmdline_do_action.h>
#include <cmdline/cmdline_download.h>
#include <cmdline/cmdline_dump_resolver.h>
#include <cmdline/cmdline_extract_cache_subset.h>
#include <cmdline/cmdline_forget_new.h>
#include <cmdline/cmdline_moo.h>
#include <cmdline/cmdline_prompt.h>
#include <cmdline/cmdline_search.h>
#include <cmdline/cmdline_show.h>
#include <cmdline/cmdline_update.h>
#include <cmdline/cmdline_upgrade.h>
#include <cmdline/cmdline_user_tag.h>
#include <cmdline/cmdline_why.h>

#include <sigc++/functors/ptr_fun.h>

#include <apt-pkg/error.h>
#include <apt-pkg/init.h>

#include "ui.h"

#include "progress.h"
#include "pkg_columnizer.h"
#include "pkg_grouppolicy.h"
#include "pkg_view.h"

namespace cw = cwidget;

#if 0
// These are commented out so as to not punish users unduly for coding
// errors.  The cw::util::transcoder now substitutes conspicuous '?' characters
// into its output, which should be enough of a clue.


/** Handles a coding error using the apt error mechanism. */
std::wstring handle_mbtow_error(int error,
				const std::wstring &partial,
				const std::string &input)
{
  _error->Errno("iconv", _("Can't decode multibyte string after \"%ls\""),
		partial.c_str());
  return partial;
}

std::string handle_wtomb_error(int error,
			       const std::string &partial,
			       const std::wstring &input)
{
  _error->Errno("iconv", _("Can't decode wide-character string after \"%s\""),
		partial.c_str());
  return partial;
}
#endif

static void show_version()
{
  printf(_("%s %s compiled at %s %s\n"),
	   PACKAGE, VERSION, __DATE__, __TIME__);
#ifdef __GNUC__
  printf(_("Compiler: g++ %s\n"), __VERSION__);
#endif
  printf("%s", _("Compiled against:\n"));
  printf(_("  apt version %d.%d.%d\n"),
	 APT_PKG_MAJOR, APT_PKG_MINOR, APT_PKG_RELEASE);
#ifndef NCURSES_VERSION
  printf(_("  NCurses version: Unknown\n"));
#else
  printf(_("  NCurses version %s\n"), NCURSES_VERSION);
#endif
  printf(_("  libsigc++ version: %s\n"), SIGC_VERSION);
#ifdef HAVE_EPT
  printf(_("  Ept support enabled.\n"));
#else
  printf(_("  Ept support disabled.\n"));
#endif

  printf("%s", _("\nCurrent library versions:\n"));
  printf(_("  NCurses version: %s\n"), curses_version());
  printf(_("  cwidget version: %s\n"), cwidget::version().c_str());
  printf(_("  Apt version: %s\n"), pkgLibVersion);
}

static void usage()
{
  printf(PACKAGE " " VERSION "\n");
  printf(_("Usage: aptitude [-S fname] [-u|-i]"));
  printf("\n");
  printf(_("       aptitude [options] <action> ..."));
  printf("\n");
  printf(_("  Actions (if none is specified, aptitude will enter interactive mode):\n\n"));
  printf(_(" install      - Install/upgrade packages\n"));
  printf(_(" remove       - Remove packages\n"));
  printf(_(" purge        - Remove packages and their configuration files\n"));
  printf(_(" hold         - Place packages on hold\n"));
  printf(_(" unhold       - Cancel a hold command for a package\n"));
  printf(_(" markauto     - Mark packages as having been automatically installed\n"));
  printf(_(" unmarkauto   - Mark packages as having been manually installed\n"));
  // This is out of line with the rest to avoid messing up
  // translations.  Eventually all the usage information will move to
  // a table-layout-based approach, which will avoid this sort of
  // problem.
  printf(_(" forbid-version - Forbid aptitude from upgrading to a specific package version.\n"));
  printf(_(" update       - Download lists of new/upgradable packages\n"));
  printf(_(" safe-upgrade - Perform a safe upgrade\n"));
  printf(_(" full-upgrade - Perform an upgrade, possibly installing and removing packages\n"));
  printf(_(" forget-new   - Forget what packages are \"new\"\n"));
  printf(_(" search       - Search for a package by name and/or expression\n"));
  printf(_(" show         - Display detailed information about a package\n"));
  printf(_(" clean        - Erase downloaded package files\n"));
  printf(_(" autoclean    - Erase old downloaded package files\n"));
  printf(_(" changelog    - View a package's changelog\n"));
  printf(_(" download     - Download the .deb file for a package\n"));
  printf(_(" reinstall    - Download and (possibly) reinstall a currently installed package\n"));
  printf("\n");
  printf(_("  Options:\n"));
  printf(_(" -h             This help text\n"));
  printf(_(" -s             Simulate actions, but do not actually perform them.\n"));
  printf(_(" -d             Only download packages, do not install or remove anything.\n"));
  printf(_(" -P             Always prompt for confirmation or actions\n"));
  printf(_(" -y             Assume that the answer to simple yes/no questions is 'yes'\n"));
  printf(_(" -F format      Specify a format for displaying search results; see the manual\n"));
  printf(_(" -O order       Specify how search results should be sorted; see the manual\n"));
  printf(_(" -w width       Specify the display width for formatting search results\n"));
  printf(_(" -f             Aggressively try to fix broken packages.\n"));
  printf(_(" -V             Show which versions of packages are to be installed.\n"));
  printf(_(" -D             Show the dependencies of automatically changed packages.\n"));
  printf(_(" -Z             Show the change in installed size of each package.\n"));
  printf(_(" -v             Display extra information. (may be supplied multiple times)\n"));
  printf(_(" -t [release]   Set the release from which packages should be installed\n"));
  printf(_(" -q             In command-line mode, suppress the incremental progress indicators.\n"));
  printf(_(" -o key=val     Directly set the configuration option named 'key'\n"));
  printf(_(" --with(out)-recommends	Specify whether or not to treat recommends as\n                strong dependencies\n"));
  printf(_(" -S fname       Read the aptitude extended status info from fname.\n"));
  printf(_(" -u             Download new package lists on startup.\n"));
  printf(_(" -i             Perform an install run on startup.\n"));
  printf("\n");
  printf(_("                  This aptitude does not have Super Cow Powers.\n"));
}

// This handles options with no single-character equivalent
enum {
  OPTION_VERSION = 1,
  OPTION_VISUAL_PREVIEW,
  OPTION_QUEUE_ONLY,
  OPTION_PURGE_UNUSED,
  OPTION_ALLOW_UNTRUSTED,
  OPTION_NO_NEW_INSTALLS,
  OPTION_NO_NEW_UPGRADES,
  OPTION_ALLOW_NEW_INSTALLS,
  OPTION_ALLOW_NEW_UPGRADES,
  OPTION_ADD_USER_TAG,
  OPTION_ADD_USER_TAG_TO,
  OPTION_REMOVE_USER_TAG,
  OPTION_REMOVE_USER_TAG_FROM,
  OPTION_SAFE_RESOLVER,
  OPTION_FULL_RESOLVER,
  OPTION_ARCH_ONLY,
  OPTION_NOT_ARCH_ONLY
};
int getopt_result;

option opts[]={
  {"help", 0, NULL, 'h'},
  {"version", 0, &getopt_result, OPTION_VERSION},
  {"display-format", 1, NULL, 'F'},
  {"quiet", 2, NULL, 'q'},
  {"width", 1, NULL, 'w'},
  {"simulate", 0, NULL, 's'},
  {"allow-untrusted", 0, &getopt_result, OPTION_ALLOW_UNTRUSTED},
  {"with-recommends", 0, NULL, 'r'},
  {"without-recommends", 0, NULL, 'R'},
  {"download-only", 0, NULL, 'd'},
  {"assume-yes", 0, NULL, 'y'},
  {"verbose", 0, NULL, 'v'},
  {"show-versions", 0, NULL, 'V'},
  {"show-deps", 0, NULL, 'D'},
  {"show-why", 0, NULL, 'W'},
  {"prompt", 0, NULL, 'P'},
  {"sort", 1, NULL, 'O'},
  {"target-release", 1, NULL, 't'},
  {"no-new-installs", 0, &getopt_result, OPTION_NO_NEW_INSTALLS},
  {"no-new-upgrades", 0, &getopt_result, OPTION_NO_NEW_UPGRADES},
  {"allow-new-installs", 0, &getopt_result, OPTION_ALLOW_NEW_INSTALLS},
  {"allow-new-upgrades", 0, &getopt_result, OPTION_ALLOW_NEW_UPGRADES},
  {"safe-resolver", 0, &getopt_result, OPTION_SAFE_RESOLVER},
  {"full-resolver", 0, &getopt_result, OPTION_FULL_RESOLVER},
  {"visual-preview", 0, &getopt_result, OPTION_VISUAL_PREVIEW},
  {"schedule-only", 0, &getopt_result, OPTION_QUEUE_ONLY},
  {"purge-unused", 0, &getopt_result, OPTION_PURGE_UNUSED},
  {"add-user-tag", 1, &getopt_result, OPTION_ADD_USER_TAG},
  {"add-user-tag-to", 1, &getopt_result, OPTION_ADD_USER_TAG_TO},
  {"remove-user-tag", 1, &getopt_result, OPTION_REMOVE_USER_TAG},
  {"remove-user-tag-from", 1, &getopt_result, OPTION_REMOVE_USER_TAG_FROM},
  {"arch-only", 0, &getopt_result, OPTION_ARCH_ONLY},
  {"not-arch-only", 0, &getopt_result, OPTION_NOT_ARCH_ONLY},
  {0,0,0,0}
};

const char *argv0;

int main(int argc, char *argv[])
{
  srandom(time(0));

  // See earlier note
  //
  //cw::util::transcode_mbtow_err=handle_mbtow_error;
  //cw::util::transcode_wtomb_err=handle_wtomb_error;

  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);

  apt_preinit();

  char *status_fname=NULL;
  // The filename to read status information from.
  string display_format=aptcfg->Find(PACKAGE "::CmdLine::Package-Display-Format", "%c%a%M %p# - %d#");
  string sort_policy="name";
  string width=aptcfg->Find(PACKAGE "::CmdLine::Package-Display-Width", "");
  bool simulate = aptcfg->FindB(PACKAGE "::CmdLine::Simulate", false) ||
    aptcfg->FindB(PACKAGE "::Simulate", false);
  bool download_only=aptcfg->FindB(PACKAGE "::CmdLine::Download-Only", false);;
  bool arch_only = aptcfg->FindB("Apt::Get::Arch-Only", false);

  bool update_only=false, install_only=false, queue_only=false;
  bool assume_yes=aptcfg->FindB(PACKAGE "::CmdLine::Assume-Yes", false);
  bool fix_broken=aptcfg->FindB(PACKAGE "::CmdLine::Fix-Broken", false);
  bool safe_upgrade_no_new_installs = aptcfg->FindB(PACKAGE "::CmdLine::Safe-Upgrade::No-New-Installs", false);
  bool safe_resolver_no_new_installs = aptcfg->FindB(PACKAGE "::Safe-Resolver::No-New-Installs", false);
  bool safe_resolver_no_new_upgrades = aptcfg->FindB(PACKAGE "::Safe-Resolver::No-New-Upgrades", false);
  bool always_use_safe_resolver = aptcfg->FindB(PACKAGE "::Always-Use-Safe-Resolver", false);
  bool safe_resolver_option = false;
  bool full_resolver_option = false;

  // This tracks whether we got a --*-new-installs command-line
  // argument, so we can present a useful warning message to the user
  // if it's not applicable.
  bool saw_new_installs_option = false;
  // Same for --*-new-upgrades
  bool saw_new_upgrades_option = false;

  bool showvers=aptcfg->FindB(PACKAGE "::CmdLine::Show-Versions", false);
  bool showdeps=aptcfg->FindB(PACKAGE "::CmdLine::Show-Deps", false);
  bool showsize=aptcfg->FindB(PACKAGE "::CmdLine::Show-Size-Changes", false);
  bool showwhy = aptcfg->FindB(PACKAGE "::CmdLine::Show-Why", false);
  bool visual_preview=aptcfg->FindB(PACKAGE "::CmdLine::Visual-Preview", false);
  bool always_prompt=aptcfg->FindB(PACKAGE "::CmdLine::Always-Prompt", false);
  int verbose=aptcfg->FindI(PACKAGE "::CmdLine::Verbose", 0);
  bool seen_quiet = false;
  int quiet = 0;
  std::vector<aptitude::cmdline::tag_application> user_tags;

  int curopt;
  // The last option seen

  // NOTE: this can of course be spoofed.  Anyone bothering to is off their
  //      rocker.
  argv0=argv[0];

  // Backwards bug-compatibility; old versions used a misleading name
  // for this option.
  if(aptcfg->Find(PACKAGE "::Keep-Unused-Pattern", "") == "")
    {
      aptcfg->Set(PACKAGE "::Keep-Unused-Pattern",
		  aptcfg->Find(PACKAGE "::Delete-Unused-Pattern", ""));
      aptcfg->Set(PACKAGE "::Delete-Unused-Pattern", "");
    }
  else
    aptcfg->Set(PACKAGE "::Delete-Unused-Pattern", "");

  // Read the arguments:
  while((curopt=getopt_long(argc, argv, "DVZWvhS:uiF:w:sO:fdyPt:q::Rro:", opts, NULL))!=-1)
    {
      switch(curopt)
	{
	case 'S':
	  status_fname=strdup(optarg);
	  break;
	case 'h':
	  usage();
	  exit(0);
	case 'u':
	  update_only=true;
	  break;
	case 'i':
	  install_only=true;
	  break;
	case 'F':
	  display_format=optarg;
	  break;
	case 'w':
	  width=optarg;
	  break;
	case 's':
	  simulate=true;
	  break;
	case 'O':
	  sort_policy=optarg;
	  break;
	case 'f':
	  fix_broken=true;
	  break;
	case 'd':
	  download_only=true;
	  break;
	case 'y':
	  assume_yes=true;
	  break;
	case 'q':
	  if(optarg == 0)
	    ++quiet;
	  else
	    {
	      if(*optarg == '=')
		++optarg;

	      if(*optarg == 0)
		{
		  fprintf(stderr, _("Expected a number after -q=\n"));
		  return -1;
		}

	      char *tmp;
	      quiet = strtol(optarg, &tmp, 0);

	      if(*tmp != '\0')
		{
		  fprintf(stderr, _("Expected a number after -q=, got %s\n"),
			  optarg);
		  return -1;
		}
	    }
	  seen_quiet = true;
	  break;
	case 'r':
	  aptcfg->SetNoUser("Apt::Install-Recommends", "true");
	  break;
	case 'R':
	  aptcfg->SetNoUser("Apt::Install-Recommends", "false");
	  aptcfg->SetNoUser(PACKAGE "::Keep-Recommends", "true");
	  break;
	case 't':
	  aptcfg->SetNoUser("APT::Default-Release", optarg);
	  break;
	case 'o':
	  {
	    string s=optarg;
	    string::size_type eqloc=s.find('=');

	    // note that if eqloc!=s.npos, s.size()>0
	    if(eqloc==s.npos || eqloc==s.size()-1)
	      fprintf(stderr, _("-o requires an argument of the form key=value, got %s\n"), optarg);
	    else
	      {
		string key(s, 0, eqloc), value(s, eqloc+1);

		aptcfg->SetNoUser(key, value);
	      }
	  }
	  break;
	case 'v':
	  ++verbose;
	  break;
	case 'V':
	  showvers=true;
	  break;
	case 'D':
	  showdeps=true;
	  break;
	case 'Z': // ew
	  showsize=true;
	  break;
	case 'W':
	  showwhy = true;
	  break;
	case 'P':
	  always_prompt=true;
	  break;
	case ':':
	case '?':
	  usage();
	  exit(1);
	case 0:
	  switch(getopt_result)
	    {
	    case OPTION_VERSION:
	      show_version();
	      exit(0);
	    case OPTION_ALLOW_UNTRUSTED:
	      aptcfg->Set(PACKAGE "::CmdLine::Ignore-Trust-Violations", true);
	      break;
	    case OPTION_NO_NEW_INSTALLS:
	      safe_upgrade_no_new_installs = true;
	      safe_resolver_no_new_installs = true;
	      saw_new_installs_option = true;
	      break;
	    case OPTION_ALLOW_NEW_INSTALLS:
	      safe_upgrade_no_new_installs = false;
	      safe_resolver_no_new_installs = false;
	      saw_new_installs_option = true;
	      break;
	    case OPTION_NO_NEW_UPGRADES:
	      safe_resolver_no_new_upgrades = true;
	      saw_new_upgrades_option = false;
	      break;
	    case OPTION_ALLOW_NEW_UPGRADES:
	      safe_resolver_no_new_upgrades = false;
	      saw_new_upgrades_option = false;
	      break;
	    case OPTION_SAFE_RESOLVER:
	      always_use_safe_resolver = true;
	      safe_resolver_option = true;
	      break;
	    case OPTION_FULL_RESOLVER:
	      always_use_safe_resolver = false;
	      full_resolver_option = true;
	      break;
	    case OPTION_VISUAL_PREVIEW:
	      visual_preview=true;	      
	      break;
	    case OPTION_QUEUE_ONLY:
	      queue_only=true;
	      break;
	    case OPTION_PURGE_UNUSED:
	      aptcfg->Set(PACKAGE "::Purge-Unused", "true");
	      break;
	    case OPTION_ADD_USER_TAG:
	    case OPTION_REMOVE_USER_TAG:
	      {
		using aptitude::cmdline::tag_application;
		const bool is_add = (getopt_result == OPTION_ADD_USER_TAG);
		user_tags.push_back(tag_application(is_add, optarg, NULL));
		break;
	      }
	    case OPTION_ADD_USER_TAG_TO:
	    case OPTION_REMOVE_USER_TAG_FROM:
	      {
		using aptitude::cmdline::tag_application;
		const bool is_add = (getopt_result == OPTION_ADD_USER_TAG);
		const std::string arg = optarg;
		const std::string::size_type splitloc = arg.find(',');
		if(splitloc == arg.npos)
		  {
		    fprintf(stderr, _("No comma following tag name \"%s\".\n"), arg.c_str());
		    return -1;
		  }
		else
		  {
		    const std::string patternstr(arg, splitloc + 1);
		    const std::vector<const char *> terminators;
		    aptitude::matching::pkg_matcher *m =
		      aptitude::matching::parse_pattern(patternstr,
							terminators);

		    if(m == NULL)
		      {
			_error->DumpErrors();
			return -1;
		      }

		    user_tags.push_back(tag_application(is_add, optarg, m));
		  }
	      }
	    case OPTION_NOT_ARCH_ONLY:
	      arch_only = false;
	      break;
	    case OPTION_ARCH_ONLY:
	      arch_only = true;
	      break;
	    default:
	      fprintf(stderr, "%s",
		      _("WEIRDNESS: unknown option code received\n"));
	      break;
	    }

	  getopt_result=0;

	  break;
	default:
	  fprintf(stderr, "%s",
		  _("WEIRDNESS: unknown option code received\n"));
	  break;
	}
    }

  if(quiet == 0 && !isatty(1))
    aptcfg->SetNoUser("quiet", 1);
  else if(seen_quiet)
    aptcfg->SetNoUser("quiet", quiet);

  if(simulate)
    aptcfg->SetNoUser(PACKAGE "::Simulate", true);

  // Sanity-check
  if(update_only && install_only)
    {
      fprintf(stderr, "%s",
	      _("Only one of -u and -i may be specified\n"));
      usage();
      exit(1);
    }

  if((update_only || install_only) && optind!=argc)
    {
      fprintf(stderr, "%s\n",
	      _("-u and -i may not be specified in command-line mode (eg, with 'install')"));
      usage();
      exit(1);
    }

  // Possibly run off and do other commands.
  if(optind!=argc)
    {
      try
	{
	  // Connect up the "please consume errors" routine for the
	  // command-line.
	  consume_errors.connect(sigc::mem_fun(_error, &GlobalError::DumpErrors));

	  // Read the terminal width and set up its update function.
	  signal(SIGWINCH, update_screen_width);
	  update_screen_width();

	  if(update_only || install_only)
	    {
	      fprintf(stderr, "%s\n",
		      _("-u and -i may not be specified with a command"));
	      usage();
	      exit(1);
	    }

	  // TODO: warn the user if they passed --full-resolver to
	  // something other than "upgrade" or do_action.

	  if(!strcasecmp(argv[optind], "update"))
	    return cmdline_update(argc-optind, argv+optind, verbose);
	  else if(!strcasecmp(argv[optind], "clean"))
	    return cmdline_clean(argc-optind, argv+optind, simulate);
	  else if(!strcasecmp(argv[optind], "autoclean"))
	    return cmdline_autoclean(argc-optind, argv+optind, simulate);
	  else if(!strcasecmp(argv[optind], "forget-new"))
	    return cmdline_forget_new(argc-optind, argv+optind,
				      status_fname, simulate);
	  else if(!strcasecmp(argv[optind], "search"))
	    return cmdline_search(argc-optind, argv+optind,
				  status_fname,
				  display_format, width,
				  sort_policy);
	  else if(!strcasecmp(argv[optind], "why"))
	    return cmdline_why(argc - optind, argv + optind,
			       status_fname, verbose, false);
	  else if(!strcasecmp(argv[optind], "why-not"))
	    return cmdline_why(argc - optind, argv + optind,
			       status_fname, verbose, true);
	  else if( (!strcasecmp(argv[optind], "install")) ||
		   (!strcasecmp(argv[optind], "reinstall")) ||
		   (!strcasecmp(argv[optind], "dist-upgrade")) ||
		   (!strcasecmp(argv[optind], "full-upgrade")) ||
		   (!strcasecmp(argv[optind], "remove")) ||
		   (!strcasecmp(argv[optind], "purge")) ||
		   (!strcasecmp(argv[optind], "hold")) ||
		   (!strcasecmp(argv[optind], "unhold")) ||
		   (!strcasecmp(argv[optind], "markauto")) ||
		   (!strcasecmp(argv[optind], "unmarkauto")) ||
		   (!strcasecmp(argv[optind], "forbid-version")) ||
		   (!strcasecmp(argv[optind], "keep")) ||
		   (!strcasecmp(argv[optind], "keep-all")) ||
		   (!strcasecmp(argv[optind], "build-dep")) ||
		   (!strcasecmp(argv[optind], "build-depends")))
	    {
	      return cmdline_do_action(argc-optind, argv+optind,
				       status_fname,
				       simulate, assume_yes, download_only,
				       fix_broken, showvers, showdeps,
				       showsize, showwhy,
				       visual_preview, always_prompt,
				       always_use_safe_resolver,
				       safe_resolver_no_new_installs, safe_resolver_no_new_upgrades,
				       user_tags,
				       arch_only, queue_only, verbose);
	    }
	  else if(!strcasecmp(argv[optind], "safe-upgrade") ||
		  !strcasecmp(argv[optind], "upgrade"))
	    {
	      if(full_resolver_option || safe_resolver_option)
		{
		  fprintf(stderr, _("The options --safe-resolver and --full-resolver are not meaningful\nfor %s, which always uses the safe resolver.\n"),
			  argv[optind]);
		  return -1;
		}

	      return cmdline_upgrade(argc-optind, argv+optind,
				     status_fname, simulate,
				     safe_upgrade_no_new_installs,
				     assume_yes, download_only,
				     showvers, showdeps,
				     showsize, showwhy,
				     user_tags,
				     visual_preview, always_prompt,
				     arch_only, queue_only, verbose);
	    }
	  else if(!strcasecmp(argv[optind], "add-user-tag") ||
		  !strcasecmp(argv[optind], "remove-user-tag"))
	    return aptitude::cmdline::cmdline_user_tag(argc - optind, argv + optind,
						       quiet, verbose);
	  else if(!strcasecmp(argv[optind], "extract-cache-subset"))
	    return aptitude::cmdline::extract_cache_subset(argc - optind,
							   argv + optind);
	  else if(!strcasecmp(argv[optind], "download"))
	    return cmdline_download(argc-optind, argv+optind);
	  else if(!strcasecmp(argv[optind], "changelog"))
	    return cmdline_changelog(argc-optind, argv+optind);
	  else if(!strcasecmp(argv[optind], "moo"))
	    return cmdline_moo(argc-optind, argv+optind, verbose);
	  else if(!strcasecmp(argv[optind], "show"))
	    return cmdline_show(argc-optind, argv+optind, verbose);
	  else if(!strcasecmp(argv[optind], "dump-resolver"))
	    return cmdline_dump_resolver(argc-optind, argv+optind, status_fname);
	  else if(!strcasecmp(argv[optind], "check-resolver"))
	    return cmdline_check_resolver(argc-optind, argv+optind, status_fname);
	  else if(!strcasecmp(argv[optind], "help"))
	    {
	      usage();
	      exit(0);
	    }
	  // Debugging/profiling commands:
	  else if(!strcasecmp(argv[optind], "nop"))
	    {
	      OpTextProgress p(aptcfg->FindI("Quiet", 0));
	      _error->DumpErrors();
	      apt_init(&p, true);
	      exit(0);
	    }
	  else if(!strcasecmp(argv[optind], "nop-noselections"))
	    {
	      OpTextProgress p(aptcfg->FindI("Quiet", 0));
	      _error->DumpErrors();
	      apt_init(&p, false);
	      exit(0);
	    }
	  else
	    {
	      fprintf(stderr, _("Unknown command \"%s\"\n"), argv[optind]);
	      usage();
	      exit(1);
	    }
	}
      catch(StdinEOFException)
	{
	  printf("%s", _("Abort.\n"));
	  return -1;
	}
      catch(const cwidget::util::Exception &e)
	{
	  fprintf(stderr, _("Uncaught exception: %s\n"), e.errmsg().c_str());

	  std::string backtrace = e.get_backtrace();
	  if(!backtrace.empty())
	    fprintf(stderr, _("Backtrace:\n%s\n"), backtrace.c_str());
	  return -1;
	}
    }

  ui_init();

  try
    {
      progress_ref p=gen_progress_bar();
      // We can avoid reading in the package lists in the case that
      // we're about to update them (since they'd be closed and
      // reloaded anyway).  Obviously we still need them for installs,
      // since we have to get information about what to install from
      // somewhere...
      if(!update_only)
	apt_init(p.unsafe_get_ref(), true, status_fname);
      if(status_fname)
	free(status_fname);
      check_apt_errors();

      file_quit.connect(sigc::ptr_fun(cw::toplevel::exitmain));

      if(apt_cache_file)
	{
	  (*apt_cache_file)->package_state_changed.connect(sigc::ptr_fun(cw::toplevel::update));
	  (*apt_cache_file)->package_category_changed.connect(sigc::ptr_fun(cw::toplevel::update));
	}

      do_new_package_view(*p.unsafe_get_ref());
      p->destroy();
      p = NULL;

      if(update_only)
	do_update_lists();
      else if(install_only)
	do_package_run_or_show_preview();
      //install_or_remove_packages();

      ui_main();
    }
  catch(const cwidget::util::Exception &e)
    {
      cw::toplevel::shutdown();

      fprintf(stderr, _("Uncaught exception: %s\n"), e.errmsg().c_str());

      std::string backtrace = e.get_backtrace();
      if(!backtrace.empty())
	fprintf(stderr, _("Backtrace:\n%s\n"), backtrace.c_str());
      return -1;
    }

  return 0;
}
