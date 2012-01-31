// main.cc  (neé testscr.cc)
//
//  Copyright 1999-2011 Daniel Burrows
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
#include <generic/apt/matching/match.h>
#include <generic/apt/matching/parse.h>
#include <generic/apt/matching/pattern.h>

#include <generic/problemresolver/exceptions.h>

#include <generic/util/logging.h>
#include <generic/util/temp.h>
#include <generic/util/util.h>

#ifdef HAVE_GTK
#include <gtkmm.h>
#endif

#ifdef HAVE_QT
#include <QtCore/qglobal.h> // To get the Qt version number
#endif

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
#include <cmdline/cmdline_user_tag.h>
#include <cmdline/cmdline_versions.h>
#include <cmdline/cmdline_why.h>
#include <cmdline/terminal.h>

#include <sigc++/functors/ptr_fun.h>

#include <apt-pkg/error.h>
#include <apt-pkg/init.h>

#include <boost/format.hpp>
#include <boost/optional.hpp>

#ifdef HAVE_GTK
#include "gtk/gui.h"
#include "gtk/init.h"
#endif

#ifdef HAVE_QT
#include "qt/qt_main.h"
#endif

#include <fstream>

#include "loggers.h"
#include "progress.h"
#include "pkg_columnizer.h"
#include "pkg_grouppolicy.h"
#include "pkg_view.h"
#include "ui.h"

namespace cw = cwidget;

using aptitude::Loggers;

using boost::optional;

using logging::DEBUG_LEVEL;
using logging::ERROR_LEVEL;
using logging::FATAL_LEVEL;
using logging::INFO_LEVEL;
using logging::OFF_LEVEL;
using logging::WARN_LEVEL;
using logging::TRACE_LEVEL;

using logging::Logger;
using logging::LoggerPtr;
using logging::log_level;

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
#ifdef HAVE_GTK
  printf(_("  Gtk+ version %d.%d.%d\n"),
	 GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION);
  printf(_("  Gtk-- version %d.%d.%d\n"),
	 GTKMM_MAJOR_VERSION, GTKMM_MINOR_VERSION, GTKMM_MICRO_VERSION);
#else
  printf(_("  Gtk+ support disabled.\n"));
#endif
#ifdef HAVE_QT
  printf(_("  Compiled with Qt %s\n"), QT_VERSION_STR);
  printf(_("  Running on Qt %s\n"), qVersion());
#else
  printf(_("  Qt support disabled.\n"));
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
  printf(_(" install      - Install/upgrade packages.\n"));
  printf(_(" remove       - Remove packages.\n"));
  printf(_(" purge        - Remove packages and their configuration files.\n"));
  printf(_(" hold         - Place packages on hold.\n"));
  printf(_(" unhold       - Cancel a hold command for a package.\n"));
  printf(_(" markauto     - Mark packages as having been automatically installed.\n"));
  printf(_(" unmarkauto   - Mark packages as having been manually installed.\n"));
  printf(_(" forbid-version - Forbid aptitude from upgrading to a specific package version.\n"));
  printf(_(" update       - Download lists of new/upgradable packages.\n"));
  printf(_(" safe-upgrade - Perform a safe upgrade.\n"));
  printf(_(" full-upgrade - Perform an upgrade, possibly installing and removing packages.\n"));
  printf(_(" build-dep    - Install the build-dependencies of packages.\n"));
  printf(_(" forget-new   - Forget what packages are \"new\".\n"));
  printf(_(" search       - Search for a package by name and/or expression.\n"));
  printf(_(" show         - Display detailed information about a package.\n"));
  printf(_(" versions     - Displays the versions of specified packages.\n"));
  printf(_(" clean        - Erase downloaded package files.\n"));
  printf(_(" autoclean    - Erase old downloaded package files.\n"));
  printf(_(" changelog    - View a package's changelog.\n"));
  printf(_(" download     - Download the .deb file for a package.\n"));
  printf(_(" reinstall    - Download and (possibly) reinstall a currently installed package.\n"));
  printf(_(" why          - Show the manually installed packages that require a package, or\n"
           "                why one or more packages would require the given package.\n"));
  printf(_(" why-not      - Show the manually installed packages that lead to a conflict\n"
           "                with the given package, or why one or more packages would\n"
           "                lead to a conflict with the given package if installed.\n"));
  printf("\n");
  printf(_("  Options:\n"));
  printf(_(" -h             This help text.\n"));
#ifdef HAVE_GTK
  printf(_(" --gui          Use the GTK GUI even if disabled in the configuration.\n"));
#endif
  printf(_(" --no-gui       Do not use the GTK GUI even if available.\n"));
#ifdef HAVE_QT
  printf(_(" --qt           Use the Qt GUI.\n"));
  printf(_(" --no-qt        Do not use the Qt GUI even if enabled in the configuration.\n"));
#endif
  printf(_(" -s             Simulate actions, but do not actually perform them.\n"));
  printf(_(" -d             Only download packages, do not install or remove anything.\n"));
  printf(_(" -P             Always prompt for confirmation of actions.\n"));
  printf(_(" -y             Assume that the answer to simple yes/no questions is 'yes'.\n"));
  printf(_(" -F format      Specify a format for displaying search results; see the manual.\n"));
  printf(_(" -O order       Specify how search results should be sorted; see the manual.\n"));
  printf(_(" -w width       Specify the display width for formatting search results.\n"));
  printf(_(" -f             Aggressively try to fix broken packages.\n"));
  printf(_(" -V             Show which versions of packages are to be installed.\n"));
  printf(_(" -D             Show the dependencies of automatically changed packages.\n"));
  printf(_(" -Z             Show the change in installed size of each package.\n"));
  printf(_(" -v             Display extra information. (may be supplied multiple times).\n"));
  printf(_(" -t [release]   Set the release from which packages should be installed.\n"));
  printf(_(" -q             In command-line mode, suppress the incremental progress\n"
           "                indicators.\n"));
  printf(_(" -o key=val     Directly set the configuration option named 'key'.\n"));
  printf(_(" --with(out)-recommends	Specify whether or not to treat recommends as\n"
           "                strong dependencies.\n"));
  printf(_(" -S fname       Read the aptitude extended status info from fname.\n"));
  printf(_(" -u             Download new package lists on startup.\n"));
  printf(_("                  (terminal interface only)\n"));
  printf(_(" -i             Perform an install run on startup.\n"));
  printf(_("                  (terminal interface only)\n"));
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
  OPTION_SHOW_RESOLVER_ACTIONS,
  OPTION_NO_SHOW_RESOLVER_ACTIONS,
  OPTION_ARCH_ONLY,
  OPTION_NOT_ARCH_ONLY,
  OPTION_DISABLE_COLUMNS,
  OPTION_GUI,
  OPTION_NO_GUI,
  OPTION_QT_GUI,
  OPTION_NO_QT_GUI,
  OPTION_LOG_LEVEL,
  OPTION_LOG_FILE,
  OPTION_LOG_CONFIG_FILE,
  OPTION_LOG_RESOLVER,
  OPTION_SHOW_SUMMARY,
  OPTION_AUTOCLEAN_ON_STARTUP,
  OPTION_CLEAN_ON_STARTUP,
  OPTION_GROUP_BY,
  OPTION_SHOW_PACKAGE_NAMES,
  OPTION_NEW_GUI,
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
  {"disable-columns", 0, &getopt_result, OPTION_DISABLE_COLUMNS},
  {"no-new-installs", 0, &getopt_result, OPTION_NO_NEW_INSTALLS},
  {"no-new-upgrades", 0, &getopt_result, OPTION_NO_NEW_UPGRADES},
  {"allow-new-installs", 0, &getopt_result, OPTION_ALLOW_NEW_INSTALLS},
  {"allow-new-upgrades", 0, &getopt_result, OPTION_ALLOW_NEW_UPGRADES},
  {"safe-resolver", 0, &getopt_result, OPTION_SAFE_RESOLVER},
  {"full-resolver", 0, &getopt_result, OPTION_FULL_RESOLVER},
  {"show-resolver-actions", 0, &getopt_result, OPTION_SHOW_RESOLVER_ACTIONS},
  {"no-show-resolver-actions", 0, &getopt_result, OPTION_NO_SHOW_RESOLVER_ACTIONS},
  {"visual-preview", 0, &getopt_result, OPTION_VISUAL_PREVIEW},
  {"schedule-only", 0, &getopt_result, OPTION_QUEUE_ONLY},
  {"purge-unused", 0, &getopt_result, OPTION_PURGE_UNUSED},
  {"add-user-tag", 1, &getopt_result, OPTION_ADD_USER_TAG},
  {"add-user-tag-to", 1, &getopt_result, OPTION_ADD_USER_TAG_TO},
  {"remove-user-tag", 1, &getopt_result, OPTION_REMOVE_USER_TAG},
  {"remove-user-tag-from", 1, &getopt_result, OPTION_REMOVE_USER_TAG_FROM},
  {"arch-only", 0, &getopt_result, OPTION_ARCH_ONLY},
  {"not-arch-only", 0, &getopt_result, OPTION_NOT_ARCH_ONLY},
#ifdef HAVE_GTK
  {"gui", 0, &getopt_result, OPTION_GUI},
#endif
  {"no-gui", 0, &getopt_result, OPTION_NO_GUI},
#ifdef HAVE_QT
  {"qt", 0, &getopt_result, OPTION_QT_GUI},
  {"no-qt", 0, &getopt_result, OPTION_NO_QT_GUI},
#endif
  {"log-level", 1, &getopt_result, OPTION_LOG_LEVEL},
  {"log-file", 1, &getopt_result, OPTION_LOG_FILE},
  {"log-config-file", 1, &getopt_result, OPTION_LOG_CONFIG_FILE},
  {"log-resolver", 0, &getopt_result, OPTION_LOG_RESOLVER},
  {"show-summary", 2, &getopt_result, OPTION_SHOW_SUMMARY},
  {"autoclean-on-startup", 0, &getopt_result, OPTION_AUTOCLEAN_ON_STARTUP},
  {"clean-on-startup", 0, &getopt_result, OPTION_CLEAN_ON_STARTUP},
  {"group-by", 1, &getopt_result, OPTION_GROUP_BY},
  {"show-package-names", 1, &getopt_result, OPTION_SHOW_PACKAGE_NAMES},
  {"new-gui", 0, &getopt_result, OPTION_NEW_GUI},
  {0,0,0,0}
};

const char *argv0;

namespace
{
  bool strncase_eq_with_translation(const std::string &s1, const char *s2)
  {
    if(strcasecmp(s1.c_str(), s2) == 0)
      return true;
    else if(strcasecmp(s1.c_str(), _(s2)) == 0)
      return true;
    else
      return false;
  }

  class log_level_map
  {
    std::map<std::string, log_level> levels;

    void add_level(const std::string &s, log_level level)
    {
      std::string tmp;
      for(std::string::const_iterator it = s.begin(); it != s.end(); ++it)
	tmp.push_back(toupper(*it));

      levels[tmp] = level;
    }

  public:
    log_level_map()
    {
      using namespace logging;

      // ForTranslators: This is a log level that the user can pass on
      // the command-line or set in the configuration file.
      add_level(N_("trace"), TRACE_LEVEL);
      // ForTranslators: This is a log level that the user can pass on
      // the command-line or set in the configuration file.
      add_level(N_("debug"), DEBUG_LEVEL);
      // ForTranslators: This is a log level that the user can pass on
      // the command-line or set in the configuration file.
      add_level(N_("info"), INFO_LEVEL);
      // ForTranslators: This is a log level that the user can pass on
      // the command-line or set in the configuration file.
      add_level(N_("warn"), WARN_LEVEL);
      // ForTranslators: This is a log level that the user can pass on
      // the command-line or set in the configuration file.
      add_level(N_("error"), ERROR_LEVEL);
      // ForTranslators: This is a log level that the user can pass on
      // the command-line or set in the configuration file.
      add_level(N_("fatal"), FATAL_LEVEL);
      // ForTranslators: This is a log level that the user can pass on
      // the command-line or set in the configuration file.
      add_level(N_("off"), OFF_LEVEL);

      std::vector<std::pair<std::string, log_level> >
	tmp(levels.begin(), levels.end());

      for(std::vector<std::pair<std::string, log_level> >::const_iterator
	    it = tmp.begin(); it != tmp.end(); ++it)
	{
	  // Make sure that the untranslated entries always override
	  // the translated ones so that instructions in English
	  // always work.
	  std::map<std::string, log_level>::const_iterator found =
	    levels.find(it->first);

	  if(found == levels.end())
	    add_level(_(it->first.c_str()), it->second);
	}
    }

    typedef std::map<std::string, log_level>::const_iterator const_iterator;

    const_iterator find(const std::string &s) const
    {
      std::string tmp;
      for(std::string::const_iterator it = s.begin(); it != s.end(); ++it)
	tmp.push_back(toupper(*it));

      return levels.find(tmp);
    }

    const_iterator end() const
    {
      return levels.end();
    }
  };

  log_level_map log_levels;

  /** \brief Parse a logging level.
   *
   *  Logging levels have the form [<logger>:]level, where
   *  <logger> is an optional logger name.
   */
  void apply_logging_level(const std::string &s)
  {
    std::string::size_type colon_loc = s.rfind(':');
    std::string level_name;
    std::string logger_name;

    if(colon_loc == std::string::npos)
      level_name = s;
    else
      {
	level_name = std::string(s, colon_loc + 1);
	logger_name = std::string(s, 0, colon_loc);
      }

    optional<log_level> level;

    log_level_map::const_iterator found =
      log_levels.find(level_name);
    if(found != log_levels.end())
      level = found->second;

    if(!level)
      {
	// ForTranslators: both the translated and the untranslated
	// log level names are accepted here.
	_error->Error(_("Unknown log level name \"%s\" (expected \"trace\", \"debug\", \"info\", \"warn\", \"error\", \"fatal\", or \"off\")."),
		      level_name.c_str());
	return;
      }

    LoggerPtr targetLogger = Logger::getLogger(logger_name);

    if(!targetLogger)
      {
	_error->Error(_("Invalid logger name \"%s\"."),
		      logger_name.c_str());
	return;
      }

    targetLogger->setLevel(*level);
  }

  /** \brief Apply logging levels from the configuration file. */
  void apply_config_file_logging_levels(Configuration *config)
  {
    const Configuration::Item *tree = config->Tree(PACKAGE "::Logging::Levels");
    if(tree == NULL)
      return;

    for(Configuration::Item *item = tree->Child; item != NULL;
	item = item->Next)
      apply_logging_level(item->Value);
  }

  /** \brief Set some standard logging levels to output log
   *  information about the resolver.
   *
   *  The information this generates is enough to let the log parser
   *  show a visualization of what happened.
   */
  void enable_resolver_log()
  {
    Loggers::getAptitudeResolverSearch()->setLevel(TRACE_LEVEL);
    Loggers::getAptitudeResolverSearchCosts()->setLevel(INFO_LEVEL);
  }
}

// Ensure that the cache is always closed when main() exits.  Without
// this, there might be dangling flyweights hanging around, and those
// can trigger aborts when the static flyweight pool is destroyed.
//
// TBH, I think it might be worth writing our own flyweight stand-in
// to avoid this particular bit of stupid.  On the other hand, it
// might be better to fully shut down the cache all the time, to
// better detect leaks and so on?  I'm undecided -- and it shouldn't
// take too long to clear out the cache.
struct close_cache_on_exit
{
  close_cache_on_exit()
  {
  }

  ~close_cache_on_exit()
  {
    apt_shutdown();
  }
};

void do_message_logged(std::ostream &out,
                       const char *sourceFilename,
                       int sourceLineNumber,
                       log_level level,
                       LoggerPtr logger,
                       const std::string &msg)
{
  time_t current_time = 0;
  struct tm local_current_time;

  time(&current_time);
  localtime_r(&current_time, &local_current_time);

  out << sstrftime("%F %T", &local_current_time)
      << " [" << pthread_self() << "] "
      << sourceFilename << ":" << sourceLineNumber
      << " " << describe_log_level(level)
      << " " << logger->getCategory()
      << " - " << msg << std::endl << std::flush;
}

void handle_message_logged(const char *sourceFilename,
                           int sourceLineNumber,
                           log_level level,
                           LoggerPtr logger,
                           const std::string &msg,
                           const std::string &filename)
{
  if(filename == "-")
    do_message_logged(std::cout,
                      sourceFilename,
                      sourceLineNumber,
                      level,
                      logger,
                      msg);
  else
    {
      std::ofstream f(filename.c_str(), std::ios::app);
      if(f)
        do_message_logged(f,
                          sourceFilename,
                          sourceLineNumber,
                          level,
                          logger,
                          msg);
      // Since logging is just for debugging, I don't do anything if
      // the log file can't be opened.
    }
}

int main(int argc, char *argv[])
{
  // Block signals that we want to sigwait() on by default and put the
  // signal mask into a known state.  This ensures that unless threads
  // deliberately ask for a signal, they don't get it, meaning that
  // sigwait() should work as expected.  (the alternative, blocking
  // all signals, is troublesome: we would have to ensure that fatal
  // signals and other things that shouldn't be blocked get removed)
  //
  // When aptitude used log4cxx, we ran into the fact that it doesn't
  // ensure that its threads don't block signals, so cwidget wasn't
  // able to sigwait() on SIGWINCH without this.  (cwidget is guilty
  // of the same thing, but that doesn't cause problems for aptitude)
  {
    sigset_t mask;

    sigemptyset(&mask);

    sigaddset(&mask, SIGWINCH);

    pthread_sigmask(SIG_BLOCK, &mask, NULL);
  }

  srandom(time(0));

  // See earlier note
  //
  //cw::util::transcode_mbtow_err=handle_mbtow_error;
  //cw::util::transcode_wtomb_err=handle_wtomb_error;

  setlocale(LC_ALL, "");
  bindtextdomain(PACKAGE, LOCALEDIR);
  textdomain(PACKAGE);

  // An environment variable is used mostly because of the utterly
  // lame option parsing that aptitude does.  A better option parser
  // would pre-parse the options and remember them in a structure of
  // some sort, meaning that I could parse the command-line options
  // before I set up the apt configuration structures.
  const char * const rootdir = getenv("APT_ROOT_DIR");
  apt_preinit(rootdir);

  close_cache_on_exit close_on_exit;

  // The filename to read status information from.
  char *status_fname=NULL;
  string package_display_format = aptcfg->Find(PACKAGE "::CmdLine::Package-Display-Format", "%c%a%M %p# - %d#");
  string version_display_format = aptcfg->Find(PACKAGE "::CmdLine::Version-Display-Format", "%c%a%M %p# %t %i");
  string group_by_mode_string = aptcfg->Find(PACKAGE "::CmdLine::Versions-Group-By", "auto");
  string show_package_names_mode_string = aptcfg->Find(PACKAGE "::CmdLine::Versions-Show-Package-Names", "auto");
  string sort_policy="name,version";
  string width=aptcfg->Find(PACKAGE "::CmdLine::Package-Display-Width", "");
  // Set to a non-empty string to enable logging simplistically; set
  // to "-" to log to stdout.
  string log_file = aptcfg->Find(PACKAGE "::Logging::File", "");
  bool simulate = aptcfg->FindB(PACKAGE "::CmdLine::Simulate", false) ||
    aptcfg->FindB(PACKAGE "::Simulate", false);
  bool download_only=aptcfg->FindB(PACKAGE "::CmdLine::Download-Only", false);;
  bool arch_only = aptcfg->FindB("Apt::Get::Arch-Only", false);

  bool update_only=false, install_only=false, queue_only=false;
  bool autoclean_only = false;
  bool clean_only = false;
  bool assume_yes=aptcfg->FindB(PACKAGE "::CmdLine::Assume-Yes", false);
  bool fix_broken=aptcfg->FindB(PACKAGE "::CmdLine::Fix-Broken", false);
  bool safe_resolver_no_new_installs = aptcfg->FindB(PACKAGE "::Safe-Resolver::No-New-Installs", false);
  bool safe_resolver_no_new_upgrades = aptcfg->FindB(PACKAGE "::Safe-Resolver::No-New-Upgrades", false);
  bool safe_resolver_show_resolver_actions = aptcfg->FindB(PACKAGE "::Safe-Resolver::Show-Resolver-Actions", false);

  resolver_mode_tp resolver_mode = resolver_mode_default;
  if(aptcfg->FindB(PACKAGE "::Always-Use-Safe-Resolver", false))
    resolver_mode = resolver_mode_safe;

  bool disable_columns = aptcfg->FindB(PACKAGE "::CmdLine::Disable-Columns", false);

  bool showvers=aptcfg->FindB(PACKAGE "::CmdLine::Show-Versions", false);
  bool showdeps=aptcfg->FindB(PACKAGE "::CmdLine::Show-Deps", false);
  bool showsize=aptcfg->FindB(PACKAGE "::CmdLine::Show-Size-Changes", false);
  bool showwhy = aptcfg->FindB(PACKAGE "::CmdLine::Show-Why", false);
  string show_why_summary_mode = aptcfg->Find(PACKAGE "::CmdLine::Show-Summary", "no-summary");
  bool visual_preview=aptcfg->FindB(PACKAGE "::CmdLine::Visual-Preview", false);
  bool always_prompt=aptcfg->FindB(PACKAGE "::CmdLine::Always-Prompt", false);
  int verbose=aptcfg->FindI(PACKAGE "::CmdLine::Verbose", 0);
  bool seen_quiet = false;
  int quiet = 0;
  std::vector<aptitude::cmdline::tag_application> user_tags;

#ifdef HAVE_GTK
  // TODO: this should be a configuration option.
  bool use_gtk_gui = aptcfg->FindB(PACKAGE "::Start-Gui", true);
  // Use the in-progress new GUI harness instead of the old code.
  bool use_new_gtk_gui = false;
#endif

#ifdef HAVE_QT
  // Use Qt frontend.
  bool use_qt_gui = false;
#endif

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

  // By default don't log anything below WARN.
  Logger::getLogger("")->setLevel(WARN_LEVEL);

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
	  package_display_format=optarg;
          version_display_format = optarg;
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
	  aptcfg->SetNoUser("Apt::AutoRemove::RecommendsImportant", "true");
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
	    case OPTION_SHOW_RESOLVER_ACTIONS:
	      safe_resolver_show_resolver_actions = true;
	      break;
	    case OPTION_NO_SHOW_RESOLVER_ACTIONS:
	      safe_resolver_show_resolver_actions = false;
	      break;
	    case OPTION_NO_NEW_INSTALLS:
	      safe_resolver_no_new_installs = true;
	      break;
	    case OPTION_ALLOW_NEW_INSTALLS:
	      safe_resolver_no_new_installs = false;
	      break;
	    case OPTION_NO_NEW_UPGRADES:
	      safe_resolver_no_new_upgrades = true;
	      break;
	    case OPTION_ALLOW_NEW_UPGRADES:
	      safe_resolver_no_new_upgrades = false;
	      break;
	    case OPTION_SAFE_RESOLVER:
	      resolver_mode = resolver_mode_safe;
	      break;
	    case OPTION_FULL_RESOLVER:
	      resolver_mode = resolver_mode_full;
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
		    cwidget::util::ref_ptr<aptitude::matching::pattern> p =
		      aptitude::matching::parse(patternstr,
						terminators,
						true,
						false);

		    if(!p.valid())
		      {
			_error->DumpErrors();
			return -1;
		      }

		    user_tags.push_back(tag_application(is_add, optarg, p));
		  }
	      }
	    case OPTION_NOT_ARCH_ONLY:
	      arch_only = false;
	      break;
	    case OPTION_ARCH_ONLY:
	      arch_only = true;
	      break;
	    case OPTION_DISABLE_COLUMNS:
	      disable_columns = true;
	      break;
#ifdef HAVE_GTK
	    case OPTION_GUI:
	      use_gtk_gui = true;
	      break;
	    case OPTION_NO_GUI:
	      use_gtk_gui = false;
	      break;
#else
	    case OPTION_NO_GUI:
	      // Recognize it as a NOP.
	      break;
#endif
	    case OPTION_LOG_LEVEL:
	      apply_logging_level(optarg);
	      break;
	    case OPTION_LOG_FILE:
	      log_file = optarg;
	      break;
	    case OPTION_LOG_RESOLVER:
	      enable_resolver_log();
	      break;

	    case OPTION_SHOW_SUMMARY:
	      if(optarg == NULL)
		show_why_summary_mode = "first-package";
	      else
		show_why_summary_mode = optarg;
	      break;

	    case OPTION_AUTOCLEAN_ON_STARTUP:
	      autoclean_only = true;
	      break;

	    case OPTION_CLEAN_ON_STARTUP:
	      clean_only = true;
	      break;

            case OPTION_GROUP_BY:
              group_by_mode_string = optarg;
              break;

            case OPTION_SHOW_PACKAGE_NAMES:
              show_package_names_mode_string = optarg;
              break;

            case OPTION_NEW_GUI:
#ifdef HAVE_GTK
              use_new_gtk_gui = true;
#endif
              break;
#ifdef HAVE_QT
	    case OPTION_QT_GUI:
	      use_qt_gui = true;
	      break;

	    case OPTION_NO_QT_GUI:
	      use_qt_gui = false;
	      break;
#endif
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

  group_by_option group_by_mode;
  try
    {
      group_by_mode = parse_group_by_option(group_by_mode_string);
    }
  catch(std::exception &ex)
    {
      _error->Error("%s", ex.what());
      group_by_mode = group_by_auto;
    }

  show_package_names_option show_package_names_mode;
  if(show_package_names_mode_string == "never" ||
     show_package_names_mode_string == P_("--show-package-names|never"))
    show_package_names_mode = show_package_names_never;
  else if(show_package_names_mode_string == "auto" ||
          show_package_names_mode_string == P_("--show-package-names|auto"))
    show_package_names_mode = show_package_names_auto;
  else if(show_package_names_mode_string == "always" ||
          show_package_names_mode_string == P_("--show-package-names|always"))
    show_package_names_mode = show_package_names_always;
  else
    {
      _error->Error("%s",
                    (boost::format(_("Invalid package names display mode \"%s\" (should be \"never\", \"auto\", or \"always\")."))
                     % show_package_names_mode_string).str().c_str());
      show_package_names_mode = show_package_names_auto;
    }

  aptitude::why::roots_string_mode why_display_mode;
  if(show_why_summary_mode == "no-summary" || show_why_summary_mode == _("no-summary"))
    why_display_mode = aptitude::why::no_summary;
  else if(show_why_summary_mode == "first-package" || show_why_summary_mode == _("first-package"))
    why_display_mode = aptitude::why::show_requiring_packages;
  else if(show_why_summary_mode == "first-package-and-type" || show_why_summary_mode == _("first-package-and-type"))
    why_display_mode = aptitude::why::show_requiring_packages_and_strength;
  else if(show_why_summary_mode == "all-packages" || show_why_summary_mode == _("all-packages"))
    why_display_mode = aptitude::why::show_chain;
  else if(show_why_summary_mode == "all-packages-with-dep-versions" || show_why_summary_mode == _("all-packages-with-dep-versions"))
    why_display_mode = aptitude::why::show_chain_with_versions;
  else
    {
      // ForTranslators: "why" here is the aptitude command name and
      // should not be translated.
      _error->Error(_("Invalid \"why\" summary mode \"%s\": expected \"no-summary\", \"first-package\", \"first-package-and-type\", \"all-packages\", or \"all-packages-with-dep-versions\"."),
		    show_why_summary_mode.c_str());
      why_display_mode = aptitude::why::no_summary;
    }

  if(!log_file.empty())
    Logger::getLogger("")
      ->connect_message_logged(sigc::bind(sigc::ptr_fun(&handle_message_logged),
                                          log_file));

  temp::initialize("aptitude");

  const bool debug_search = aptcfg->FindB(PACKAGE "::CmdLine::Debug-Search", false);

  int curr_quiet = aptcfg->FindI("quiet", 0);
  if(seen_quiet)
    aptcfg->SetNoUser("quiet", quiet);
  if(quiet == 0 && !isatty(1))
    aptcfg->SetNoUser("quiet", std::max(curr_quiet, 1));

  if(simulate)
    aptcfg->SetNoUser(PACKAGE "::Simulate", true);

  // Sanity-check
  {
    int num_startup_actions = 0;
    if(update_only)
      ++num_startup_actions;
    if(install_only)
      ++num_startup_actions;
    if(autoclean_only)
      ++num_startup_actions;
    if(clean_only)
      ++num_startup_actions;

    if(num_startup_actions > 1)
      {
	fprintf(stderr, "%s",
		_("Only one of --auto-clean-on-startup, --clean-on-startup, -i, and -u may be specified\n"));
	usage();
	exit(1);
      }
  }

  if((update_only || install_only || autoclean_only || clean_only) && optind != argc)
    {
      fprintf(stderr, "%s\n",
	      _("-u, -i, and --clean-on-startup may not be specified in command-line mode (eg, with 'install')"));
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
	  consume_errors.connect(sigc::mem_fun(_error, (void (GlobalError::*)()) &GlobalError::DumpErrors));

	  if(update_only || install_only || autoclean_only || clean_only)
	    {
	      fprintf(stderr, "%s\n",
		      _("-u, -i, and --clean-on-startup may not be specified with a command"));
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
				  package_display_format, width,
				  sort_policy,
				  disable_columns,
				  debug_search);
          else if(!strcasecmp(argv[optind], "versions"))
            return cmdline_versions(argc - optind, argv + optind,
                                    status_fname,
                                    version_display_format, width,
                                    sort_policy,
                                    disable_columns,
                                    debug_search,
                                    group_by_mode,
                                    show_package_names_mode);
	  else if(!strcasecmp(argv[optind], "why"))
	    return cmdline_why(argc - optind, argv + optind,
			       status_fname, verbose,
			       why_display_mode, false);
	  else if(!strcasecmp(argv[optind], "why-not"))
	    return cmdline_why(argc - optind, argv + optind,
			       status_fname, verbose,
			       why_display_mode, true);
	  else if( (!strcasecmp(argv[optind], "install")) ||
		   (!strcasecmp(argv[optind], "reinstall")) ||
		   (!strcasecmp(argv[optind], "dist-upgrade")) ||
		   (!strcasecmp(argv[optind], "full-upgrade")) ||
		   (!strcasecmp(argv[optind], "safe-upgrade")) ||
		   (!strcasecmp(argv[optind], "upgrade")) ||
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
				       resolver_mode, safe_resolver_show_resolver_actions,
				       safe_resolver_no_new_installs, safe_resolver_no_new_upgrades,
				       user_tags,
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

#ifdef HAVE_QT
  if(use_qt_gui)
    {
      if(aptitude::gui::qt::main(argc, argv))
        return 0;

      // Otherwise, fall back to trying to start a curses interface
      // (assume that we can't contact the X server, or maybe that we
      // can't load the UI definition)
    }
#endif

#ifdef HAVE_GTK
  if(use_gtk_gui)
    {
      if(use_new_gtk_gui)
        {
          if(gui::init(argc, argv))
            return 0;
        }
      else if(gui::main(argc, argv))
	return 0;
      // Otherwise, fall back to trying to start a curses interface
      // (assume that we can't contact the X server, or maybe that we
      // can't load the UI definition)
    }
#endif

    {
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
            apt_init(p->get_progress().unsafe_get_ref(), true, status_fname);
          if(status_fname)
            free(status_fname);
          check_apt_errors();

          file_quit.connect(sigc::ptr_fun(cw::toplevel::exitmain));

          if(apt_cache_file)
            {
              (*apt_cache_file)->package_state_changed.connect(sigc::ptr_fun(cw::toplevel::update));
              (*apt_cache_file)->package_category_changed.connect(sigc::ptr_fun(cw::toplevel::update));
            }

	  if(!aptcfg->FindB(PACKAGE "::UI::Flat-View-As-First-View", false))
	    do_new_package_view(*p->get_progress().unsafe_get_ref());
	  else
	    do_new_flat_view(*p->get_progress().unsafe_get_ref());

          p->destroy();
          p = NULL;

          if(update_only)
            do_update_lists();
          else if(install_only)
            do_package_run_or_show_preview();
	  else if(autoclean_only)
	    do_autoclean();
	  else if(clean_only)
	    do_clean();

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

      // The cache is closed when the close_on_exit object declared
      // above is destroyed.

      return 0;
    }
}
