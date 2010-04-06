// cmdline_changelog.cc
//
//  Copyright 2004 Daniel Burrows

#include "cmdline_changelog.h"

#include "cmdline_common.h"
#include "cmdline_main_loop.h"
#include "cmdline_progress.h"
#include "cmdline_util.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_queue.h>
#include <generic/apt/pkg_changelog.h>

#include <apt-pkg/error.h>
#include <apt-pkg/metaindex.h>
#include <apt-pkg/progress.h>
#include <apt-pkg/sourcelist.h>
#include <apt-pkg/srcrecords.h>

#include <boost/format.hpp>
#include <boost/make_shared.hpp>
#include <boost/ref.hpp>

#include <sigc++/adaptors/bind.h>

#include <signal.h>
#include <stdlib.h>

#include <sstream>

using namespace std;

namespace
{

void set_name(temp::name n, temp::name *target)
{
  *target = n;
}

  // Temporary hack to display download progress for a single file.
  //
  // Eventually there should be a generic class to track the progress
  // of a group of downloads, but I don't have enough infrastructure
  // for that yet.
  class single_download_progress : public aptitude::download_callbacks
  {
    std::string description;
    bool quiet;

  public:
    single_download_progress(const std::string &_description,
			     bool _quiet)
      : description(_description), quiet(_quiet)
    {
    }

    void partial_download(const temp::name &name,
			  unsigned long currentSize,
			  unsigned long totalSize)
    {
      // Hack: ripped this code from AcqTextStatus.

      if (quiet)
	return;

      unsigned long TotalBytes = totalSize;
      unsigned long CurrentBytes = currentSize;

      std::ostringstream bufferStream;

      bufferStream << long(double(CurrentBytes * 100.0) / double(TotalBytes));
      bufferStream << "% [" << description.c_str() << "]";

      std::string Buffer(bufferStream.str());

      sigset_t Sigs,OldSigs;
      sigemptyset(&Sigs);
      sigaddset(&Sigs,SIGWINCH);
      sigprocmask(SIG_BLOCK,&Sigs,&OldSigs);

      int Len = (int)Buffer.size();
      if (Len < screen_width)
	Buffer.insert(Buffer.end(), screen_width - Len, ' ');
      std::string BlankLine(screen_width, ' ');
      sigprocmask(SIG_SETMASK,&OldSigs,0);

      // Draw the current status
      if (Buffer.size() == BlankLine.size())
	std::cout << '\r' << Buffer << std::flush;
      else
	std::cout << '\r' << BlankLine << '\r' << Buffer << std::flush;
    }

    void success(const temp::name &filename)
    {
      if(!quiet)
	std::cout << "\r" << std::string(screen_width, ' ') << "\r";

      std::cout << _("Get:") << " " << description << std::endl;
    }

    void failure(const std::string &msg)
    {
      if(!quiet)
	std::cout << "\r" << std::string(screen_width, ' ') << "\r";

      std::cout << _("Err ") << description << std::endl;
    }
  };

  class changelog_download_callbacks : public single_download_progress
  {
    // Where to store the changelog file, if any.
    temp::name &out_changelog_file;

  public:
    changelog_download_callbacks(temp::name &_out_changelog_file,
				 const std::string &short_description)
      : single_download_progress(short_description,
				 aptcfg->FindI("Quiet", 0) > 0),
	out_changelog_file(_out_changelog_file)
    {
    }

    void success(const temp::name &name)
    {
      single_download_progress::success(name);

      out_changelog_file = name;

      aptitude::cmdline::exit_main();
    }

    void failure(const std::string &msg)
    {
      single_download_progress::failure(msg);

      _error->Error(_("Changelog download failed: %s"), msg.c_str());
      _error->DumpErrors();

      out_changelog_file = temp::name();
      aptitude::cmdline::exit_main();
    }
  };

  void get_changelog(const pkgCache::VerIterator &ver,
		     temp::name &out_changelog_file)
  {
    const std::string short_description =
      (boost::format("Changelog of %s") % ver.ParentPkg().Name()).str();

    boost::shared_ptr<changelog_download_callbacks>
      callbacks = boost::make_shared<changelog_download_callbacks>(boost::ref(out_changelog_file),
								   short_description);

    get_changelog(aptitude::apt::changelog_info::create(ver),
		  callbacks,
		  aptitude::cmdline::post_thunk);

    aptitude::cmdline::main_loop();
  }

  /** \brief download a source package's changelog.
   *
   *  \param srcpkg the source package name
   *  \param ver the version of the source package
   *  \param section the section of the source package
   *  \param name the name of the package that the user provided
   *              (e.g., the binary package that the changelog command
   *               was executed on)
   *  \param out_changelog_file  Where to store the resulting file.
   *
   *  This routine exits once the download is complete.
   */
  void get_changelog_from_source(const std::string &srcpkg,
				 const std::string &ver,
				 const std::string &section,
				 const std::string &name,
				 temp::name &out_changelog_file)
  {
    const std::string short_description =
      (boost::format("Changelog of %s") % name).str();

    boost::shared_ptr<changelog_download_callbacks>
      callbacks = boost::make_shared<changelog_download_callbacks>(boost::ref(out_changelog_file),
								   short_description);

    boost::shared_ptr<aptitude::apt::changelog_info>
      info = aptitude::apt::changelog_info::create(srcpkg, ver, section, name);

    get_changelog(info,
		  callbacks,
		  aptitude::cmdline::post_thunk);

    aptitude::cmdline::main_loop();
  }


/** Try to find a particular package version without knowing the
 *  section that it occurs in.  The resulting name will be invalid if
 *  no changelog could be found.
 */
temp::name changelog_by_version(const std::string &pkg,
				const std::string &ver)
{
  // Try forcing the particular version that was
  // selected, using various sections.  FIXME: relies
  // on specialized knowledge about how get_changelog
  // works; in particular, that it only cares whether
  // "section" has a first component.

  temp::name rval;

  get_changelog_from_source(pkg, ver, "", pkg, rval);

  if(!rval.valid())
    {
      get_changelog_from_source(pkg, ver, "contrib/foo", pkg, rval);
    }

  if(!rval.valid())
    {
      get_changelog_from_source(pkg, ver, "non-free/foo", pkg, rval);
    }

  return rval;
}
}

void do_cmdline_changelog(const vector<string> &packages)
{
  const char *pager="/usr/bin/sensible-pager";

  if(access("/usr/bin/sensible-pager", X_OK)!=0)
    {
      _error->Warning(_("Can't execute sensible-pager, is this a working Debian system?"));

      pager=getenv("PAGER");

      if(pager==NULL)
	pager="more";
    }

  string default_release = aptcfg->Find("APT::Default-Release");

  for(vector<string>::const_iterator i=packages.begin(); i!=packages.end(); ++i)
    {
      // We need to do this because some code (see above) checks
      // PendingError to see whether everything is OK.  In addition,
      // dumping errors means we get sensible error message output
      // (this will be true even if the PendingError check is removed
      // ... which it arguably should be).
      _error->DumpErrors();
      string input=*i;

      cmdline_version_source source;
      string package, sourcestr;

      if(!cmdline_parse_source(input, source, package, sourcestr))
	continue;

      if(source == cmdline_version_cand && !default_release.empty())
	{
	  source    = cmdline_version_archive;
	  sourcestr = default_release;
	}

      pkgCache::PkgIterator pkg=(*apt_cache_file)->FindPkg(package);

      temp::name filename;

      // For real packages/versions, we can do a sanity check on the
      // version and warn the user if it looks like it doesn't have a
      // corresponding source package.
      if(!pkg.end())
	{
	  pkgCache::VerIterator ver=cmdline_find_ver(pkg,
						     source, sourcestr);

	  if(!ver.end())
	    {
	      // Move this to a central location and just display an
	      // apt error?
	      bool in_debian=false;

	      for(pkgCache::VerFileIterator vf=ver.FileList();
		  !vf.end() && !in_debian; ++vf)
		if(!vf.File().end() && vf.File().Origin()!=NULL &&
		   strcmp(vf.File().Origin(), "Debian")==0)
		  in_debian=true;

	      if(!in_debian)
		{
		  _error->Error(_("%s is not an official Debian package, cannot display its changelog."), input.c_str());
		  continue;
		}
	    }

	  aptitude::cmdline::source_package p =
	    aptitude::cmdline::find_source_package(package,
						   source,
						   sourcestr);

	  // Use the source package if one was found; otherwise try to
	  // use an explicit version.
	  if(p.valid())
	    {
	      get_changelog_from_source(p.get_package(),
					p.get_version(),
					p.get_section(),
					pkg.Name(),
					filename);
	    }
	  else
	    {
	      // Fall back to string-based guessing if the version is
	      // invalid.
	      if(ver.end())
                {
                  if(source == cmdline_version_version)
                    filename = changelog_by_version(package, sourcestr);
                  // If we don't even have a version string, leave
                  // filename blank; we'll fail below.
                }
	      else
		{
		  get_changelog(ver, filename);
		}
	    }
	}
      else
	{
	  aptitude::cmdline::source_package p =
	    aptitude::cmdline::find_source_package(package,
						   source,
						   sourcestr);

	  if(p.valid())
	    {
	      get_changelog_from_source(p.get_package(),
					p.get_version(),
					p.get_section(),
					p.get_package(),
					filename);
	    }
	  else
	    {
	      // We couldn't find a real or source package with the
	      // given name and version.
	      //
	      // If the user didn't specify a version or selected a
	      // candidate and we couldn't find anything, we have no
	      // recourse.  But if they passed a version number, we
	      // can fall back to just blindly guessing that the
	      // version exists.

	      switch(source)
		{
		case cmdline_version_cand:
		  break;

		case cmdline_version_curr_or_cand:
		  break;

		case cmdline_version_archive:
		  break;

		case cmdline_version_version:
		  filename = changelog_by_version(package, sourcestr);
		  break;
		}
	    }
	}

      if(!filename.valid())
	_error->Error(_("Couldn't find a changelog for %s"), input.c_str());
      else
	// Run the user's pager.
	system((string(pager) + " " + filename.get_name()).c_str());
    }

  _error->DumpErrors();
}

// TODO: fetch them all in one go.
int cmdline_changelog(int argc, char *argv[])
{
  _error->DumpErrors();

  OpProgress progress;
  apt_init(&progress, false);

  if(_error->PendingError())
    {
      _error->DumpErrors();
      return -1;
    }

  vector<string> packages;
  for(int i=1; i<argc; ++i)
    packages.push_back(argv[i]);

  do_cmdline_changelog(packages);

  _error->DumpErrors();

  return 0;
}
