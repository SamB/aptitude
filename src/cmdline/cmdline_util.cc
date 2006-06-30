// cmdline_util.cc
//
//  Copyright 2004 Daniel Burrows

#include "cmdline_util.h"

#include "cmdline_common.h"
#include "cmdline_progress.h"

#include <aptitude.h>
#include <ui.h>
#include <vs_progress.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_manager.h>
#include <generic/apt/download_signal_log.h>

#include <vscreen/vscreen.h>

#include <sigc++/functors/ptr_fun.h>

void ui_preview()
{
  ui_init();
  file_quit.connect(sigc::ptr_fun(vscreen_exitmain));
  do_package_run_or_show_preview();
  ui_main();
  exit(0);
}

void ui_solution_screen()
{
  ui_init();
  file_quit.connect(sigc::ptr_fun(vscreen_exitmain));

  vs_progress_ref p = gen_progress_bar();
  do_new_package_view(*p.unsafe_get_ref());

  do_examine_solution();
  ui_main();
  exit(0);
}

void cmdline_show_stringlist(strvector &items)
{
  int loc=2;

  printf("  ");

  for(strvector::iterator i=items.begin(); i!=items.end(); ++i)
    {
      if(loc+i->size()>screen_width-5)
	{
	  printf("\n  ");
	  loc=2;
	}

      printf("%s ", i->c_str());
      loc+=i->size()+1;
    }

  printf("\n");
}

void cmdline_show_pkglist(pkgvector &items)
{
  strvector tmp;

  for(pkgvector::iterator i=items.begin(); i!=items.end(); ++i)
    tmp.push_back(i->Name());

  cmdline_show_stringlist(tmp);
}

pkgCache::VerIterator cmdline_find_ver(pkgCache::PkgIterator pkg,
				       cmdline_version_source source,
				       string sourcestr)
{
  switch(source)
    {
    case cmdline_version_curr_or_cand:
      if(!pkg.CurrentVer().end())
	return pkg.CurrentVer();
      // Fall-through.
    case cmdline_version_cand:
      {
	pkgCache::VerIterator candver=(*apt_cache_file)[pkg].CandidateVerIter(*apt_cache_file);

	if(candver.end())
	  {
	    if(source == cmdline_version_cand)
	      printf(_("No candidate version found for %s\n"), pkg.Name());
	    else
	      printf(_("No current or candidate version found for %s\n"), pkg.Name());
	  }

	return candver;
      }
    case cmdline_version_archive:
      for(pkgCache::VerIterator ver=pkg.VersionList(); !ver.end(); ++ver)
	for(pkgCache::VerFileIterator verfile=ver.FileList();
	    !verfile.end(); ++verfile)
	  {
	    pkgCache::PkgFileIterator pkgfile=verfile.File();
	    if(pkgfile.Archive() && sourcestr==pkgfile.Archive())
	      return ver;
	  }

      printf(_("Unable to find an archive \"%s\" for the package \"%s\"\n"),
	     sourcestr.c_str(),
	     pkg.Name());

      return pkgCache::VerIterator(*apt_cache_file, 0);
    case cmdline_version_version:
      for(pkgCache::VerIterator ver=pkg.VersionList(); !ver.end(); ++ver)
	if(sourcestr==ver.VerStr())
	  return ver;

      printf(_("Unable to find a version \"%s\" for the package \"%s\"\n"),
	     sourcestr.c_str(),
	     pkg.Name());

      return pkgCache::VerIterator(*apt_cache_file, 0);
    default:
      printf(_("Internal error: invalid value %i passed to cmdline_find_ver!\n"),
	     source);
      return pkg.VersionList();
    }
}

bool cmdline_parse_source(const string &input,
			  cmdline_version_source &source,
			  string &package,
			  string &sourcestr)
{
  string scratch=input;

  source=cmdline_version_cand;
  sourcestr="";

  if(scratch.find('/')!=scratch.npos)
    {
      source=cmdline_version_archive;
      // Use the last one.
      string::size_type loc=scratch.rfind('/');

      sourcestr=string(scratch, loc+1);
      scratch.erase(loc);
    }

  if(scratch.find('=')!=scratch.npos)
    {
      if(source==cmdline_version_archive)
	{
	  printf(_("You cannot specify both an archive and a version for a package\n"));
	  return false;
	}

      source=cmdline_version_version;
      string::size_type loc=scratch.rfind('=');

      sourcestr=string(scratch, loc+1);
      scratch.erase(loc);
    }

  package=scratch;

  return true;
}

download_manager::result cmdline_do_download(download_manager *m)
{
  OpTextProgress progress(aptcfg->FindI("Quiet", 0));
  std::auto_ptr<download_signal_log> log(gen_cmdline_download_progress());

  if(!m->prepare(progress, *log.get(), log.get()))
    return download_manager::failure;

  download_manager::result finish_res;

  do
    {
      pkgAcquire::RunResult download_res = m->do_download();
      finish_res = m->finish(download_res, progress);
    }
  while(finish_res == download_manager::do_again);

  return finish_res;
}
