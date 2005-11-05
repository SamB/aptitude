// pkg_grouppolicy.cc
//
//  Copyright 1999-2005 Daniel Burrows
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
//  Implementation of stuff that needs implementing.

#include "aptitude.h"

#include "pkg_grouppolicy.h"
#include "pkg_item.h"
#include "pkg_subtree.h"

#include <vscreen/transcode.h>

#include <generic/apt/apt.h>
#include <generic/apt/matchers.h>
#include <generic/apt/pkg_hier.h>
#include <generic/apt/tags.h>
#include <generic/apt/tasks.h>

#include <generic/util/util.h>

#include <map>
#include <set>

#include <ctype.h>

#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/version.h>

#include <sigc++/functors/mem_fun.h>
#include <sigc++/trackable.h>

using namespace std;
using namespace __gnu_cxx;

pkg_grouppolicy_factory::~pkg_grouppolicy_factory()
{
}

// This special tree munges its tag to allow an integer to be prepended to it.
// Ok, it's a dreadful hack.  I admit it.
class pkg_subtree_with_order:public pkg_subtree
{
  wstring my_tag;
public:
  pkg_subtree_with_order(wstring name, wstring description,
			 sigc::signal1<void, std::wstring> *_info_signal,
			 unsigned char order, bool expand=false)
    :pkg_subtree(name, description, _info_signal, expand)
  {
    my_tag+=order;
    my_tag+=pkg_subtree::tag();
  }

  pkg_subtree_with_order(wstring name, unsigned char order, bool expand=false)
    :pkg_subtree(name, expand)
  {
    my_tag+=order;
    my_tag+=pkg_subtree::tag();
  }

  virtual const wchar_t *tag()
  {
    return my_tag.c_str();
  }
};

// The following class is a special policy which is used to terminate a
// policy chain:
class pkg_grouppolicy_end:public pkg_grouppolicy
{
public:
  pkg_grouppolicy_end(pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig) {}

  virtual void add_package(const pkgCache::PkgIterator &i, pkg_subtree *root)
    {
      root->add_child(new pkg_item(i, get_sig()));
    }
};

pkg_grouppolicy *pkg_grouppolicy_end_factory::instantiate(pkg_signal *_sig,
							  desc_signal *_desc_sig)
{
  return new pkg_grouppolicy_end(_sig, _desc_sig);
}

/*****************************************************************************/

// Groups packages so that the latest version of the package is shown, sorted
// by section.

class pkg_grouppolicy_section:public pkg_grouppolicy
{
  typedef hash_map<string, pair<pkg_grouppolicy *, pkg_subtree *> > section_map;
  section_map sections;

  pkg_grouppolicy_factory *chain;

  // As in the factory
  int split_mode;
  bool passthrough;

  // The descriptions are in the style used by package descriptions.
  static hash_map<string, wstring> section_descriptions;
  static void init_section_descriptions();
public:
  pkg_grouppolicy_section(int _split_mode,
			  bool _passthrough,
			  pkg_grouppolicy_factory *_chain,
			  pkg_signal *_sig,
			  desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig), chain(_chain),
     split_mode(_split_mode), passthrough(_passthrough)
  {
    init_section_descriptions();
  }

  virtual void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root);

  virtual ~pkg_grouppolicy_section()
    {
      for(section_map::iterator i=sections.begin(); i!=sections.end(); i++)
        delete i->second.first;
    }
};

pkg_grouppolicy *pkg_grouppolicy_section_factory::instantiate(pkg_signal *_sig,
							      desc_signal *_desc_sig)
{
  return new pkg_grouppolicy_section(split_mode, passthrough, chain,
				     _sig, _desc_sig);
}

hash_map<string, wstring> pkg_grouppolicy_section::section_descriptions;

// Should this be externally configurable?
void pkg_grouppolicy_section::init_section_descriptions()
{
  static bool already_done=false;

  if(already_done)
    return;

  section_descriptions["Tasks"]=transcode(_("Packages which set up your computer to perform a particular task\n Packages in the 'Tasks' section contain no files; they merely depend upon other packages. These packages provide an easy way to select a predefined set of packages for a specialized task."));

  section_descriptions["Unknown"]=transcode(_("Packages with no declared section\n No section is given for these packages. Perhaps there is an error in the Packages file?"));

  section_descriptions["admin"]=transcode(_("Administrative utilities (install software, manage users, etc)\n Packages in the 'admin' section allow you to perform administrative tasks such as installing software, managing users, configuring and monitoring your system, examining network traffic, and so on."));

  section_descriptions["alien"]=transcode(_("Packages converted from foreign formats (rpm, tgz, etc)\n Packages in the 'alien' section were created by the 'alien' program from a non-Debian package format such as RPM"));
  section_descriptions["base"]=transcode(_("The Debian base system\n Packages in the 'base' section are part of the initial system installation."));
  section_descriptions["comm"]=transcode(_("Programs for faxmodems and other communications devices\n Packages in the 'comm' section are used to control modems and other hardware communications devices. This includes software to control faxmodems (for instance, PPP for dial-up internet connections and programs originally written for that purpose, such as zmodem/kermit), as well as software to control cellular phones, interface with FidoNet, and run a BBS."));
  section_descriptions["devel"]=transcode(_("Utilities and programs for software development\n Packages in the 'devel' section are used to write new software and work on existing software. Non-programmers who do not compile their own software probably do not need much software from this section.\n .\n It includes compilers, debugging tools, programmer's editors, source processing tools, and other things related to software development."));
  section_descriptions["doc"]=transcode(_("Documentation and specialized programs for viewing documentation\n Packages in the 'doc' section document parts of the Debian system, or are viewers for documentation formats."));
  section_descriptions["editors"]=transcode(_("Text editors and word processors\n Packages in the 'editors' section allow you to edit plain ASCII text. These are not necessarily word processors, although some word processors may be found in this section."));
  section_descriptions["electronics"]=transcode(_("Programs for working with circuits and electronics\n Packages in the 'electronics' section include circuit design tools, simulators and assemblers for microcontrollers, and other related software."));
  section_descriptions["embedded"]=transcode(_("Programs for embedded systems\n Packages in the 'embedded' section are meant to run on embedded devices. Embedded devices are specialized hardware devices with much less power than a typical desktop system: for instance, a PDA, a cell phone, or a Tivo."));
  section_descriptions["gnome"]=transcode(_("The GNOME Desktop System\n GNOME is a collection of software which provides an easy-to-use desktop environment for Linux.  Packages in the 'gnome' section are part of the GNOME environment or closely integrated into it."));
  section_descriptions["games"]=transcode(_("Games, toys, and fun programs\n Packages in the 'games' section are meant primarily for entertainment."));
  section_descriptions["graphics"]=transcode(_("Utilities to create, view, and edit graphics files\n Packages in the 'graphics' section include viewers for image files, image processing and manipulation software, software to interact with graphics hardware (such as video cards, scanners, and digital cameras), and programming tools for handling graphics."));
  section_descriptions["hamradio"]=transcode(_("Software for ham radio operators\n Packages in the 'hamradio' section are meant primarily for ham radio operators."));
  section_descriptions["interpreters"]=transcode(_("Interpreters for interpreted languages\n Packages in the 'interpreters' section include interpreters for languages like Python, Perl, and Ruby, and libraries for these same languages."));
  section_descriptions["kde"]=transcode(_("The KDE Desktop System\n KDE is a collection of software which provides an easy-to-use desktop environment for Linux.  Packages in the 'kde' section are part of the KDE environment or closely integrated into it."));
  section_descriptions["libdevel"]=transcode(_("Development files for libraries\n Packages in the 'libdevel' section contain files required for building programs that use libraries in the 'libs' section.  You don't need packages from this section unless you want to compile software yourself."));
  section_descriptions["libs"]=transcode(_("Collections of software routines\n Packages in the 'libs' section provide necessary functionality for other software on the computer. With very few exceptions, you should not need to explicitly install a package from this section; the package system will install them as required to fulfill dependencies."));
  section_descriptions["perl"]=transcode(_("Perl interpreter and libraries\n Packages in the 'perl' section provide the Perl programming language and many third-party libraries for it. Unless you are a Perl programmer, you don't need to install packages from this section explicitly; the package system will install them if they are required."));
  section_descriptions["python"]=transcode(_("Python interpreter and libraries\n Packages in the 'python' section provide the Python programming language and many third-party libraries for it. Unless you are a Python programmer, you don't need to install packages from this section explicitly; the package system will install them if they are required."));
  section_descriptions["mail"]=transcode(_("Programs to write, send, and route email messages\n Packages in the 'mail' section include mail readers, mail transport daemons, mailing list software, and spam filters, as well as various other software related to electronic mail."));
  section_descriptions["math"]=transcode(_("Numeric analysis and other mathematics-related software\n Packages in the 'math' section include calculators, languages for mathematical computation (similar to Mathematica), symbolic algebra packages, and programs to visualize mathematical objects."));
  section_descriptions["misc"]=transcode(_("Miscellaneous software\n Packages in the 'misc' section have too unusual a function to be classified."));
  section_descriptions["net"]=transcode(_("Programs to connect to and provide various services\n Packages in the 'net' section include clients and servers for many protocols, tools to manipulate and debug low-level network protocols, IM systems, and other network-related software."));
  section_descriptions["news"]=transcode(_("Usenet clients and servers\n Packages in the 'news' section are related to the Usenet distributed news system.  They include news readers and news servers."));
  section_descriptions["oldlibs"]=transcode(_("Obsolete libraries\n Packages in the 'oldlibs' section are obsolete and should not be used by new software.  They are provided for compatibility reasons, or because software distributed by Debian still requires them.\n .\n With very few exceptions, you should not need to explicitly install a package from this section; the package system will install them as required to fulfill dependencies."));
  section_descriptions["otherosfs"]=transcode(_("Emulators and software to read foreign filesystems\n Packages in the 'otherosfs' section emulate hardware and operating systems and provide tools for transferring data between different operating systems and hardware platforms. (for instance, utilities to read DOS floppies, and utilities to communicate with Palm Pilots)\n .\n It is worth noting that CD burning software is included in THIS section."));
  section_descriptions["science"]=transcode(_("Software for scientific work\n Packages in the 'science' section include tools for astronomy, biology, and chemistry, as well as other science-related software."));
  section_descriptions["shells"]=transcode(_("Command shells and alternative console environments\n Packages in the 'shells' section include programs providing a command-line interface."));
  section_descriptions["sound"]=transcode(_("Utilities to play and record sound\n Packages in the 'sound' section include sound players, recorders, and encoders for many formats, mixers and volume controls, MIDI sequencers and programs to generate musical notation, drivers for sound hardware, and sound processing software."));
  section_descriptions["tex"]=transcode(_("The TeX typesetting system\n Packages in the 'tex' section are related to TeX, a system for producing high-quality typeset output.  They include TeX itself, TeX packages, editors designed for TeX, utilities to convert TeX and TeX output files to various formats, TeX fonts, and other software related to TeX."));
  section_descriptions["text"]=transcode(_("Text processing utilities\n Packages in the 'text' section include text filters and processors, spelling checkers, dictionary programs, utilities to convert between character encodings and text file formats (eg, Unix and DOS), text formatters and pretty-printers, and other software which operates on plain text."));
  section_descriptions["utils"]=transcode(_("Various system utilities\n Packages in the 'utils' section are utilities whose purpose is too unique to be classified."));
  section_descriptions["web"]=transcode(_("Web browsers, servers, proxies, and other tools\n Packages in the 'web' section include Web browsers, Web servers and proxies, software to write CGI scripts or Web-based programs, pre-written Web-based programs, and other software related to the World Wide Web."));
  section_descriptions["x11"]=transcode(_("The X window system and related software\n Packages in the 'x11' section include the core packages for the X window system, window managers, utility programs for X, and miscellaneous programs with an X GUI which were placed here because they didn't fit anywhere else."));

  section_descriptions["contrib"]=transcode(_("Programs which depend on software not in Debian\n Packages in the 'contrib' section are not part of Debian.\n .\n These packages are Free Software, but they depend on software which is not part of Debian.  This may be because it is not Free Software, but is packaged in the non-free section of the archive, because Debian cannot distribute it at all, or (in rare cases) because no-one has packaged it yet.\n .\n For more information about what Debian considers to be Free Software, see http://www.debian.org/social_contract#guidelines"));
  section_descriptions["main"]=transcode(_("The main Debian archive\n The Debian distribution consists of packages from the 'main' section. Every package in 'main' is Free Software.\n .\n For more information about what Debian considers to be Free Software, see http://www.debian.org/social_contract#guidelines"));
  section_descriptions["non-US"]=transcode(_("Programs stored outside the US due to export controls\n Packages in 'non-US' likely contain cryptography; a few implement patented algorithms. Because of this, they cannot be exported from the United States, and hence are stored on a server in the \"free world\".\n .\n Note: the Debian Project is currently merging cryptographic software into the US-based archives after consulting with legal experts about recent changes in export policies. Most packages which were formerly found in this section, therefore, are now in 'main'."));
  section_descriptions["non-free"]=transcode(_("Programs which are not free software\n Packages in the 'non-free' section are not part of Debian.\n .\n These packages fail to meet one or more of the requirements of the Debian Free Software Guidelines (see below). You should read the license of programs in this section to be sure that you are allowed to use them in the way you intend.\n .\n For more information about what Debian considers to be Free Software, see http://www.debian.org/social_contract#guidelines"));

  section_descriptions["virtual"]=transcode(_("Virtual packages\n These packages do not exist; they are names other packages use to require or provide some functionality."));

  already_done=true;
}

void pkg_grouppolicy_section::add_package(const pkgCache::PkgIterator &pkg,
					  pkg_subtree *root)
{
  const char *section=pkg.VersionList().Section();
  const char *name=pkg.Name();
  bool maypassthrough=false; // FIXME: HACK!

  if(name[0] == 't' && name[1] == 'a' && name[2] == 's' && name[3] == 'k' && name[4] == '-')
    {
      maypassthrough=true;
      section=split_mode!=pkg_grouppolicy_section_factory::split_none?_("Tasks/Tasks"):_("Tasks");
    }

  if(!section)
    {
      maypassthrough=true;
      section=split_mode!=pkg_grouppolicy_section_factory::split_none?_("Unknown/Unknown"):_("Unknown");
    }

  if(pkg.VersionList().end())
    {
      maypassthrough=true;
      section=split_mode!=pkg_grouppolicy_section_factory::split_none?_("virtual/virtual"):_("virtual");
    }

  const char *split=strchr(section, '/');
  const char *subdir=split?split+1:section;

  string tag;

  if(split_mode==pkg_grouppolicy_section_factory::split_none)
    tag=section;
  else if(split_mode==pkg_grouppolicy_section_factory::split_topdir)
    tag=(split?string(section,split-section):string(_("main")));
  else if(split_mode==pkg_grouppolicy_section_factory::split_subdir)
    tag=subdir;

  section_map::iterator found=sections.find(tag);

  if(maypassthrough && passthrough)
    {
      if(found==sections.end())
	sections[tag].first=chain->instantiate(get_sig(), get_desc_sig());

      sections[tag].first->add_package(pkg, root);
    }
  else
    {
      if(found==sections.end())
	{
	  pkg_subtree *newtree;
	  string realtag=tag;

	  // Go by the last element of the section for multi-level sections.
	  if(tag.find('/')!=tag.npos)
	    realtag=string(tag, tag.rfind('/')+1);

	  if(section_descriptions.find(realtag)!=section_descriptions.end())
	    {
	      wstring desc=section_descriptions[realtag];

	      if(desc.find(L'\n')!=desc.npos)
		newtree=new pkg_subtree(transcode(tag)+L" - "+wstring(desc, 0, desc.find('\n')),
					desc,
					get_desc_sig());
	      else
		newtree=new pkg_subtree(transcode(tag)+desc);
	    }
	  else
	    newtree=new pkg_subtree(transcode(tag));

	  sections[tag].first=chain->instantiate(get_sig(), get_desc_sig());
	  sections[tag].second=newtree;

	  root->add_child(newtree);
	}

      sections[tag].first->add_package(pkg, sections[tag].second);
    }
}

/*****************************************************************************/

class pkg_grouppolicy_status:public pkg_grouppolicy
{
  static const int numgroups=7;
  pkg_grouppolicy_factory *chain;

  enum states {security_upgradable, upgradable, newpkg, installed, not_installed, obsolete_pkg, virtual_pkg};

  static const char * const state_titles[numgroups];
  // FIXME: need better titles :)

  pair<pkg_grouppolicy *, pkg_subtree *> children[numgroups];
public:
  pkg_grouppolicy_status(pkg_grouppolicy_factory *_chain,
			 pkg_signal *_sig,
			 desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig), chain(_chain)
    {
      for(int i=0; i<( (int) (sizeof(children)/sizeof(children[0]))); i++)
	{
	  children[i].first=NULL;
	  children[i].second=NULL;
	}
    }

  virtual void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root);

  virtual ~pkg_grouppolicy_status()
    {
      for(int i=0; i<numgroups; i++)
	delete children[i].first;
    }
};

const char * const pkg_grouppolicy_status::state_titles[numgroups] =
{
  N_("Security Updates\n Security updates for these packages are available from security.debian.org."),
  N_("Upgradable Packages\n A newer version of these packages is available."),
  N_("New Packages\n These packages have been added to Debian since the last time you cleared the list of \"new\" packages (choose \"Forget new packages\" from the Actions menu to empty this list)."),
  N_("Installed Packages\n These packages are currently installed on your computer."),
  N_("Not Installed Packages\n These packages are not installed on your computer."),
  N_("Obsolete and Locally Created Packages\n These packages are currently installed on your computer, but they are not available from any apt source.  They may be obsolete and removed from the archive, or you may have built a private version of them yourself."),
  N_("Virtual Packages\n These packages do not exist; they are names other packages use to require or provide some functionality.")
};

pkg_grouppolicy *pkg_grouppolicy_status_factory::instantiate(pkg_signal *_sig,
							     desc_signal *_desc_sig)
{
  return new pkg_grouppolicy_status(chain, _sig, _desc_sig);
}

// Stolen from apt-watch:

/** Tests whether a particular version is security-related.
 *
 *  \return \b true iff the given package version comes from security.d.o
 */
static bool version_is_security(const pkgCache::VerIterator &ver)
{
  for(pkgCache::VerFileIterator F=ver.FileList(); !F.end(); ++F)
    if(string(F.File().Site())=="security.debian.org")
      return true;

  return false;
}

void pkg_grouppolicy_status::add_package(const pkgCache::PkgIterator &pkg,
					 pkg_subtree *root)
{
  states section;
  pkgDepCache::StateCache &state=(*apt_cache_file)[pkg];

  if(pkg.VersionList().end())
    section=virtual_pkg;
  else
    {
      if(pkg_obsolete(pkg))
	section=obsolete_pkg;
      else if((*apt_cache_file)->get_ext_state(pkg).new_package)
	section=newpkg;
      else if(state.Status==2)
	section=not_installed;
      else if(state.Upgradable())
	{
	  pkgCache::VerIterator candver=state.CandidateVerIter(*apt_cache_file);
	  if(version_is_security(candver))
	    section=security_upgradable;
	  else
	    section=upgradable;
	}
      else
	section=installed;
    }

  if(!children[section].second)
    {
      wstring desc=transcode(_(state_titles[section]));
      wstring shortdesc(desc, 0, desc.find('\n'));

      pkg_subtree *newtree=new pkg_subtree_with_order(shortdesc, desc,
						      get_desc_sig(), section);
      children[section].first=chain->instantiate(get_sig(), get_desc_sig());
      children[section].second=newtree;
      root->add_child(newtree);
    }

  children[section].first->add_package(pkg, children[section].second);
}

/*****************************************************************************/
class pkg_grouppolicy_filter:public pkg_grouppolicy
{
  pkg_matcher *filter;

  pkg_grouppolicy *chain;
public:
  pkg_grouppolicy_filter(pkg_grouppolicy_factory *_chain, pkg_matcher *_filter,
			 pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig),
     filter(_filter),
     chain(_chain->instantiate(_sig, _desc_sig))
  {
  }

  virtual void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root)
  {
    if(filter->matches(pkg))
      chain->add_package(pkg, root);
  }

  virtual ~pkg_grouppolicy_filter() {delete chain;}
};

pkg_grouppolicy *pkg_grouppolicy_filter_factory::instantiate(pkg_signal *_sig,
							     desc_signal *_desc_sig)
{
  return new pkg_grouppolicy_filter(chain, filter, _sig, _desc_sig);
}

pkg_grouppolicy_filter_factory::~pkg_grouppolicy_filter_factory()
{
  delete chain;
  delete filter;
}

/*****************************************************************************/

class pkg_grouppolicy_mode:public pkg_grouppolicy
{
  const static char * const child_names[];
  pair<pkg_grouppolicy *, pkg_subtree *> children[num_pkg_action_states];
  pair<pkg_grouppolicy *, pkg_subtree *> suggested_child;
  pair<pkg_grouppolicy *, pkg_subtree *> recommended_child;
  pkg_grouppolicy_factory *chain;
public:
  pkg_grouppolicy_mode(pkg_grouppolicy_factory *_chain,
		       pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig), chain(_chain)
  {
    for(int i=0; i<num_pkg_action_states; i++)
      {
	children[i].first=NULL;
	children[i].second=NULL;
      }
  }

  void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root)
  {
    int group=find_pkg_state(pkg);
    if(group!=pkg_unchanged)
      {
	if(!children[group].second)
	  {
	    wstring desc=transcode(_(child_names[group]));
	    wstring shortdesc(desc, 0, desc.find('\n'));

	    pkg_subtree *newtree=new pkg_subtree_with_order(shortdesc,
							    desc,
							    get_desc_sig(),
							    group,
							    true);
	    root->add_child(newtree);
	    children[group].first=chain->instantiate(get_sig(),
						     get_desc_sig());
	    children[group].second=newtree;
	  }

	children[group].first->add_package(pkg, children[group].second);
      }
    else if(pkg.CurrentVer().end() && package_recommended(pkg))
      {
	if(!recommended_child.second)
	  {
	    const wstring desc=transcode(_("Packages which are recommended by other packages\n These packages are not strictly required, but they may be necessary to provide full functionality in some other programs that you are installing or upgrading."));
	    const wstring shortdesc(desc, 0, desc.find('\n'));

	    pkg_subtree *newtree=new pkg_subtree_with_order(shortdesc,
							    desc,
							    get_desc_sig(),
							    num_pkg_action_states,
							    true);
	    root->add_child(newtree);
	    recommended_child.first=chain->instantiate(get_sig(),
						       get_desc_sig());
	    recommended_child.second=newtree;
	  }

	recommended_child.first->add_package(pkg, recommended_child.second);
      }
    else if(pkg.CurrentVer().end() && package_suggested(pkg))
      {
	if(!suggested_child.second)
	  {
	    const wstring desc=transcode(_("Packages which are suggested by other packages\n These packages are not required in order to make your system function properly, but they may provide enhanced functionality for some programs that you are currently installing."));
	    const wstring shortdesc(desc, 0, desc.find('\n'));

	    pkg_subtree *newtree=new pkg_subtree_with_order(shortdesc,
							    desc,
							    get_desc_sig(),
							    num_pkg_action_states+1,
							    false);
	    root->add_child(newtree);
	    suggested_child.first=chain->instantiate(get_sig(),
						     get_desc_sig());
	    suggested_child.second=newtree;
	  }

	suggested_child.first->add_package(pkg, suggested_child.second);
      }
  }

  virtual ~pkg_grouppolicy_mode()
  {
    for(int i=0; i<num_pkg_action_states; i++)
      delete children[i].first;
  }
};

const char * const pkg_grouppolicy_mode::child_names[num_pkg_action_states]=
{
  N_("Packages with unsatisfied dependencies\n The dependency requirements of these packages will be unmet after the install is complete.\n .\n The presence of this tree probably indicates that something is broken, either on your system or in the Debian archive."),
  N_("Packages being removed because they are no longer used\n These packages are being deleted because they were automatically installed to fulfill dependencies, and the planned action will result in no installed package declaring an 'important' dependency on them.\n"),
  N_("Packages being automatically held in their current state\n These packages could be upgraded, but they have been kept in their current state to avoid breaking dependencies."),
  N_("Packages being automatically installed to satisfy dependencies\n These packages are being installed because they are required by another package you have chosen for installation."),
  N_("Packages being deleted due to unsatisfied dependencies\n These packages are being deleted because one or more of their dependencies is no longer available, or because another package conflicts with them."),
  N_("Packages to be downgraded\n An older version of these packages than is currently installed will be installed."),
  N_("Packages being held back\n These packages could be upgraded, but you have asked for them to be held at their current version."),
  N_("Packages to be reinstalled\n These packages will be reinstalled."),
  N_("Packages to be installed\n These packages have been manually selected for installation on your computer."),
  N_("Packages to be removed\n These packages have been manually selected for removal."),
  N_("Packages to be upgraded\n These packages will be upgraded to a newer version."),
};

pkg_grouppolicy *pkg_grouppolicy_mode_factory::instantiate(pkg_signal *_sig,
							   desc_signal *_desc_sig)
{
  return new pkg_grouppolicy_mode(chain, _sig, _desc_sig);
}

/*****************************************************************************/

class pkg_grouppolicy_firstchar:public pkg_grouppolicy
{
  pkg_grouppolicy_factory *chain;

  typedef map<char, pair<pkg_grouppolicy *, pkg_subtree *> > childmap;
  // Store the child group policies and their associated subtrees.
  childmap children;
public:
  pkg_grouppolicy_firstchar(pkg_grouppolicy_factory *_chain,
			    pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig), chain(_chain)
  {
  }

  ~pkg_grouppolicy_firstchar()
  {
    for(childmap::iterator i=children.begin(); i!=children.end(); i++)
      delete i->second.first;
  }

  void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root)
  {
    eassert(pkg.Name());

    char firstchar=toupper(pkg.Name()[0]);

    childmap::iterator found=children.find(firstchar);
    if(found!=children.end())
      found->second.first->add_package(pkg, found->second.second);
    else
      {
	string treename;
	treename+=firstchar;

	pkg_subtree *newtree=new pkg_subtree(transcode(treename));
	pkg_grouppolicy *newchild=chain->instantiate(get_sig(),
						     get_desc_sig());
	children[firstchar].first=newchild;
	children[firstchar].second=newtree;
	root->add_child(newtree);

	newchild->add_package(pkg, newtree);
      }
  }
};

pkg_grouppolicy *pkg_grouppolicy_firstchar_factory::instantiate(pkg_signal *sig,
								desc_signal *desc_sig)
{
  return new pkg_grouppolicy_firstchar(chain, sig, desc_sig);
}

/*****************************************************************************/

// Groups packages by priority
class pkg_grouppolicy_priority:public pkg_grouppolicy
{
  // a map may be overkill, but I figure better safe than sorry..
  // who knows, maybe someone will change the number of priorities someday..
  typedef map<unsigned char,
	      pair<pkg_grouppolicy *, pkg_subtree *> > childmap;

  childmap children;
  pkg_grouppolicy_factory *chain;

  pkg_grouppolicy *spillover;
public:
  pkg_grouppolicy_priority(pkg_grouppolicy_factory *_chain,
			   pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig),
     chain(_chain),
     spillover(_chain->instantiate(get_sig(), get_desc_sig()))
  {
  }

  ~pkg_grouppolicy_priority()
  {
    for(childmap::iterator i=children.begin(); i!=children.end(); i++)
      delete i->second.first;
  }

  void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root)
  {
    unsigned char priority;
    const char *pstr;

    if(!pkg.VersionList().end())
      {
	if(!pkg.CurrentVer().end())
	  priority=pkg.CurrentVer()->Priority;
	else
	  priority=pkg.VersionList()->Priority;

	pstr=(*apt_cache_file)->GetCache().Priority(priority);
      }
    else
      {
	pstr=NULL;
	priority=0;
      }

    // Some packages don't have a reasonable priority
    if(!pstr)
      {
	pstr=_("unknown");
	priority=0;
      }

    childmap::iterator found=children.find(priority);

    if(found!=children.end())
      found->second.first->add_package(pkg, found->second.second);
    else
      {
	char buf[512];
	snprintf(buf, 512, _("Priority %s"), pstr);
	int order;
	switch(priority)
	  {
	  case pkgCache::State::Required : order=1; break;
	  case pkgCache::State::Important: order=2; break;
	  case pkgCache::State::Standard : order=3; break;
	  case pkgCache::State::Optional : order=4; break;
	  case pkgCache::State::Extra    : order=5; break;
	  default                        : order=6; break;
	  };

	pkg_subtree *newtree=new pkg_subtree_with_order(transcode(buf), order);
	pkg_grouppolicy *newchild=chain->instantiate(get_sig(),
						     get_desc_sig());
	children[priority].first=newchild;
	children[priority].second=newtree;
	root->add_child(newtree);

	newchild->add_package(pkg, newtree);
      }
  }  
};

pkg_grouppolicy *pkg_grouppolicy_priority_factory::instantiate(pkg_signal *sig,
							       desc_signal *desc_sig)
{
  return new pkg_grouppolicy_priority(chain, sig, desc_sig);
}

/*****************************************************************************/

// This class generates a pkg_subtree from a hierarchy
class pkg_hier_grouper:public pkg_hier::hierarchy_realizer, public sigc::trackable
{
  pkg_subtree *uncategorized;
  pkg_grouppolicy *uncategorized_policy;

  pkg_grouppolicy_factory *chain;

  pkg_signal *sig;
  desc_signal *desc_sig;

  pkg_subtree *root;

  typedef pair<pkg_grouppolicy *, pkg_subtree *> nodedata;

  // This tracks the grouping policies we have created so that we can
  // destroy them.
  vector<nodedata *> data_out_there;

  void handle_reload()
  {
    for(vector<nodedata *>::iterator i=data_out_there.begin();
     	i!=data_out_there.end();
     	++i)
      {
     	delete (*i)->first;
     	delete *i;
      }
    data_out_there.clear();

    uncategorized=NULL;
    uncategorized_policy=NULL;

    root=NULL;

    reset_groupinfo();
  }
public:
  pkg_hier_grouper(pkg_hier *_hier,
		   pkg_grouppolicy_factory *_chain,
		   pkg_signal *_sig,
		   desc_signal *_desc_sig)
    :hierarchy_realizer(_hier), uncategorized(NULL),
     uncategorized_policy(NULL), chain(_chain), sig(_sig), desc_sig(_desc_sig),
     root(NULL)
  {
    hier_reloaded.connect(sigc::mem_fun(*this, &pkg_hier_grouper::handle_reload));
  }

  ~pkg_hier_grouper()
  {
    delete uncategorized_policy;
    for(vector<nodedata *>::iterator i=data_out_there.begin();
     	i!=data_out_there.end();
     	++i)
      {
     	delete (*i)->first;
     	delete *i;
      }
  }

  // HACK: the root has to be set each time
  void set_root(pkg_subtree *_root)
  {
    root=_root;
  }

  void realize_item(pkg_hier::item *item, void *parent_data)
  {
    pkgCache::PkgIterator pkg=(*apt_cache_file)->FindPkg(item->name);
    nodedata *data=(nodedata *) parent_data;

    if(data)
      data->first->add_package(pkg, data->second);
    else
      {
	if(!uncategorized)
	  {
	    uncategorized=new pkg_subtree(transcode(_("UNCATEGORIZED")));
	    uncategorized_policy=chain->instantiate(sig, desc_sig);
	    root->add_child(uncategorized);
	  }

	uncategorized_policy->add_package(pkg, uncategorized);
      }
  }

  void *realize_group(pkg_hier::group *group, void *parent_data)
  {
    nodedata *data=(nodedata *) parent_data;

    pkg_subtree *newtree=new pkg_subtree(transcode(group->description));
    pkg_grouppolicy *newpolicy=chain->instantiate(sig, desc_sig);

    nodedata *rval=new nodedata(newpolicy, newtree);
    data_out_there.push_back(rval);

    if(data)
      data->second->add_child(newtree);
    else
      root->add_child(newtree);

    return rval;
  }
};

class pkg_grouppolicy_hier:public pkg_grouppolicy
{
  pkg_hier *hier;
  pkg_hier_grouper grouper;

public:
  pkg_grouppolicy_hier(pkg_hier *_hier, pkg_grouppolicy_factory *_chain,
		       pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig), hier(_hier),
     grouper(_hier, _chain, _sig, _desc_sig)
  {
  }

  void add_package(const pkgCache::PkgIterator &pkg, pkg_subtree *root)
  {
    // HACK:
    grouper.set_root(root);

    if(!hier->realize_item_up(pkg.Name(), &grouper))
      {
	pkg_hier::item tmp;
	tmp.name=pkg.Name();

	// MORE HACK
	grouper.realize_item(&tmp, NULL);
      }
  }
};

pkg_grouppolicy *pkg_grouppolicy_hier_factory::instantiate(pkg_signal *sig,
							   desc_signal *desc_sig)
{
  return new pkg_grouppolicy_hier(hier, chain, sig, desc_sig);
}

pkg_grouppolicy_hier_factory::~pkg_grouppolicy_hier_factory()
{
  if(del_hier)
    delete hier;
  delete chain;
}

/****************************************************************************/

//  This class generates a "hierarchy" by grouping packages by Task, and
// optionally throwing away non-task packages.

class pkg_grouppolicy_task:public pkg_grouppolicy
{
  /** The subtrees are indexed by Task. */
  typedef hash_map<string, pkg_subtree *> subtree_map;

  /** A list of [task,description] pairs. */
  typedef vector<pair<string, wstring> > desclist;

  // HACK: put all our stuff under this.
  pkg_subtree *tasks_subtree;

  subtree_map task_children, section_children;

  pkg_grouppolicy *chain;

  static desclist task_section_descriptions;
  // Lifted from tasksel.  There are so few of them that it's not
  // worth using a hash_map.  Even a map is overkill.

  static void init_section_descriptions();
public:
  pkg_grouppolicy_task(pkg_grouppolicy_factory *_chain,
		       pkg_signal *_sig, desc_signal *_desc_sig)
    :pkg_grouppolicy(_sig, _desc_sig), tasks_subtree(NULL),
     chain(_chain?_chain->instantiate(_sig, _desc_sig):NULL)
  {
    init_section_descriptions();
  }

  ~pkg_grouppolicy_task() {delete chain;}

  void add_package(const pkgCache::PkgIterator &pkg,
		   pkg_subtree *root);
};

pkg_grouppolicy_task::desclist pkg_grouppolicy_task::task_section_descriptions;

void pkg_grouppolicy_task::init_section_descriptions()
{
  static bool already_done=false;

  if(already_done)
    return;

  task_section_descriptions.push_back(pair<string,wstring>("user", transcode(dgettext("tasksel", "End-user"))));
  task_section_descriptions.push_back(pair<string,wstring>("server", transcode(dgettext("tasksel", "Servers"))));
  task_section_descriptions.push_back(pair<string,wstring>("devel", transcode(dgettext("tasksel", "Development"))));
  task_section_descriptions.push_back(pair<string,wstring>("l10n", transcode(dgettext("tasksel", "Localization"))));
  task_section_descriptions.push_back(pair<string,wstring>("hware", transcode(dgettext("tasksel", "Hardware Support"))));
  task_section_descriptions.push_back(pair<string,wstring>("misc", transcode(dgettext("tasksel", "Miscellaneous"))));
  task_section_descriptions.push_back(pair<string,wstring>("unknown", transcode(dgettext("tasksel", "Unrecognized tasks"))));
}

/** Uses the fact that 0<=Relevance<=10 to encode Relevance as a character. */
class task_subtree:public pkg_subtree
{
  char relevance[2];
public:
  task_subtree(const std::wstring &_name, const std::wstring &_description,
	       desc_signal *_info_signal,
	       int _relevance)
    :pkg_subtree(_name, _description, _info_signal)
  {
    relevance[0]='z'-_relevance;
    relevance[1]=0;
  }

  const char *tag() const {return relevance;}
};

void pkg_grouppolicy_task::add_package(const pkgCache::PkgIterator &pkg,
				       pkg_subtree *root)
{
  list<string> *tasks=get_tasks(pkg);

  eassert(tasks);

  chain->add_package(pkg, root);

  for(list<string>::iterator i=tasks->begin(); i!=tasks->end(); ++i)
    {
      subtree_map::iterator found=task_children.find(*i);

      if(found==task_children.end())
	{
	  string section;
	  map<string,task>::iterator taskfound=task_list->find(*i);
	  pkg_subtree *newtree, *sectiontree;

	  if(taskfound==task_list->end())
	    section="unknown";
	  else
	    {
	      if(!taskfound->second.keys_present())
		continue; // skip to the next task of this package.

	      section=taskfound->second.section;
	    }

	  if(!tasks_subtree)
	    {
	      tasks_subtree=new pkg_subtree(transcode(_("Tasks")),
					    transcode(_("\n Tasks are groups of packages which provide an easy way to select a predefined set of packages for a particular purpose.")), get_desc_sig());
	      root->add_child(tasks_subtree);
	    }

	  subtree_map::iterator sectionfound=section_children.find(section);
	  if(sectionfound==section_children.end())
	    {
	      wstring sectiondesc=transcode(section, "ASCII");

	      for(desclist::iterator j=task_section_descriptions.begin();
		  j!=task_section_descriptions.end();
		  ++j)
		if(j->first==section)
		  {
		    sectiondesc=j->second;
		    break;
		  }

	      sectiontree=new pkg_subtree(sectiondesc);
	      section_children[section]=sectiontree;
	      tasks_subtree->add_child(sectiontree);
	    }
	  else
	    sectiontree=sectionfound->second;

	  if(taskfound!=task_list->end())
	    newtree=new task_subtree(transcode(taskfound->second.shortdesc),
				     transcode(taskfound->second.longdesc),
				     get_desc_sig(),
				     taskfound->second.relevance);
	  else
	    newtree=new task_subtree(transcode(*i), L"",
				     get_desc_sig(), 5);

	  task_children[*i]=newtree;

	  sectiontree->add_child(newtree);

	  newtree->add_child(new pkg_item(pkg, get_sig()));
	}
      else
	found->second->add_child(new pkg_item(pkg, get_sig()));
    }
}

pkg_grouppolicy *pkg_grouppolicy_task_factory::instantiate(pkg_signal *sig,
							   desc_signal *desc_sig)
{
  return new pkg_grouppolicy_task(chain, sig, desc_sig);
}

pkg_grouppolicy_task_factory::~pkg_grouppolicy_task_factory()
{
  delete chain;
}


class pkg_grouppolicy_matchers : public pkg_grouppolicy
{
public:
  typedef pkg_grouppolicy_matchers_factory::match_pair match_pair;

  struct subtree_pair
  {
    pkg_grouppolicy *policy;
    pkg_subtree *tree;

    subtree_pair():policy(NULL), tree(NULL)
    {
    }

    subtree_pair(pkg_grouppolicy *_policy, pkg_subtree *_tree)
      :policy(_policy), tree(_tree)
    {
    }
  };
private:
  pkg_grouppolicy_factory *chain;
  const vector<match_pair> &subgroups;
  typedef map<wstring, subtree_pair> subtree_map;
  subtree_map subtrees;

  wstring substitute(const wstring &s,
		     pkg_match_result *res)
  {
    wstring rval;

    wstring::const_iterator i = s.begin();
    while(i != s.end())
      {
	while(i != s.end() && *i != L'\\')
	  {
	    rval += *i;
	    ++i;
	  }

	if(i != s.end())
	  {
	    ++i;
	    if(i == s.end())
	      rval += L'\\';
	    else if(*i == L'\\')
	      {
		rval += L'\\';
		++i;
	      }
	    else if(iswdigit(*i))
	      {
		wstring tocvt;

		while(i != s.end() && iswdigit(*i))
		  {
		    tocvt += *i;
		    ++i;
		  }

		wchar_t *endptr = NULL;
		unsigned long val = wcstoul(tocvt.c_str(), &endptr, 0);

		if(endptr && (*endptr) != L'\0')
		  {
		    wchar_t buf[512];

		    swprintf(buf, 512, transcode(_("Bad number in format string: %ls")).c_str(),
			     tocvt.c_str());

		    return buf;
		  }

		if(val < 1)
		  {
		    wchar_t buf[512];
		    swprintf(buf, 512, transcode(_("Match indices must be 1 or greater, not \"%s\"")).c_str(),
			     tocvt.c_str());
		    return buf;
		  }

		--val;

		if(val >= res->num_groups())
		  {
		    string group_values;
		    for(unsigned int i = 0; i<res->num_groups(); ++i)
		      {
			if(i > 0)
			  group_values += ",";
			group_values += res->group(i);
		      }

		    wchar_t buf[1024];
		    swprintf(buf, 1024, transcode(_("Match index %ls is too large; available groups are (%s)")).c_str(),
			     tocvt.c_str(), group_values.c_str());

		    return buf;
		  }

		rval += transcode(res->group(val));
	      }
	  }
      }

    return rval;
  }
public:
  pkg_grouppolicy_matchers(pkg_grouppolicy_factory *_chain,
			   pkg_signal *_sig, desc_signal *_desc_sig,
			   const vector<match_pair> &_subgroups)
        :pkg_grouppolicy(_sig, _desc_sig),
	 chain(_chain), subgroups(_subgroups)
  {
  }

  ~pkg_grouppolicy_matchers()
  {
    for(subtree_map::const_iterator i = subtrees.begin();
	i != subtrees.end(); ++i)
      delete i->second.policy;
  }

  void add_package(const pkgCache::PkgIterator &pkg,
		   pkg_subtree *root)
  {
    for(vector<match_pair>::const_iterator i = subgroups.begin();
	i != subgroups.end(); ++i)
	{
	  pkg_match_result *res = i->matcher->get_match(pkg);
	  if(res != NULL)
	    {
	      wstring title = substitute(i->tree_name, res);
	      delete res;

	      subtree_map::const_iterator found =
		subtrees.find(title);

	      if(found != subtrees.end())
		found->second.policy->add_package(pkg, found->second.tree);
	      else
		{
		  pkg_subtree *tree = new pkg_subtree(title);
		  pkg_grouppolicy *policy = chain->instantiate(get_sig(),
							       get_desc_sig());
		  root->add_child(tree);

		  subtrees[title]=subtree_pair(policy, tree);

		  policy->add_package(pkg, tree);
		}
	    }
	}
  }
};

pkg_grouppolicy *pkg_grouppolicy_matchers_factory :: instantiate(pkg_signal *sig,
								 desc_signal *_desc_sig)
{
  return new pkg_grouppolicy_matchers(chain, sig, _desc_sig, subgroups);
}

pkg_grouppolicy_matchers_factory :: ~pkg_grouppolicy_matchers_factory()
{
  for(std::vector<match_pair>::const_iterator i = subgroups.begin();
      i != subgroups.end(); ++i)
    delete i->matcher;
  delete chain;
}





class pkg_grouppolicy_tag : public pkg_grouppolicy
{
  pkg_grouppolicy_factory *chain;

  string facet;

  typedef hash_map<string, pair<pkg_grouppolicy *, pkg_subtree *> > childmap;

  childmap children;
public:
  pkg_grouppolicy_tag(const string &_facet, pkg_grouppolicy_factory *_chain,
		      pkg_signal *sig, desc_signal *desc_sig)
    :pkg_grouppolicy(sig, desc_sig), chain(_chain), facet(_facet)
  {
  }

  virtual void add_package(const pkgCache::PkgIterator &pkg,
			   pkg_subtree *root)
  {
    const set<tag> *tags = get_tags(pkg);
    for(set<tag>::const_iterator ti = tags->begin();
	ti != tags->end(); ++ti)
      {
	tag::const_iterator j = ti->begin();
	if(j == ti->end())
	  continue;

	string thisfacet = *j;

	if(thisfacet != facet)
	  continue;

	++j;
	if(j == ti->end())
	  continue;

	string tagname = *j;
	childmap::const_iterator found =
	  children.find(tagname);

	pkg_subtree *subtree = NULL;
	pkg_grouppolicy *subpolicy = NULL;

	if(found == children.end())
	  {
	    string desc = tag_description(ti->str());
	    string shortdesc(desc, 0, desc.find('\n'));

	    subtree = new pkg_subtree(swsprintf(L"%s - %s",
						tagname.c_str(),
						shortdesc.c_str()),
				      transcode(desc),
				      get_desc_sig());
	    root->add_child(subtree);
	    subpolicy = chain->instantiate(get_sig(),
					   get_desc_sig());

	    children[tagname] = pair<pkg_grouppolicy *, pkg_subtree *>(subpolicy, subtree);
	  }
	else
	  {
	    subpolicy = found->second.first;
	    subtree = found->second.second;
	  }

	subpolicy->add_package(pkg, subtree);
      }
  }

  ~pkg_grouppolicy_tag()
  {
    // Delete the subpolicies.
    for(childmap::const_iterator i = children.begin();
	i != children.end(); ++i)
      delete i->second.first;
  }
};

pkg_grouppolicy *pkg_grouppolicy_tag_factory::instantiate(pkg_signal *pkg_sig,
							  desc_signal *desc_sig)
{
  return new pkg_grouppolicy_tag(facet, chain, pkg_sig, desc_sig);
}

pkg_grouppolicy_tag_factory::~pkg_grouppolicy_tag_factory()
{
  delete chain;
}





class pkg_grouppolicy_facet_tag : public pkg_grouppolicy
{
  pkg_grouppolicy_factory *chain;

  typedef hash_map<string, pair<pkg_grouppolicy *, pkg_subtree *> > tagmap;

  typedef hash_map<string, pair<tagmap *, pkg_subtree *> > facetmap;

  pkg_subtree *untagged_tree;
  pkg_grouppolicy *untagged_policy;

  facetmap children;
public:
  pkg_grouppolicy_facet_tag(pkg_grouppolicy_factory *_chain,
			    pkg_signal *pkg_sig,
			    desc_signal *desc_sig)
    :pkg_grouppolicy(pkg_sig, desc_sig),
     chain(_chain),
     untagged_tree(NULL),
     untagged_policy(NULL)
  {
  }


  virtual void add_package(const pkgCache::PkgIterator &pkg,
			   pkg_subtree *root)
  {
    const set<tag> *tags = get_tags(pkg);

    // Put all untagged, non-virtual packages into a separate list.
    if(tags->empty() && !pkg.VersionList().end())
      {
	if(untagged_tree == NULL)
	  {
	    eassert(untagged_policy == NULL);

	    untagged_tree = new pkg_subtree(transcode(_("TAGLESS PACKAGES")),
					    transcode(_("\n These packages have not yet been classified in debtags.")),
					    get_desc_sig());
	    root->add_child(untagged_tree);

	    untagged_policy = chain->instantiate(get_sig(), get_desc_sig());
	  }

	untagged_policy->add_package(pkg, untagged_tree);
      }

    for(set<tag>::const_iterator ti = tags->begin();
	ti != tags->end(); ++ti)
      {
	tag::const_iterator j = ti->begin();

	eassert(j != ti->end());

	string thisfacet = *j;

	if(j != ti->end())
	  ++j;

	string thistag;

	if(j == ti->end())
	  thistag = _("MISSING TAG");
	else
	  thistag = *j;

	facetmap::const_iterator facetfound =
	  children.find(thisfacet);

	tagmap *tagchildren = NULL;
	pkg_subtree *tagtree = NULL;

	if(facetfound == children.end())
	  {
	    string desc = facet_description(thisfacet);
	    string shortdesc(desc, 0, desc.find('\n'));

	    if(!shortdesc.empty())
	      tagtree = new pkg_subtree(swsprintf(L"%s - %s",
						  thisfacet.c_str(),
						  shortdesc.c_str()),
					transcode(desc),
					get_desc_sig());
	    else
	      tagtree = new pkg_subtree(transcode(thisfacet),
					transcode(desc),
					get_desc_sig());
	    root->add_child(tagtree);
	    tagchildren = new tagmap;

	    children[thisfacet] = pair<tagmap *, pkg_subtree *>(tagchildren, tagtree);
	  }
	else
	  {
	    tagchildren = facetfound->second.first;
	    tagtree = facetfound->second.second;
	  }

	tagmap::const_iterator tagfound =
	  tagchildren->find(thistag);

	pkg_grouppolicy *subpolicy = NULL;
	pkg_subtree *subtree = NULL;

	if(tagfound == tagchildren->end())
	  {
	    string desc = tag_description(ti->str());
	    string shortdesc(desc, 0, desc.find('\n'));

	    if(!shortdesc.empty())
	      subtree = new pkg_subtree(swsprintf(L"%s - %s",
						  thistag.c_str(),
						  shortdesc.c_str()),
					transcode(desc),
					get_desc_sig());
	    else
	      subtree = new pkg_subtree(transcode(thistag),
					transcode(desc),
					get_desc_sig());

	    tagtree->add_child(subtree);
	    subpolicy = chain->instantiate(get_sig(), get_desc_sig());

	    (*tagchildren)[thistag] = pair<pkg_grouppolicy *, pkg_subtree *>(subpolicy, subtree);
	  }
	else
	  {
	    subpolicy = tagfound->second.first;
	    subtree = tagfound->second.second;
	  }

	subpolicy->add_package(pkg, subtree);
      }
  }

  ~pkg_grouppolicy_facet_tag()
  {
    for(facetmap::const_iterator i = children.begin();
	i != children.end(); ++i)
      {
	for(tagmap::const_iterator j = i->second.first->begin();
	    j != i->second.first->end(); ++j)
	  delete j->second.first;

	delete i->second.first;
      }
  }
};

pkg_grouppolicy *
pkg_grouppolicy_facet_tag_factory::instantiate(pkg_signal *sig,
					       desc_signal *desc_sig)
{
  return new pkg_grouppolicy_facet_tag(chain, sig, desc_sig);
}

pkg_grouppolicy_facet_tag_factory::~pkg_grouppolicy_facet_tag_factory()
{
  delete chain;
}
