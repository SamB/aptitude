// -*-c++-*-

// changelog.cpp
//
//  Copyright 1999-2008 Daniel Burrows
//  Copyright 2008 Obey Arthur Liu
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

#include "changelog.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <apt-pkg/fileutl.h>
#include <apt-pkg/pkgsystem.h>
#include <apt-pkg/tagfile.h>
#include <apt-pkg/version.h>

#include <generic/apt/download_manager.h>
#include <generic/apt/pkg_changelog.h>

#include <gtk/gui.h>
#include <gtk/progress.h>

namespace gui
{
  dummyPkgAcquireStatus::dummyPkgAcquireStatus()
  {
    ;;
  }

  bool dummyPkgAcquireStatus::MediaChange(std::string, std::string)
  {
    return true;
  }

  string change_text_fragment(const std::string &s)
  {
    string lines;

    std::string::size_type start = 0;
    std::string::size_type next_nl;

    do
      {
        next_nl = s.find('\n', start);

        if(s[start] == ' ')
          ++start;

        if(next_nl == start + 1 && s[start] == '.')
          {
            lines += "\n";
            start = next_nl + 1;
            continue;
          }

        std::string this_line;
        if(next_nl != std::string::npos)
          this_line.assign(s, start, next_nl - start);
        else
          this_line.assign(s, start, std::string::npos);

        size_t first_nonspace = 0;
        while(first_nonspace < this_line.size() && isspace(this_line[first_nonspace]))
          ++first_nonspace;

        bool has_bullet = false;
        if(first_nonspace < this_line.size())
          switch(this_line[first_nonspace])
            {
            case '*':
            case '+':
            case '-':
              has_bullet = true;
              break;
            }

        if(has_bullet)
          {
            string item =
              ssprintf("%s %s%s\n",
                        std::string(this_line, 0, first_nonspace).c_str(),
                        std::string(this_line, first_nonspace, 1).c_str(),
                        std::string(this_line, first_nonspace + 1).c_str());
            lines += item;
          }
        else
          lines += ssprintf("%s\n", this_line.c_str());

        start = next_nl + 1;
      } while(next_nl != std::string::npos);

    return lines;
  }

  void ChangeLogView::parse_predigested_changelog(const temp::name &digest,
                                            const std::string &curver)
  {
    FileFd digestfd(digest.get_name(), FileFd::ReadOnly);

    if(digestfd.IsOpen())
      {
        pkgTagFile tagfile(&digestfd);

        pkgTagSection sec;

        bool first = true;

        while(tagfile.Step(sec))
          {
            std::string version(sec.FindS("Version"));
            std::string changes(sec.FindS("Changes"));
            std::string maintainer(sec.FindS("Maintainer"));
            std::string date(sec.FindS("Date"));

            textBuffer->insert(textBuffer->end(), "\n");

            textBuffer->insert(textBuffer->end(), ssprintf("\n -- %s  %s",
                                        maintainer.c_str(),
                                        date.c_str()));

            if (!first)
              textBuffer->insert(textBuffer->end(), "\n");

            textBuffer->insert(textBuffer->end(), change_text_fragment(changes));

            first = false;

            /*
            if(!curver.empty() && _system->VS->CmpVersion(version, curver) > 0)
              {
                cw::style s = cw::get_style("ChangelogNewerVersion");
                fragments.push_back(cw::style_fragment(f, s));
              }
            else
              fragments.push_back(f);
            */
          }
      }
  }

  temp::name ChangeLogView::digest_changelog(const temp::name &changelog)
  {
    temp::name rval(changelog.get_parent(), "parsedchangelog");

    if(system(ssprintf("parsechangelog --all --format rfc822 -l %s > %s 2> /dev/null",
                       changelog.get_name().c_str(),
                       rval.get_name().c_str()).c_str()) == 0)
      return rval;
    else
      return temp::name();
  }

  void ChangeLogView::do_view_changelog(temp::name n,
                                string pkgname,
                                string curverstr)
  {
    string menulabel =
      ssprintf(_("ChangeLog of %s"), pkgname.c_str());
    string tablabel = ssprintf(_("%s changes"), pkgname.c_str());
    string desclabel = _("View the list of changes made to this Debian package.");

    add_changelog_buffer(n, curverstr);
    textview->set_buffer(textBuffer);
  }

  void ChangeLogView::add_changelog_buffer(const temp::name &file,
                                        const std::string &curver)
  {
    temp::name digested = digest_changelog(file);

    if (digested.valid())
      parse_predigested_changelog(digested, curver);
  }

  ChangeLogView::ChangeLogView(Glib::RefPtr<Gnome::Glade::Xml> refGlade,
                               Glib::ustring gladename)
  {
    refGlade->get_widget(gladename, textview);
  }

  ChangeLogView::~ChangeLogView()
  {
    // TODO Auto-generated destructor stub
  }

  void ChangeLogView::load_version(pkgCache::VerIterator ver)
  {
    textBuffer = Gtk::TextBuffer::create();

    bool in_debian=false;

    string pkgname = ver.ParentPkg().Name();

    pkgCache::VerIterator curver = ver.ParentPkg().CurrentVer();
    string curverstr;
    if(!curver.end() && curver.VerStr() != NULL)
      curverstr = curver.VerStr();

    // TODO: add a configurable association between origins and changelog URLs.
    for(pkgCache::VerFileIterator vf=ver.FileList();
        !vf.end() && !in_debian; ++vf)
      if(!vf.File().end() && vf.File().Origin()!=NULL &&
         strcmp(vf.File().Origin(), "Debian")==0)
        in_debian=true;

    if(!in_debian)
      {
        textview->get_buffer()->set_text(_("You can only view changelogs of official Debian packages."));
        return;
      }

    std::auto_ptr<download_manager> manager(get_changelog(ver, sigc::bind(sigc::mem_fun(*this, &ChangeLogView::do_view_changelog), pkgname, curverstr)));

    start_download(manager.release(),
		   _("Downloading changelogs"),
		   false,
		   pMainWindow->get_notifyview());
  }
}
