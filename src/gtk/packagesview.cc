// packagesview.cc
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

#include "packagesview.h"
#include "aptitude.h"

#undef OK
#include <gtkmm.h>

#include <apt-pkg/error.h>

#include <generic/apt/apt.h>
#include <generic/apt/apt_undo_group.h>
#include <generic/apt/matchers.h>

#include <gtk/gui.h>
#include <gtk/entityview.h>
#include <gtk/pkgview.h>

namespace gui
{
  void PackagesViewGenerator::add(pkgCache::PkgIterator pkg)
  {
    Gtk::TreeModel::iterator iter = store->append();
    Gtk::TreeModel::Row row = *iter;
    PkgEntity ent(cols, pkg);
    ent.fill_row(row);
  }

  bool PackagesViewGenerator::add_reverse(Gtk::TreeModel::iterator iter)
  {
    p->OverallProgress(0, total, ++num, _("Finishing view"));
    Entity * ent = (*iter)[cols->EntObject];
    PkgEntity * pkgent = dynamic_cast<PkgEntity *>(ent);
    revstore->insert(std::make_pair(pkgent->pkg, iter));
    return false;
  }

  void PackagesViewGenerator::prefinish()
  {
    store->set_sort_column(cols->Name, Gtk::SORT_ASCENDING);
    // FIXME: Hack while finding a nonblocking thread join.
    finished = true;
  }

  void PackagesViewGenerator::finish()
  {
    num = 0;
    total = store->children().size();
    store->foreach_iter(sigc::mem_fun(*this, &PackagesViewGenerator::add_reverse));
    p->OverallProgress(total, total, 1, _("Finishing view"));
  }

  void PackagesViewGenerator::build_store(Glib::ustring limit)
  {
    p = gen_progress_bar();

    num = 0;
    total = (*apt_cache_file)->Head().PackageCount;
    bool limited = false;
    aptitude::matching::pkg_matcher * limiter = NULL;
    if (limit != "")
    {
      limiter = aptitude::matching::parse_pattern(limit);
      limited = (limiter != NULL);
    }

    for(pkgCache::PkgIterator pkg=(*apt_cache_file)->PkgBegin(); !pkg.end(); pkg++)
      {
        p->OverallProgress(num, total, 1, _("Building view"));

        ++num;

        // Filter useless packages up-front.
        if(pkg.VersionList().end() && pkg.ProvidesList().end())
          continue;
        if (!limited || aptitude::matching::apply_matcher(limiter, pkg, *apt_cache_file, *apt_package_records))
        {
          add(pkg);
        }
      }

    p->OverallProgress(total, total, 1,  _("Prefinalizing view"));

    Glib::Thread * sort_thread = Glib::Thread::create(sigc::mem_fun(*this, &PackagesViewGenerator::prefinish), true);
    while(!finished)
    {
      pMainWindow->get_progress_bar()->pulse();
      gtk_update();
      Glib::usleep(100000);
    }
    sort_thread->join();

    p->OverallProgress(total, total, 1,  _("Finalizing view"));

    finish();

    delete p;
  }

  PackagesViewGenerator::PackagesViewGenerator(EntityColumns *cols)
  {
    this->cols = cols;
    store = Gtk::ListStore::create(*cols);
    revstore = new std::multimap<pkgCache::PkgIterator, Gtk::TreeModel::iterator>();
  }

}
