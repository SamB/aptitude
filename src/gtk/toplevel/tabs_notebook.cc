/** \file tabs_notebook.cc */

// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include "tabs_notebook.h"

#include <generic/util/enumerator.h>

#include <loggers.h>
#include <gtk/util/property.h>

#include <boost/weak_ptr.hpp>

#include <gtkmm.h>

using aptitude::Loggers;
using aptitude::util::dynamic_set;
using aptitude::util::enumerator;
using boost::shared_ptr;
using boost::weak_ptr;

namespace gui
{
  namespace toplevel
  {
    namespace
    {
      std::string safe_get_name(const boost::shared_ptr<tab_display_info> &tab)
      {
        if(tab.get() == NULL)
          return "(null)";
        else
          return tab->get_name();
      }

      // This property is attached to the widget stored in each tab,
      // to allow it to be traced back to the corresponding
      // tab_display_info object.
      property<weak_ptr<tab_display_info> > tab_property("aptitude-tabs-notebook-tab-info");

      // \note I'd like to find a way to hide the fact that this is a
      // Notebook; right now a savvy client could use a legal downcast
      // (e.g., dynamic_cast<>) to treat this as a notebook and break
      // its invariants.  I'm not sure this is possible in the gtk--
      // framework, though (at least, not without some implications --
      // I could allocate a vbox and put this in it, but will that
      // affect layout?  Unsure).
      class tabs_notebook : public Gtk::Notebook
      {
        shared_ptr<dynamic_set<shared_ptr<tab_display_info> > > tabs;
        logging::LoggerPtr logger;
        // Used to know which page we're switching away from when we
        // switch.
        shared_ptr<tab_display_info> last_active_tab;

        /** \brief Get the tab of the currently selected page, or NULL
         *  if nothing is selected (or if the selected page has no
         *  tab, but that should be impossible).
         */
        shared_ptr<tab_display_info> get_current_tab();

      public:
        tabs_notebook();

        /** \brief Set the tabs shown by this tab object.
         *
         *  Closes all the tabs currently displayed, then adds the new
         *  ones.
         */
        void set_tabs(const shared_ptr<dynamic_set<shared_ptr<tab_display_info> > > &tabs);

      private:
        void handle_inserted(const shared_ptr<tab_display_info> &tab);
        void handle_removed(const shared_ptr<tab_display_info> &tab);
        void handle_switch_page(GtkNotebookPage *page,
                                guint page_num);
      };

      tabs_notebook::tabs_notebook()
        : logger(Loggers::getAptitudeGtkToplevelTabs())
      {
        signal_switch_page().connect(sigc::mem_fun(*this,
                                                   &tabs_notebook::handle_switch_page));
      }

      void tabs_notebook::set_tabs(const shared_ptr<dynamic_set<shared_ptr<tab_display_info> > > &new_tabs)
      {
        LOG_TRACE(logger, "Attaching to the tabs set at " << new_tabs
                  << " with " << new_tabs->size() << " initial entries.");
        if(tabs != NULL)
          {
            LOG_TRACE(logger, "Closing " << tabs->size()
                      << " existing tabs.");

            // We have to force-close the old tabs; note that I can't rely
            // on set enumerators being stable over deletes.  This code
            // assumes that force_close behaves as advertised; i.e., that
            // it actually causes the tab to be removed from the set, and
            // that doing so triggers this object's removed() routine.

            std::vector<shared_ptr<tab_display_info> > tabs_copy;
            for(shared_ptr<enumerator<shared_ptr<tab_display_info> > > e
                  = tabs->enumerate(); e->advance(); )
              tabs_copy.push_back(e->get_current());

            for(std::vector<shared_ptr<tab_display_info> >::const_iterator it =
                  tabs_copy.begin(); it != tabs_copy.end(); ++it)
              (*it)->force_close();



            // Now, just in case the comment above was wrong, destroy any
            // remaining tabs.  Any tabs that are left represent a bug in
            // the program.
            tabs_copy.clear();
            for(shared_ptr<enumerator<shared_ptr<tab_display_info> > > e
                  = tabs->enumerate(); e->advance(); )
              {
                shared_ptr<tab_display_info> tab = e->get_current();

                if(tab.get() == NULL)
                  {
                    LOG_ERROR(logger, "NULL tab encountered while replacing tabs list.");
                    continue;
                  }

                LOG_ERROR(logger, "The tab " << safe_get_name(tab) << " ("
                          << tab << ") was not closed by force_close()!.");

                if(tab->get_widget() == NULL)
                  LOG_ERROR(logger, "The tab " << safe_get_name(tab)
                            << " (" << tab << ") was already destroyed.");
                else
                  {
                    remove_page(*tab->get_widget());
                    tab->reset_widget();
                  }
              }
          }


        // Now inject the new tabs:
        tabs = new_tabs;
        for(shared_ptr<enumerator<shared_ptr<tab_display_info> > > e = tabs->enumerate();
            e->advance(); )
          handle_inserted(e->get_current());

        tabs->connect_inserted(sigc::mem_fun(*this,
                                             &tabs_notebook::handle_inserted));

        tabs->connect_removed(sigc::mem_fun(*this,
                                            &tabs_notebook::handle_removed));
      }

      void tabs_notebook::handle_inserted(const shared_ptr<tab_display_info> &tab)
      {
        if(tab.get() == NULL)
          {
            LOG_TRACE(logger, "Attempt to insert a NULL tab.");
            return;
          }

        LOG_TRACE(logger,
                  "Inserting " << safe_get_name(tab) << " (" << tab << ")");

        // Sanity-check.
        if(tab->get_widget() == NULL)
          {
            LOG_ERROR(logger,
                      "The tab " << safe_get_name(tab) << " (" << tab
                      << ") was already destroyed.");
            return;
          }

        weak_ptr<tab_display_info> tab_weak(tab);

        // Backlink the widget to its tab, and start listening for
        // signals on the tab.  Note that the backlink must be weak,
        // to avoid reference cycles.
        tab_property.set_on(tab->get_widget(), tab_weak);

        append_page(*manage(tab->get_widget()));
      }

      void tabs_notebook::handle_removed(const shared_ptr<tab_display_info> &tab)
      {
        if(tab.get() == NULL)
          {
            LOG_ERROR(logger, "Attempt to remove a NULL tab.");
            return;
          }

        LOG_TRACE(logger, "Removing " << safe_get_name(tab)
                  << " (" << tab << ")");

        if(tab->get_widget() == NULL)
          {
            LOG_ERROR(logger, "The tab " << safe_get_name(tab)
                      << " (" << tab << ") was already destroyed.");
            return;
          }

        remove_page(*tab->get_widget());

        last_active_tab = get_current_tab();
        if(last_active_tab.get() != NULL)
          last_active_tab->set_active(true);
      }

      shared_ptr<tab_display_info> tabs_notebook::get_current_tab()
      {
        int page = get_current_page();
        if(page == -1)
          return shared_ptr<tab_display_info>();
        else
          {
            Gtk::Widget * const widget = get_nth_page(page);

            if(widget == NULL)
              {
                LOG_ERROR(logger,
                          "Page " << page <<
                          " has no associated widget.");
                return shared_ptr<tab_display_info>();
              }
            else
              {
                shared_ptr<tab_display_info> rval =
                  tab_property.get_from(widget,
                                        weak_ptr<tab_display_info>()).lock();

                if(rval.get() == NULL)
                  LOG_ERROR(logger,
                            "Page " << page <<
                            " has no associated tab_display_info object.");

                return rval;
              }
          }
      }

      void tabs_notebook::handle_switch_page(GtkNotebookPage *page,
                                             guint page_num)
      {
        shared_ptr<tab_display_info> new_tab = get_current_tab();
        LOG_TRACE(logger, "Switching from "
                  << safe_get_name(last_active_tab) << "("
                  << last_active_tab << ") to "
                  << safe_get_name(new_tab) << " ("
                  << new_tab << ")");

        last_active_tab->set_active(false);


        last_active_tab = new_tab;
        new_tab->set_active(true);
      }
    }

    Gtk::Widget *
    create_tabs_notebook(const shared_ptr<dynamic_set<shared_ptr<tab_display_info> > > &tabs)
    {
      tabs_notebook *rval = new tabs_notebook;
      rval->set_tabs(tabs);
      return rval;
    }
  }
}
