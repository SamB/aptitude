#include "treeview_cell_tooltips.h"

#include <map>

namespace gui
{
  namespace
  {
    Glib::Quark treeview_tooltip_signal_connection_property("aptitude-treeview-cell-tooltip-signal-connection");
    Glib::Quark treeview_cell_tooltip_property("aptitude-treeview-cell-tooltip");

    /** \brief A state object that manages the tooltip of a view column.
     *
     *  This will have the same lifetime as the associated column
     *  object; its lifetime is managed by set_column_tooltip() and
     *  column_tooltip_destroy_notify().
     */
    class treeview_cell_tooltip
    {
      bool is_markup;
      Gtk::TreeModelColumn<Glib::ustring> tooltip_column;

    public:
      treeview_cell_tooltip(bool _is_markup, Gtk::TreeModelColumn<Glib::ustring> _tooltip_column)
	: is_markup(_is_markup),
	  tooltip_column(_tooltip_column)
      {
      }

      /** \brief Set up the properties of the given tooltip object
       *  to actually display this tooltip.
       *
       *  \param tooltip   The tooltip to initialize.
       *  \param row       The tree model row to draw tooltip values from.
       *
       *  \return \b true if there is a tooltip corresponding to this column
       *  in this row.
       */
      bool setup_tooltip(const Glib::RefPtr<Gtk::Tooltip> &tooltip,
			 const Gtk::TreeModel::Row &row)
      {
	const Glib::ustring text(row[tooltip_column]);

	if(text.empty())
	  return false;

	if(is_markup)
	  tooltip->set_markup(text);
	else
	  tooltip->set_text(text);

	return true;
      }
    };


    // Routines to manage the tooltip property on columns.
    void cell_tooltip_destroy_notify(gpointer data)
    {
      treeview_cell_tooltip *tooltip =
	static_cast<treeview_cell_tooltip *>(data);

      delete tooltip;
    }

    treeview_cell_tooltip *get_column_tooltip(Gtk::TreeViewColumn *view_column)
    {
      return static_cast<treeview_cell_tooltip *>(view_column->get_data(treeview_cell_tooltip_property));
    }

    void set_column_tooltip(Gtk::TreeViewColumn *view_column,
			    treeview_cell_tooltip *tooltip)
    {
      view_column->set_data(treeview_cell_tooltip_property, tooltip,
			    &cell_tooltip_destroy_notify);
    }



    bool do_signal_query_tooltip(int x, int y,
				 bool keyboard_tooltip,
				 const Glib::RefPtr<Gtk::Tooltip> &tooltip,
				 Gtk::TreeView &treeview)
    {
      if(keyboard_tooltip)
	// We have no X or Y; no tooltip for you!  \todo It might be
	// annoying, but we *could* figure out which cell is focused
	// and use that fact.
	return false;

      // Find the current column.
      Gtk::TreeModel::Path path;
      Gtk::TreeViewColumn *column;
      int cell_x, cell_y;
      if(!treeview.get_path_at_pos(x, y, path, column, cell_x, cell_y))
	return false;

      treeview_cell_tooltip *cell_tooltip(get_column_tooltip(column));
      if(tooltip == NULL)
	return false;

      Gtk::TreeModel::iterator iter = treeview.get_model()->get_iter(path);
      Gtk::TreeModel::Row row(*iter);

      return cell_tooltip->setup_tooltip(tooltip, row);
    }
  }


  // Properties managing the query-tooltip signal connection property
  // on tree-views.
  sigc::connection *get_treeview_tooltip_signal_connection(Gtk::TreeView *treeview)
  {
    void *rval = treeview->get_data(treeview_tooltip_signal_connection_property);
    return static_cast<sigc::connection *>(rval);
  }

  void tooltip_signal_connection_property_destroy(void *data)
  {
    sigc::connection *conn = static_cast<sigc::connection *>(data);
    if(conn != NULL)
      {
	conn->disconnect();
	delete conn;
      }
  }

  void set_treeview_tooltip_signal_connection(Gtk::TreeView *treeview,
					      const sigc::connection &tooltip_signal_connection)
  {
    treeview->set_data(treeview_tooltip_signal_connection_property,
		       static_cast<void *>(new sigc::connection(tooltip_signal_connection)),
		       &tooltip_signal_connection_property_destroy);
  }

  void clear_treeview_tooltip_signal_connection(Gtk::TreeView *treeview)
  {
    treeview->set_data(treeview_tooltip_signal_connection_property, NULL);
  }



  void enable_column_tooltips(Gtk::TreeView *treeview)
  {
    if(get_treeview_tooltip_signal_connection(treeview) == NULL)
      {
	sigc::connection conn = treeview->signal_query_tooltip().connect(sigc::bind(sigc::ptr_fun(&do_signal_query_tooltip),
										    sigc::ref(*treeview)));

	treeview->property_has_tooltip() = true;

	set_treeview_tooltip_signal_connection(treeview, conn);
      }
  }

  void disable_column_tooltips(Gtk::TreeView *treeview)
  {
    clear_treeview_tooltip_signal_connection(treeview);
  }



  void set_text_tooltip(Gtk::TreeView *treeview,
			Gtk::TreeViewColumn *view_column,
			const Gtk::TreeModelColumn<Glib::ustring> &model_column)
  {
    enable_column_tooltips(treeview);
    set_column_tooltip(view_column,
		       new treeview_cell_tooltip(false, model_column));
  }

  void set_markup_tooltip(Gtk::TreeView *treeview,
			  Gtk::TreeViewColumn *view_column,
			  const Gtk::TreeModelColumn<Glib::ustring> &model_column)
  {
    enable_column_tooltips(treeview);
    set_column_tooltip(view_column,
		       new treeview_cell_tooltip(true, model_column));
  }
}
