// vs_radiogroup.h                    -*-c++-*-
//
//  Ok, here's how radio-button-like behavior is implemented:
//
//  Radio-groups store some group of buttons.  They constrain the buttons
// so that exactly one is on.  This is done by various devious means:
// the first button added is always selected, and subsequently added selected
// buttons override previously selected buttons.  You can manually select a
// button through this class (by ID) or via the button's own set_checked
// routine.
//
//  (note that radio-groups are NOT WIDGETS!!!)
//  (note that this does NOT attempt to memory-manage its "children"!)
//  (note that you should generally delete this at the same time as its
//   children, or Bad Things[tm] may happen.. (more specifically, if you
//   delete a selected child, some other random option will be selected))
//
//  Oh, one more note: although any togglebutton can be used in a radio
// widget, passing in checkbuttons has a high probability of causing weird
// things.  Use radiobuttons.  (if you want them to look like checkbuttons,
// use the extended togglebutton constructor..)

#ifndef VS_RADIOGROUP_H
#define VS_RADIOGROUP_H

#include "ref_ptr.h"

#include <vector>

#include <sigc++/connection.h>
#include <sigc++/trackable.h>

class vs_togglebutton;

class vs_radiogroup:public sigc::trackable
{
  struct item
  {
    ref_ptr<vs_togglebutton> b;
    int id;

    // Needed, unfortunately.
    sigc::connection destroyed_conn, pressed_conn;

    item(const ref_ptr<vs_togglebutton> &_b, int _id,
	 const sigc::connection &_dconn, const sigc::connection &_pconn)
      :b(_b), id(_id), destroyed_conn(_dconn), pressed_conn(_pconn) {}
  };

  typedef std::vector<item> itemlist;

  itemlist items;

  // The index of the currently selected button
  itemlist::size_type selected;

  // Called when a particular button is selected.
  // The argument is the *index* of the button.
  void button_pressed(itemlist::size_type index);
public:
  vs_radiogroup();
  ~vs_radiogroup();

  void add_button(const ref_ptr<vs_togglebutton> &b, int id);
  void rem_button(const ref_ptr<vs_togglebutton> &b);

  void rem_button_bare(vs_togglebutton &b);

  /** \return \b true if a button is selected. */
  bool selection_valid();

  /** \return the id of the selected button, if the selection is valid. */
  int get_selected();

  // Selects a button by id.
  void select(int id);

  /** Destroy this radio group. */
  void destroy();

  // Emitted when one of the sub-items is chosen.  (you could also collect
  // the individual button signals; this is just a higher-level view of it)
  sigc::signal1<void, int> item_selected;
};

#endif
