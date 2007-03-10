// ui.h    -*-c++-*-
//
//  Copyright 2000,2001 Daniel Burrows
//
//  Global UI definitions and routines.  A lot of things here just provide a
// single point for common actions so those actions can be customized later.
// (eg, the progress-bar can appear in various forms)

#ifndef UI_H
#define UI_H

#include <apt-pkg/pkgcache.h>

#include <sigc++/signal.h>

#include <string>

#include <generic/util/bool_accumulate.h>
#include <generic/util/slotarg.h>

#include <vscreen/vs_editline.h>
// Bleah..this needs to be included so histories work

class OpProgress;

class download_signal_log;
class fragment;
class pkg_hier;
class vs_progress;
class vscreen_widget;
template<class T> class ref_ptr;

typedef ref_ptr<vscreen_widget> vs_widget_ref;
typedef ref_ptr<vs_progress> vs_progress_ref;

/******************************************************************************
 * Global signals:
 *****************************************************************************/

// File menu
extern sigc::signal0<void> file_quit;

/* A signal that widgets watching package states should update themselves.
 *
 * This should be triggered at the end of any keystroke (or mouse-stroke)
 * handler that alters the package states.  At a minimum, it will
 * call vscreen_update().
 *
 * \todo this seems to be at least partly redundant with the similar
 * signal on the apt cache file.  Should one of them be removed or
 * scaled back?
 */
extern sigc::signal0<void> package_states_changed;

/** Tests whether Undo -> Undo is enabled. */
extern sigc::signal0<bool, accumulate_or> undo_undo_enabled;

/** Emitted for Undo -> Undo. */
extern sigc::signal0<bool, accumulate_or> undo_undo;

/** Used to determine whether a target is available for the "package actions".
 */
extern sigc::signal0<bool, accumulate_or> package_menu_enabled;

/** Emitted for Package -> Install. */
extern sigc::signal0<bool, accumulate_or> package_install;

/** Emitted for Package -> Remove. */
extern sigc::signal0<bool, accumulate_or> package_remove;

/** Emitted for Package -> Purge. */
extern sigc::signal0<bool, accumulate_or> package_purge;

/** Emitted for Package -> Hold. */
extern sigc::signal0<bool, accumulate_or> package_hold;

/** Emitted for Package -> Keep. */
extern sigc::signal0<bool, accumulate_or> package_keep;

/** Emitted for Package -> Mark Auto */
extern sigc::signal0<bool, accumulate_or> package_mark_auto;

/** Emitted for Package -> Mark Manual */
extern sigc::signal0<bool, accumulate_or> package_unmark_auto;

/** Tests whether Package -> Forbid Version is enabled. */
extern sigc::signal0<bool, accumulate_or> package_forbid_enabled;

/** Emitted for Package -> Forbid. */
extern sigc::signal0<bool, accumulate_or> package_forbid;

/** Tests whether Package -> Package Information is enabled. */
extern sigc::signal0<bool, accumulate_or> package_information_enabled;

/** Emitted for Package -> Package Information. */
extern sigc::signal0<bool, accumulate_or> package_information;

/** Tests whether Package -> Changelog is enabled. */
extern sigc::signal0<bool, accumulate_or> package_changelog_enabled;

/** Emitted for Package -> Changelog. */
extern sigc::signal0<bool, accumulate_or> package_changelog;


/** Emitted for Resolver -> Toggle Rejected. */
extern sigc::signal0<bool, accumulate_or> resolver_toggle_rejected;

/** Tests whether Resolver -> Reject/Unreject Action is enabled. */
extern sigc::signal0<bool, accumulate_or> resolver_toggle_rejected_enabled;

/** Emitted for Resolver -> Toggle Approved. */
extern sigc::signal0<bool, accumulate_or> resolver_toggle_approved;

/** Tests whether Resolver -> Toggle Approved is enabled. */
extern sigc::signal0<bool, accumulate_or> resolver_toggle_approved_enabled;
/** Emitted for Resolver -> View Target */
extern sigc::signal0<bool, accumulate_or> resolver_view_target;

/** Tests whether Resolver -> View Target is enabled. */
extern sigc::signal0<bool, accumulate_or> resolver_view_target_enabled;


/** Tests whether Search -> Find is enabled. */
extern sigc::signal0<bool, accumulate_or> find_search_enabled;

/** Tests whether Search -> Find Backwards is enabled. */
extern sigc::signal0<bool, accumulate_or> find_search_back_enabled;

/** Tests whether Search -> Find Again is enabled. */
extern sigc::signal0<bool, accumulate_or> find_research_enabled;

/** Tests whether Search -> Find Again Reverse is enabled. */
extern sigc::signal0<bool, accumulate_or> find_repeat_search_back_enabled;

/** Tests whether Search -> Limit is enabled. */
extern sigc::signal0<bool, accumulate_or> find_limit_enabled;

/** Tests whether Search -> Cancel Limit is enabled. */
extern sigc::signal0<bool, accumulate_or> find_cancel_limit_enabled;

/** Tests whether Searc -> Find Broken is enabled. */
extern sigc::signal0<bool, accumulate_or> find_broken_enabled;

/** Emitted for Search -> Find. */
extern sigc::signal0<bool, accumulate_or> find_search;

/** Emitted for Search -> Find Backwards. */
extern sigc::signal0<bool, accumulate_or> find_search_back;

/** Emitted for Search -> Find Again. */
extern sigc::signal0<bool, accumulate_or> find_research;

/** Emitted for Search -> Find Again Reverse. */
extern sigc::signal0<bool, accumulate_or> find_repeat_search_back;

/** Emitted for Search -> Limit. */
extern sigc::signal0<bool, accumulate_or> find_limit;

/** Emitted for Search -> Cancel Limit. */
extern sigc::signal0<bool, accumulate_or> find_cancel_limit;

/** Emitted for Search -> Find Broken. */
extern sigc::signal0<bool, accumulate_or> find_broken;

/*****************************************************************************/

void ui_init();
void ui_main();

// Displays a "popup" widget.  If the second argument is false, show_all
// will not be called on the widget.
void popup_widget(const vs_widget_ref &w, bool do_show_all=true);

// Displays a widget on the "main" program level, inserting the given string
// into the menu listing available "main" screens to reference it.
//
// The "insert" form adds the new widget directly after the currently visible
// widget.

/** Add a new widget to the main interaction area.
 *
 *  \param w the widget to add
 *  \param menuref the text to add in the corresponding menu
 *  \param menudesc the description of the generated menu item
 *  \param tablabel the label of the corresponding tab
 */
void add_main_widget(const vs_widget_ref &w,
		     const std::string &menuref,
		     const std::string &menudesc,
		     const std::string &tablabel);
void insert_main_widget(const vs_widget_ref &w,
			const std::string &menuref,
			const std::string &menudesc,
			const std::string &tablabel);

/** Returns the currently active main widget. */
vs_widget_ref active_main_widget();

// Generates a progress bar.
vs_progress_ref gen_progress_bar();

// Generates an appropriate box to wrap text.
fragment *wrapbox(fragment *contents);

/** Generates a download progress object based on the user's current
 *  settings.
 *
 *  \param force_noninvasive if \b true, the generated UI object will
 *            always be "noninvasive" (typically a bar at the bottom of
 *            the screen).
 *  \param list_update       if \b true, this bar is for a list update
 *                           (meaning that we have to deal with apt's
 *                           utterly useless progress indication in this
 *                           case)
 *  \param title             if a new view is generated, this string is
 *                           used as its title; it will be transcoded.
 *  \param longtitle         if a new view is generated, this string is
 *                           used as its long title; it will be transcoded.
 *  \param abortslot         the slot to trigger if the download is aborted.
 *
 *  \return the new download manager and the download status widget.
 */
std::pair<download_signal_log *,
	  vs_widget_ref>
gen_download_progress(bool force_noninvasive,
		      bool list_update,
		      const std::string &title,
		      const std::string &longtitle,
		      const std::string &tablabel,
		      slot0arg abortslot);

// Asks the user for simple input (the question will appear in a "minibuffer"
// or in a dialog according to preferences)

void prompt_string(const std::string &prompt,
		   const std::string &text,
		   slotarg<sigc::slot1<void, std::wstring> > slot,
		   slotarg<sigc::slot0<void> > cancel_slot,
		   slotarg<sigc::slot1<void, std::wstring> > changed_slot,
		   vs_editline::history_list *history);

void prompt_string(const std::wstring &prompt,
		   const std::wstring &text,
		   slotarg<sigc::slot1<void, std::wstring> > slot,
		   slotarg<sigc::slot0<void> > cancel_slot,
		   slotarg<sigc::slot1<void, std::wstring> > changed_slot,
		   vs_editline::history_list *history);

void prompt_yesno(const std::string &prompt,
		  bool deflt,
		  slot0arg yesslot,
		  slot0arg noslot);

/** Display a popup dialog for a yes-no prompt.  Meant for prompts
 *  with large quantities of text.
 */
void prompt_yesno_popup(fragment *f,
			bool deflt,
			slot0arg yesslot,
			slot0arg noslot);

/** Display the given message, either in a popup dialog box or as a
 *  "transient" message at the bottom of the screen.  The message
 *  should be expected to be relatively short (ie, short enough to not
 *  need a scrollbar on a 'reasonable' terminal).  For convenience, a
 *  wrapbox is implicitly placed around the message.
 *
 *  \param msg the message to display
 *  \param okslot an optional slot to be called when the message is dismissed
 */
void show_message(fragment *msg,
		  slot0arg okslot,
		  const style &st = style_attrs_flip(A_REVERSE));

/** Display the given message, either in a popup dialog box or as a
 *  "transient" message at the bottom of the screen.  The message
 *  should be expected to be relatively short (ie, short enough to not
 *  need a scrollbar on a 'reasonable' terminal); it will be
 *  paragraph-wrapped as necessary.
 *
 *  \param msg the message to display
 *  \param okslot an optional slot to be called when the message is dismissed
 */
void show_message(const std::wstring &msg,
		  slot0arg okslot=NULL,
		  const style &st = style_attrs_flip(A_REVERSE));

/** Display the given message, either in a popup dialog box or as a
 *  "transient" message at the bottom of the screen.  The message
 *  should be expected to be relatively short (ie, short enough to not
 *  need a scrollbar on a 'reasonable' terminal); it will be
 *  paragraph-wrapped as necessary.
 *
 *  \param msg the message to display; will be transcoded
 *  \param okslot an optional slot to be called when the message is dismissed
 */
void show_message(const std::string &msg,
		  slot0arg okslot=NULL,
		  const style &st = style_attrs_flip(A_REVERSE));

/** Call this when a download starts; it sets the flag associated with
 *  the existence of a download, destroys the active preview (if any),
 *  and maeks the apt cache read-only.
 *
 *  \param hide_preview if \b true, the preview screen will be hidden
 *  prior to starting the download.
 */
void ui_start_download(bool hide_preview = true);

/** Call this when a download finishes; it clears the flag associated
 *  with the existence of a download and puts the cache back into
 *  read-write mode.
 */
void ui_stop_download();

// Can be used to manually pop up an error dialog, if necessary.
void check_apt_errors();

/** Display the solution editor screen if it isn't visible. */
void do_examine_solution();

void do_new_package_view(OpProgress &progress);
// Displays a new package-view.

void do_package_run_or_show_preview();
// Shows a preview if previews are enabled (and why would you disable them?),
// otherwise does the same thing as install_or_remove_packages.

void install_or_remove_packages();
// Installs or removes packages.  (the thing that happens after you press
// "g" twice)

void do_update_lists();
// Updates the package lists.

void do_forget_new();
// Forgets which packages are "new".

/** Advances to the next solution, if one exists. */
void do_next_solution();

/** Returns to the previous solution, if one exists. */
void do_previous_solution();

/** Applies the current solution, if it exists. */
void do_apply_solution();

// These generate particular screens of the UI:

// Info screen
vs_widget_ref make_info_screen(const pkgCache::PkgIterator &pkg,
			       const pkgCache::VerIterator &ver);
// Dependency screen
vs_widget_ref make_dep_screen(const pkgCache::PkgIterator &pkg,
			      const pkgCache::VerIterator &ver,
			      bool reverse=false);
// List of versions
vs_widget_ref make_ver_screen(const pkgCache::PkgIterator &pkg);

// Various defaults:
extern const char *default_pkgstatusdisplay;
extern const char *default_pkgheaderdisplay;
extern const char *default_grpstr;

#endif
