// vs_util.cc

#include "transcode.h"
#include "vs_button.h"
#include "vs_center.h"
#include "vs_editline.h"
#include "vs_frame.h"
#include "vs_label.h"
#include "vs_pager.h"
#include "vs_util.h"
#include "vs_scrollbar.h"
#include "vs_table.h"
#include "vs_text_layout.h"

#include <config/colors.h>
#include <config/keybindings.h>

#include <sigc++/adaptors/bind.h>
#include <sigc++/adaptors/hide.h>
#include <sigc++/functors/mem_fun.h>

#include <aptitude.h>

using namespace std;

static void do_slot0_dialog(vscreen_widget &ownerBare,
			    slot0arg okslot)
{
  vs_widget_ref owner(&ownerBare);

  owner->destroy();
  if(okslot)
    (*okslot)();
}

vs_widget_ref vs_dialog_ok(const vs_widget_ref &w,
			   slot0arg okslot,
			   const wstring &label,
			   const style &st)
{
  vs_center_ref center = vs_center::create();

  vs_table_ref table = vs_table::create();

  vs_button_ref okbutton = vs_button::create(label);

  okbutton->pressed.connect(sigc::bind(sigc::ptr_fun(&do_slot0_dialog),
				       center.weak_ref(), okslot));

  table->add_widget(w, 0, 0, 1, 1, true, true);
  table->add_widget(vs_center::create(okbutton), 1, 0, 1, 1, false, false);
  table->connect_key("Confirm", &global_bindings, okbutton->pressed.make_slot());

  vs_frame_ref frame = vs_frame::create(table);

  center->add_widget(frame);
  frame->set_bg_style(st);
  return center;
}

vs_widget_ref vs_dialog_ok(fragment *msg, slot0arg okslot,
			   const wstring &label,
			   const style &st, bool scrollbar)
{
  vs_widget_ref w;

  if(scrollbar)
    {
      vs_table_ref t = vs_table::create();
      w=t;

      vs_text_layout_ref l = vs_text_layout::create(msg);
      vs_scrollbar_ref s = vs_scrollbar::create(vs_scrollbar::VERTICAL);

      t->add_widget(l, 0, 0, 1, 1, true, true);
      t->add_widget_opts(s, 0, 1, 1, 1,
			 vs_table::ALIGN_RIGHT,
			 vs_table::ALIGN_CENTER | vs_table::FILL);

      l->location_changed.connect(sigc::mem_fun(*s.unsafe_get_ref(), &vs_scrollbar::set_slider));
      s->scrollbar_interaction.connect(sigc::mem_fun(*l.unsafe_get_ref(), &vs_text_layout::scroll));
    }
  else
    w=vs_text_layout::create(msg);

  return vs_dialog_ok(w, okslot, label, st);
}

vs_widget_ref vs_dialog_ok(fragment *msg, slot0arg okslot, const style &st, bool scrollbar)
{
  return vs_dialog_ok(msg, okslot, transcode(_("Ok")), st, scrollbar);
}

vs_widget_ref vs_dialog_ok(fragment *msg, slot0arg okslot, bool scrollbar)
{
  return vs_dialog_ok(msg, okslot, style_attrs_flip(A_REVERSE), scrollbar);
}

vs_widget_ref vs_dialog_ok(const wstring &msg, slot0arg okslot,
			   const style &st)
{
  vs_widget_ref l=vs_label::create (msg);

  return vs_dialog_ok(l, okslot, transcode(_("Ok")), st);
}

vs_widget_ref vs_dialog_ok(const wstring &msg, slot0arg okslot)
{
  return vs_dialog_ok(msg, okslot, style_attrs_flip(A_REVERSE));
}


vs_widget_ref vs_dialog_yesno(const vs_widget_ref &widget,
			      slot0arg yesslot,
			      const wstring &yeslabel,
			      slot0arg noslot,
			      const wstring &nolabel,
			      const style &st,
			      bool deflt)
{
  vs_center_ref center = vs_center::create();

  vs_table_ref table = vs_table::create();

  vs_button_ref yesbutton = vs_button::create(yeslabel);
  vs_button_ref nobutton = vs_button::create(nolabel);

  yesbutton->pressed.connect(sigc::bind(sigc::ptr_fun(&do_slot0_dialog),
					center.weak_ref(), yesslot));
  nobutton->pressed.connect(sigc::bind(sigc::ptr_fun(&do_slot0_dialog),
				       center.weak_ref(), noslot));

  table->connect_key("Yes", &global_bindings, yesbutton->pressed.make_slot());
  table->connect_key("No", &global_bindings, nobutton->pressed.make_slot());
  table->connect_key("Cancel", &global_bindings, nobutton->pressed.make_slot());

  table->add_widget(widget, 0, 0, 1, 2, true, true);
  table->add_widget_opts(yesbutton, 1, 0, 1, 1, vs_table::SHRINK|vs_table::ALIGN_CENTER, 0);
  table->add_widget_opts(nobutton, 1, 1, 1, 1, vs_table::SHRINK|vs_table::ALIGN_CENTER, 0);

  widget->show();
  yesbutton->show();
  nobutton->show();

  if(deflt)
    table->focus_widget(yesbutton);
  else
    table->focus_widget(nobutton);

  vs_frame_ref frame = vs_frame::create(table);
  frame->set_bg_style(st);

  center->add_widget(frame);

  return center;
}

vs_widget_ref vs_dialog_yesno(const wstring &msg,
			      slot0arg yesslot,
			      const wstring &yeslabel,
			      slot0arg noslot,
			      const wstring &nolabel,
			      const style &st,
			      bool deflt)
{
  vs_widget_ref txt=vs_label::create(msg);

  return vs_dialog_yesno(txt, yesslot, yeslabel, noslot, nolabel, st, deflt);
}

vs_widget_ref vs_dialog_yesno(const wstring &msg,
			      slot0arg yesslot,
			      slot0arg noslot,
			      const style &st,
			      bool deflt)
{
  return vs_dialog_yesno(msg, yesslot, transcode(_("Yes")),
			 noslot, transcode(_("No")), st, deflt);
}

vs_widget_ref vs_dialog_yesno(const wstring &msg,
			      slot0arg yesslot,
			      slot0arg noslot,
			      bool deflt)
{
  return vs_dialog_yesno(msg,
			 yesslot,
			 noslot,
			 style_attrs_flip(A_REVERSE),
			 deflt);
}


vs_widget_ref vs_dialog_yesno(fragment *msg,
			      slot0arg yesslot,
			      slot0arg noslot,
			      bool scrollbar,
			      bool deflt)
{
  return vs_dialog_yesno(msg,
			 yesslot,
			 noslot,
			 style_attrs_flip(A_REVERSE),
			 scrollbar,
			 deflt);
}

vs_widget_ref vs_dialog_yesno(fragment *msg,
			      slot0arg yesslot,
			      slot0arg noslot,
			      const style &st,
			      bool scrollbar,
			      bool deflt)
{
  return vs_dialog_yesno(msg, yesslot, transcode(_("Yes")),
			 noslot, transcode(_("No")), st,
			 scrollbar, deflt);
}

vs_widget_ref vs_dialog_yesno(fragment *msg,
			      slot0arg yesslot,
			      const std::wstring &yeslabel,
			      slot0arg noslot,
			      const std::wstring &nolabel,
			      const style &st,
			      bool scrollbar,
			      bool deflt)
{
  vs_widget_ref w;

  if(scrollbar)
    {
      vs_table_ref t = vs_table::create();
      w=t;

      vs_text_layout_ref l = vs_text_layout::create(msg);
      vs_scrollbar_ref s = vs_scrollbar::create(vs_scrollbar::VERTICAL);

      t->add_widget(l, 0, 0, 1, 1, true, true);
      t->add_widget_opts(s, 0, 1, 1, 1,
			 vs_table::ALIGN_RIGHT,
			 vs_table::ALIGN_CENTER | vs_table::FILL);

      l->location_changed.connect(sigc::mem_fun(*s.unsafe_get_ref(), &vs_scrollbar::set_slider));
      s->scrollbar_interaction.connect(sigc::mem_fun(*l.unsafe_get_ref(), &vs_text_layout::scroll));
    }
  else
    w=vs_text_layout::create(msg);

  return vs_dialog_yesno(w, yesslot, yeslabel, noslot, nolabel, st, deflt);
}

vs_widget_ref vs_dialog_fileview(const string &fn,
				 slot0arg okslot,
				 slotarg<sigc::slot1<void, vs_pager &> > search_slot,
				 slotarg<sigc::slot1<void, vs_pager &> > repeat_search_slot,
				 const style &st,
				 const char *encoding)
{
  vs_file_pager_ref p = vs_file_pager::create(fn, encoding);
  vs_scrollbar_ref scrollbar = vs_scrollbar::create(vs_scrollbar::VERTICAL, 0, 0);
  vs_table_ref t = vs_table::create();

  t->add_widget_opts(p, 0, 0, 1, 1,
		     vs_table::EXPAND | vs_table::FILL | vs_table::SHRINK | vs_table::ALIGN_CENTER,
		     vs_table::FILL | vs_table::SHRINK | vs_table::ALIGN_CENTER);
  t->add_widget_opts(scrollbar, 0, 1, 1, 1,
		     vs_table::ALIGN_CENTER,
		     vs_table::EXPAND | vs_table::FILL);

  //t->set_bg_style(style_attrs_off(A_REVERSE));

  p->line_changed.connect(sigc::mem_fun(*scrollbar.unsafe_get_ref(), &vs_scrollbar::set_slider));
  p->do_line_signal();
  scrollbar->scrollbar_interaction.connect(sigc::mem_fun(*p.unsafe_get_ref(), &vs_pager::scroll_page));

  if(search_slot)
    p->connect_key("Search", &global_bindings, sigc::bind(*search_slot, p.weak_ref()));

  if(repeat_search_slot)
    p->connect_key("ReSearch", &global_bindings, sigc::bind(*repeat_search_slot, p.weak_ref()));

  return vs_dialog_ok(t, okslot, transcode(_("Ok")), st);
}

vs_widget_ref vs_dialog_fileview(const string &fn,
				 slot0arg okslot,
				 slotarg<sigc::slot1<void, vs_pager &> > search_slot,
				 slotarg<sigc::slot1<void, vs_pager &> > repeat_search_slot,
				 const char *encoding)
{
  return vs_dialog_fileview(fn, okslot, search_slot, repeat_search_slot,
			    style_attrs_flip(A_REVERSE),
			    encoding);
}

static void do_dialog_string(vs_editline &eBare,
			     vscreen_widget &dialogBare,
			     slotarg<sigc::slot1<void, wstring> > thestrslot)
{
  vs_editline_ref e(&eBare);
  vs_widget_ref   dialog(&dialogBare);

  dialog->destroy();

  e->add_to_history(e->get_text());
  if(thestrslot)
    (*thestrslot)(e->get_text());
}

vs_widget_ref vs_dialog_string(const vs_widget_ref &msg,
			       wstring deflt,
			       slotarg<sigc::slot1<void, wstring> > slot,
			       slotarg<sigc::slot0<void> > cancel_slot,
			       slotarg<sigc::slot1<void, wstring> > changed_slot,
			       vs_editline::history_list *history,
			       const style &st)
{
  vs_table_ref t = vs_table::create();
  vs_editline_ref e = vs_editline::create(rootwin.getmaxx()-6, L"", deflt, history);
  vs_button_ref bok = vs_button::create(_("Ok"));
  vs_button_ref bcancel = vs_button::create(_("Cancel"));
  vs_frame_ref f = vs_frame::create(t);
  vs_center_ref c = vs_center::create(f);

  e->set_clear_on_first_edit(true);

  f->set_bg_style(st);

  t->add_widget(msg, 0, 0, 1, 2);
  t->add_widget(e, 1, 0, 1, 2);
  t->add_widget_opts(bok, 2, 0, 1, 1,
		     vs_table::ALIGN_CENTER|vs_table::SHRINK,
		     vs_table::ALIGN_CENTER);
  t->add_widget_opts(bcancel, 2, 1, 1, 1,
		     vs_table::ALIGN_CENTER|vs_table::SHRINK,
		     vs_table::ALIGN_CENTER);

  e->entered.connect(sigc::hide(bok->pressed.make_slot()));
  if(changed_slot)
    e->text_changed.connect(*changed_slot);

  t->connect_key_post("Cancel", &global_bindings,
		      bcancel->pressed.make_slot());

  bok->pressed.connect(sigc::bind(sigc::ptr_fun(do_dialog_string),
				  e.weak_ref(), c.weak_ref(), slot));

  bcancel->pressed.connect(sigc::bind(sigc::ptr_fun(&do_slot0_dialog),
				      c.weak_ref(), cancel_slot));

  return c;
}

vs_widget_ref vs_dialog_string(fragment *msg,
			       const wstring &deflt,
			       slotarg<sigc::slot1<void, wstring> > slot,
			       slotarg<sigc::slot0<void> > cancel_slot,
			       slotarg<sigc::slot1<void, wstring> > changed_slot,
			       vs_editline::history_list *history,
			       const style &st)
{
  return vs_dialog_string(vs_label::create(msg),
			  deflt,
			  slot,
			  cancel_slot,
			  changed_slot,
			  history,
			  st);
}

vs_widget_ref vs_dialog_string(const wstring &msg,
			       const wstring &deflt,
			       slotarg<sigc::slot1<void, wstring> > slot,
			       slotarg<sigc::slot0<void> > cancel_slot,
			       slotarg<sigc::slot1<void, wstring> > changed_slot,
			       vs_editline::history_list *history,
			       const style &st)
{
  return vs_dialog_string(vs_label::create(msg),
			  deflt,
			  slot,
			  cancel_slot,
			  changed_slot,
			  history,
			  st);
}

vs_widget_ref vs_dialog_string(const wstring &msg,
			       const wstring &deflt,
			       slotarg<sigc::slot1<void, wstring> > slot,
			       slotarg<sigc::slot0<void> > cancel_slot,
			       slotarg<sigc::slot1<void, wstring> > changed_slot,
			       vs_editline::history_list *history)
{
  return vs_dialog_string(msg,
			  deflt,
			  slot,
			  cancel_slot,
			  changed_slot,
			  history,
			  style_attrs_flip(A_REVERSE));
}
