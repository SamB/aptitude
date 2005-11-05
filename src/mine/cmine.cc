// cmine.cc
//
//  Copyright 2000 Daniel Burrows

#include "cmine.h"

#include <aptitude.h>
#include <ui.h>

#include <sigc++/adaptors/bind.h>
#include <sigc++/functors/mem_fun.h>

#include <vscreen/config/keybindings.h>
#include <vscreen/config/colors.h>

#include <vscreen/vscreen_widget.h>
#include <vscreen/vscreen.h>

#include <vscreen/transcode.h>
#include <vscreen/vs_button.h>
#include <vscreen/vs_center.h>
#include <vscreen/vs_editline.h>
#include <vscreen/vs_frame.h>
#include <vscreen/vs_label.h>
#include <vscreen/vs_radiogroup.h>
#include <vscreen/vs_table.h>
#include <vscreen/vs_togglebutton.h>
#include <vscreen/vs_util.h>

#include <string>
#include <fstream>

#ifndef DONT_USE_FANCYBOXES
// Some systems (*cough* Solaris xterms *cough*) don't like the fancy ASCII
// graphics provided by libcurses
#define MINE_ULCORNER WACS_ULCORNER
#define MINE_URCORNER WACS_URCORNER
#define MINE_HLINE WACS_HLINE
#define MINE_VLINE WACS_VLINE
#define MINE_LLCORNER WACS_LLCORNER
#define MINE_LRCORNER WACS_LRCORNER
#else
#define MINE_ULCORNER L'+'
#define MINE_URCORNER L'+'
#define MINE_HLINE L'-'
#define MINE_VLINE L'|'
#define MINE_LLCORNER L'+'
#define MINE_LRCORNER L'+'
#endif

using namespace std;

keybindings *cmine::bindings;

vs_editline::history_list cmine::load_history, cmine::save_history;

int cmine::width_request()
{
  if(board)
    return board->get_width();
  else
    return 0;
}

int cmine::height_request(int w)
{
  if(board)
    return 1+board->get_height();
  else
    return 1;
}

class cmine::update_header_event : public vscreen_event
{
  ref_ptr<cmine> cm;

public:
  update_header_event(cmine *_cm)
    : cm(_cm)
  {
  }

  void dispatch()
  {
    cm->update_header();
  }
};

void cmine::update_header()
{
  timeout_num = vscreen_addtimeout(new update_header_event(this), 500);

  vscreen_update();
};

void cmine::paint_header(const style &st)
  // Shows the header with its extra info
{
  vs_widget_ref tmpref(this);

  if(board)
    {
      int width,height;
      getmaxyx(height,width);

      wstring header=transcode(_("Minesweeper"));
      wchar_t buf[200];

      if(board->get_state()==mine_board::playing)
	swprintf(buf,
		 sizeof(buf),
		 transcode(_("%i/%i mines  %d %s")).c_str(),
		 board->get_nummines()-board->get_numflags(),
		 board->get_nummines(),
		 (int) board->get_duration(),
		 board->get_duration()==1?_("second"):_("seconds"));
      else
	swprintf(buf,
		 sizeof(buf),
		 transcode(_("    %s in %d %s")).c_str(),
		 board->get_state()==mine_board::won?_("Won"):_("Lost"),
		 (int) board->get_duration(),
		 board->get_duration()==1?_("second"):_("seconds"));

      int headerw=wcswidth(header.c_str(), header.size());
      int bufw=wcswidth(buf, wcslen(buf));

      while(headerw+bufw < width)
	{
	  header+=L' ';
	  headerw+=wcwidth(L' ');
	}

      unsigned int loc=0;
      while(headerw < width)
	{
	  eassert(buf[loc]!=0);

	  wchar_t wch=buf[loc];
	  header+=buf;
	  headerw+=wcwidth(wch);
	  ++loc;
	}

      display_header(header, st+get_style("Header"));
    }
  else
    display_header(transcode(_("Minesweeper")),
		   st+get_style("Header"));
}

void cmine::do_load_game(wstring ws)
{
  vs_widget_ref tmpref(this);

  string s=transcode(ws);

  if(s!="")
    {
      ifstream in(s.c_str());
      if(!in)
	{
	  char buf[512];

	  snprintf(buf, 512, _("Could not open file \"%s\""), s.c_str());

	  popup_widget(vs_dialog_ok(transcode(buf), NULL,
				    get_style("Error")));
	}
      else
	{
	  mine_board *brd=new mine_board;
	  // The advantage of creating a new board instead of loading into
	  // the current one is that we don't lose the current game if the
	  // user tries to load a game from, say, /etc/passwd.
	  if(!brd->load(in))
	    {
	      char buf[512];

	      snprintf(buf, 512, _("Could not load game from %s"),
		       s.c_str());

	      popup_widget(vs_dialog_ok(transcode(buf), NULL,
					get_style("Error")));
	      delete brd;
	    }
	  else
	    {
	      set_board(brd);
	      vscreen_update();
	    }
	}
    }
}

void cmine::do_save_game(wstring ws)
{
  vs_widget_ref tmpref(this);

  string s=transcode(ws);

  if(s!="")
    {
      ofstream out(s.c_str());
      if(!out)
	{
	  char buf[512];

	  snprintf(buf, 512, _("Could not open file \"%s\""), s.c_str());

	  popup_widget(vs_dialog_ok(transcode(buf), NULL,
				    get_style("Error")));
	}
      else
	{
	  board->save(out);
	  vscreen_update();
	}
    }
}

void cmine::do_start_custom_game(vscreen_widget &w_bare,
				 vs_editline &heightedit_bare,
				 vs_editline &widthedit_bare,
				 vs_editline &minesedit_bare)
{
  vs_widget_ref tmpref(this);

  // Be ultra-safe and hold strong references to everything.
  vs_widget_ref w(&w_bare);
  vs_editline_ref heightedit(&heightedit_bare),
    widthedit(&widthedit_bare), minesedit(&minesedit_bare);

  wstring s=heightedit->get_text();

  wchar_t *end=const_cast<wchar_t *>(s.c_str());

  long height=wcstol(s.c_str(), &end, 0);

  if(s.c_str()[0]=='\0' || *end!='\0' || height<1)
    {
      // FIXME: should say "positive integer" but the translators will
      // lynch me ;-)
      //
      //
      // That's ok, they'll do that anyway in a couple releases ;-)
      popup_widget(vs_dialog_ok(transcode(_("The board height must be a positive integer")),
				NULL,
				get_style("Error")));
      return;
    }

  s=widthedit->get_text();
  end=const_cast<wchar_t *>(s.c_str());
  long width=wcstol(s.c_str(), &end, 0);

  if(s.c_str()[0]=='\0' || *end!='\0' || width<1)
    {
      popup_widget(vs_dialog_ok(transcode(_("The board width must be a positive integer")),
				NULL,
				get_style("Error")));
      return;
    }

  s=minesedit->get_text();
  end=const_cast<wchar_t *>(s.c_str());
  long mines=wcstol(s.c_str(), &end, 0);

  if(s.c_str()[0]=='\0' || *end!='\0' || mines<1)
    {
      popup_widget(vs_dialog_ok(transcode(_("Invalid mine count; please enter a positive integer")),
				NULL,
				get_style("Error")));
      return;
    }

  w->destroy();

  set_board(new mine_board(width, height, mines));
}

void cmine::do_custom_game()
{
  vs_widget_ref tmpref(this);

  vs_center_ref center=vs_center::create();
  center->set_bg_style(style_attrs_flip(A_REVERSE));

  vs_table_ref table=vs_table::create();

  vs_label_ref overalllabel=vs_label::create(_("Setup custom game"));

  vs_label_ref heightlabel=vs_label::create(_("Height of board: "));
  vs_editline_ref heightedit=vs_editline::create(L"");

  vs_label_ref widthlabel=vs_label::create(_("Width of board: "));
  vs_editline_ref widthedit=vs_editline::create(L"");

  vs_label_ref mineslabel=vs_label::create(_("Number of mines: "));
  vs_editline_ref minesedit=vs_editline::create(L"");

  vs_button_ref okbutton=vs_button::create(_("Ok"));
  vs_button_ref cancelbutton=vs_button::create(_("Cancel"));

  table->connect_key("Confirm", &global_bindings, okbutton->pressed.make_slot());

  okbutton->pressed.connect(sigc::bind(sigc::mem_fun(*this, &cmine::do_start_custom_game),
				       center.weak_ref(),
				       heightedit.weak_ref(),
				       widthedit.weak_ref(),
				       minesedit.weak_ref()));
  cancelbutton->pressed.connect(sigc::mem_fun(*center.unsafe_get_ref(), &vscreen_widget::destroy));

  table->connect_key("Cancel", &global_bindings, cancelbutton->pressed.make_slot());

  vs_center_ref cyes=vs_center::create(okbutton);
  vs_center_ref cno=vs_center::create(cancelbutton);

  table->add_widget(overalllabel, 0, 0, 1, 2, true, false);

  table->add_widget(heightlabel, 1, 0, 1, 1, true, false);
  table->add_widget(heightedit, 1, 1, 1, 1, true, false);

  table->add_widget(widthlabel, 2, 0, 1, 1, true, false);
  table->add_widget(widthedit, 2, 1, 1, 1, true, false);

  table->add_widget(mineslabel, 3, 0, 1, 1, true, false);
  table->add_widget(minesedit, 3, 1, 1, 1, true, false);

  table->add_widget(cyes, 4, 0, 1, 1, false, false);
  table->add_widget(cno, 4, 1, 1, 1, false, false);

  overalllabel->show();
  heightlabel->show();
  heightedit->show();
  widthlabel->show();
  widthedit->show();
  mineslabel->show();
  minesedit->show();
  okbutton->show();
  cancelbutton->show();
  cyes->show();
  cno->show();

  vs_frame_ref frame=vs_frame::create(table);
  center->add_widget(frame);

  popup_widget(center);
}

void cmine::do_new_game()
{
  vs_widget_ref tmpref(this);

  vs_center_ref center=vs_center::create();
  center->set_bg_style(style_attrs_flip(A_REVERSE));

  vs_table_ref table=vs_table::create();

  vs_label_ref overalllabel=vs_label::create(_("Choose difficulty level"));

  vs_radiobutton_ref easybutton=vs_radiobutton::create(_("Easy"), true);
  vs_radiobutton_ref mediumbutton=vs_radiobutton::create(_("Medium"), false);
  vs_radiobutton_ref hardbutton=vs_radiobutton::create(_("Hard"), false);
  vs_radiobutton_ref custombutton=vs_radiobutton::create(_("Custom"), false);

  vs_button_ref okbutton=vs_button::create(_("Ok"));
  vs_button_ref cancelbutton=vs_button::create(_("Cancel"));

  table->connect_key_post("Confirm", &global_bindings, okbutton->pressed.make_slot());

  easybutton->connect_key("Confirm", &global_bindings, okbutton->pressed.make_slot());
  mediumbutton->connect_key("Confirm", &global_bindings, okbutton->pressed.make_slot());
  hardbutton->connect_key("Confirm", &global_bindings, okbutton->pressed.make_slot());
  custombutton->connect_key("Confirm", &global_bindings, okbutton->pressed.make_slot());

  vs_radiogroup *grp=new vs_radiogroup;
  grp->add_button(easybutton, 0);
  grp->add_button(mediumbutton, 1);
  grp->add_button(hardbutton, 2);
  grp->add_button(custombutton, 3);

  okbutton->pressed.connect(sigc::bind(sigc::mem_fun(*this, &cmine::do_continue_new_game),
				       true,
				       center.weak_ref(),
				       grp));
  cancelbutton->pressed.connect(sigc::bind(sigc::mem_fun(*this, &cmine::do_continue_new_game),
					   false,
					   center.weak_ref(),
					   grp));

  vs_center_ref cok=vs_center::create(okbutton);
  vs_center_ref ccancel=vs_center::create(cancelbutton);

  table->add_widget(overalllabel, 0, 0, 1, 2, true, false);
  table->add_widget(easybutton,   1, 0, 1, 2, true, false);
  table->add_widget(mediumbutton, 3, 0, 1, 2, true, false);
  table->add_widget(hardbutton,   4, 0, 1, 2, true, false);
  table->add_widget(custombutton, 5, 0, 1, 2, true, false);
  table->add_widget(cok,          6, 0, 1, 1, false, false);
  table->add_widget(ccancel,      6, 1, 1, 1, false, false);

  vs_frame_ref frame=vs_frame::create(table);
  center->add_widget(frame);

  popup_widget(center);
}

void cmine::do_continue_new_game(bool start,
				 vscreen_widget &w_bare,
				 vs_radiogroup *grp)
{
  vs_widget_ref tmpref(this);

  vs_widget_ref w(&w_bare);

  if(start)
    switch(grp->get_selected())
      {
      case 0:
	set_board(easy_game());
	break;
      case 1:
	set_board(intermediate_game());
	break;
      case 2:
	set_board(hard_game());
	break;
      case 3:
	do_custom_game();
	break;
      default:
	popup_widget(vs_dialog_ok(transcode("Internal error: execution reached an impossible point"),
				  NULL,
				  get_style("Error")));
	break;
      }

  delete grp;
  w->destroy();
};

cmine::cmine():board(NULL), prevwidth(0), prevheight(0)
{
  set_board(easy_game());
  vscreen_addtimeout(new update_header_event(this), 500);
  //set_status(_("n - New Game  Cursor keys - move cursor  f - flag  enter - check  ? - help"));
}

void cmine::checkend()
  // Prints out silly messages when the player wins or loses
{
  vs_widget_ref tmpref(this);

  if(board->get_state()==mine_board::won)
    // "You hold up the Amulet of Yendor.  An invisible choir sings..."
    popup_widget(vs_dialog_ok(transcode(_("You have won."))));
  else if(board->get_state()==mine_board::lost)
    {
      popup_widget(vs_dialog_ok(transcode(_("You lose!"))));
#if 0
      // (messages in reverse order because the minibuf is a stack by default..
      // I could use the special feature of sticking them at the end, but I
      // want them to override whatever's there (probably nothing :) )
      add_status_widget(new vs_label(_("You die...  --More--"),
				     retr_status_color()));
      switch(rand()/(RAND_MAX/8))
	{
	case 0:
	case 1:
	case 2:
	case 3:
	  if(rand()<(RAND_MAX/2))
	    {
	      if(rand()<(RAND_MAX/3))
		{
		  if(rand()<(RAND_MAX/2))
		    add_status_widget(vs_label::create(_("The spikes were poisoned!  The poison was deadly..  --More--"),
						       retr_status_color()));

		  add_status_widget(vs_label::create(_("You land on a set of sharp iron spikes!  --More--"),
						     retr_status_color()));
		}
	      add_status_widget(vs_label::create(_("You fall into a pit!  --More--"),
						 retr_status_color()));
	    }
	  add_status_widget(vs_label::create(_("KABOOM!  You step on a land mine.  --More--"),
					     retr_status_color()));
	  break;
	case 4:
	  if(rand()<RAND_MAX/2)
	    add_status_widget(vs_label::create(_("The dart was poisoned!  The poison was deadly...  --More--"),
					       retr_status_color()));
	  add_status_widget(vs_label::create(_("A little dart shoots out at you!  You are hit by a little dart!  --More--"),
					     retr_status_color()));
	  break;
	case 5:
	  add_status_widget(vs_label::create(_("You turn to stone... --More--"),
					     retr_status_color()));
	  add_status_widget(vs_label::create(_("Touching the cockatrice corpse was a fatal mistake.  --More--"),
					     retr_status_color()));
	  add_status_widget(vs_label::create(_("You feel here a cockatrice corpse.  --More--"),
					     retr_status_color()));
	  break;
	case 6:
	  add_status_widget(vs_label::create(_("Click!  You trigger a rolling boulder trap!  You are hit by a boulder! --More--"),
					     retr_status_color()));
	  break;
	case 7:
	  if(rand()<(RAND_MAX/2))
	    {
	      string type;
	      switch(rand()/(RAND_MAX/8))
		{
		case 0:
		  type=_("sleep");
		  break;
		case 1:
		  type=_("striking");
		  break;
		case 2:
		  type=_("death");
		  break;
		case 3:
		  type=_("polymorph");
		  break;
		case 4:
		  type=_("magic missile");
		  break;
		case 5:
		  type=_("secret door detection");
		  break;
		case 6:
		  type=_("invisibility");
		  break;
		case 7:
		  type=_("cold");
		  break;
		}

	      char buf[512];

	      snprintf(buf, 512, _("Your wand of %s breaks apart and explodes!  --More--"));

	      add_status_widget(vs_label::create(buf,
						 retr_status_color()));
	    }

	  add_status_widget(vs_label::create(_("You are jolted by a surge of electricity!  --More--"),
					     retr_status_color()));
	  break;
	}
#endif
    }
}

void cmine::set_board(mine_board *_board)
{
  int width, height;
  getmaxyx(height, width);

  delete board;
  board=_board;

  curx=_board->get_width()/2;
  cury=_board->get_height()/2;

  basex=(width-_board->get_width()-2)/2;
  basey=(height-_board->get_height()-4)/2;

  vscreen_update();
}

bool cmine::handle_key(const key &k)
{
  vs_widget_ref tmpref(this);

  int width,height;
  getmaxyx(height, width);

  if(bindings->key_matches(k, "MineUncoverSweepSquare"))
    {
      if(board->get_state()==mine_board::playing)
	{
	  if(!board->get_square(curx, cury).uncovered)
	    board->uncover(curx, cury);
	  else
	    board->sweep(curx, cury);

	  checkend();
	}
      vscreen_update();
    }
  else if(bindings->key_matches(k, "MineUncoverSquare"))
    {
      board->uncover(curx, cury);
      vscreen_update();
    }
  else if(bindings->key_matches(k, "MineSweepSquare"))
    {
      board->sweep(curx, cury);
      vscreen_update();
    }
  else if(bindings->key_matches(k, "MineFlagSquare"))
    {
      board->toggle_flag(curx, cury);
      // FIXME: handle errors?
      vscreen_update();
    }
  else if(bindings->key_matches(k, "Up"))
    {
      if(cury>0)
	{
	  cury--;
	  while(basey+cury+1<1)
	    basey++;
	  vscreen_update();
	}
    }
  else if(bindings->key_matches(k, "Down"))
    {
      if(cury<board->get_height()-1)
	{
	  cury++;
	  while(basey+cury+1>=height-3)
	    basey--;
	  vscreen_update();
	}
    }
  else if(bindings->key_matches(k, "Left"))
    {
      if(curx>0)
	{
	  curx--;
	  while(basex+curx+1<1)
	    basex++;
	  vscreen_update();
	}
    }
  else if(bindings->key_matches(k, "Right"))
    {
      if(curx<board->get_width()-1)
	{
	  curx++;
	  while(basex+curx+1>=width-1)
	    basex--;
	  vscreen_update();
	}
    }
  else if(bindings->key_matches(k, "MineNewGame"))
    do_new_game();
  else if(bindings->key_matches(k, "MineLoadGame"))
    prompt_string(_("Enter the filename to load: "),
		  "",
		  arg(sigc::mem_fun(*this, &cmine::do_load_game)),
		  NULL,
		  NULL,
		  &load_history);
  else if(bindings->key_matches(k, "MineSaveGame"))
    prompt_string(_("Enter the filename to save: "),
		  "",
		  arg(sigc::mem_fun(*this, &cmine::do_save_game)),
		  NULL,
		  NULL,
		  &save_history);
  else if(bindings->key_matches(k, "Help"))
    {
      char buf[512];

      snprintf(buf, 512, HELPDIR "/%s", _("mine-help.txt"));

      const char *encoding=P_("Encoding of mine-help.txt|UTF-8");

      if(access(buf, R_OK)!=0)
	{
	  strncpy(buf, HELPDIR "/mine-help.txt", 512);
	  encoding="UTF-8";
	}

      vs_widget_ref w=vs_dialog_fileview(buf, NULL, NULL, NULL, encoding);
      w->show_all();

      popup_widget(w);
    }
  else
    return false;

  return true;
}

void cmine::paint_square(int x, int y, const style &st)
  // Displays a single square.
{
  int width,height;
  getmaxyx(height, width);

  eassert(x>=0 && x<board->get_width());
  eassert(y>=0 && y<board->get_height());

  int screenx=basex+1+x,screeny=(basey+1)+1+y;

  if(screenx>=0 && screenx<width && screeny>=0 && screeny<height-1)
    {
      wchar_t ch;
      style cur_st;
      const mine_board::board_entry &entry=board->get_square(x, y);
      // We want to handle the case of 'game-over' differently from
      // the case of 'still playing' -- when the game is over, all
      // the mines should be revealed.
      if(board->get_state()==mine_board::playing)
	{
	  if(!entry.uncovered)
	    {
	      if(entry.flagged)
		{
		  ch=L'F';
		  cur_st=get_style("MineFlag");
		}
	      else
		{
		  ch=L' ';
		  cur_st=get_style("DefaultWidgetBackground");
		}
	    }
	  else if(entry.contains_mine)
	    {
	      ch=L'^';
	      cur_st=get_style("MineBombColor");
	    }
	  else if(entry.adjacent_mines==0)
	    ch=L'.';
	  else
	    {
	      ch=(L'0'+entry.adjacent_mines);
	      string stname("MineNumber");
	      stname+=(char) '0'+entry.adjacent_mines;
	      cur_st=get_style(string("MineNumber")+char('0'+entry.adjacent_mines));
	    }
	}
      else
	{
	  if(entry.contains_mine)
	    {
	      if(board->get_state()==mine_board::lost &&
		 x==board->get_minex() && y==board->get_miney())
		{
		  ch=L'*';
		  cur_st=get_style("MineDetonated");
		}
	      else
		{
		  ch=L'^';
		  cur_st=get_style("MineBomb");
		}
	    }
	  else if(entry.uncovered || true)
	    {
	      if(entry.adjacent_mines==0)
		ch=L'.';
	      else
		{
		  ch=(L'0'+entry.adjacent_mines);
		  string stname("MineNumber");
		  stname+=(char) '0'+entry.adjacent_mines;
		  cur_st=get_style(string("MineNumber")+char('0'+entry.adjacent_mines));
		}
	    }
	  else
	    ch=L' ';
	}
      if(board->get_state()==mine_board::playing && x==curx && y==cury)
	cur_st+=style_attrs_flip(A_REVERSE);

      apply_style(cur_st);
      mvadd_wch(screeny, screenx, ch);
    }
}

void cmine::paint(const style &st)
{
  if(get_win())
    {
      int width,height;
      getmaxyx(height, width);

      if(height!=prevheight || width!=prevwidth)
	// If the window size has changed (or we just got a window for
	// the first time) we need to reset the boundaries.  Really,
	// this ought to be done by a callback (aka virtual function)
	// that gets called for the first assignment of a cwindow to
	// the vscreen or when the screen resizes.
	//
	// Probably should be connected to layout_me() if anyone
	// cares.
	{
	  prevwidth=width;
	  prevheight=height;

	  curx=board->get_width()/2;
	  cury=board->get_height()/2;

	  basex=(width-board->get_width()-2)/2;
	  basey=(height-board->get_height()-4)/2;
	}

      paint_header(st);

      if(board)
	{
	  apply_style(st+get_style("MineBorder"));

	  int right=basex+board->get_width()+1, down=basey+1+board->get_height()+1;
	  // x and y coordinates, respectively, of the right and lower
	  // board edges.
	  int line_start_x=basex>0?basex+1:1, line_start_y=basey>=0?basey+2:1;
	  // The starting coordinates of the horizontal and vertical
	  // lines, respectively.

	  int horiz_line_width, vert_line_height;
	  if(basex+board->get_width()>=width)
	    horiz_line_width=width-line_start_x;
	  else
	    horiz_line_width=basex+1+board->get_width()-line_start_x;

	  if(basey+board->get_height()>=height-2)
	    vert_line_height=height-1-line_start_y;
	  else
	    vert_line_height=basey+2+board->get_height()-line_start_y;
	  // Calculate the widths of the sides of the board.  This
	  // might look a little like black voodoo magic.  It is.  You
	  // wouldn't believe how many chickens I had to..er,
	  // nevermind :)
	  //
	  // Probably this could be cleaned up; there seems to be a
	  // lot of confusion resulting from the value of basey.  I
	  // think now that basey+y should go from 1 to height instead
	  // of 0 to height-1 -- it might make things more confusing
	  // elsewhere but would simplify the drawing routines
	  // tremendously.

	  // The reason that we only use 'basey+1' below is that we
	  // have to draw the board inside the 'main' window provided
	  // by the minibuf_win.

	  int minx=(basex<-1?-basex:0),miny=(basey<-1?-1-basey:0);
	  // The /board coordinates/ of the minimal x and y values visible
	  int maxx,maxy;
	  if(basex+1+board->get_width()>width)
	    maxx=minx+(basex==0?width-1:width);
	  // The visible area is effectively one square thinner when basex==0.
	  else
	    maxx=board->get_width();

	  if(basey+1+board->get_height()>height-2)
	    maxy=miny+(basey==0?height-3:height-2);
	  else
	    maxy=board->get_height();
	  // A hairy expression for /one plus/ the maximum visible coordinates
	  // (the one plus is to make the loop below slightly simpler)

	  /////////////////////////////////////////////////
	  // Start drawing; first the border:
	  if(basey>=0)
	    {
	      if(basex>=0)
		mvadd_wch(basey+1, basex, MINE_ULCORNER);

	      // NB: Assumes MINE_HLINE has a width of 1
	      for(int x=line_start_x;
		  x-line_start_x < horiz_line_width; ++x)
		mvadd_wch(basey+1, x, MINE_HLINE);
	    }

	  if(basex>=0)
	    {
	      for(int y=line_start_y;
		  y-line_start_y < vert_line_height; ++y)
		mvadd_wch(y, basex, MINE_VLINE);
	    }

	  if(right<width)
	    {
	      if(basey>=0)
		mvadd_wch(basey+1, right, MINE_URCORNER);

	      for(int y=line_start_y;
		  y-line_start_y < vert_line_height; ++y)
		mvadd_wch(y, right, MINE_VLINE);

	      if(down<height-1)
		mvadd_wch(down, right, MINE_LRCORNER);
	    }

	  if(down<height-1)
	    {
	      if(basex>=0)
		mvadd_wch(down, basex, MINE_LLCORNER);

	      for(int x=line_start_x;
		  x-line_start_x < horiz_line_width; ++x)
		mvadd_wch(down, x, MINE_HLINE);
	    }

	  // Now the squares:
	  for(int y=miny; y<maxy; y++)
	    for(int x=minx; x<maxx; x++)
	      paint_square(x, y, st);
	}
    }
}

void cmine::init_bindings()
{
  srand(time(0));

  set_style("MineFlag", style_fg(COLOR_RED)+style_attrs_on(A_BOLD));
  set_style("MineBomb", style_fg(COLOR_RED)+style_attrs_on(A_BOLD));
  set_style("MineDetonated", style_fg(COLOR_RED)+style_attrs_on(A_BOLD));
  set_style("MineNumber1", style_fg(COLOR_WHITE));
  set_style("MineNumber2", style_fg(COLOR_GREEN));
  set_style("MineNumber3", style_fg(COLOR_CYAN));
  set_style("MineNumber4", style_fg(COLOR_MAGENTA)+style_attrs_on(A_BOLD));
  set_style("MineNumber5", style_fg(COLOR_RED)+style_attrs_on(A_BOLD));
  set_style("MineNumber6", style_attrs_on(A_BOLD)+style_fg(COLOR_CYAN));
  set_style("MineNumber7", style_attrs_on(A_BOLD)+style_fg(COLOR_GREEN));
  set_style("MineNumber8", style_attrs_on(A_BOLD));
  set_style("MineBorder", style_attrs_on(A_BOLD));

  global_bindings.set("MineUncoverSweepSquare", key(KEY_ENTER, true));
  global_bindings.set("MineFlagSquare", key(L'f', false));
  global_bindings.set("MineNewGame", key(L'n', false));
  global_bindings.set("MineSaveGame", key(L'S', false));
  global_bindings.set("MineLoadGame", key(L'L', false));

  bindings=new keybindings(&global_bindings);
}
