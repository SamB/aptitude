// vs_treeitem.cc
//
//  Copyright 1999 Daniel Burrows
//
//  Implementation of stuff in vs_treeitem.h

#include "vs_treeitem.h"
#include "vs_tree.h"

using namespace std;

void vs_treeitem::highlighted(vs_tree *win)
{
}

void vs_treeitem::paint(vs_tree *win, int y, bool hierarchical,
			const wstring &str, int depth_shift)
{
  int width, height;
  int basex=hierarchical?depth_shift*get_depth():0;
  win->getmaxyx(height,width);

  win->move(y,0);
  int x=0;

  while(x<basex && x<width)
    {
      win->add_wch(L' ');
      x+=wcwidth(L' ');
    }

  if(x>=width)
    return;

  size_t i=0;
  while(i < str.size() && x < width)
    {
      wchar_t ch=str[i];

      win->add_wch(ch);
      x+=wcwidth(ch);
      ++i;
    }

  while(x<width)
    {
      win->add_wch(L' ');
      x+=wcwidth(L' ');
    }
}      
