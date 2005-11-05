// columnify.cc
//
//  Copyright 2000, 2005 Daniel Burrows
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

#include "columnify.h"

#include "transcode.h"

using namespace std;

column_disposition::column_disposition(const std::string &_text,
				       int _minx,
				       const char *encoding)
  :text(transcode(_text, encoding)), minx(_minx)
{
}

wstring columnify(const layout &format, int width)
{
  wstring rval;

  layout final_info;

  unsigned int colnum=0;
  for(layout::const_iterator i=format.begin();
      i!=format.end();
      ++i, ++colnum)
    {
      eassert(colnum<format.size());

      final_info.push_back(column(i->info, i->width, i->expand, i->shrink));
    }

  // Reconcile the widths.
  int totalwidth=0;
  for(layout::iterator i=final_info.begin();
      i!=final_info.end(); ++i)
    totalwidth+=i->width;

  if(totalwidth<width)
    // Yaaay, we had too much space :)
    // Divvy it up.
    {
      int excess=width-totalwidth;
      int nexpandable=0;
      int startloc=0;

      // Figure out how to divide the wealth:
      for(layout::iterator i=final_info.begin();
	  i!=final_info.end(); ++i)
	if(i->expand)
	  ++nexpandable;

      // Now expand the columns
      for(layout::iterator i=final_info.begin();
	  i!=final_info.end() && nexpandable>0; ++i)
	{
	  if(i->expand)
	    {
	      int amt=excess/nexpandable;

	      i->width+=amt;
	      excess-=amt;
	      --nexpandable;
	    }

	  startloc+=i->width;

	  layout::iterator j=i;
	  ++j;

	  // FIXME: HACK.
	  if(j!=final_info.end() && startloc<j->info.minx)
	    {
	      int amt2=j->info.minx-startloc;
	      if(amt2>excess)
		amt2=excess;

	      i->width+=amt2;
	      excess-=amt2;
	    }
	}
    }
  else if(totalwidth>width)
    // Do everything again if we were allocated less space than requested. :(
    {
      int nshrinkable=0;
      int shortfall=totalwidth-width;
      int startloc=0;

      for(layout::iterator i=final_info.begin();
	  i!=final_info.end(); ++i)
	if(i->shrink)
	  ++nshrinkable;

      // Shrink the columns
      for(layout::iterator i=final_info.begin();
	  i!=final_info.end() && nshrinkable>0; ++i)
	{
	  if(i->shrink)
	    {
	      int amt=shortfall/nshrinkable;
	      if(amt>i->width)
		amt=i->width;

	      i->width-=amt;
	      shortfall-=amt;
	      --nshrinkable;
	    }

	  startloc+=i->width;
	}

      if(shortfall>0) // Egad!  Truncate things at random, R->L..
	for(layout::reverse_iterator i=final_info.rbegin();
	    i!=final_info.rend() && shortfall>0; i++)
	  if(i->width<=shortfall)
	    {
	      shortfall-=i->width;
	      i->width=0;
	    }
	  else
	    {
	      i->width-=shortfall;
	      shortfall=0;
	    }
    }

  int curwidth=0, nextwidth=0;
  const int spacewidth=wcwidth(L' ');
  for(layout::iterator i=final_info.begin();
      i!=final_info.end(); ++i)
    {
      wstring::size_type amt=0;
      nextwidth+=i->width;
      for( ; curwidth<nextwidth &&
	     amt<i->info.text.size(); ++amt)
	{
	  wchar_t wch=i->info.text[amt];
	  // Watch out for wide characters overrunning the column
	  // boundary!
	  if(curwidth+wcwidth(wch)<=nextwidth)
	    {
	      rval+=wch;
	      curwidth+=wcwidth(wch);
	    }
	  else
	    break;
	}
      // Is it sane for spacewidth to ever differ from unity?
      while(curwidth+spacewidth<=nextwidth)
	{
	  rval+=L' ';
	  curwidth+=spacewidth;
	}
    }

  while(curwidth<width)
    {
      rval+=L' ';
      curwidth+=spacewidth;
    }

  return rval;
}
