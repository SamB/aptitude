// column_definition.cc
//
//  Copyright 2000,2001, 2005 Daniel Burrows
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

#include "column_definition.h"

#include <apt-pkg/error.h>
#include <apt-pkg/strutl.h>

// For _()
#include <aptitude.h>

using namespace std;

column_parameters::~column_parameters()
{
}

int empty_column_parameters::param_count()
{
  return 0;
}

wstring empty_column_parameters::get_param(int n)
{
  abort();
}

column_generator::~column_generator()
{
}

column_definition_list *parse_columns(wstring config,
				      column_parser_func parser,
				      column_type_defaults *defaults)
{
  column_definition_list *rval=new column_definition_list;

  wstring::size_type start=0;
  wstring::size_type firstbreak;
  do
    {
      firstbreak=config.find_first_of(L"%#", start);
      if(firstbreak!=start)
	{
	  wstring literal=wstring(config,
				  start,
				  firstbreak==config.npos?config.npos:firstbreak-start);

	  // Grok backslashes, sort of
	  wstring de_backslashified;

	  for(wstring::size_type i=0; i<literal.size(); ++i)
	    {
	      if(literal[i]!=L'\\' || i==literal.size()-1)
		de_backslashified+=literal[i];
	      else switch(literal[++i])
		{
		case L'n':
		  de_backslashified+=L'\n';
		  break;
		default:
		  de_backslashified+=literal[i];
		}
	    }

	  rval->push_back(column_definition(de_backslashified, false, false));
	}

      if(firstbreak!=config.npos)
	{
	  if(config[firstbreak]==L'#')
	    {
	      if(rval->empty())
		rval->push_back(column_definition(L"", true, false));
	      else
		rval->back().expand=true;

	      firstbreak++;
	    }
	  else if(config[firstbreak]==L'%')
	    {
	      int width=-1;
	      bool dynamic_width=false;

	      firstbreak++;

	      if(firstbreak==config.size())
		{
		  _error->Error(_("Formatting marker with missing format code"));
		  delete rval;
		  return NULL;
		}

	      if(iswdigit(config[firstbreak]))
		{
		  unsigned long val;

		  wstring tocvt=L"";
		  while(firstbreak < config.size() && iswdigit(config[firstbreak]))
		    {
		      tocvt+=config[firstbreak];
		      ++firstbreak;
		    }

		  if(firstbreak==config.size())
		    {
		      _error->Error(_("Formatting marker with missing format code"));
		      delete rval;
		      return NULL;
		    }

		  wchar_t *endptr;
		  val=wcstol(tocvt.c_str(), &endptr, 0);
		  if(*endptr!=L'\0')
		    {
		      _error->Error(_("Bad number in format string: '%ls'"), tocvt.c_str());
		      delete rval;
		      return NULL;
		    }

		  width=val;
		}

	      if(config[firstbreak]==L'?')
		{
		  ++firstbreak;
		  dynamic_width=true;
		}

	      if(config[firstbreak]==L'%')
		{
		  rval->push_back(column_definition(L"%",
						    false,
						    false));
		  ++firstbreak;
		}
	      else if(config[firstbreak]==L'#')
		// Parameter substitution is introduced by %[nn]#:
		{
		  ++firstbreak;

		  if(!iswdigit(config[firstbreak]))
		    {
		      _error->Error(_("Missing parameter number in format string"));
		      delete rval;
		      return NULL;
		    }

		  unsigned long val;

		  wstring tocvt=L"";
		  while(firstbreak<config.size() && iswdigit(config[firstbreak]))
		    {
		      tocvt+=config[firstbreak];
		      ++firstbreak;
		    }

		  wchar_t *endptr;
		  val=wcstol(tocvt.c_str(), &endptr, 0);
		  if(*endptr!=L'\0')
		    {
		      _error->Error(_("Bad number in format string: '%ls'"), tocvt.c_str());
		      delete rval;
		      return NULL;
		    }

		  if(val<1)
		    {
		      _error->Error(_("Parameter numbers must be 1 or greater, not %ld"), val);
		      delete rval;
		      return NULL;
		    }

		  // Default for parameters is to be dynamic.
		  if(width == -1)
		    {
		      width=0;
		      dynamic_width=true;
		    }

		  rval->push_back(column_definition(column_definition::COLUMN_PARAM,
						    val-1,
						    width,
						    false,
						    false,
						    dynamic_width));
		}
	      else
		{
		  int itemtype=parser(config[firstbreak]);
		  ++firstbreak;

		  if(itemtype==-1)
		    {
		      _error->Error(_("Unknown formatting code '%lc'"),
				    config[firstbreak]);
		      delete rval;
		      return NULL;
		    }

		  if(width==-1)
		    width=defaults[itemtype].width;

		  rval->push_back(column_definition(column_definition::COLUMN_GENERATED,
						    itemtype,
						    width,
						    defaults[itemtype].expand,
						    defaults[itemtype].shrink,
						    dynamic_width));
		}
	    }
	  start=firstbreak;
	}
    } while(firstbreak!=config.npos);
  return rval;
}

wstring column_generator::layout_columns(unsigned int width,
					 column_parameters &p)
{
  layout l;

  for(column_definition_list::iterator j=columns.begin();
      j!=columns.end();
      j++)
    {
      column_disposition disp(L"", 0);
      unsigned int width;

      if(j->type == column_definition::COLUMN_LITERAL)
	{
	  disp = column_disposition(j->arg, 0);
	  width = wcswidth(j->arg.c_str(), j->arg.size());
	}
      else
	{
	  eassert(j->type == column_definition::COLUMN_GENERATED ||
		 j->type == column_definition::COLUMN_PARAM);

	  if(j->type == column_definition::COLUMN_GENERATED)
	    disp = setup_column(j->ival);
	  else
	    {
	      if(p.param_count() <= j->ival)
		disp = column_disposition(_("Bad format parameter"), 0);
	      else
		disp = column_disposition(p.get_param(j->ival), 0);
	    }

	  if(j->dynamic_size)
	    width = wcswidth(disp.text.c_str(), disp.text.size());
	  else
	    width = j->width;
	}

      l.push_back(column(disp, width, j->expand, j->shrink));
    }

  return columnify(l, width);
}
