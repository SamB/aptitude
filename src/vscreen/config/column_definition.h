// column_definition.h   -*-c++-*-
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
//
//  This routine provides a general interface for parsing configuration data
// about a column format and later instantiating that information.  The caller
// has to provide some information, in the form of tables and callbacks, that's
// used to do the actual formatting.  (the callbacks are actually classes,
// since we need closureish behavior and classes are the best way to hack that
// into C++)
//
//  Originally arbitrary strings could be used to represent column information.
//  This was cute and readable, but not very flexible.  So I've switched to a
// "printf" style; the major reason is that this allows arbitrary strings to
// be embedded in the format.  This also means that the "padding" parmeters
// that I used before are no longer necessary, and eliminates various nasty
// hacks.
//
//  Column types are integers; -1 is reserved for internal use.  Column types
// can take arguments (although this isn't implemented yet, it's used
// internally for the "literal" column type)

#ifndef COLUMN_DEFINITION_H
#define COLUMN_DEFINITION_H

#include <list>
#include <string>

#include <generic/util/eassert.h>

#include <vscreen/columnify.h>

/** Defined the default settings for a particular column type. */
struct column_type_defaults
{
  unsigned int width;
  bool expand, shrink;
};

/** Abstraction of the parameters passed into the layout process. */
class column_parameters
{
public:
  virtual int param_count()=0;
  virtual std::wstring get_param(int n)=0;

  virtual ~column_parameters();
};

/** An empty list of parameters. */
class empty_column_parameters : public column_parameters
{
public:
  int param_count();
  std::wstring get_param(int n);
};

/** Defines how a single column is to be generated. */
struct column_definition
{
  // A literal column is taken from a literal string passed as a
  // parameter.  A generated one is created by calling a virtual
  // method.  A parameterized column is taken from a column_parameters
  // object passed into the layout method.
  enum column_type {COLUMN_LITERAL, COLUMN_GENERATED, COLUMN_PARAM};
  column_type type;

  // For parametric columns, this is the parameter number;
  // for generated ones, it's the type of column:
  int ival;

  // For literal columns only:
  std::wstring arg;

  // For generated or parametric columns:
  unsigned int width;
  bool expand:1, shrink:1;

  // For generated or parametric columns.  If \b true, "width"
  // will be ignored and the true width of the incoming string will
  // be given to the layout algorithm.
  bool dynamic_size:1;

  column_definition(const std::wstring &_arg, bool _expand, bool _shrink)
    :type(COLUMN_LITERAL), arg(_arg), expand(_expand), shrink(_shrink)
  {
  }

  column_definition(column_type _type,
		    int _ival, int _width, bool _expand, bool _shrink,
		    bool _dynamic_size)
    :type(_type), ival(_ival), width(_width),
     expand(_expand), shrink(_shrink), dynamic_size(_dynamic_size)
  {
    eassert(_width>=0);
  }
};

typedef std::list<column_definition> column_definition_list;

typedef int (*column_parser_func)(char id);

class column_generator
// Stores the information needed to parse and then generate columns.
{
  column_definition_list columns;
protected:
  virtual column_disposition setup_column(int type)=0;
  // Sets up a column of the given type (the width field may be overridden?)
public:
  column_generator(const column_definition_list &_columns)
    :columns(_columns) {}

  virtual ~column_generator();

  std::wstring layout_columns(unsigned int width,
			      column_parameters &p);
  // Lays out the columns into columns.
};

column_definition_list *parse_columns(std::wstring config,
				      column_parser_func parser,
				      column_type_defaults *defaults);
// Allocates the array itself; the caller must delete[] it.

#endif
