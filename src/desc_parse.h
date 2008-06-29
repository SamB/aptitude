// desc_parse.h                                     -*-c++-*-
//
//   Copyright 2004-2005, 2008 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#ifndef DESC_PARSE_H
#define DESC_PARSE_H

#include <string>
#include <vector>

#include <apt-pkg/pkgcache.h>

#include <cwidget/generic/util/ref_ptr.h>

/** \file desc_parse.h
 */

namespace cwidget
{
  class fragment;
}

namespace aptitude
{
  /** \brief Represents a single piece of syntactic structure in a
   *  description.
   *
   *  This is a poor man's algebraic datatype.  Its structure is:
   *
   *    Element = Paragraph String | Literal String | BulletList [Element]
   *
   *  with blank lines being denoted by paragraphs containing an empty
   *  string.
   */
  class description_element
  {
  public:
    /** \brief The type tag of elements. */
    enum element_type
      {
	/** \brief Indicates that an element represents a paragraph of text.
	 *
	 *  Paragraphs should be word-wrapped, and adjacent paragraphs
	 *  should be separated by vertical whitespace.  Paragraphs
	 *  with empty string data insert vertical whitespace and no
	 *  text at all.
	 */
	paragraph,
	/** \brief Indicates that an element represents literally formatted text.
	 *
	 *  These elements should be formatted without word-wrapping
	 *  or other interpretation of the text.  Vertical whitespace
	 *  should not be used to offset literal elements from their
	 *  surrounding text.
	 */
	literal,
	/** \brief Indicates that an element represents a bulletted list.
	 *
	 *  Each entry in the list should be formatted by placing a
	 *  bullet to its left and indenting the whole entry.
	 */
	bullet_list
      };

  private:

    element_type type;

    std::string string_payload;
    std::vector<cwidget::util::ref_ptr<description_element> > list_payload;

    int refcount;

    description_element(element_type _type, const std::string &_string_payload,
			const std::vector<cwidget::util::ref_ptr<description_element> > &_list_payload)
      : type(_type),
	string_payload(_string_payload),
	list_payload(_list_payload)
    {
    }

  public:
    void incref()
    {
      eassert(refcount > 0);
      ++refcount;
    }

    void decref()
    {
      eassert(refcount > 0);

      --refcount;
      if(refcount == 0)
	delete this;
    }

    static description_element make_paragraph(const std::string &text)
    {
      return description_element(paragraph, text,
				 std::vector<cwidget::util::ref_ptr<description_element> >());
    }

    static description_element make_literal(const std::string &text)
    {
      return description_element(literal, text,
				 std::vector<cwidget::util::ref_ptr<description_element> >());
    }

    static description_element make_bullet_list(const std::vector<cwidget::util::ref_ptr<description_element> > &elements)
    {
      return description_element(bullet_list, std::string(), elements);
    }

    /** \brief Retrieve the type tag of this element. */
    element_type get_type() const { return type; }
    /** \brief Get the string of a paragraph or literal element.
     *
     *  It is an error to invoke this routine on any other element type.
     */
    const std::string &get_string() const
    {
      eassert(type == paragraph || type == literal);
      return string_payload;
    }

    /** \brief Get the list of elements associated with a bullet-list element.
     *
     *  It is an error to invoke this routine on any other element type.
     */
    const std::vector<cwidget::util::ref_ptr<description_element> > &get_elements()
    {
      eassert(type == bullet_list);
      return list_payload;
    }
  };
  typedef cwidget::util::ref_ptr<description_element> description_element_ref;
}

/** Parses the given description string according to the standard
 *  formatting rules.
 *
 *  \param desc a Description tag to parse
 *  \return a cwidget::fragment representing that description
 */
cwidget::fragment *make_desc_fragment(const std::wstring &desc);

/** \return a cwidget::fragment listing the tags of the given package, or \b
 *  NULL if there are no tags.
 *
 *  The global cache, apt_cache_file, should be available when you
 *  call this routine.
 */
cwidget::fragment *make_tags_fragment(const pkgCache::PkgIterator &pkg);

#endif
