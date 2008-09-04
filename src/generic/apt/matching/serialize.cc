// serialize.cc       -*-c++-*-
//
//   Copyright (C) 2008 Daniel Burrows
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

#include "serialize.h"

#include "pattern.h"

using cwidget::util::ref_ptr;

namespace aptitude
{
  namespace matching
  {
    namespace
    {
      void serialize_string(const std::string &s,
			    std::string &out)
      {
	out.push_back('"');
	for(std::string::const_iterator it = s.begin();
	    it != s.end(); ++it)
	  {
	    switch(*it)
	      {
	      case '\n':
		out += "\\n";
		break;
	      case '\t':
		out += "\\t";
		break;
	      case '"':
	      case '\\':
		out.push_back('\\');
		// Fall-through.
	      default:
		out.push_back(*it);
	      }
	  }
	out.push_back('"');
      }

      void serialize_regexp(const pattern::regex_info &info,
			    std::string &out)
      {
	const std::string &regex_string = info.get_regex_string();

	serialize_string(regex_string, out);
      }

      void serialize_regexp_term(const std::string &name,
				 const pattern::regex_info &info,
				 std::string &out)
      {
	out.push_back('?');
	out += name;
	out.push_back('(');
	serialize_regexp(info, out);
	out.push_back(')');
      }

      void serialize_pattern_term(const std::string &name,
				  const ref_ptr<pattern> &p,
				  std::string &out)
      {
	out.push_back('?');
	out += name;
	out.push_back('(');
	serialize_pattern(p);
	out.push_back(')');
      }

      void serialize_pattern_list(const std::vector<ref_ptr<pattern> > &patterns,
				  std::string &out)
      {
	bool first = true;
	for(std::vector<ref_ptr<pattern> >::const_iterator it =
	      patterns.begin(); it != patterns.end(); ++it)
	  {
	    if(first)
	      first = false;
	    else
	      out += ", ";

	    serialize_pattern(*it, out);
	  }
      }

      void serialize_deptype(pkgCache::Dep::DepType deptype,
			     std::string &out)
      {
	switch(deptype)
	  {
	  case pkgCache::Dep::Depends:
	    out += "depends";
	    break;

	  case pkgCache::Dep::PreDepends:
	    out += "predepends";
	    break;

	  case pkgCache::Dep::Recommends:
	    out += "recommends";
	    break;

	  case pkgCache::Dep::Suggests:
	    out += "suggests";
	    break;

	  case pkgCache::Dep::Conflicts:
	    out += "conflicts";
	    break;

	  case pkgCache::Dep::DpkgBreaks:
	    out += "breaks";
	    break;

	  case pkgCache::Dep::Replaces:
	    out += "replaces";
	    break;

	  case pkgCache::Dep::Obsoletes:
	    out += "obsoletes";
	    break;
	  }
      }

      // Precedence levels are used to determine where to insert parens.
      // You need parens if something with a lower precedence is inside
      // something with a higher precedence.
      const int default_precedence = 0;
      const int and_precedence = 0;
      const int or_precedence = 1;
      const int not_precedence = 2;
      const int atomic_precedence = 3;

      int term_precedence(const ref_ptr<pattern> &p)
      {
	switch(p->get_type())
	  {
	  case pattern::and_tp:
	    return and_precedence;

	  case pattern::or_tp:
	    return or_precedence;

	  case pattern::not_tp:
	    return not_precedence;

	  default:
	    return atomic_precedence;
	  }
      }

      void serialize_term(const ref_ptr<pattern> &p,
			  std::string &out,
			  int precedence)
      {
	const int local_precedence = term_precedence(p);
	if(local_precedence <= precedence)
	  out.push_back('(');

	switch(p->get_type())
	  {
	  case pattern::archive:
	    serialize_regexp_term("archive", p->get_archive_regex_info(), out);
	    break;

	  case pattern::action:
	    out += "?action(";
	    switch(p->get_action_action_type())
	      {
	      case pattern::action_install:
		out += "install";
		break;

	      case pattern::action_upgrade:
		out += "upgrade";
		break;

	      case pattern::action_downgrade:
		out += "downgrade";
		break;

	      case pattern::action_remove:
		out += "remove";
		break;

	      case pattern::action_purge:
		out += "purge";
		break;

	      case pattern::action_reinstall:
		out += "reinstall";
		break;

	      case pattern::action_hold:
		out += "hold";
		break;

	      case pattern::action_keep:
		out += "keep";
		break;

	      default:
		throw MatchingException("Internal error: bad action-type flag.");
	      }
	    out.push_back(')');
	    break;

	  case pattern::all_versions:
	    serialize_pattern_term("all-versions",
				   p->get_all_versions_pattern(),
				   out);
	    break;

	  case pattern::any_version:
	    serialize_pattern_term("any-version",
				   p->get_any_version_pattern(),
				   out);
	    break;

	  case pattern::automatic:
	    out += "?automatic";
	    break;

	  case pattern::and_tp:
	    {
	      bool first = true;
	      const std::vector<ref_ptr<pattern> > &patterns = p->get_and_patterns();

	      for(std::vector<ref_ptr<pattern> >::const_iterator it =
		    patterns.begin(); it != patterns.end(); ++it)
		{
		  if(first)
		    first = false;
		  else
		    out.push_back(' ');

		  serialize_term(*it, out, and_precedence);
		}
	    }
	    break;

	  case pattern::bind:
	    out += "?bind(";
	    out += p->get_bind_variable_name();
	    out.push_back(')');
	    break;

	  case pattern::broken:
	    out += "?broken";
	    break;

	  case pattern::broken_type:
	    out += "?broken-";
	    serialize_deptype(p->get_broken_type_depends_type(), out);
	    break;

	  case pattern::candidate_version:
	    out += "?version(CANDIDATE)";
	    break;

	  case pattern::config_files:
	    out += "?config-files";
	    break;

	  case pattern::current_version:
	    out += "?version(CURRENT)";
	    break;

	  case pattern::depends:
	    out.push_back('?');
	    if(p->get_depends_broken())
	      out += "broken-";
	    serialize_deptype(p->get_depends_depends_type(), out);
	    out.push_back('(');
	    serialize_pattern(p->get_depends_pattern(), out);
	    out.push_back(')');
	    break;

	  case pattern::description:
	    serialize_regexp_term("description",
				  p->get_description_regex_info(),
				  out);
	    break;

	  case pattern::essential:
	    out += "?essential";
	    break;

	  case pattern::equal:
	    out += "?=";
	    out += p->get_equal_variable_name();
	    break;

	  case pattern::false_tp:
	    out += "?false";
	    break;

	  case pattern::for_tp:
	    out += "?for ";
	    out += p->get_for_variable_name();
	    out += ": ";
	    serialize_pattern(p->get_for_pattern(), out);

	  case pattern::garbage:
	    out += "?garbage";
	    break;

	  case pattern::install_version:
	    out += "?version(INSTALL)";
	    break;

	  case pattern::installed:
	    out += "?installed";
	    break;

	  case pattern::maintainer:
	    serialize_regexp_term("maintainer",
				  p->get_maintainer_regex_info(),
				  out);
	    break;

	  case pattern::name:
	    serialize_regexp_term("name",
				  p->get_name_regex_info(),
				  out);
	    break;

	  case pattern::narrow:
	    out += "?narrow(";
	    serialize_pattern(p, out);
	    out.push_back(')');
	    break;

	  case pattern::new_tp:
	    out += "?new";
	    break;

	  case pattern::not_tp:
	    out.push_back('!');
	    serialize_term(p, out, not_precedence);
	    break;

	  case pattern::obsolete:
	    out += "?obsolete";
	    break;

	  case pattern::or_tp:
	    {
	      bool first = true;
	      const std::vector<ref_ptr<pattern> > &patterns = p->get_and_patterns();

	      for(std::vector<ref_ptr<pattern> >::const_iterator it =
		    patterns.begin(); it != patterns.end(); ++it)
		{
		  if(first)
		    first = false;
		  else
		    out += " | ";

		  serialize_term(*it, out, or_precedence);
		}
	    }
	    break;

	  case pattern::origin:
	    serialize_regexp_term("origin",
				  p->get_origin_regex_info(),
				  out);
	    break;

	  case pattern::priority:
	    out += "?priority(";
	    out += p->get_priority_priority_name();
	    out.push_back(')');
	    break;

	  case pattern::provides:
	    out += "?provides(";
	    serialize_pattern(p->get_provides_pattern(), out);
	    out.push_back(')');
	    break;

	  case pattern::reverse_depends:
	    out.push_back('?');
	    if(p->get_depends_broken())
	      out += "broken-";
	    out += "reverse-";
	    serialize_deptype(p->get_depends_depends_type(), out);
	    out.push_back('(');
	    serialize_pattern(p->get_depends_pattern(), out);
	    out.push_back(')');
	    break;

	  case pattern::reverse_provides:
	    out += "?reverse-provides(";
	    serialize_pattern(p->get_reverse_provides_pattern(), out);
	    out.push_back(')');
	    break;

	  case pattern::section:
	    serialize_regexp_term("section",
				  p->get_section_regex_info(),
				  out);
	    break;

	  case pattern::source_package:
	    serialize_regexp_term("source-package",
				  p->get_source_package_regex_info(),
				  out);
	    break;

	  case pattern::source_version:
	    serialize_regexp_term("source-version",
				  p->get_source_version_regex_info(),
				  out);
	    break;

	  case pattern::tag:
	    serialize_regexp_term("tag",
				  p->get_tag_regex_info(),
				  out);
	    break;

	  case pattern::task:
	    serialize_regexp_term("task",
				  p->get_task_regex_info(),
				  out);
	    break;

	  case pattern::true_tp:
	    out += "?true";
	    break;

	  case pattern::upgradable:
	    out += "?upgradable";
	    break;

	  case pattern::user_tag:
	    serialize_regexp_term("user-tag",
				  p->get_user_tag_regex_info(),
				  out);
	    break;

	  case pattern::version:
	    serialize_regexp_term("version",
				  p->get_user_tag_regex_info(),
				  out);
	    break;

	  case pattern::virtual_tp:
	    out += "?virtual";
	    break;

	  case pattern::widen:
	    out += "?widen";
	    break;
	  }

	if(local_precedence <= precedence)
	  out.push_back(')');
      }
    }

    void serialize_pattern(const ref_ptr<pattern> &p,
			   std::string &out)
    {
      serialize_term(p, out, default_precedence);
    }
  }
}
