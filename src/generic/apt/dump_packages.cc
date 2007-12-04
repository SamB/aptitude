// dump_packages.cc     -*-c++-*-
//
//   Copyright (C) 2007 Daniel Burrows
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

#include "dump_packages.h"

#include "apt.h"

#include <cwidget/generic/util/eassert.h>
#include <cwidget/generic/util/exception.h>

namespace aptitude
{
  namespace apt
  {
    void dump_verfile(const pkgCache::VerFileIterator &vf,
		      std::ostream &out)
    {
      eassert(apt_cache_file != NULL);
      eassert(apt_package_records != NULL);

      pkgRecords::Parser &p = apt_package_records->Lookup(vf);
      const char *start, *stop;
      p.GetRec(start, stop);

      out.write(start, stop - start);
    }

    void dump_version(const pkgCache::VerIterator &ver,
		      std::ostream &out)
    {
      eassert(apt_cache_file != NULL);
      eassert(apt_package_records != NULL);

      bool first = true;
      for(pkgCache::VerFileIterator vf = ver.FileList();
	  !vf.end(); ++vf)
	{
	  if(first)
	    first = false;
	  else
	    out << std::endl;
	  dump_verfile(vf, out);
	}
    }

    void dump_versions(const std::vector<pkgCache::VerIterator> &packages,
		       std::ostream &out)
    {
      eassert(apt_cache_file != NULL);
      eassert(apt_package_records != NULL);

      for(std::vector<pkgCache::VerIterator>::const_iterator it =
	    packages.begin(); it != packages.end(); ++it)
	{
	  dump_version(*it, out);
	  out << std::endl;
	}
    }

    namespace
    {
      class dep_target
      {
	std::string name;
	/** \brief Either an empty string or the contents of the parens.
	 *
	 *  This is used instead of interpreting the version
	 *  information because we don't need it and this reduces
	 *  the burden on our parser.
	 */
	std::string version_information;

      public:
	dep_target(const std::string &_name,
		   const std::string &_version_information)
	  : name(_name),
	    version_information(_version_information)
	{
	}

	const std::string &get_name() const { return name; }
	const std::string &get_version_information() const { return version_information; }
      };

      /** \brief Internal notion of one element of a dependency. */
      class dep_element
      {
	/** \brief The elements of the OR. */
	std::vector<dep_target> targets;

      public:
	dep_element(const std::vector<dep_target> &_targets)
	  : targets(_targets)
	{
	}

	const std::vector<dep_target> &get_targets() const
	{
	  return targets;
	}
      };

      class ParseException : public cwidget::util::Exception
      {
	std::string msg;

      public:
	ParseException(const std::string &_msg)
	  : msg(_msg)
	{
	}

	std::string errmsg() const { return msg; }
      };

      // Note: this parser might be a bit more lenient than the dpkg
      // parser..
      void parse_deps(const char *&start, const char *stop,
		      std::vector<dep_element> &out)
      {
	while(start != stop)
	  {
	    if(isspace(*start))
	      {
		if(*start == '\n')
		  {
		    // Quit when we hit a newline that ends
		    // the dep group.
		    ++start;
		    if(start == stop || !isspace(*start))
		      return;
		  }
		else
		  ++start;
	      }
	    else
	      {
		std::vector<dep_target> targets;

		if(*start == ',' || *start == '(' || *start == '|')
		  throw ParseException("Unexpected metacharacter in package name.");

		while(start != stop && *start != ',')
		  {
		    std::string pkg_name;
		    while(start != stop && !isspace(*start) &&
			  *start != ',' && *start != '(' && *start != '|')
		      {
			pkg_name += *start;
			++start;
		      }

		    while(start != stop && isspace(*start))
		      {
			if(*start == '\n')
			  {
			    ++start;
			    if(start == stop || !isspace(*start))
			      {
				// End of the Depends element -- save
				// our place and stop.
				targets.push_back(dep_target(pkg_name, ""));
				out.push_back(dep_element(targets));
				return;
			      }
			  }
			++start;
		      }

		    std::string version_information;
		    if(start != stop && *start == '(')
		      {
			++start;
			while(start != stop && *start != ')')
			  {
			    version_information += *start;
			    ++start;
			  }

			if(start == stop)
			  throw ParseException("Unexpected EOF parsing version information.");
			else
			  ++start;
		      }

		    targets.push_back(dep_target(pkg_name, version_information));

		    // NB: we'll ignore consecutive pipe characters.
		    // That's ok, this isn't a strict parser.
		    while(start != stop && (*start == '|' || isspace(*start)))
		      {
			if(*start == '\n')
			  {
			    ++start;
			    if(start == stop || !isspace(*start))
			      {
				out.push_back(dep_element(targets));
				return;
			      }
			  }
			else
			  ++start;
		      }
		  }

		if(start != stop)
		  {
		    eassert(*start == ',');
		    out.push_back(dep_element(targets));
		    ++start;
		  }
	      }
	  }
      }

      // Functor matching dep targets whose target is not in the set
      // of visited packages (or provided by a version of a visited
      // package!).
      bool is_irrelevant_dep(const dep_target &target,
			     const std::set<pkgCache::PkgIterator> &visited_packages)
      {
	pkgCache::PkgIterator pkg = (*apt_cache_file)->FindPkg(target.get_name());
	if(pkg.end())
	  return true;
	else if(visited_packages.find(pkg) != visited_packages.end())
	  return false;
	else
	  {
	    // Also return false if anything providing the target
	    // package is in the live set.
	    for(pkgCache::PrvIterator prvIt = pkg.ProvidesList();
		!prvIt.end(); ++prvIt)
	      {
		if(visited_packages.find(prvIt.OwnerPkg()) != visited_packages.end())
		  return false;
	      }

	    return true;
	  }
      }

      // Drop targets of dependencies that are irrelevant;
      // drop dependencies with only irrelevant targets.
      void filter_deps(const std::vector<dep_element> &in_elements,
		       const std::set<pkgCache::PkgIterator> &visited_packages,
		       std::vector<dep_element> &out_elements)
      {
	for(std::vector<dep_element>::const_iterator inIt = in_elements.begin();
	    inIt != in_elements.end(); ++inIt)
	  {
	    std::vector<dep_target> targets;

	    for(std::vector<dep_target>::const_iterator targetIt = inIt->get_targets().begin();
		targetIt != inIt->get_targets().end(); ++targetIt)
	      {
		if(!is_irrelevant_dep(*targetIt, visited_packages))
		  targets.push_back(*targetIt);
	      }

	    if(!targets.empty())
	      out_elements.push_back(dep_element(targets));
	  }
      }

      void dump_dep_line(const std::vector<dep_element> &elements,
			 std::ostream &out)
      {
	bool first_element = true;
	for(std::vector<dep_element>::const_iterator eltIt = elements.begin();
	    eltIt != elements.end(); ++eltIt)
	  {
	    if(first_element)
	      first_element = false;
	    else
	      out << ", ";

	    bool first_target = true;
	    for(std::vector<dep_target>::const_iterator targetIt =
		  eltIt->get_targets().begin();
		targetIt != eltIt->get_targets().end(); ++targetIt)
	      {
		if(first_target)
		  first_target = false;
		else
		  out << " | ";

		out << targetIt->get_name();
		if(!targetIt->get_version_information().empty())
		  {
		    out << " (" << targetIt->get_version_information() << ")";
		  }
	      }
	  }

	out << std::endl;
      }

      void dump_truncated_version_file(const pkgCache::VerFileIterator &vf,
				       const std::set<pkgCache::PkgIterator> &visited_packages,
				       std::ostream &out)
      {
	eassert(apt_cache_file != NULL);
	eassert(apt_package_records != NULL);

	pkgRecords::Parser &p = apt_package_records->Lookup(vf);
	const char *start, *stop;
	p.GetRec(start, stop);

	while(start != stop)
	  {
	    const char *line_end =
	      reinterpret_cast<const char *>(memchr(start, '\n', stop - start));

	    if(isspace(*start))
	      {
		if(line_end == NULL)
		  {
		    out.write(start, stop - start);
		    start = stop;
		  }
		else
		  // We expect that lines starting with blanks
		  // should be copied exactly (if not, they should
		  // be handled below).
		  {
		    out.write(start, line_end - start + 1);
		    start = line_end + 1;
		  }
	      }
	    else
	      {
		// Two cases of interest:
		//  (1) Not a dependency line.  In this case we
		//      just output the line and all its
		//      successors literally.
		//  (2) A dependency line.  In this case we parse
		//      the line and its successors (eek) and
		//      build our own internal notion of the result.
		const char * const end   = line_end == NULL  ? stop : line_end;
		const char * const next  = line_end == NULL  ? stop : line_end + 1;
		const char * const colon = reinterpret_cast<const char *>(memchr(start, ':', end - start));

		if(colon == NULL) // ??
		  {
		    out.write(start, end - start);
		    start = next;
		  }
		else
		  {
		    static const char *depends_fields[] = {
		      "Depends",
		      "Recommends",
		      "Suggests",
		      "Enhances",
		      "Pre-Depends",
		      "Conflicts",
		      "Replaces",
		      "Obsoletes",
		      "Breaks",
		      NULL
		    };

		    // Check whether any of these match the field
		    // we've encountered.
		    bool found = false;
		    for(const char **fld = depends_fields; !found && *fld != NULL; ++fld)
		      {
			if(static_cast<size_t>(colon - start)  !=  strlen(*fld))
			  continue;

			if(strncasecmp(start, *fld, colon - start) == 0)
			  found = true;
		      }

		    if(!found)
		      {
			// Write to *next* so we include the newline
			// if any.
			out.write(start, next - start);
			start = next;
		      }
		    else
		      {
			const char *newStart = colon + 1;
			std::vector<dep_element> deps;
			parse_deps(newStart, stop, deps);
			std::vector<dep_element> filtered_deps;
			filter_deps(deps, visited_packages, filtered_deps);

			if(!deps.empty())
			  {
			    // Write out everything up to **and including**
			    // the colon.
			    out.write(start, colon - start + 1);
			    out << " ";
			    dump_dep_line(filtered_deps, out);
			  }

			start = newStart;
		      }
		  }
	      }
	  }
      }
    }

    void dump_truncated_packages(const std::set<pkgCache::PkgIterator> &packages,
				 std::ostream &out)
    {
      try
	{
	  bool first = true;
	  for(std::set<pkgCache::PkgIterator>::const_iterator it = packages.begin();
	      it != packages.end(); ++it)
	    {
	      for(pkgCache::VerIterator vIt = it->VersionList(); !vIt.end(); ++vIt)
		{
		  for(pkgCache::VerFileIterator vfIt = vIt.FileList();
		      !vfIt.end(); ++vfIt)
		    {
		      if(first)
			first = false;
		      else
			out << std::endl;
		      dump_truncated_version_file(vfIt, packages, out);
		    }
		}
	    }
	}
      catch(const cwidget::util::Exception &e)
	{
	  out << std::endl << "Uncaught exception: "
	      << e.errmsg() << std::endl;
	}
    }
  }
}
