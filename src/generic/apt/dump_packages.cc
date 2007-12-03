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
  }
}
