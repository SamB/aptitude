// -*-c++-*-

// description.cc
//
//  Copyright 1999-2008 Daniel Burrows
//  Copyright 2008 Obey Arthur Liu
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

#include "description.h"
#include "aptitude.h"

#include <string>
#include <iostream>

#include <apt-pkg/pkgcache.h>
#include <apt-pkg/strutl.h>
#include <apt-pkg/pkgrecords.h>

#include <generic/apt/apt.h>

namespace gui
{

  PackagesDescription::PackagesDescription(pkgCache::PkgIterator pkg, pkgCache::VerIterator ver)
  {
    pkgRecords::Parser &rec=apt_package_records->Lookup(ver.FileList());

    name = pkg.Name();

    version = ver.VerStr();

    priority = pkgCache::VerIterator(ver).PriorityType()?pkgCache::VerIterator(ver).PriorityType():_("Unknown");

    section = pkg.Section()?pkg.Section():_("Unknown");

    maintainer = rec.Maintainer().c_str();

    compressed_size = SizeToStr(ver->Size).c_str();

    uncompressed_size = SizeToStr(ver->InstalledSize).c_str();

    source_package = rec.SourcePkg().empty()?pkg.Name():rec.SourcePkg().c_str();

    std::string fulldesc = cwidget::util::transcode(get_long_description(ver, apt_package_records), "UTF-8");

    std::string::size_type pos = fulldesc.find("\n");

    if (pos != std::string::npos)
      {
        short_description = fulldesc.substr(0, pos);
        long_description = fulldesc.substr(pos+1);
      }
    else
      {
        short_description = fulldesc;
        long_description = fulldesc;
      }
  }

}
