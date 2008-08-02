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
#include <generic/apt/desc_parse.h>
#include <generic/apt/config_signal.h>

namespace gui
{
  std::wstring PackagesDescription::make_desc_fragment(const std::vector<aptitude::description_element_ref> &elements,
      int level)
  {
    std::wstring fragments;

    for(std::vector<aptitude::description_element_ref>::const_iterator it = elements.begin();
        it != elements.end(); ++it)
      {
        const aptitude::description_element_ref &elt(*it);

        switch(elt->get_type())
          {
          case aptitude::description_element::blank_line:
            fragments.append(L"\n");
            break;
          case aptitude::description_element::paragraph:
            fragments.append(elt->get_string());
            break;
          case aptitude::description_element::literal:
            fragments.append(elt->get_string());
            break;
          case aptitude::description_element::bullet_list:
            {
              std::wstring bullet;
              bullet.append(std::wstring(L"*+-"[level%3]+L""));
              fragments.append(bullet);

              fragments.append(make_desc_fragment(elt->get_elements(),
                                                             level + 1));

            }
            break;
          }
      }

    return fragments;
  }

  void PackagesDescription::make_level_fragment(const std::wstring &desc,
                                  std::wstring::size_type indent,
                                  std::wstring::size_type &start,
                                  bool recognize_bullets,
                                  std::vector<aptitude::description_element_ref> &output)
  {
    bool first=true;

    while(start<desc.size())
      {
        std::wstring::size_type loc=start;
        unsigned int nspaces;

        if(!first)
          {
            nspaces=0;

            while(loc<desc.size() && desc[loc]==L' ' && nspaces<indent)
              {
                ++loc;
                ++nspaces;
              }

            // This handles the case of " .\n" breaking list paragraphs.
            // I arbitrarily put the paragraph break inside the indented
            // text even when it actually terminates the list.
            if(nspaces == 1 && loc < desc.size() && desc[loc] == L'.')
              ; // Handled uniformly below for both this case and the
                // case of a leading blank line.
            else if(nspaces < indent) // Anything but a " .\n" that has
                                      // the wrong indent will break the
                                      // list.
              break;
          }
        else
          {
            nspaces=indent;
            first=false;
          }

        // Only insert blank lines for full stops that have exactly one
        // space; other full stops are treated as part of a paragraph.
        if(nspaces == 1 && desc[loc] == '.')
          {
            output.push_back(aptitude::description_element::make_blank_line());

            while(loc < desc.size() && desc[loc] != L'\n')
              ++loc;

            if(loc < desc.size())
              ++loc;

            start=loc;

            continue; // Skip the switch statement below.
          }

        switch(desc[loc])
          {
          case L' ':
            {
              // Stores the number of spaces up to a bullet, if any.
              unsigned int nspaces2=nspaces+1;

              ++loc;

              // Provisionally check if it's a bulletted line --
              // *ignoring leading spaces*.
              std::wstring::size_type loc2=loc;

              while(loc2<desc.size() && desc[loc2] == L' ')
                {
                  ++loc2;
                  ++nspaces2;
                }

              if(recognize_bullets &&
                 loc2 + 1 < desc.size() &&
                 (desc[loc2] == L'+' ||
                  desc[loc2] == L'-' ||
                  desc[loc2] == L'*') &&
                 desc[loc2 + 1] == L' ')
                {
                  // Start a list item (i.e., an indented region).

                  start = loc2 + 2;

                  std::vector<aptitude::description_element_ref> item_contents;

                  make_level_fragment(desc,
                                      nspaces2 + 2,
                                      start,
                                      recognize_bullets,
                                      item_contents);

                  output.push_back(aptitude::description_element::make_bullet_list(item_contents));
                }
              else
                {
                  int amt=0;
                  while(loc+amt<desc.size() && desc[loc+amt]!=L'\n')
                    ++amt;

                  output.push_back(aptitude::description_element::make_literal(std::wstring(desc, loc, amt)));

                  loc+=amt;
                  if(loc<desc.size())
                    ++loc;

                  start=loc;
                }
            }

            break;
          default:
            // It's a paragraph.
            {
              bool cont=true;
              std::wstring::size_type amt=0;
              std::wstring par=L"";

              do {
                amt=0;
                while(loc+amt<desc.size() && desc[loc+amt]!=L'\n')
                  ++amt;

                par=par+std::wstring(desc, loc, amt);

                loc+=amt;

                // If we hit a newline and didn't just output a whitespace
                // character, insert one.
                if(loc<desc.size() && par.size()>0 && par[par.size()-1]!=' ')
                  par+=L" ";

                // Skip the newline
                if(loc<desc.size())
                  ++loc;

                // Update start.
                start=loc;

                // Find how much indentation this line has.
                nspaces=0;
                while(loc<desc.size() && desc[loc]==L' ')
                  {
                    ++loc;
                    ++nspaces;
                  }

                // Check if we should continue (if not, we back up and
                // start parsing again from "start").  Note that *any*
                // change in the indentation requires us to restart --
                // more indentation is a literal line, while less means
                // we should exit this indent level.
                if(nspaces != indent)
                  cont=false;
                else if(loc>=desc.size())
                  cont=false;
                else if(nspaces == 1 && desc[loc] == '.')
                  cont=false;
              } while(cont);

              output.push_back(aptitude::description_element::make_paragraph(par));
            }
          }
      }
  }

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

    const std::wstring fulldesc = get_long_description(ver, apt_package_records);

    std::wstring::size_type loc = 0;

    // Skip the short description
    while(loc < fulldesc.size() && fulldesc[loc]!=L'\n')
      ++loc;

    short_description = cwidget::util::transcode(fulldesc.substr(0, loc), "UTF-8");

    if(loc < fulldesc.size()) // Skip the '\n'
      ++loc;

    // Skip leading whitespace on the first line if there is any.
    if(loc<fulldesc.size() && fulldesc[loc] == L' ')
      ++loc;

    std::vector<aptitude::description_element_ref> elements;

    // The initial indentation level is 1 because in a Packages file,
    // all Description lines get at least one character of indentation
    // and we want to strip that off.
    make_level_fragment(fulldesc, (std::wstring::size_type)1, loc,
                        aptcfg->FindB(PACKAGE "::Parse-Description-Bullets",
                                      true),
                        elements);

    long_description = cwidget::util::transcode(make_desc_fragment(elements, 0), "UTF-8");
  }

}
