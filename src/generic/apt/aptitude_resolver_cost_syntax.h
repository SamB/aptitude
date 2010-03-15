/** \file aptitude_resolver_cost_syntax.h */     // -*-c++-*-

// Copyright (C) 2010 Daniel Burrows

// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#ifndef APTITUDE_RESOLVER_COST_SYNTAX_H
#define APTITUDE_RESOLVER_COST_SYNTAX_H

#include "aptitude_resolver_cost_types.h"

#include <boost/shared_ptr.hpp>
#include <vector>

/** \brief Parse a settings string.
 *
 *  Throws a cwidget::util::Exception if parsing failed.
 */
boost::shared_ptr<std::vector<cost_component_structure> >
parse_cost_settings(const std::string settings);

/** \brief Write some settings to a stream.
 *
 *  This is the inverse of parse_cost_settings.
 */
void dump_settings(std::ostream &out, const boost::shared_ptr<std::vector<cost_component_structure> > &settings);


#endif // APTITUDE_RESOLVER_COST_SYNTAX_H

