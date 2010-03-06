/** \file tier_limits.cc */


// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#include "tier_limits.h"

const int tier_limits::maximum_level;
const int tier_limits::conflict_structural_level;
const int tier_limits::defer_structural_level;
const int tier_limits::already_generated_structural_level;
const int tier_limits::minimum_level;

const tier tier_limits::maximum_tier(tier_limits::maximum_level);
const tier tier_limits::conflict_tier(tier_limits::conflict_structural_level);
const tier tier_limits::already_generated_tier(tier_limits::already_generated_structural_level);
const tier tier_limits::defer_tier(tier_limits::defer_structural_level);
const tier tier_limits::minimum_tier(tier_limits::minimum_level);

const tier_operation tier_limits::increase_to_maximum_op(tier_operation::make_advance_structural_level(maximum_level));
const tier_operation tier_limits::increase_to_conflict_op(tier_operation::make_advance_structural_level(conflict_structural_level));
const tier_operation tier_limits::increase_to_already_generated_op(tier_operation::make_advance_structural_level(already_generated_structural_level));
const tier_operation tier_limits::increase_to_defer_op(tier_operation::make_advance_structural_level(defer_structural_level));
const tier_operation tier_limits::minimum_op;
