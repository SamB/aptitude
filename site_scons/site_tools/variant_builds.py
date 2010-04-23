# Copyright (C) 2010 Daniel Burrows
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.


'''Support for easily creating a matrix of build variants and building
some or all of them.'''

# TODO: need an interface to disable support for a class of variants
# (selected the same way the variants to build are selected); be sure
# to check whether all variants have been hidden and emit an error in
# that case, probably from AllVariantEnvs.
#
# Use case here: I would rather only check for GTK+ once, in the
# configure step, and then disable the GTK+ variant if it isn't
# available.  Removing it from the list of variants passed to
# DefineVariants is not ideal, since then it won't show up in help
# etc.

import operator

from SCons.Script import AddOption, GetOption

def exists(env):
    return True

def generate(env):
    # NB: export VariantAxis and Variant because the user can't import
    # this file directly.
    env.AddMethod(VariantAxis)
    env.AddMethod(Variant)
    env.AddMethod(DefineVariants)
    env.AddMethod(AllVariantEnvs)
    env.AddMethod(IsVariantActive)

class VariantAxis:
    '''Represents a single axis of choice for a variant build.

For instance, VariantAxis("debug", "optimized") sets up a variant axis
that lets the user pick from either debug builds or optimized builds.
If another axes lets them choose between "lemon" and "orange",
they\'ll end up with "debug-lemon", "debug-orange", "optimized-lemon",
and "optimized-orange" builds.

The given help text will be printed just above the list of options.'''
    def __init__(self, env, helptext, *options):
        self.options = list(options)
        self.helptext = helptext

    def __str__(self):
        return str(self.options)

    def __repr__(self):
        return 'VariantAxis(%s)' % repr(self.options)

class Variant:
    '''Represents a single value in a variant axis.'''
    def __init__(self, env, name, helptext = None,
                 flags = '',
                 vars = {},
                 initf = lambda *args: None,
                 build_dir = None):
        '''Create a new variant.

The given flags are passed to MergeFlags.  Variables specified in vars
are updated using Replace.  initf is then invoked on the environment
for its side-effects, if any (none by default).'''
        if name == 'all':
            raise Exception('The name "all" is reserved for internal use.')
        elif '-' in name:
            raise Exception('Variant names cannot contain "-".')

        self.name = name
        self.helptext = helptext
        self.flags = flags
        self.vars = vars
        self.initf = initf

    def Prepare(self, env):
        env.MergeFlags(self.flags)
        env.Replace(self.vars)
        self.initf(env)

    def __hash__(self):
        return hash(self.name)

    def __eq__(self, other):
        return self.name == other.name

    def __str__(self):
        return self.name

    def __repr__(self):
        return 'Variant(%s)' % repr(self.name)

class VariantChoice:
    '''Represents an assignment of options to all variants.'''

    def __init__(self, selections):
        '''Create a variant choice by selecting the given options.
"selections" is a list in which the first entry is the option selected
along the first axis, the second entry is the option selected along the
second axis, and so on.'''
        # NB: use a tuple because its hash and comparison semantics
        # are what we want.
        self.selections = tuple(selections)

    def BuildEnv(self, env):
        '''Produce a build environment specialized for this collection
of variant selections.'''

        specialized = env.Clone(VARIANT = str(self))
        for v in self.selections:
            v.Prepare(specialized)

        specialized.VariantDir('#/build/%s' % self, '#')

        return specialized

    def __str__(self):
        '''Compute the string that is used to represent this choice.'''
        return '-'.join([x.name for x in self.selections])

    def __repr__(self):
        return 'VariantChoice(%s)' % repr(str(self))

    def __contains__(self, option):
        '''Returns true if the given option is selected by this choice.'''
        return option in self.selections

    def __eq__(self, other):
        return self.selections == other.selections

    def __hash__(self):
        return hash(self.selections)

def AllVariants(axes):
    '''Compute a set of all the variants of the given list of axes.'''
    all_variants = [[]]
    for axis in axes:
        all_variants = [variant + [option]
                        for option in axis.options
                        for variant in all_variants]

    all_variants = set(map(VariantChoice, all_variants))

    return all_variants

def GetOptions(axes):
    options = {}
    axis_number = 0
    for axis in axes:
        for option in axis.options:
            if option.name in options:
                raise Exception('Duplicate variant option \"%s\" (occurs in axis %s and again in axis %s).'
                                % (option, options[option][0], axis_number))

            options[option.name] = (axis_number, option)

        axis_number += 1

    return options

def GetAxisChoices(entry, options):
    '''Parse a variant selector entry (something like debug-lemon) and
return a dictionary mapping axis numbers to the options chosen on each
axis.  options maps option names to pairs (axis_number, option).'''
    components = [x.strip() for x in entry.split('-')]
    axis_choices = { }
    for component in components:
        if component not in options:
            raise Exception('Invalid build variant "%s"' % component)

        (axis_number, v) = options[component]
        if axis_number in axis_choices and axis_choices[axis_number] != v:
            raise Exception('Conflicting build variants \"%s\" and \"%s\" in variant selector \"%s\".'
                            % (axis_choices[axis_number].name,
                               component,
                               entry))

        axis_choices[axis_number] = v

    return axis_choices

def ParseVariants(axes, variants):
    '''Parse a variant selection option.  options is a dictionary
mapping axis option names onto pairs (axis_number, variant) where
variant is the Variant object representing the chosen option.  axes
is a list of VariantAxis objects.

Returns a set of VariantChoices representing the variants that match
the given configuration string.'''

    # Lame, lazy way of doing this: build a list of the Cartesian
    # product of all the lists first.  Each entry in this list is a
    # list containing a single option from each variant axis.
    #
    # Lame because it would be better (more scalable) to generate
    # these lazily.  But for now I expect to have 2-3 axes with
    # perhaps 5 entries on an axis, so it would be a waste of time to
    # do this better.
    #
    # Also lame because it's a hideously inefficient algorithm.
    all_variants = AllVariants(axes)
    options = GetOptions(axes)

    chosen_variants = set()

    entries = [x.strip() for x in variants.split(',')]
    for entry in entries:
        if entry == 'all': # Magic!
            chosen_variants = chosen_variants.union(all_variants)
        else:
            axis_choices = GetAxisChoices(entry, options)

            # Now, put each variant matching the given axis choices
            # into chosen_variants.
            for v in all_variants:
                matches = True
                for axis_number, selection in axis_choices.iteritems():
                    if v.selections[axis_number] != selection:
                        matches = False
                        break

                if matches:
                    chosen_variants.add(v)

    return chosen_variants

def DefineVariants(env, axes, default):
    '''Define the available variants and configure the build for the
ones the user chose.

Variants are defined on one or more orthogonal axes.  Each axis
provides one or more options, and the user can choose from these
options independently.  For instance, the first axis might provide
"debug" and "optimized", while the second one provides "lemon" and
"orange".  The build directories are named after the axis values in
order, so you get things like "debug-lemon".

Option names must be unique across all axes; this allows the user to
specify them in any order ("debug,lemon" or "lemon,debug" always mean
the same thing).  They should not contain hyphens, to avoid
conflicting with the naming scheme for build directories.

A command-line variable "VARIANTS=foo" and an option "--variants=foo"
are created, with the variable overriding the option if both are
specified.  The argument to this command is a comma-separated list of
"variant selectors" describing which variants to build; all the
variants produced by each selector are built.  Variant selectors are
hyphen-separated lists of one or more option names, e.g.:
"lemon-debug" or "optimized-orange"; variants that match the selector
on the axes implied by its options are chosen by that selector.

Once variants have been chosen, the caller should use ForEachVariant()
to select the active variants.'''

    if hasattr(env, 'variant_axes'):
        raise Exception('Build file error: DefineVariants() was invoked twice.')

    # Generate help text.
    helplines = []
    helplines.append('Build variants (set with VARIANTs= or --variants=):')
    for axis in axes:
        helplines.append('  %s:' % axis.helptext)
        for option in axis.options:
            line = '    %s' % option.name
            if option.helptext is not None:
                line += ': %s' % option.helptext
            helplines.append(line)
        helplines.append('')

    helplines.append('Default variant(s): %s' % default)

    env.Help('\n'.join(helplines))

    AddOption('--variants',
              dest = 'variants',
              nargs = 1,
              type = 'string',
              action = 'store',
              metavar = 'VARIANTS',
              help = 'build variants')
    if 'VARIANTS' not in env:
        env['VARIANTS'] = GetOption('variants')

    # Save the variant definitions for later use.
    env.variant_axes = axes

def AllVariantEnvs(env):
    '''Create and the environment variants for the given environment.

Returns one environment for each build variant, specialized for that variant.'''

    if 'VARIANTS' not in env or not hasattr(env, 'variant_axes'):
        raise Exception('No variants defined -- call DefineVariants first.')

    axes = env.variant_axes
    variant_string = env['VARIANTS']
    active_variants = ParseVariants(axes, variant_string)

    return [v.BuildEnv(env) for v in active_variants]

def IsVariantActive(env, variant_pattern):
    '''Return true if the given environment is specialized for a build
variant matching the given variant_pattern.'''

    if 'VARIANT' not in env:
        return False

    # This is why we need to preserve variant_axes in specialized
    # environments -- so we can parse tests like this.
    axes = env.variant_axes
    active_variant = env['VARIANT']
    active_variants = ParseVariants(axes, active_variant)
    if len(active_variants) > 1:
        raise Exception('Exactly one variant should be active at a time.')
    selected_variants = ParseVariants(axes, variant_string)

    return active_variants.issubset(selected_variants)
