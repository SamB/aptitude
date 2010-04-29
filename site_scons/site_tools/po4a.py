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

from SCons.Script import File

import re
import os.path

def exists():
    return True

def generate(env):
    env.AddMethod(Po4A)
    env.AddMethod(Po4AConfiguration)

# Expression matching blank lines.
blank_line_re = re.compile(r'^\s*$')
# Expression matching lines that start a configuration entry.  The
# rest of the line isn't matched and should be matched by argument_re
# (below).
entry_start_re = re.compile(r'^\s*\[\s*(?:([a-zA-Z0-9_\-]+)\s*:)?\s*([a-zA-Z0-9_\-]+)\]')
# Expression matching an entire argument to a entry, including any
# double quotes.
#
# NOTE: Currently quote escaping isn't handled.  To add it, you'd need
# to modify this expression to recognize escaped quotes, then modify
# the quote stripping to remove the escape but not the quote.  SInce
# this is not a general solution, it's OK.
#
# Note the optional EOL with optional preceding backslash.
#
# Also note that backslashes aren't allowed anywhere except at EOL.
# Again, this is a simplification that works because aptitude's po4a
# configuration file doesn't need this.
argument_re = re.compile(r'\s*((?:[^\s\\"]|"[^"]*")+)\s*(\\?$)?')

def find_file(path, fn):
    '''Look for a file in a path.'''
    for d in path:
        d_path = os.path.join(str(path), str(fn))
        if os.path.isfile(d_path):
            return d_path

    # Arbitrarily return the first option if none exist.  This happens
    # to be exactly right, since we'll normaly have only one option
    # (implementing the general case is just future-proofing)
    return os.path.join(str(path[0]), str(fn))

def path_function(env, dot, targets, sources):
    '''path_function used to produce a path for the scanner.

Always produces a path containing just the directory containing
the SConscript file.'''
    return (dot,)

class ConfigEntry:
    '''Represents one entry in the po4a config file.;'''
    def __init__(self, category = None, name = None):
        if name is None:
            raise Exception('Missing entry name argument.')
        self.category = category
        self.name = name
        self.args = []

    def add_arg(self, arg):
        self.args.append(arg)

    def __repr__(self):
        if self.category is None:
            return '[%s] %s' % (self.name, ' '.join(self.args))
        else:
            return '[%s:%s] %s' % (self.category, self.name, ' '.join(self.args))

def parse_po4a_config_entries(f):
    '''Parse a file object and extract all po4a.cfg-style entries
from it.

Entries have the syntax:

[CLASS: ENTRY-NAME] ARG1 ...

where CLASS: is optional.  Argument lists can contain quoted strings,
and newlines can be backslash-escaped to continue a single entry.'''

    entries = []

    # Note that the current entry might be a "live", i.e., a direct
    # reference to the most recently parsed entry.  This happens
    # when the last line ended with a trailing backspace.
    current_entry = None
    for line in f:
        if blank_line_re.match(line):
            # Cancel out the current entry so we don't append to the
            # last line.
            current_entry = None
            continue

        # Trailing newlines mess up pattern matching since the regexp
        # EOL doesn't match actual newlines.
        line = line.rstrip('\n')

        entry_start = entry_start_re.match(line)
        if entry_start:
            if current_entry is not None:
                raise Exception('po4a configuration file syntax error: entries are run together.')
            else:
                current_entry = ConfigEntry(category = entry_start.group(1),
                                            name = entry_start.group(2))
                entries.append(current_entry)
            start_args_index = entry_start.end()
        else:
            if current_entry is None:
                raise Exception('po4a configuration file syntax error: saw a non-entry line, but I have no current entry: %s' % `line`)
            start_args_index = 0

        saw_end = False
        saw_continuation = False
        for arg_match in argument_re.finditer(line[start_args_index:]):
            arg_text = arg_match.group(1)
            current_entry.add_arg(arg_text)
            if arg_match.group(2) is not None:
                saw_end = True
                if arg_match.group(2) == '\\':
                    saw_continuation = True

        if not saw_continuation:
            current_entry = None

        if not saw_end:
            raise Exception('Syntax error in po4a configuration file: not all of the line could be parsed as entry arguments: %s' % `line`)

    return entries

# Function to get lists of sources and targets declared in a po4a.cfg
# file.
#
# Note: this is hand-crafted to meet the current needs of aptitude.
# It will need to be adjusted should we start using new configuration
# options.  The reason this is useful is that it means that
# translators don't have to modify a second piece of information about
# the dependencies of the po files.
#
# Note: the configuration file format of po4a is undocumented!  This
# scanner was written based on aptitude's po4a configuration and is
# unlikely to generalize very well.
def get_po4a_targets(env, source):
    extra_sources = []
    outputs = []
    entries = parse_po4a_config_entries(file(source.abspath))
    # Current behavior is to heuristically guess what's a source and
    # what's an output.
    for entry in entries:
        if entry.category is None and entry.name == 'po4a_paths':
            for arg in entry.args:
                parts = arg.split(':', 1)
                outputs.append(parts[-1])
        elif entry.category == 'type' and entry.name == 'docbook':
            # Each entry might or might not have a entry prefix
            # (like "de:" or "opt:").  If it has a entry prefix and
            # the prefix is "opt:", assume it's an option and
            # ignore it; otherwise, assume the part after the
            # prefix is a file name.
            for arg in entry.args:
                parts = arg.split(':', 1)
                # The arguments can contain output files (with a
                # "$LANG:" prefix), options (with an "opt:"
                # prefix) and input files (no prefix).
                if len(parts) == 1:
                    extra_sources.append(parts[0])
                elif parts[0] != 'opt':
                    outputs.append(parts[1])

    return (extra_sources, outputs)

# I use a wrapper around the builder to ensure that the various
# SideEffect calls take place as required.
def Po4AConfiguration(env, cfg_file):
    print os.getcwd()

    if isinstance(cfg_file, basestring):
        cfg_file = File(cfg_file)
    cfg_file = cfg_file.srcnode()

    extra_sources, outputs = get_po4a_targets(env, cfg_file)
    targets = []
    side_effects = []
    for entry in env.Flatten(outputs):
        if entry.endswith('.pot') or entry.endswith('.po'):
            # Note that we update the pofiles *in the source
            # directory*, not in the build directory.  This is
            # deliberate: the normal workflow for translators updates
            # pofiles in place.
            targets.append(File(entry).srcnode())
        # The other files declared in po4a.cfg are the translated XML
        # files.  They are excluded from the build below (with
        # --no-translations).

    result = env.Command(targets, (cfg_file, extra_sources),
                         'cd ${PO4A_CFGFILE.dir.dir} && po4a --no-translations ${PO4A_CFGFILE.abspath}',
                         PO4A_CFGFILE = cfg_file)

    return result

def Po4A(env, target, master, pofile,
         addendum = None,
         percent_translated = 75,
         master_charset = 'utf-8',
         format = 'docbook'):
    '''Run po4a over a document to produce a translated document.'''
    extra_args = []
    deps = [master, pofile]
    if addendum is not None:
        # This is what we should do, but the addendums were never used
        # in the old system and they break the build!  Disabling them
        # for now.
        #
        #extra_args += ['-a', addendum]
        #deps.append(addendum)
        pass
    return env.Command(target, deps,
                       [[
            env.get('PO4A_TRANSLATE', 'po4a-translate'),
            '-k', str(percent_translated),
            '-M', master_charset,
            '-f', format,
            '-m', master,
            '-p', pofile,
            '-l', '$TARGET'
            ] + extra_args])
