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

import re

def NonHeaders(src):
    """Filter out headers from the given list of source files."""

    return [ f for f in src if not str(f).endswith('.h') ]

# This searches for either the start of the string or a non-backslash,
# followed by an even number of backslashes, and then a character that
# should be escaped.
cstring_toescapere = re.compile(r'(["\\\n\t\r\b])',
                                re.MULTILINE)

def CString(s):
    """Represent \"s\" as a C string constant."""

    def escapechar(c):
        """Escape a single character for a C string."""
        if c == '\n':
            return '\\n'
        elif c == '\t':
            return '\\t'
        elif c == '\r':
            return '\\r'
        elif c == '\b':
            return '\\b'
        else:
            return '\\%s' % c

    def processmatch(m):
        return escapechar(m.group(1))

    return '"%s"' % cstring_toescapere.sub(processmatch, s)

# Poor man's unit tests
assert(CString('abc') == '"abc"')
assert(CString('"abc"') == '"\\"abc\\""')
assert(CString('\\') == '"\\\\"')
assert(CString('\\\\') == '"\\\\\\\\"')
assert(CString('abc\ndef\\') == '"abc\\ndef\\\\"')

# This just backslash-escapes anything that isn't an alphanumeric
# character.
shellescape_toescapere = re.compile(r'([^a-zA-Z0-9])')

def ShellEscape(s):
    def processmatch(m):
        return '\\%s' % m.group(1)

    return shellescape_toescapere.sub(processmatch, s)

assert(ShellEscape('\\') == '\\\\')
assert(ShellEscape('"') == '\\"')
