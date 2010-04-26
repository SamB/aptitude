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

import errno
import xml.parsers.expat
from SCons.Script import File, Scanner

def exists():
    return True

def generate(env):
    env.Append(SCANNERS = XMLExternalEntitiesScanner)

def XMLExternalEntities(node, env, path):

    result = []

    class Finished:
        pass

    def entity_decl(entityName,
                    is_parameter_entity,
                    value,
                    base,
                    systemId,
                    publicId,
                    notationName):
        if value is None:
            result.append(systemId)

    def start_element(name, attrs):
        raise Finished()

    try:
        infile = file(node.path)
    except EnvironmentError, e:
        if e.errno == errno.ENOENT:
            return []
        else:
            raise

    try:
        parser = xml.parsers.expat.ParserCreate()
        parser.StartElementHandler = start_element
        parser.EntityDeclHandler = entity_decl

        parser.ParseFile(infile)
    except Finished:
        pass
    except xml.parsers.expat.ExpatError, e:
        print e
        # Probably an XML syntax error -- don't blow up the scanner
        # for that!
        pass
    finally:
        infile.close()

    return result

XMLExternalEntitiesScanner = Scanner(function = XMLExternalEntities,
                                     skeys = [ '.xml' ],
                                     recursive = True,
                                     )
