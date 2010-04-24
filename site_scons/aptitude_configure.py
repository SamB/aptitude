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


# Contains the logic to configure aptitude.
from SCons.Script import DefaultEnvironment
from SCons import SConf
import os

import aptitude_configure_checks
import aptitude_configure_utils
from aptitude_configure_checks import TryInclude
from aptitude_configure_utils import RequireCheck

languages = [
    "ar",
    "ast",
    "bs",
    "ca",
    "cs",
    "da",
    "de",
    "dz",
    "el",
    "es",
    "eu",
    "fi",
    "fr",
    "gl",
    "hu",
    "it",
    "ja",
    "km",
    "ku",
    "lt",
    "mr",
    "nb",
    "ne",
    "nl",
    "nn",
    "pl",
    "pt",
    "pt_BR",
    "ro",
    "ru",
    "sk",
    "sv",
    "th",
    "tl",
    "tr",
    "uk",
    "vi",
    "zh_CN",
    "zh_TW",
    ]

def DoConfigure(env):
    """Configure the build environment 'env'."""
    conf = aptitude_configure_checks.Configure(env)

    RequireCheck(conf.CheckForNCursesW(tries = [ TryInclude('/usr/include'),
                                                 TryInclude('/usr/include/ncursesw') ]),
                 "Can't find libncursesw -- please install libncursesw5-dev.")
    RequireCheck(conf.CheckForApt(),
                 "Can't find the APT libraries -- please install libapt-pkg-dev.")
    RequireCheck(conf.CheckForPThread(),
                 "Can't find the POSIX thread libraries.")
    RequireCheck(conf.CheckForBoostIOStreams(),
                 "Can't find Boost.IOStreams")
    if conf.CheckDDTP():
        conf.Define('HAVE_DDTP', 1)

    conf.CheckForPo4A()
    aptitude_configure_checks.FindGettext(conf)

    conf.Define('SIGC_VERSION',
                         '"%s"' % os.popen('pkg-config --modversion sigc++-2.0').read().strip())

    aptitude_configure_utils.RunConfigureFinishHooks(conf)

    conf.Finish()
