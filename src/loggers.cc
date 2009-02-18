/** \file loggers.cc */

//   Copyright (C) 2009 Daniel Burrows

//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.

//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.

//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#include "loggers.h"

using namespace log4cxx;

namespace aptitude
{
  LoggerPtr Loggers::getAptitudeDpkgStatusPipe()
  {
    return Logger::getLogger("aptitude.dpkg.statusPipe");
  }

  LoggerPtr Loggers::getAptitudeDpkgTerminal()
  {
    return Logger::getLogger("aptitude.dpkg.terminal");
  }

  LoggerPtr Loggers::getAptitudeDpkgTerminalInactivity()
  {
    return Logger::getLogger("aptitude.dpkg.terminal.inactivity");
  }

  LoggerPtr Loggers::getAptitudeGtkDashboardUpgradeResolver()
  {
    return Logger::getLogger("aptitude.gtk.dashboard.upgrade.resolver");
  }

  LoggerPtr Loggers::getAptitudeGtkResolver()
  {
    return Logger::getLogger("aptitude.gtk.resolver");
  }

  LoggerPtr Loggers::getAptitudeResolver()
  {
    return Logger::getLogger("aptitude.resolver");
  }

  LoggerPtr Loggers::getAptitudeResolverHints()
  {
    return Logger::getLogger("aptitude.resolver.hints");
  }

  LoggerPtr Loggers::getAptitudeResolverHintsCompare()
  {
    return Logger::getLogger("aptitude.resolver.hints.compare");
  }

  LoggerPtr Loggers::getAptitudeResolverHintsMatch()
  {
    return Logger::getLogger("aptitude.resolver.hints.match");
  }

  LoggerPtr Loggers::getAptitudeResolverHintsParse()
  {
    return Logger::getLogger("aptitude.resolver.hints.parse");
  }
 
  LoggerPtr Loggers::getAptitudeResolverSafeResolver()
  {
    return Logger::getLogger("aptitude.resolver.safeResolver");
  }
 
  LoggerPtr Loggers::getAptitudeResolverSafeResolverSetup()
  {
    return Logger::getLogger("aptitude.resolver.safeResolver.setup");
  }

  LoggerPtr Loggers::getAptitudeResolverScores()
  {
    return Logger::getLogger("aptitude.resolver.scores");
  }

  LoggerPtr Loggers::getAptitudeWhy()
  {
    return Logger::getLogger("aptitude.why");
  }

  LoggerPtr Loggers::getAptitudeWhyGtk()
  {
    return Logger::getLogger("aptitude.why.gtk");
  }
}
