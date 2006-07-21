// cmdline_update.cc
//
//   Copyright (C) 2004-2006 Daniel Burrows
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of
//   the License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful,
//   but WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//   General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; see the file COPYING.  If not, write to
//   the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
//   Boston, MA 02111-1307, USA.

#include "cmdline_util.h"

#include <aptitude.h>

#include <generic/apt/apt.h>
#include <generic/apt/config_signal.h>
#include <generic/apt/download_update_manager.h>

#include <apt-pkg/error.h>
#include <apt-pkg/progress.h>

void print_autoclean_msg()
{
  printf(_("Deleting obsolete downloaded files\n"));
}

int cmdline_update(int argc, char *argv[])
{
  _error->DumpErrors();

  if(argc!=1)
    {
      fprintf(stderr, _("E: The update command takes no arguments\n"));
      return -1;
    }

  OpTextProgress progress(aptcfg->FindI("Quiet", 0));

  if(_error->PendingError())
    {
      _error->DumpErrors();
      return -1;
    }

  download_update_manager m;
  m.pre_autoclean_hook.connect(sigc::ptr_fun(print_autoclean_msg));
  int rval =
    (cmdline_do_download(&m) == download_manager::success ? 0 : -1);

  if(_error->PendingError())
    rval = -1;

  _error->DumpErrors();

  return rval;
}

