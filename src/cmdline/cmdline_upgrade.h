// cmdline_upgrade.h                        -*-c++-*-
//
//  Copyright 2004 Daniel Burrows

#ifndef CMDLINE_UPGRADE_H
#define CMDLINE_UPGRADE_H

#include "cmdline_util.h"

#include <vector>

/** \file cmdline_upgrade.h
 */

int cmdline_upgrade(int argc, char *argv[],
		    const char *status_fname, bool simulate,
		    bool no_new_installs, bool show_resolver_actions,
		    bool assume_yes, bool download_only,
		    bool showvers, bool showdeps,
		    bool showsize, bool showwhy,
		    const std::vector<aptitude::cmdline::tag_application> &user_tags,
		    bool visual_preview, bool always_prompt,
		    bool arch_only, bool queue_only,
		    int verbose);

#endif // CMDLINE_UPGRADE_H
