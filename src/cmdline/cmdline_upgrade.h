// cmdline_upgrade.h                        -*-c++-*-
//
//  Copyright 2004 Daniel Burrows

#ifndef CMDLINE_UPGRADE_H
#define CMDLINE_UPGRADE_H

int cmdline_upgrade(int argc, char *argv[],
		    const char *status_fname, bool simulate,
		    bool assume_yes, bool download_only,
		    bool showvers, bool showdeps, bool showsize,
		    bool visual_preview, bool always_prompt,
		    bool queue_only, int verbose);

#endif // CMDLINE_UPGRADE_H
