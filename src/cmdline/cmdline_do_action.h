// cmdline_do_action.h                            -*-c++-*-
//
//  Copyright 2004 Daniel Burrows

#ifndef CMDLINE_DO_ACTION_H
#define CMDLINE_DO_ACTION_H

int cmdline_do_action(int argc, char *argv[],
		      const char *status_fname, bool simulate,
		      bool assume_yes, bool download_only, bool fix_broken,
		      bool showvers, bool showdeps, bool showsize,
		      bool visual_preview, bool always_prompt,
		      bool queue_only,
		      int verbose);

#endif // CMDLINE_DO_ACTION_H
