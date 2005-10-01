// apt_undo_group.cc

#include "apt_undo_group.h"

#include "apt.h"

void apt_undo_group::undo()
{
  (*apt_cache_file)->begin_action_group();

  undo_group::undo();

  // Discard changes (could be used to generate a "redo" item?)
  (*apt_cache_file)->end_action_group(NULL);
}
