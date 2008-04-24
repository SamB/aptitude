// tasks.h             -*-c++-*-
//
//  Copyright 2001 Daniel Burrows
//

#include <string>
#include <set>
#include <map>
#include <apt-pkg/pkgcache.h>

/** \brief Handles parsing the list of tasks and getting the task of a given
 *  package.
 * 
 *  \file tasks.h
 */

class OpProgress;

class task
{
private:
  bool keys_present_cache;
  bool keys_present_cache_stale;
public:
  task():keys_present_cache(false), keys_present_cache_stale(true) {}

  std::string name;
  std::string section;
  std::string shortdesc;
  std::string longdesc;
  std::set<std::string> keys;

  bool keys_present();

  int relevance;
};

/** \brief Get the set of tasks associated with the given package.
 *
 *  The caller should not delete this set; it's managed internally by
 *  the tasks module.
 */
std::set<std::string> *get_tasks(const pkgCache::PkgIterator &pkg);

// Stores the various tasks.
extern std::map<std::string, task> *task_list;

// (re)loads in the current list of available tasks.  Necessary after a
// cache reload, for obvious reasons.  apt_reload_cache will call this.
void load_tasks(OpProgress &progress);

// Discards the current task list and readies a new one to be loaded.
// Since the task list contains package iterators, we have to do something
// in case they're still hanging around.
void reset_tasks();
