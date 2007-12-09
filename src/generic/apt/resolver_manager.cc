// resolver_manager.cc
//
//   Copyright (C) 2005, 2007 Daniel Burrows
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

#include "resolver_manager.h"

#include "apt.h"
#include "aptitude_resolver.h"
#include "aptitude_resolver_universe.h"
#include "config_signal.h"
#include "dump_packages.h"

#include <generic/problemresolver/problemresolver.h>
#include <generic/util/undo.h>

#include <sigc++/functors/mem_fun.h>

class resolver_manager::resolver_interaction
{
public:
  /** \brief The type tag of a resolver interaction. */
  enum tag
    {
      reject_version,
      unreject_version,
      mandate_version,
      unmandate_version,
      harden_dep,
      unharden_dep,
      approve_broken_dep,
      unapprove_broken_dep,
      undo
    };

private:
  tag type;

  aptitude_resolver_version version;
  aptitude_resolver_dep dep;

  resolver_interaction(tag _type,
		       const aptitude_resolver_version &_version,
		       const aptitude_resolver_dep &_dep)
    : type(_type), version(_version), dep(_dep)
  {
  }

public:
  static resolver_interaction RejectVersion(const aptitude_resolver_version &version)
  {
    return resolver_interaction(reject_version, version,
				aptitude_resolver_dep());
  }

  static resolver_interaction UnRejectVersion(const aptitude_resolver_version &version)
  {
    return resolver_interaction(unreject_version, version,
				aptitude_resolver_dep());
  }

  static resolver_interaction MandateVersion(const aptitude_resolver_version &version)
  {
    return resolver_interaction(mandate_version, version,
				aptitude_resolver_dep());
  }

  static resolver_interaction UnMandateVersion(const aptitude_resolver_version &version)
  {
    return resolver_interaction(unmandate_version, version,
				aptitude_resolver_dep());
  }

  static resolver_interaction HardenDep(const aptitude_resolver_dep &dep)
  {
    return resolver_interaction(harden_dep,
				aptitude_resolver_version(),
				dep);
  }

  static resolver_interaction UnHardenDep(const aptitude_resolver_dep &dep)
  {
    return resolver_interaction(unharden_dep,
				aptitude_resolver_version(),
				dep);
  }

  static resolver_interaction ApproveBrokenDep(const aptitude_resolver_dep &dep)
  {
    return resolver_interaction(approve_broken_dep,
				aptitude_resolver_version(),
				dep);
  }

  static resolver_interaction UnApproveBrokenDep(const aptitude_resolver_dep &dep)
  {
    return resolver_interaction(unapprove_broken_dep,
				aptitude_resolver_version(),
				dep);
  }

  static resolver_interaction Undo()
  {
    return resolver_interaction(undo,
				aptitude_resolver_version(),
				aptitude_resolver_dep());
  }

  tag get_type() const { return type; }
  const aptitude_resolver_version &get_version() const
  {
    eassert(!version.get_pkg().end());
    return version;
  }
  const aptitude_resolver_dep &get_dep() const
  {
    eassert(!dep.get_dep().end());
    return dep;
  }
};

// NB: we need a recursive mutex because some routines can be called
// either by other routines of the class (already have a mutex lock)
// or by the user (don't have a mutex lock); I could sidestep this
// with some clever magic, but there's no point unless it turns out to
// be a bottleneck.
resolver_manager::resolver_manager(aptitudeDepCache *_cache)
  :cache(_cache),
   resolver(NULL),
   undos(new undo_list),
   solution_search_aborted(false),
   selected_solution(0),
   background_thread_killed(false),
   background_thread_running(false),
   resolver_null(true),
   background_thread_suspend_count(0),
   background_thread_in_resolver(false),
   resolver_thread(NULL),
   mutex(cwidget::threads::mutex::attr(PTHREAD_MUTEX_RECURSIVE))
{
  cache->pre_package_state_changed.connect(sigc::mem_fun(this, &resolver_manager::discard_resolver));
  cache->package_state_changed.connect(sigc::mem_fun(this, &resolver_manager::maybe_create_resolver));

  aptcfg->connect(PACKAGE "::Recommends-Important",
		  sigc::mem_fun(this,
				&resolver_manager::discard_resolver));

  start_background_thread();

  maybe_create_resolver();
}

resolver_manager::~resolver_manager()
{
  eassert(background_thread_running);

  discard_resolver();

  kill_background_thread();

  for(unsigned int i = 0; i < solutions.size(); ++i)
    {
      delete solutions[i].first;
      delete solutions[i].second;
    }

  delete undos;
}

void resolver_manager::reset_resolver()
{
  discard_resolver();
  maybe_create_resolver();
}

resolver_manager::background_continuation::~background_continuation()
{
}

class resolver_manager::background_suspender
{
  resolver_manager &m;

  bool suspended;
public:
  background_suspender(resolver_manager &_m)
    :m(_m), suspended(true)
  {
    m.suspend_background_thread();
  }

  void unsuspend()
  {
    if(suspended)
      {
	m.unsuspend_background_thread();
	suspended = false;
      }
  }

  ~background_suspender()
  {
    if(suspended)
      m.unsuspend_background_thread();
  }
};

/** A class that assigns a value to an object when it is destroyed.
 */
template<typename T>
class set_when_destroyed
{
  T &target;
  T val;

public:
  /** Create a set_when_destroyed.
   *
   *  \param _target The object to be set.
   *  \param _val The value to assign to _target.
   */
  set_when_destroyed(T &_target, const T &_val)
    : target(_target), val(_val)
  {
  }

  /** Assign val to target. */
  ~set_when_destroyed()
  {
    target = val;
  }
};

void resolver_manager::dump_visited_packages(const std::set<aptitude_resolver_package> &visited_packages)
{
  if(resolver_trace_dir.empty())
    return;

  std::set<pkgCache::PkgIterator> packages;
  for(std::set<aptitude_resolver_package>::const_iterator it = visited_packages.begin();
      it != visited_packages.end(); ++it)
    {
      packages.insert((*it).get_pkg());
    }

  aptitude::apt::make_truncated_state_copy(resolver_trace_dir, packages);
}

// This assumes that background_resolver_active is empty when it
// starts (see restart_background_resolver)
//
// FIXME: max_steps should be changed when the configuration is (not a
// visible bug at the moment since you can't change that option
// interactively)
void resolver_manager::background_thread_execution()
{
  std::set<aptitude_resolver_package> visited_packages;

  cwidget::threads::mutex::lock l(background_control_mutex);
  set_when_destroyed<bool> cancel_set_running(background_thread_running, false);

  while(1)
    {
      while((background_thread_suspend_count > 0 || resolver_null || pending_jobs.empty()) &&
	    !background_thread_killed)
	background_control_cond.wait(l);

      if(background_thread_killed)
	break;

      job_request job = pending_jobs.top();
      pending_jobs.pop();
      background_thread_in_resolver = true;
      background_resolver_cond.wake_all();
      l.release();

      try
	{
	  aptitude_resolver::solution *sol = do_get_solution(job.max_steps,
							     job.sol_num,
							     visited_packages);

	  // Set the state variable BEFORE exiting the resolver; this
	  // is done so that if there are no more jobs, the foreground
	  // thread sees that we're out of the resolver when it
	  // examines the solution.
	  l.acquire();
	  dump_visited_packages(visited_packages);
	  background_thread_in_resolver = false;
	  background_resolver_cond.wake_all();
	  l.release();

	  job.k->success(*sol);
	}
      catch(InterruptedException)
	{
	  // Put it back into the pot.
	  l.acquire();
	  dump_visited_packages(visited_packages);
	  background_thread_in_resolver = false;
	  background_resolver_cond.wake_all();
	  pending_jobs.push(job);

	  // HACK: protect job.k from deletion.
	  job.k = NULL;
	  l.release();
	}
      catch(NoMoreSolutions)
	{
	  l.acquire();
	  dump_visited_packages(visited_packages);
	  background_thread_in_resolver = false;
	  background_resolver_cond.wake_all();
	  l.release();

	  job.k->no_more_solutions();
	}
      catch(NoMoreTime)
	{
	  l.acquire();
	  dump_visited_packages(visited_packages);
	  background_thread_in_resolver = false;
	  background_resolver_cond.wake_all();
	  l.release();

	  job.k->no_more_time();
	}
      catch(cwidget::util::Exception &e)
	{
	  dump_visited_packages(visited_packages);
	  job.k->aborted(e);
	}

      l.acquire();
      delete job.k;

      // If the user asked us to, dump out the 

      background_thread_in_resolver = false;
      background_resolver_cond.wake_all();
    }
}

// Need this because sigc slots aren't threadsafe :-(
struct resolver_manager::background_thread_bootstrap
{
  resolver_manager &m;
public:
  background_thread_bootstrap(resolver_manager &_m)
    :m(_m)
  {
  }

  void operator()()
  {
    m.background_thread_execution();
  }
};

void resolver_manager::start_background_thread()
{
  cwidget::threads::mutex::lock l(mutex);

  if(resolver_thread == NULL)
    {
      background_thread_running = true;
      resolver_thread = new cwidget::threads::thread(background_thread_bootstrap(*this));
    }
}

void resolver_manager::kill_background_thread()
{
  cwidget::threads::mutex::lock l(mutex);

  if(resolver_thread != NULL)
    {
      cwidget::threads::mutex::lock control_lock(background_control_mutex);

      if(resolver != NULL)
	resolver->cancel_solver();
      background_thread_killed = true;
      background_control_cond.wake_all();

      control_lock.release();

      resolver_thread->join();
      delete resolver_thread;
      resolver_thread = NULL;


      // Reset the associated data structures.
      control_lock.acquire();
      pending_jobs = std::priority_queue<job_request, std::vector<job_request>, job_request_compare>();
      background_thread_killed = false;
      background_thread_suspend_count = 0;
      background_thread_in_resolver = false;
      solution_search_aborted = false;
      solution_search_abort_msg.clear();
    }
}

void resolver_manager::suspend_background_thread()
{
  cwidget::threads::mutex::lock l(mutex);

  // May occur due to background_suspend objects existing while
  // kill_background_thread runs.
  if(resolver_thread == NULL)
    return;

  cwidget::threads::mutex::lock control_lock(background_control_mutex);

  if(resolver != NULL)
    resolver->cancel_solver();

  ++background_thread_suspend_count;
  background_control_cond.wake_all();

  while(background_thread_in_resolver)
    background_resolver_cond.wait(control_lock);

  if(resolver != NULL)
    resolver->uncancel_solver();
}

void resolver_manager::unsuspend_background_thread()
{
  cwidget::threads::mutex::lock l(mutex);

  if(resolver_thread == NULL)
    return;

  cwidget::threads::mutex::lock control_lock(background_control_mutex);

  eassert(background_thread_suspend_count > 0);
  --background_thread_suspend_count;
  background_control_cond.wake_all();
}

void resolver_manager::maybe_create_resolver()
{
  cwidget::threads::mutex::lock l(mutex);

  if(resolver == NULL && cache->BrokenCount() > 0)
    {
      {
	cwidget::threads::mutex::lock l(background_control_mutex);
	resolver_trace_dir = aptcfg->Find(PACKAGE "::ProblemResolver::Trace-Directory", "");
      }
      create_resolver();
    }

  // Always signal a state change: we are signalling for the whole
  // discard/create pair, and even if we didn't create a new resolver
  // we have to inform the listeners that the old one went away
  // (maybe).
  l.release();
  state_changed();
}

void resolver_manager::discard_resolver()
{
  cwidget::threads::mutex::lock l(mutex);

  if(resolver == NULL)
    return;

  background_suspender bs(*this);

  undos->clear_items();

  delete resolver;

  {
    cwidget::threads::mutex::lock l2(solutions_mutex);
    solutions.clear();
    solution_search_aborted = false;
    solution_search_abort_msg.clear();
    selected_solution = 0;
  }

  resolver = NULL;

  {
    cwidget::threads::mutex::lock l2(background_control_mutex);
    resolver_null = true;
    while(!pending_jobs.empty())
      {
	delete pending_jobs.top().k;
	pending_jobs.pop();
      }
    background_control_cond.wake_all();
  }
}

void resolver_manager::create_resolver()
{
  cwidget::threads::mutex::lock l(mutex);
  eassert(resolver == NULL);

  // NOTE: the performance of the resolver is highly sensitive to
  // these settings; choosing bad ones can result in hitting
  // exponential cases in practical situations.  In general,
  // penalizing actions means that the resolver will be more likely to
  // blow up trying to avoid them, with the danger increasing as the
  // penalty does.  Thus, aside from broken deps (which are penalized
  // to guide us towards a solution), I only penalize removals (which
  // are usually either unnecessary or easy to prove necessary) and
  // leaving soft dependencies (recommendations) unfixed.  The
  // relative penalties of these are also important; for instance,
  // penalizing unfixed soft deps more than removals means that the
  // resolver will actually remove packages rather than leaving their
  // Recommends: field unsatisfied!

  resolver=new aptitude_resolver(aptcfg->FindI(PACKAGE "::ProblemResolver::StepScore", 70),
				 aptcfg->FindI(PACKAGE "::ProblemResolver::BrokenScore", -100),
				 aptcfg->FindI(PACKAGE "::ProblemResolver::UnfixedSoftScore", -200),
				 aptcfg->FindI(PACKAGE "::ProblemResolver::Infinity", 1000000),
				 aptcfg->FindI(PACKAGE "::ProblemResolver::Max-Successors", 0),
				 aptcfg->FindI(PACKAGE "::ProblemResolver::ResolutionScore", 50),
				 cache);

  resolver->add_action_scores(aptcfg->FindI(PACKAGE "::ProblemResolver::PreserveManualScore", 60),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::PreserveAutoScore", 0),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::RemoveScore", -300),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::KeepScore", 0),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::InstallScore", -20),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::UpgradeScore", 0),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::NonDefaultScore", -40),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::EssentialRemoveScore", -100000),
			      aptcfg->FindI(PACKAGE "::ProblemResolver::BreakHoldScore", -300));

  resolver->add_priority_scores(aptcfg->FindI(PACKAGE "::ProblemResolver::ImportantScore", 5),
				aptcfg->FindI(PACKAGE "::ProblemResolver::RequiredScore", 4),
				aptcfg->FindI(PACKAGE "::ProblemResolver::StandardScore", 3),
				aptcfg->FindI(PACKAGE "::ProblemResolver::OptionalScore", 1),
				aptcfg->FindI(PACKAGE "::ProblemResolver::ExtraScore", -1));

  {
    cwidget::threads::mutex::lock l2(background_control_mutex);
    resolver_null = false;
    background_control_cond.wake_all();
  }
}

void resolver_manager::set_resolver_trace_dir(const std::string &path)
{
  cwidget::threads::mutex::lock l(background_control_mutex);
  resolver_trace_dir = path;
}

void resolver_manager::set_debug(bool activate)
{
  cwidget::threads::mutex::lock l(mutex);
  background_suspender bs(*this);

  eassert(resolver_exists());

  resolver->set_debug(activate);
}

bool resolver_manager::resolver_exists() const
{
  cwidget::threads::mutex::lock l(mutex);

  return resolver != NULL;
}

unsigned int resolver_manager::generated_solution_count() const
{
  cwidget::threads::mutex::lock l(mutex);
  cwidget::threads::mutex::lock l2(solutions_mutex);

  return solutions.size();
}

bool resolver_manager::solution_generation_complete() // const
{
  return state_snapshot().solutions_exhausted;
}

bool resolver_manager::solutions_at_start() const
{
  cwidget::threads::mutex::lock l(mutex);

  if(!resolver_exists())
    return true;
  else
    return selected_solution == 0;
}

bool resolver_manager::background_thread_active()
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock ctl_l(background_control_mutex);

  return !pending_jobs.empty() || background_thread_in_resolver;
}

bool resolver_manager::background_thread_aborted()
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock sol_l(solutions_mutex);

  return solution_search_aborted;
}

std::string resolver_manager::background_thread_abort_msg()
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock sol_l(solutions_mutex);

  return solution_search_aborted ? solution_search_abort_msg : "";
}

resolver_manager::state resolver_manager::state_snapshot()
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock ctl_l(background_control_mutex);

  cwidget::threads::mutex::lock sol_l(solutions_mutex);

  state rval;

  rval.selected_solution           = selected_solution;
  rval.generated_solutions         = solutions.size();
  rval.resolver_exists             = (resolver != NULL);
  rval.background_thread_active    = !solution_search_aborted &&
                                        (!pending_jobs.empty() ||
				         background_thread_in_resolver);
  rval.background_thread_aborted   = solution_search_aborted;
  rval.background_thread_abort_msg = solution_search_abort_msg;

  if(resolver != NULL)
    {
      aptitude_resolver::queue_counts c = resolver->get_counts();

      rval.open_size      = c.open;
      rval.closed_size    = c.closed;
      rval.deferred_size  = c.deferred;
      rval.conflicts_size = c.conflicts;
      rval.solutions_exhausted = (rval.open_size == 0 && c.finished);
    }
  else
    {
      rval.open_size      = 0;
      rval.closed_size    = 0;
      rval.deferred_size  = 0;
      rval.conflicts_size = 0;

      rval.solutions_exhausted = false;
    }

  return rval;
}

aptitude_resolver::solution *resolver_manager::do_get_solution(int max_steps, unsigned int solution_num,
							       std::set<aptitude_resolver_package> &visited_packages)
{
  cwidget::threads::mutex::lock sol_l(solutions_mutex);
  if(solution_num < solutions.size())
    return solutions[solution_num].second;

  while(solution_num >= solutions.size())
    {
      sol_l.release();

      try
	{
	  generic_solution<aptitude_universe> sol = resolver->find_next_solution(max_steps, &visited_packages);

	  sol_l.acquire();
	  solutions.push_back(std::make_pair(new std::vector<resolver_interaction>(actions_since_last_solution),
					     new aptitude_resolver::solution(sol.clone())));
	  actions_since_last_solution.clear();
	  sol_l.release();
	}
      catch(NoMoreTime)
	{
	  throw NoMoreTime();
	}
      catch(NoMoreSolutions)
	{
	  throw NoMoreSolutions();
	}
      catch(cwidget::util::Exception &e)
	{
	  sol_l.acquire();
	  solution_search_aborted = true;
	  solution_search_abort_msg = e.errmsg();
	  throw;
	}
    }

  return solutions[solution_num].second;
}

/** A continuation that works by either placing \b true in the Boolean
 *  variable corresponding to the thrown exception, or updating the
 *  given solution, then signalling the given condition.  If the
 *  search terminated with an exception, we set the output solution to
 *  NULL and set abort_msg to the exception's description.
 */
class solution_return_continuation : public resolver_manager::background_continuation
{
  const generic_solution<aptitude_universe> * &sol;
  std::string &abort_msg;
  bool &oot;
  bool &oos;
  cwidget::threads::mutex &m;
  cwidget::threads::condition &c;
public:
  solution_return_continuation(const generic_solution<aptitude_universe> * &_sol,
			       std::string &_abort_msg,
			       bool &_oot,
			       bool &_oos,
			       cwidget::threads::mutex &_m,
			       cwidget::threads::condition &_c)
    :sol(_sol), abort_msg(_abort_msg), oot(_oot), oos(_oos), m(_m), c(_c)
  {
  }

  void success(const generic_solution<aptitude_universe> &result)
  {
    cwidget::threads::mutex::lock l(m);

    sol = &result;
    c.wake_all();
  }

  void no_more_time()
  {
    cwidget::threads::mutex::lock l(m);

    oot = true;
    c.wake_all();
  }

  void no_more_solutions()
  {
    cwidget::threads::mutex::lock l(m);
    oos = true;
    c.wake_all();
  }

  void interrupted()
  {
    // Should never happen, since we hold the big lock.
    abort();
  }

  void aborted(const cwidget::util::Exception &e)
  {
    cwidget::threads::mutex::lock l(m);

    sol = NULL;
    abort_msg = e.errmsg();
    c.wake_all();
  }
};

/** \brief Sadly, we can't easily save the real exception and reuse it
 *  :(.
 */
class SolutionSearchAbortException : public cwidget::util::Exception
{
  std::string msg;

public:
  SolutionSearchAbortException(const std::string &_msg)
  {
    msg = _msg;
  }

  std::string errmsg() const
  {
    return msg;
  }
};

const aptitude_resolver::solution &resolver_manager::get_solution(unsigned int solution_num,
								  int max_steps)
{
  cwidget::threads::mutex::lock l(mutex);

  eassert(resolver);

  {
    cwidget::threads::mutex::lock l2(solutions_mutex);
    if(solution_num < solutions.size())
      return *solutions[solution_num].second;
  }


  const generic_solution<aptitude_universe> *sol = NULL;
  std::string abort_msg;
  bool oot = false;
  bool oos = false;
  cwidget::threads::mutex m;
  cwidget::threads::condition c;

  get_solution_background(solution_num, max_steps, new solution_return_continuation(sol, abort_msg, oot, oos, m, c));
  l.release();

  cwidget::threads::mutex::lock cond_l(m);

  while(!sol && !oot && !oos)
    c.wait(cond_l);

  if(oot)
    throw NoMoreTime();
  else if(oos)
    throw NoMoreSolutions();
  else if(sol == NULL)
    throw SolutionSearchAbortException(abort_msg);

  return *sol;
}

void resolver_manager::get_solution_background(unsigned int solution_num,
					       int max_steps,
					       background_continuation *k)
{
  cwidget::threads::mutex::lock l(mutex);

  // It's necessary to stop the background thread because we might be
  // decreasing the maximum number of steps to search.
  background_suspender bs(*this);

  eassert(resolver_exists());

  cwidget::threads::mutex::lock sol_l(solutions_mutex);
  if(solution_num < solutions.size())
    {
      generic_solution<aptitude_universe> *sol = solutions[solution_num].second;
      sol_l.release();

      k->success(*sol);
      return;
    }
  sol_l.release();


  cwidget::threads::mutex::lock control_lock(background_control_mutex);
  pending_jobs.push(job_request(solution_num, max_steps, k));
  background_control_cond.wake_all();
}

class blocking_continuation : public resolver_manager::background_continuation
{
  /** The real continuation */
  resolver_manager::background_continuation *k;

  /** The solution for which we are searching. */
  unsigned int solution_num;

  /** The channel through which the result should be announced. */
  cwidget::threads::box<bool> &result_box;

  /** The number of steps to try searching for a solution after this if
   *  time runs out.
   */
  int remaining_steps;

  /** The manager associated with this continuation. */
  resolver_manager &m;
public:
  blocking_continuation(background_continuation *_k,
			unsigned int _solution_num,
			cwidget::threads::box<bool> &_result_box,
			int _remaining_steps,
			resolver_manager &_m)
    : k(_k), solution_num(_solution_num), result_box(_result_box),
      remaining_steps(_remaining_steps), m(_m)
  {
  }

  ~blocking_continuation()
  {
    delete k;
  }

  void success(const generic_solution<aptitude_universe> &sol)
  {
    k->success(sol);
    result_box.put(true);
  }

  void no_more_solutions()
  {
    k->no_more_solutions();
    result_box.put(true);
  }

  void no_more_time()
  {
    m.get_solution_background(solution_num, remaining_steps, k);
    k = NULL;
    result_box.put(false);
  }

  void interrupted()
  {
    result_box.put(false);
  }

  void aborted(const cwidget::util::Exception &e)
  {
    k->aborted(e);
    result_box.put(true);
  }
};

bool resolver_manager::get_solution_background_blocking(unsigned int solution_num,
							int max_steps,
							int block_steps,
							background_continuation *k)
{
  if(block_steps == 0)
    {
      get_solution_background(solution_num, max_steps, k);
      return false;
    }

  int remaining;

  if(block_steps < max_steps)
    remaining = max_steps - block_steps;
  else
    remaining = 0;

  cwidget::threads::box<bool> rbox;

  get_solution_background(solution_num, block_steps,
			  new blocking_continuation(k, solution_num, rbox,
						    remaining, *this));

  return rbox.take();
}

template<typename T>
void resolver_manager::resolver_manipulation(const T &t,
					     void (generic_problem_resolver<aptitude_universe>::*action)(const T &, undo_group *),
					     const resolver_interaction &act)
{
  cwidget::threads::mutex::lock l(mutex);
  background_suspender bs(*this);

  undo_group *undo = new undo_group;
  (resolver->*action)(t, undo);
  if(undo->empty())
    delete undo;
  else
    undos->add_item(undo);

  actions_since_last_solution.push_back(act);

  l.release();
  bs.unsuspend();
  state_changed();
}

void resolver_manager::reject_version(const aptitude_resolver_version &ver)
{
  resolver_manipulation(ver, &aptitude_resolver::reject_version,
			resolver_interaction::RejectVersion(ver));
}

void resolver_manager::unreject_version(const aptitude_resolver_version &ver)
{
  resolver_manipulation(ver, &aptitude_resolver::unreject_version,
			resolver_interaction::UnRejectVersion(ver));
}

bool resolver_manager::is_rejected(const aptitude_resolver_version &ver)
{
  cwidget::threads::mutex::lock l(mutex);
  eassert(resolver);

  return resolver->is_rejected(ver);
}

void resolver_manager::mandate_version(const aptitude_resolver_version &ver)
{
  resolver_manipulation(ver, &aptitude_resolver::mandate_version,
			resolver_interaction::MandateVersion(ver));
}

void resolver_manager::unmandate_version(const aptitude_resolver_version &ver)
{
  resolver_manipulation(ver, &aptitude_resolver::unmandate_version,
			resolver_interaction::UnMandateVersion(ver));
}

bool resolver_manager::is_mandatory(const aptitude_resolver_version &ver)
{
  cwidget::threads::mutex::lock l(mutex);
  eassert(resolver);

  return resolver->is_mandatory(ver);
}

void resolver_manager::harden_dep(const aptitude_resolver_dep &dep)
{
  resolver_manipulation(dep, &aptitude_resolver::harden,
			resolver_interaction::HardenDep(dep));
}

void resolver_manager::unharden_dep(const aptitude_resolver_dep &dep)
{
  resolver_manipulation(dep, &aptitude_resolver::unharden,
			resolver_interaction::UnHardenDep(dep));
}

bool resolver_manager::is_hardened(const aptitude_resolver_dep &dep)
{
  cwidget::threads::mutex::lock l(mutex);
  eassert(resolver);

  return resolver->is_hardened(dep);
}

void resolver_manager::approve_broken_dep(const aptitude_resolver_dep &dep)
{
  resolver_manipulation(dep, &aptitude_resolver::approve_break,
			resolver_interaction::ApproveBrokenDep(dep));
}

void resolver_manager::unapprove_broken_dep(const aptitude_resolver_dep &dep)
{
  resolver_manipulation(dep, &aptitude_resolver::unapprove_break,
			resolver_interaction::UnApproveBrokenDep(dep));
}

bool resolver_manager::is_approved_broken(const aptitude_resolver_dep &dep)
{
  cwidget::threads::mutex::lock l(mutex);
  eassert(resolver != NULL);

  return resolver->is_approved_broken(dep);
}

bool resolver_manager::has_undo_items()
{
  cwidget::threads::mutex::lock l(mutex);

  return undos->size() > 0;
}

bool resolver_manager::undo()
{
  cwidget::threads::mutex::lock l(mutex);

  if(undos->size() > 0)
    {
      background_suspender bs(*this);

      undos->undo();

      actions_since_last_solution.push_back(resolver_interaction::Undo());

      bs.unsuspend();
      l.release();

      state_changed();

      return true;
    }
  else
    return false;
}

void resolver_manager::select_solution(unsigned int solnum)
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock sol_l(solutions_mutex);
  if(solnum >= 0 && solnum <= solutions.size())
    selected_solution = solnum;
  sol_l.release();

  l.release();
  state_changed();
}

void resolver_manager::discard_error_information()
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock sol_l(solutions_mutex);

  solution_search_aborted = false;
  solution_search_abort_msg.clear();

  sol_l.release();
  l.release();

  state_changed();
}

void resolver_manager::select_next_solution()
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock sol_l(solutions_mutex);
  if(selected_solution < solutions.size())
    ++selected_solution;
  sol_l.release();

  l.release();
  state_changed();
}

void resolver_manager::select_previous_solution()
{
  cwidget::threads::mutex::lock l(mutex);

  cwidget::threads::mutex::lock sol_l(solutions_mutex);
  if(selected_solution > 0)
    --selected_solution;
  sol_l.release();

  l.release();
  state_changed();
}

void resolver_manager::tweak_score(const pkgCache::PkgIterator &pkg,
				   const pkgCache::VerIterator &ver,
				   int score)
{
  cwidget::threads::mutex::lock l(mutex);
  background_suspender bs(*this);

  eassert(resolver_exists());
  eassert(resolver->fresh());

  resolver->add_version_score(aptitude_resolver_version(pkg, ver, cache),
			      score);
}

void resolver_manager::dump(ostream &out)
{
  cwidget::threads::mutex::lock l(mutex);
  background_suspender bs(*this);

  if(!resolver_exists())
    return;

  // First, dump the universe.
  dump_universe(resolver->get_universe(), out);

  // Now dump the scores as a test instruction.
  out << "TEST " << resolver->get_step_score() << " "
      << resolver->get_broken_score() << " "
      << resolver->get_unresolved_soft_dep_score() << " "
      << resolver->get_infinity() << " "
      << resolver->get_max_successors() << " "
      << resolver->get_full_solution_score() << " ";

  resolver->dump_scores(out);

  out << "EXPECT ( " << aptcfg->FindI(PACKAGE "::Resolver::StepLimit", 5000) << " ANY )" << std::endl;
}
