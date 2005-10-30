// resolver_manager.h                                -*-c++-*-
//
//   Copyright (C) 2005 Daniel Burrows
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
//
// A higher-level resolver interface.  This code is responsible for
// maintaining a list of previously observed solutions, for passing
// certain actions on to the underlying resolver (protecting users
// from having to actually import the whole resolver definition), for
// managing the resolver in the face of cache reloads and resets, and
// for managing threaded access to the resolver.

#ifndef RESOLVER_MANAGER_H
#define RESOLVER_MANAGER_H

#include <generic/util/exception.h>
#include <generic/util/threads.h>

#include <apt-pkg/pkgcache.h>

#include <sigc++/signal.h>
#include <sigc++/trackable.h>

#include <queue>
#include <vector>

class aptitudeDepCache;
class aptitude_universe;
class aptitude_resolver_version;
class aptitude_resolver_dep;
template<typename PackageUniverse> class generic_solution;
template<typename PackageUniverse> class generic_problem_resolver;
class aptitude_resolver;
class undo_group;
class undo_list;

/** Manages a resolver for a single cache object.  When broken
 *  packages arise, a new resolver is created; whenever the state of a
 *  package changes, the resolver is deleted and reset.  While a
 *  resolver is active, users of this class can "select" a particular
 *  solution, then "generate" it.
 *
 *  Solutions can be generated in a background thread, but of course
 *  only one background thread may be running at a time.  The
 *  solutions returned should only be accessed from one thread at a
 *  time unless you clone() them.
 *
 *  Note: of course it would also be possible to simply query the
 *  manager for the Nth solution; however, using a selection pointer
 *  makes it easy for different UI modules to share information about
 *  the currently selected solution.
 */
class resolver_manager : public sigc::trackable
{
public:
  /** This class represents the continuation of get_solution() in a
   *  background thread.  See get_background_solution() for details.
   */
  class background_continuation
  {
  public:
    virtual ~background_continuation();

    /** Invoked when a solution has been successfully generated. */
    virtual void success(const generic_solution<aptitude_universe> &) = 0;
    /** Invoked when all solutions have been exhausted (corresponds to
     *  the NoMoreSolutions exception).
     */
    virtual void no_more_solutions() = 0;

    /** Invoked when time has expired. (corresponds to NoMoreTime) */
    virtual void no_more_time() = 0;

    /** Invoked when the solver was interrupted. (corresponds to
     *  InterruptedException)
     */
    virtual void interrupted() = 0;
  };

  /** A snapshot of the state of the resolver. */
  struct state
  {
    /** The currently selected solution. */
    int selected_solution;

    /** The number of already-generated solutions. */
    int generated_solutions;

    /** If \b true, then there are no more solutions to generate. */
    bool solutions_exhausted;

    /** If \b true, then the resolver is not \b null (i.e., it exists;
     *  i.e., there are broken packages).
     */
    bool resolver_exists;

    /** If \b true, the background thread has jobs. */
    bool background_thread_active;

    /** The size of the resolver's open queue. */
    size_t open_size;

    /** The size of the resolver's closed queue. */
    size_t closed_size;

    /** The size of the resolver's deferred queue. */
    size_t deferred_size;

    /** The number of conflicts discovered by the resolver. */
    size_t conflicts_size;
  };

private:
  /** Information about a single request posted to the background
   *  thread.
   */
  struct job_request
  {
    /** The solution number to be calculated. */
    int sol_num;

    /** The number of steps to allow for this calculation. */
    int max_steps;

    /** The continuation of this computation. */
    background_continuation *k;

    job_request(int _sol_num, int _max_steps, background_continuation *_k)
      :sol_num(_sol_num), max_steps(_max_steps), k(_k)
    {
    }
  };

  /** Sort job requests by their solution number and step count. */
  struct job_request_compare
  {
    bool operator()(const job_request &jr1, const job_request &jr2) const
    {
      return jr1.sol_num < jr2.sol_num ||
	(jr1.sol_num == jr2.sol_num && jr1.max_steps < jr2.max_steps);
    }
  };

  /** The cache file on which this manager operates. */
  aptitudeDepCache *cache;

  /** The active resolver, or \b NULL if none is active. */
  aptitude_resolver *resolver;

  /** An undo list for resolver-specific items.  This is cleared
   *  whenever the resolver is discarded.
   */
  undo_list *undos;

  /** The solutions generated by this manager since the last change to
   *  the cache.
   */
  std::vector<generic_solution<aptitude_universe> *> solutions;

  /** A lock for the list of solutions; used to allow the background
   *  thread to immediately post results without taking the big class
   *  lock (since that might be taken by stop_background_resolver())
   */
  mutable threads::mutex solutions_mutex;

  /** The index of the currently selected solution. */
  unsigned int selected_solution;

  /** The pending job requests for the background thread.
   */
  std::priority_queue<job_request, std::vector<job_request>,
		      job_request_compare> pending_jobs;

  /** If \b true, the background thread should abort its execution. */
  bool background_thread_killed;

  /** If \b true, the background thread is currently running. */
  bool background_thread_running;

  /** If \b true, the resolver is \b NULL.  (this is used rather than
   *  checking the variable directly in order to make it painfully
   *  clear what the proper locking protocol is)
   */
  bool resolver_null;

  /** The number of times the background thread has been suspended; it
   *  will only be allowed to run if this value is 0.
   */
  int background_thread_suspend_count;

  /** If \b true, the background thread is currently running in the
   *  resolver; this indicates that foreground threads trying to
   *  suspend the background thread should wait on
   *  background_in_resolver_cond until this becomes \b false.
   */
  bool background_thread_in_resolver;

  /** A lock around pending_jobs, background_thread_killed,
   *  background_thread_suspend_count, background_thread_in_resolver,
   *  and resolver_null.
   */
  threads::mutex background_control_mutex;

  /** A condition signalled for pending_jobs,
   *  background_thread_killed, background_thread_suspend_count, and
   *  resolver_null.
   */
  threads::condition background_control_cond;

  /** A condition signalled for background_thread_in_resolver.
   */
  threads::condition background_resolver_cond;

  /** The thread in which a background resolver is running, or \b NULL
   *  if none is.
   */
  threads::thread *resolver_thread;

  /** This lock is used to serialize all accesses to this object,
   *  except background_get_solution().
   */
  mutable threads::mutex mutex;

  void discard_resolver();
  void create_resolver();

  /** A class that bootstraps the routine below. */
  class background_thread_bootstrap;
  friend class background_thread_bootstrap;

  /** A class that stops the background thread when it's created, and
   *  restarts it when it's destroyed.  If background_resolver_active
   *  is set to \b false in the meantime, the resolver won't be
   *  restarted.
   */
  class background_suspender;
  friend class background_suspender;

  /** Low-level code to get a solution; it does not take the global
   *  lock, does not stop a background thread, and may run either in
   *  the foreground or in the background.  It is called by
   *  background_thread_execution and get_solution.
   */
  generic_solution<aptitude_universe> *do_get_solution(int max_steps, unsigned int solution_number);

  /** The actual background thread. */
  void background_thread_execution();

  /** Start a background thread if none exists. */
  void start_background_thread();

  /** Destroy the background thread completely and reset its control
   *  parameters.  Waits until the thread has terminated to return.
   *
   *  If no thread exists, do nothing.
   *
   *  \warning This routine must only be invoked by the
   *  resolver_manager destructor; the resolver thread should survive
   *  until the resolver manager is destroyed.
   */
  void kill_background_thread();

  /** Increments the suspend count of the background thread, and (if
   *  necessary) interrupts a running resolution and waits for the
   *  thread to leave the resolver.
   */
  void suspend_background_thread();

  /** Decrements the suspend count of the background thread, and (if
   *  necessary) unsuspends it.
   */
  void unsuspend_background_thread();

  /** Create a resolver if necessary. */
  void maybe_create_resolver();

  /** Collects common code for the resolver manipulations such as
   *  reject_version, unreject_version, etc: locks this class,
   *  suspends the resolver, runs the manipulation and adds any undo
   *  that is generated to the undo list, and finally executes
   *  state_changed().
   */
  template<typename T>
  void resolver_manipulation(const T &t,
			     void (generic_problem_resolver<aptitude_universe>::*action)(const T &, undo_group *));
public:
  /** Create a new resolver manager for the given cache file. */
  resolver_manager(aptitudeDepCache *cache);

  virtual ~resolver_manager();

  /** If \b true, then a resolver has been created, indicating that
   *  problems may exist in the cache.
   */
  bool resolver_exists() const;


  /** Requires that resolver_exists() is \b true.
   *
   *  \param activate if \b true, enable debugging to cout.  Any
   *  change to the state of any package will reset this to the
   *  default (off).  \todo allow any ostream.
   */
  void set_debug(bool activate);

  /** The number of solutions generated. */
  unsigned int generated_solution_count() const;

  /** Get the selection location, which will be in the range
   *  [0,generated_solution_count()).  Note that this is meaningless
   *  if generated_solution_count==0.
   */
  unsigned int get_selected_solution() const {return selected_solution;}

  /** Requires that resolver_exists() is \b true.  Return the solution
   *  in the given position, generating it if it is past the end of
   *  the list; will continue a search even if it ran out of time
   *  previously.
   *
   *  If solution_num refers to an already-generated solution, this
   *  routine returns immediately (without suspending the thread).
   *
   *  \throw NoMoreSolutions if the list of solutions is exhausted
   *  \throw NoMoreTime if time is exhausted while searching for
   *                    the solution (time here is counted separately
   *                    at each step).
   *  \throw ResolverManagerThreadClashException if a new solution
   *         would be generated and a background thread exists.
   */
  const generic_solution<aptitude_universe> &get_solution(unsigned int solution_num,
							  int max_steps);

  /** As get_solution, but run in a background thread if necessary.
   *
   *  \param solution_num the solution to retrieve
   *
   *  \param max_steps the number of steps to allow the computation
   *
   *  \param k a background_continuation object; when the background
   *  computation is finished, a method corresponding to its result
   *  will be invoked on continuation in the background thread.  It is
   *  safe for this method to manipulate the resolver (for instance,
   *  to enqueue a new computation).
   *
   *  k is owned by this object and will be deleted at its discretion.
   *
   *  \throw ResolverManagerThreadClashException if a background
   *         resolver already exists.
   */
  void get_solution_background(unsigned int solution_num,
			       int max_steps,
			       background_continuation *k);

  /** Like get_solution_background, but blocks until the background
   *  solver has \i either found a solution or examined at least
   *  block_count solutions.
   *
   *  \param solution_num the solution to retrieve
   *
   *  \param max_steps the number of steps to allow the computation
   *
   *  \param block_count the number of steps to wait before returning
   *
   *  \return \b true if the search terminated in block_count steps or
   *  less
   */
  bool get_solution_background_blocking(unsigned int solution_num,
					int max_steps,
					int block_count,
					background_continuation *k);

  /** If \b true, all solutions have been generated.  This is equivalent
   *  to the solutions_exhausted member of the state snapshot.
   */
  bool solution_generation_complete() /*const*/;

  /** If \b true, the solution pointer is set to the first
   *  solution.
   */
  bool solutions_at_start() const;

  /** If \b true, the background thread is working on a job. */
  bool background_thread_active();

  /** Get a snapshot of the current resolver state; contains the
   *  values that would be returned by get_selected_solution(),
   *  generated_solution_count(), and solutions_exhausted(); however,
   *  this snapshot is taken atomically.
   */
  state state_snapshot();



  /** Requires that resolver_exists() is \b true.  Temporarily rejects
   *  any solutions generated by the currently active installer that
   *  involve installing the given version; the rejection will be
   *  discarded when the resolver is.
   */
  void reject_version(const aptitude_resolver_version &ver);

  /** Requires that resolver_exists() is \b true.  Cancels a
   *  rejection created via resolver_reject_version().
   */
  void unreject_version(const aptitude_resolver_version &ver);

  /** Requires the resolver_exists() is \b true.  Returns \b true if
   *  the given version is currently rejected.
   */
  bool is_rejected(const aptitude_resolver_version &ver);

  /** Requires that resolver_exists() is \b true.  Like
   *  resolver_reject_version, but rejects any solution that does \b
   *  not install the given version.
   */
  void mandate_version(const aptitude_resolver_version &ver);

  /** Cancels a resolver_mandate_version call. */
  void unmandate_version(const aptitude_resolver_version &ver);

  /** \return \b true if the given version is mandatory. */
  bool is_mandatory(const aptitude_resolver_version &ver);

  /** Requires that resolver_exists is \b true.  Force the resolver to
   *  treat the given soft dependency as a hard dependency; as above,
   *  you can always cancel this instruction later.
   */
  void harden_dep(const aptitude_resolver_dep &dep);

  /** Cancels a resolver_harden_dep call. */
  void unharden_dep(const aptitude_resolver_dep &dep);

  /** \return \b true if the given dep is hardened. */
  bool is_hardened(const aptitude_resolver_dep &dep);

  /** Require the resolver to leave the given soft dependency broken
   *  whenever possible.
   */
  void approve_broken_dep(const aptitude_resolver_dep &dep);

  /** Cancel an approval set up with approve_broken(). */
  void unapprove_broken_dep(const aptitude_resolver_dep &dep);

  /** \return \b true if the given dependency is in the
   *  approved-broken set.
   */
  bool is_approved_broken(const aptitude_resolver_dep &dep);



  /** \return \b true if undo items exist in this resolver manager. */
  bool has_undo_items();

  /** If this resolver has any undo items, invoke the "topmost" one.
   *
   *  \return \b true if an undo item was invoked.
   */
  bool undo();


  /** Set the selection pointer to a particular solution. */
  void select_solution(unsigned int solnum);

  /** Move the selection pointer to the next solution, without
   *  generating it.
   */
  void select_next_solution();

  /** Move the selection pointer to the previous solution, without
   *  generating it.
   *
   *  \throws NoMoreSolutions if solutions_at_start()
   */
  void select_previous_solution();

  /** Tweak the resolver score of a particular package/version.  This
   *  requires that resolver_exists() and that the resolver is "fresh"
   *  (i.e., that next_solution() and current_solution() have never
   *  been called)
   *
   *  \param pkg the package to adjust
   *  \param ver the version to adjust
   *  \param score an adjustment to be added to the score of pkg:ver
   */
  void tweak_score(const pkgCache::PkgIterator &pkg,
		   const pkgCache::VerIterator &ver,
		   int score);

  /** If a resolver exists, write its state (including scores, etc)
   *  to the given stream.
   */
  void dump(std::ostream &out);

  /** This signal is emitted when the selected solution changes, when
   *  the user takes an action that might change the number of
   *  available solutions (such as un-rejecting a package), and when a
   *  new resolver is created.
   *
   *  Note that this is NOT signalled when a new solution is added to
   *  the solution list by the background thread.  You are free to
   *  manually emit the signal, but of course be aware of threading
   *  considerations if you do so.
   */
  sigc::signal0<void> state_changed;
};

#endif
