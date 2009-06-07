/** \file search_graph.h */     // -*-c++-*-


//   Copyright (C) 2009 Daniel Burrows
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

#ifndef SEARCH_GRAPH_H
#define SEARCH_GRAPH_H

#include <loggers.h>

#include "choice.h"
#include "choice_indexed_map.h"
#include "choice_set.h"
#include "promotion_set.h"
#include "solution.h"
#include "tier_limits.h"

#include <generic/util/immlist.h>
#include <generic/util/immset.h>

// solver_information and dep_solvers are top-level declarations so
// they can easily get an operator<< that works.

/** \brief Information about a single solver of a dependency.
 *
 *  The solver itself is not stored here; this just tracks
 *  metadata.
 */
template<typename PackageUniverse>
class generic_solver_information
{
public:
  typedef typename PackageUniverse::tier tier;
  typedef generic_choice_set<PackageUniverse> choice_set;
  typedef generic_tier_limits<PackageUniverse> tier_limits;

private:
  tier t;
  choice_set reasons;
  cwidget::util::ref_ptr<expression_box<bool> > tier_valid_listener;
  cwidget::util::ref_ptr<expression_box<bool> > is_deferred_listener;

public:
  generic_solver_information()
    : t(tier_limits::minimum_tier),
      reasons(),
      tier_valid_listener(),
      is_deferred_listener()
  {
  }

  /** \brief Create a new solver_information.
   *
   *  \param _t       The tier of the associated solver.
   *  \param _reason  The reasons for the solver's tier (other than
   *                  the solver itself).
   *  \param _tier_valid_listener
   *                  A side-effecting expression that computes
   *                  "true" if the tier assignment is valid.
   *  \param deferred A side-effecting expression whose
   *                  sub-expression is true exactly when this solver
   *                  violates a user-imposed constraint.
   */
  generic_solver_information(const tier &_t,
			     const choice_set &_reasons,
			     const cwidget::util::ref_ptr<expression_box<bool> > &_tier_valid_listener,
			     const cwidget::util::ref_ptr<expression_box<bool> > &_is_deferred_listener)
    : t(_t), reasons(_reasons),
      tier_valid_listener(_tier_valid_listener),
      is_deferred_listener(_is_deferred_listener)
  {
  }

  /** \brief Retrieve the tier of the associated solver. */
  const tier &get_tier() const { return t; }

  /** \brief Retrieve the reason that this solver has the tier
   *  that it does.
   */
  const choice_set &get_reasons() const { return reasons; }

  const cwidget::util::ref_ptr<expression_box<bool> > &
  get_tier_valid_listener() const
  {
    return tier_valid_listener;
  }

  /** \brief Retrieve an expression that returns whether this
   *  solver's tier is valid.
   *
   *  This is held here mainly because it will side-effect and
   *  reset this solver's tier.  Also, it can be used to generate
   *  promotion validity conditions.
   */
  cwidget::util::ref_ptr<expression<bool> >
  get_tier_valid() const
  {
    if(tier_valid_listener.valid())
      return tier_valid_listener->get_child();
    else
      return cwidget::util::ref_ptr<expression_box<bool> >();
  }

  /** \brief Retrieve the listener that tracks whether the solver
   *  is deferred.
   */
  const cwidget::util::ref_ptr<expression_box<bool> > &
  get_is_deferred_listener() const
  {
    return is_deferred_listener;
  }

  /** \brief Retrieve an expression that returns whether the
   *  solver is deferred.
   */
  cwidget::util::ref_ptr<expression<bool> >
  get_is_deferred() const
  {
    if(is_deferred_listener.valid())
      return is_deferred_listener->get_child();
    else
      return cwidget::util::ref_ptr<expression<bool> >();
  }
};

/** \brief A structure that tracks the state of the solvers of a
 *  dependency.
 */
template<typename PackageUniverse>
class generic_dep_solvers
{
public:
  typedef generic_choice<PackageUniverse> choice;
  typedef generic_compare_choices_by_effects<PackageUniverse> compare_choices_by_effects;

private:
  imm::map<choice, generic_solver_information<PackageUniverse>, compare_choices_by_effects> solvers;
  imm::list<choice> structural_reasons;

public:
  generic_dep_solvers()
  {
  }

  /** \brief Return the outstanding solvers of this dependency and
   *  the current state of each one.
   */
  imm::map<choice, generic_solver_information<PackageUniverse>, compare_choices_by_effects> &get_solvers()
  {
    return solvers;
  }

  /** \brief Return the outstanding solvers of this dependency and
   *  the current state of each one.
   */
  const imm::map<choice, generic_solver_information<PackageUniverse>, compare_choices_by_effects> &get_solvers() const
  {
    return solvers;
  }

  /** \brief Return the reasons that the set of solvers for this
   *  dependency was narrowed.
   */
  imm::list<choice> &get_structural_reasons()
  {
    return structural_reasons;
  }

  /** \brief Return the reasons that the set of solvers for this
   *  dependency was narrowed.
   */
  const imm::list<choice> &get_structural_reasons() const
  {
    return structural_reasons;
  }
};

/** \brief Represents the current search graph.
 *
 *  This structure and its operations track all the visited search
 *  nodes and their parent-child relationships, handle inserting new
 *  steps into the graph, and handle backpropagation of promotions
 *  (currently disabled as it didn't work well in practice).
 */
template<typename PackageUniverse>
class generic_search_graph
{
  typedef typename PackageUniverse::package package;
  typedef typename PackageUniverse::version version;
  typedef typename PackageUniverse::dep dep;
  typedef typename PackageUniverse::tier tier;

  typedef generic_solution<PackageUniverse> solution;
  typedef generic_choice<PackageUniverse> choice;
  typedef generic_choice_set<PackageUniverse> choice_set;
  typedef generic_promotion<PackageUniverse> promotion;
  typedef generic_promotion_set<PackageUniverse> promotion_set;
  typedef generic_tier_limits<PackageUniverse> tier_limits;
  typedef generic_compare_choices_by_effects<PackageUniverse> compare_choices_by_effects;

  // Structures that store the search graph.
  //
  // This information is used to backpropagate promotions/conflicts.
  // If all the children of a step ended up in a conflict, we can use
  // that information to infer a conflict for the step itself.  But
  // since aptitude has a flexible search order, the children might be
  // run after we've "finished" processing the parent step.  So it's
  // necessary to somehow store all the steps with children that are
  // pending evaluation.
  //
  // Actually, *all* steps are stored, not just ones with pending
  // children: this lets us handle situations where promotions are
  // generated more than once for the same step (for instance, if a
  // step eventually ends up at tier 40,000 but has one child that
  // passes through tier 30,000, it will first be promoted to tier
  // 30,000, then later to tier 40,000).
  //
  // The lifetime of a step is as follows:
  //  (1) Created with no children, an initial intrinsic promotion,
  //      and maybe a parent link.
  //  (2) Children added, along with successor constraints.
  //  (3) When each of its children has registered a promotion,
  //      the step's promotion is set and its parent is examined
  //      to see if it should get a promotion now.
  //
  // Regarding (3), what we do is this: whenever a new promotion is
  // computed for a step, it goes into the set of promotions for that
  // step and onto the list of promotions if it was really new.  Then,
  // we add the parent to a set of nodes that should be examined for
  // promotion propagation.  After the step finishes, the nodes in
  // this set are processed for propagation from their children.
  //
  // To save space and keep things compact, the tree is represented as
  // an array (actually a deque), with parent and child links stored
  // as indices into the array.  This made more sense when it had just
  // a few members; maybe now it should be allocated on the heap and
  // reference-counted?
public:
  struct step
  {
    // The index of this step; mainly useful when generating debug
    // output.
    int step_num;
    // If true, this is the last child in its parent's child list.
    // Meaningless if parent is -1 (meaning there is no parent node).
    bool is_last_child : 1;
    // If true, this step is a "blessed" solution.  The tier of a
    // blessed solution cannot be increased above the deferral tier
    // (hence it will not be discarded).  Blessed solutions are
    // solutions that have been moved into the pending future
    // solutions queue and are just waiting for the future solution
    // "counter" to be exhausted.
    //
    // \todo One subtlety here: it would be nice if we could throw out
    // already-visited solutions if we found another solution that was
    // a strict subset.  However, that's rather unlikely and I don't
    // want to introduce lots of mechanism (e.g., a whole new matching
    // mode for promotions) just to accomplish it.  I could instead
    // just filter out promotions that are exactly the size of a
    // blessed solution -- but that could easily run into problems
    // with generalized promotions built from the already-generated
    // promotion ... better to just say "if we've processed it, it's
    // safe".
    bool is_blessed_solution : 1;
    // Index of the parent step, or -1 if there is no parent.
    int parent;
    // Index of the first child step, or -1 if there are no children.
    // This is always -1 to start with, and is updated when the step's
    // successors are generated.
    int first_child;

    /** \brief Members related to generating a step's
     *  successor.
     */

    // @{

    typedef generic_solver_information<PackageUniverse> solver_information;
    typedef generic_dep_solvers<PackageUniverse> dep_solvers;

    /** \brief The actions performed by this step. */
    choice_set actions;

    /** \brief The score of this step. */
    int score;

    /** \brief The combined score due to choices that were made and
     *  distance from the root -- "score" is calculated by adding the
     *  broken-dependency count to this.
     */
    int action_score;

    /** \brief The tier of this step. */
    tier step_tier;

    /** \brief A side-effecting expression that fires when this step's
     *  tier changes.
     *
     *  The pure validity condition is a child of this value.
     */
    cwidget::util::ref_ptr<expression_box<bool> > step_tier_valid_listener;

    /** \brief The dependencies that are unresolved in this step; each
     *	one maps to the reasons that any of its solvers were
     *	dropped.
     */
    imm::map<dep, dep_solvers> unresolved_deps;

    /** \brief The unresolved dependencies, sorted by the number of
     *  solvers each one has.
     *
     *  This is a "poor man's heap".
     */
    imm::set<std::pair<int, dep> > unresolved_deps_by_num_solvers;

    /** \brief Maps choices to lists of the dependencies that they
     *  solve.
     *
     *  Every unresolved dependency is represented here, but some
     *  dependencies in each list might already be resolved.  We defer
     *  dropping them to save time and memory (no need to make copies
     *  of (part of) the list just to throw entries away).
     */
    generic_choice_indexed_map<PackageUniverse, imm::list<dep> > deps_solved_by_choice;

    /** \brief Versions that are structurally forbidden and the reason
     *  each one is forbidden.
     */
    imm::map<version, choice> forbidden_versions;

    // @}

    /** \brief Members related to backpropagating promotions. */

    // @{

    // A set listing all the clones of this step (steps that have the
    // same solution).  If this is non-empty, this step is the
    // canonical copy of its clones.  What that means is that the
    // "promotions" set of the canonical copy is used whenever the set
    // of promotions is needed; same for the "promotions" list (but
    // the index of the last promotion propagated to the parent is
    // different in each clone!).
    std::set<int> clones;

    // The canonical copy of this step, or -1 if none.
    int canonical_clone;

    // The choice associated with this step (meaningless if parent ==
    // -1).  Set when the step is created, and used when
    // backpropagating promotions: when the parent is computing its
    // promotion, it removes each child's reason from that child's
    // promotion's choice set.
    choice reason;
    // The choices that constrained the successors to this node.  This
    // is used, along with information from the successors, to compute
    // the promotion associated with this node (if any).
    //
    // This contains versions that either structurally knocked out
    // possible resolutions to the dependency that was selected for
    // expansion, *or* that caused a resolution to hit a conflict /
    // already-generated promotion.  NOTE: the entries in this set
    // might not be represented in every promotion at this step; some
    // promotions could be generated from dependencies that weren't
    // actually expanded.  This is used when accumulating this node's
    // sub-promotions and filling in new promotions; the parent
    // shouldn't examine it.
    //
    // This only has a meaningful value if first_child is not -1.
    choice_set successor_constraints;
    // All the promotions associated with this step; each promotion is
    // universally valid but was discovered in the context of this
    // step.  No attempt is made to eliminate redundant promotions at
    // the moment.
    //
    // If this is a cloned step, this variable will never be used (the
    // canonical clone's version will be used -- but since this is
    // only used when adding new promotions and new promotions are
    // added to the canonical clone, the promotions set isn't used).
    std::set<promotion> promotions;

    // The same, but in the order that they were added; used to
    // quickly partition the list into "new" and "old" promotions.
    //
    // If this is a cloned step, the vector in the canonical clone
    // will be used instead.
    //
    // TODO: should be a list of const_iterators referencing the above
    // set.
    std::vector<promotion> promotions_list;

    // The first index in the promotions list that represents a "new"
    // promotion.  This is always used even in cloned steps (each step
    // could have a different number of promotions that haven't been
    // propagated to its particular parent).
    typename std::vector<promotion>::size_type promotions_list_first_new_promotion;

    // @}

    /** \brief Default step constructor; only exists for use
     *  by STL containers.
     */
    step()
      : is_last_child(true),
	is_blessed_solution(false),
	parent(-1), first_child(-1),
	step_tier_valid_listener(),
	canonical_clone(-1),
	reason(),
	successor_constraints(), promotions(),
	promotions_list(),
	promotions_list_first_new_promotion(0)
    {
    }

    /** \brief Make a step suitable for use at the root.
     *
     *  The step has no parent, children, promotion, or successor
     *  constraints.
     */
    step(const choice_set &_actions,
	 int _score,
	 int _action_score)
      : is_last_child(true),
	is_blessed_solution(false),
	parent(-1), first_child(-1), canonical_clone(-1),
	actions(_actions),
	score(_score),
	action_score(_action_score),
	step_tier_valid_listener(),
	reason(),
	successor_constraints(), promotions(),
	promotions_list(), promotions_list_first_new_promotion(0)
    {
    }

    /** \brief Make a step with the given parent.
     *
     *  The step initially has no children or successor constraints.
     */
    step(const choice_set &_actions,
	 int _score, int _action_score,
	 int _parent,
	 const choice &_reason, bool _is_last_child)
      : is_last_child(_is_last_child),
	is_blessed_solution(false),
	parent(_parent),
	first_child(-1), canonical_clone(-1),
	actions(_actions),
	score(_score),
	action_score(_action_score),
	reason(_reason),
	successor_constraints(), promotions(),
	promotions_list(), promotions_list_first_new_promotion(0)
    {
    }
  };

  /** \brief Describes how a choice occurs in a step. */
  enum choice_mapping_type
    {
      /** \brief The choice is an action performed by the step. */
      choice_mapping_action,

      /** \brief The choice solves a dependency that is unresolved
       *  in the step.
       */
      choice_mapping_solver
    };


  /** \brief Stores all steps where a choice occurs.
   *
   *  Steps where the choice occurs as an action are indexed by the
   *  dependency that's solved.  This allows us to easily pick the
   *  right solver to modify when a deferral is canceled.
   */
  class choice_mapping_info
  {
    // The steps (if any) that introduced this choice as a solver or
    // an action, grouped by the dependency that each one solves.
    imm::map<dep, imm::set<int> > steps;

  public:
    choice_mapping_info()
    {
    }

    choice_mapping_info(const imm::map<dep, imm::set<int> > &_steps)
      : steps(_steps)
    {
    }

    const imm::map<dep, imm::set<int> > &get_steps() const
    {
      return steps;
    }
  };

private:
  /** \brief The maximum number of promotions to propagate
   *  through any one step.
   *
   *  This avoids an exponential growth in the size of promotion sets.
   *  This is disabled for the moment: I was seeing aptitude
   *  allocating truly enormous amounts of memory for propagated
   *  promotions, and the benefits of backpropagation have been
   *  minimal in practice.  If additional enhancements to the resolver
   *  framework are implemented, I expect that we might see
   *  backpropagation become cheaper and more useful, in which case it
   *  could be worth turning it on again (but beware of possible
   *  bitrot!).
   */
  static const unsigned int max_propagated_promotions = 0;

  log4cxx::LoggerPtr logger;

  // We keep a reference to the promotions set so that we can stuff
  // new promotions in during backpropagation.
  promotion_set &promotions;

  std::deque<step> steps;
  // Steps whose children have pending propagation requests.  Stored
  // in reverse order, because we should handle later steps first
  // (since they might be children of earlier steps and thus add new
  // promotions to them).
  std::set<int, std::greater<int> > steps_pending_promotion_propagation;

  /** \brief The step numbers in which a choice was introduced as an
   *  action or a solver.
   *
   *  This is used to efficiently update existing steps that are "hit"
   *  by a new promotion, and to efficiently un-defer steps when the
   *  set of user constraints changes.
   *
   *  This map needs to be updated when a new step is added to the
   *  graph, and also when one of a version's successors is struck.
   */
  generic_choice_indexed_map<PackageUniverse, choice_mapping_info> steps_related_to_choices;

public:
  /** \brief Add an entry to the choice->step reverse index.
   *
   *  \param c         The choice to bind.
   *  \param step_num  The step number in which c occurs.
   *  \param reason    The dependency that this choice solves, if how
   *                   is choice_mapping_solver.
   */
  void bind_choice(const choice &c, int step_num, dep reason)
  {
    // \todo Write a proper operator<<.
    LOG_TRACE(logger, "Marking the choice " << c
	      << " as present in step "
	      << step_num << " with dependency " << reason);

    choice_mapping_info inf;
    steps_related_to_choices.try_get(c, inf);
    imm::map<dep, imm::set<int> >
      new_steps(inf.get_steps());
    imm::set<int>
      new_dep_steps(new_steps.get(reason, imm::set<int>()));

    new_dep_steps.insert(step_num);
    new_steps.put(reason, new_dep_steps);
    steps_related_to_choices.put(c, choice_mapping_info(new_steps));
  }

  /** \brief Remove an entry from the choice->step reverse index.
   *
   *  This will remove the mapping associated with the top of a "tree"
   *  of occurrences.
   *
   *  \warning Correctness of this protocol relies on the fact that
   *  the structure of the resolver means that if a solver becomes
   *  irrelevant in a step, it will also be irrelevant in all children
   *  of that step.  Proof: there are only two ways a solver can
   *  become irrelevant.  It could be structurally excluded (in which
   *  case all children lack it by default), or it could be knocked
   *  out by a promotion.  In the latter case, since children are
   *  supersets of their parents, each child will contain the same
   *  promotion, and hence the choice will be knocked out in every
   *  child that it occurs in.
   *
   *  This also relies on the fact that promotions that knock out
   *  choices are never retracted (if they are, we'll have to be
   *  careful to only reinstate a choice in the top step it occurs in,
   *  but that is not likely in the near future).
   *
   *  \param c         The choice to unbind.
   *  \param step_num  The step number in which c no longer occurs.
   *  \param reason    The dependency that this choice solved, if how
   *                   is choice_mapping_solver.
   */
  void remove_choice(const choice &c, int step_num, const dep &reason)
  {
    // \todo Write a proper operator<<.
    LOG_TRACE(logger, "Marking the choice " << c
              << " as not present in step "
              << step_num);

    choice_mapping_info info;
    if(steps_related_to_choices.try_get(c, info))
      {
        imm::map<dep, imm::set<int> >
          new_steps(info.get_steps());

        typename imm::map<dep, imm::set<int> >::node
          found_solver(new_steps.lookup(reason));

        if(found_solver.isValid())
          {
            imm::set<int>
              new_dep_steps(found_solver.getVal().second);

            new_dep_steps.erase(step_num);
            if(new_dep_steps.empty())
              new_steps.erase(reason);
            else
              new_steps.put(reason, new_dep_steps);
          }

        choice_mapping_info new_info(new_steps);
        steps_related_to_choices.put(c, new_info);
      }
  }

private:
  // Walks down a list of siblings, applying the given function to
  // each of them, until either the function returns false or it runs
  // out of siblings.  If step_num is -1, nothing is visited.
  template<typename F>
  bool visit_siblings(int step_num,
		      F f) const
  {
    while(step_num != -1)
      {
	if(!f(step_num))
	  return false;

	if(get_step(step_num).is_last_child)
	  step_num = -1;
	else
	  ++step_num;
      }

    return true;
  }

  template<typename F>
  class visit_choice_mapping_steps
  {
    // The choice to pass to the sub-function.
    const choice &c;
    const generic_search_graph &graph;

    F f;

    // Could save some code by merging this with
    // visit_choice_mapping_steps_for_dep() (using a virtual
    // function?).
    bool visit(const step &s, choice_mapping_type how) const
    {
      if(!f(c, how, s.step_num))
	return false;
      else
	return graph.visit_siblings(s.first_child, *this);
    }

  public:
    visit_choice_mapping_steps(const choice &_c,
			       const generic_search_graph &_graph, F _f)
      : c(_c), graph(_graph), f(_f)
    {
    }

    bool operator()(int step_num) const
    {
      const step &s(graph.get_step(step_num));
      if(s.actions.contains(c))
	return visit(s, choice_mapping_action);
      else if(s.deps_solved_by_choice.contains_key(c))
	return visit(s, choice_mapping_solver);
      else
	return true;
    }
  };

  /** \brief Apply the given function object to (c', how, step_num)
   *  for each step in each dependency mapping passed to this object.
   */
  template<typename F>
  class visit_choice_dep_mapping
  {
    const choice &c;
    const generic_search_graph &graph;

    F f;

  public:
    visit_choice_dep_mapping(const choice &_c,
			     const generic_search_graph &_graph, F _f)
      : c(_c), graph(_graph), f(_f)
    {
    }

    bool operator()(const std::pair<dep, imm::set<int> > &mapping) const
    {
      return mapping.second.for_each(visit_choice_mapping_steps<F>(c, graph, f));
    }
  };

  /** \brief Apply the given function object to (c', how, step_num)
   *  for each step in each mapping information structure visited.
   */
  template<typename F>
  class visit_choice_mapping
  {
    F f;
    const generic_search_graph &graph;

  public:
    visit_choice_mapping(const generic_search_graph &_graph, F _f)
      : f(_f), graph(_graph)
    {
    }

    bool operator()(const choice &c, const choice_mapping_info &inf) const
    {
      const imm::map<dep, imm::set<int> > &steps(inf.get_steps());
      return steps.for_each(visit_choice_dep_mapping<F>(c, graph, f));
    }
  };

public:
  /** \brief Apply the given function object to (c', how, step_num)
   *  for each binding (c', step_num) in the choice->step reverse
   *  index such that c' is contained in c as indicated by how.
   */
  template<typename F>
  void for_each_step_related_to_choice(const choice &c, F f) const
  {
    visit_choice_mapping<F> visit_mappings_f(*this, f);
    steps_related_to_choices.for_each_key_contained_in(c, visit_mappings_f);
  }

private:
  template<typename F>
  class visit_choice_mapping_steps_solvers_of_dep
  {
    // The choice to pass to the sub-function.
    const choice &c;
    // The dependency whose solvers are being visited.
    const dep &d;
    const generic_search_graph &graph;

    F f;

    bool visit(const step &s, choice_mapping_type how) const
    {
      if(!f(c, how, s.step_num))
	return false;
      else
	return graph.visit_siblings(s.first_child, *this);
    }

  public:
    visit_choice_mapping_steps_solvers_of_dep(const choice &_c,
					      const dep &_d,
					      const generic_search_graph &_graph, F _f)
      : c(_c), d(_d), graph(_graph), f(_f)
    {
    }

    bool operator()(int step_num) const
    {
      const step &s(graph.get_step(step_num));
      choice step_choice;
      if(s.actions.get_contained_choice(c, step_choice) &&
	 step_choice.get_dep() == d)
	return visit(s, choice_mapping_action);
      else
	{
	  typename imm::map<dep, typename step::dep_solvers>::node found =
	    s.unresolved_deps.lookup(d);
	  if(found.isValid() &&
	     found.getVal().second.get_solvers().domain_contains(c))
	    return visit(s, choice_mapping_solver);
	  else
	    return true;
	}
    }
  };

  template<typename F>
  class visit_choice_mapping_solvers_of_dep
  {
    // The dependency to visit.
    dep d;
    const generic_search_graph &graph;

    F f;

  public:
    visit_choice_mapping_solvers_of_dep(const dep &_d,
					const generic_search_graph &_graph, F _f)
      : d(_d), graph(_graph), f(_f)
    {
    }

    bool operator()(const choice &c, const choice_mapping_info &info) const
    {
      typename imm::map<dep, imm::set<int> >::node
	found(info.get_steps().lookup(d));

      if(found.isValid())
	{
	  visit_choice_mapping_steps_solvers_of_dep<F> visit_step_f(c, d, graph, f);
	  return found.getVal().second.for_each(visit_step_f);
	}
      else
	return true;
    }
  };

public:
  /** \brief Apply the given function to (c', how, step_num) for each
   *  binding (c', how, step_num) in the choice->step reverse index
   *  such that c' is contained in c and c' was added as a solver for
   *  the given dependency.
   */
  template<typename F>
  void for_each_step_related_to_choice_with_dep(const choice &c, const dep &d, F f) const
  {
    visit_choice_mapping_solvers_of_dep<F> visit_mappings_by_dep_f(d, *this, f);
    steps_related_to_choices.for_each_key_contained_in(c, visit_mappings_by_dep_f);
  }

  /** \brief Create a search graph.
   *
   *  \param _promotions  The promotion set associated with this object.
   *                      Backpropagated promotions will be inserted
   *                      into this set.
   *
   *  The given promotion set does not have to be initialized until
   *  another method is invoked on this object.
   */
  generic_search_graph(promotion_set &_promotions)
    : logger(aptitude::Loggers::getAptitudeResolverSearchGraph()),
      promotions(_promotions)
  {
  }

  /** \brief Retrieve the nth step. */
  step &get_step(int n)
  {
    eassert(n >= 0);
    eassert((unsigned)n < steps.size());
    return steps[n];
  }

  /** \brief Retrieve the nth step. */
  const step &get_step(int n) const
  {
    eassert(n >= 0);
    eassert((unsigned)n < steps.size());
    return steps[n];
  }

  step &get_last_step()
  {
    eassert(!steps.empty());
    return steps.back();
  }

  const step &get_last_step() const
  {
    eassert(!steps.empty());
    return steps.back();
  }

  /** \brief Retrieve the number of steps. */
  typename std::vector<step>::size_type get_num_steps() const
  {
    return steps.size();
  }

public:
  step &add_step()
  {
    steps.push_back(step());
    step &rval(steps.back());
    rval.step_num = steps.size() - 1;
    return rval;
  }

  /** \brief Throw away all step information. */
  void clear()
  {
    steps.clear();
    steps_pending_promotion_propagation.clear();
    steps_related_to_choices.clear();
  }

  /** Retrieve the promotions list of the given step, returning the
   *  canonical copy if this step is a clone.
   */
  std::vector<promotion> &get_promotions_list(step &s)
  {
    if(s.canonical_clone == -1)
      return s.promotions_list;
    else
      return get_step(s.canonical_clone).promotions_list;
  }

  /** Retrieve the promotions list of the given step, returning the
   *  canonical copy if this step is a clone.
   */
  const std::vector<promotion> &get_promotions_list(const step &s) const
  {
    if(s.canonical_clone == -1)
      return s.promotions_list;
    else
      return get_step(s.canonical_clone).promotions_list;
  }

private:
  // Used to recursively build the set of all the promotions in the
  // Cartesian product of the sub-promotions that include at least one
  // promotion that's "new".
  //
  // Returns "true" if anything was generated.  We care because we
  // need to know whether to queue the parent of the parent for
  // propagation.
  template<typename AddPromotion>
  bool add_child_promotions(int parentNum, int childNum, bool has_new_promotion,
			    const choice_set &choices, const tier &t,
			    const AddPromotion &addPromotion)
  {
    // Where to insert any promotions we run into.
    const step &parent = get_step(parentNum);
    int canonicalParentNum = (parent.canonical_clone == -1
			      ? parentNum
			      : parent.canonical_clone);
    const step &canonicalParent = get_step(canonicalParentNum);
    const std::vector<promotion> &canonicalParentPromotionsList = get_promotions_list(parent);

    if(canonicalParentNum == parentNum)
      LOG_TRACE(logger, "Propagating promotions from the step " << childNum
		<< " to its parent, step " << parentNum);
    else
      LOG_TRACE(logger, "Propagating promotions from the step " << childNum
		<< " to its parent's canonical clone, step " << parentNum);

    // Don't do anything if the parent has too many propagations
    // already.
    if(canonicalParentPromotionsList.size() >= max_propagated_promotions)
      {
	LOG_TRACE(logger, "Not creating a new promotion: the parent already has too many promotions.");
	return false;
      }

    step &child = get_step(childNum);
    const std::vector<promotion> &canonicalChildPromotionsList = get_promotions_list(child);

    typename std::vector<promotion>::const_iterator begin, end = canonicalChildPromotionsList.end();
    if(child.is_last_child && !has_new_promotion)
      {
	// Only process new promotions if we don't have one yet.
	begin = canonicalChildPromotionsList.begin() + child.promotions_list_first_new_promotion;
	if(begin == end)
	  LOG_TRACE(logger, "No new promotions to process (step " << childNum << ")");
      }
    else
      {
	begin = canonicalChildPromotionsList.begin();
	if(begin == end)
	  LOG_TRACE(logger, "No promotions to process (step " << childNum << ")");
      }

    bool rval = false;
    for(typename std::vector<promotion>::const_iterator it = begin;
	it != end && canonicalParentPromotionsList.size() < max_propagated_promotions; ++it)
      {
	bool current_is_new_promotion =
	  (it - canonicalChildPromotionsList.begin()) >= (signed)child.promotions_list_first_new_promotion;

	choice_set new_choices(choices);
	tier new_tier(t);

	const promotion &p(*it);
	choice_set p_choices(p.get_choices());

	LOG_TRACE(logger, "Using the successor link of step " << childNum
		  << ", " << child.reason
		  << ", to backpropagate the promotion " << p
		  << " and add it to the current choice set " << choices);

	// Strip out the child's link before merging with the existing
	// choice set.
	p_choices.remove_overlaps(child.reason);
	const tier &p_tier(p.get_tier());
	// Augment the choice set with these new choices.  Narrowing
	// is appropriate: anything matching the promotion should
	// match all the choices we found.
	new_choices.insert_or_narrow(p_choices);
	if(p_tier < new_tier)
	  new_tier = p_tier;

	if(canonicalParent.step_tier >= new_tier)
	  // No point in generating a promotion whose tier is below
	  // the parent's tier.
	  {
	    LOG_TRACE(logger, "Not backpropagating this promotion: its tier, "
		      << new_tier
		      << " is not above the tier of step "
		      << canonicalParentNum << ", "
		      << canonicalParent.step_tier);
	    continue;
	  }


	if(child.is_last_child)
	  {
	    promotion new_promotion(new_choices, new_tier);

	    // Emit a new promotion.
	    addPromotion(canonicalParentNum, new_promotion);

	    // Actually output a new promotion in the canonical
	    // parent.
	    LOG_TRACE(logger, "New backpropagated promotion at step "
		      << canonicalParentNum << ": " << new_promotion);

	    rval = true;
	  }
	else
	  {
	    bool new_has_new_promotion = has_new_promotion || current_is_new_promotion;
	    // Recur.
	    bool generated_anything =
	      add_child_promotions(parentNum, childNum + 1,
				   new_has_new_promotion,
				   new_choices, new_tier,
				   addPromotion);

	    rval = rval || generated_anything;
	  }
      }

    child.promotions_list_first_new_promotion = canonicalChildPromotionsList.size();

    return rval;
  }

  // TODO: log when we first fail to add a promotion because we hit
  // the maximum number -- that's actually not trivial to do without
  // complicating the code.
  template<typename AddPromotion>
  void maybe_collect_child_promotions(int stepNum, const AddPromotion &addPromotion)
  {
    step &parentStep(get_step(stepNum));
    LOG_TRACE(logger, "Backpropagating promotions to step " << stepNum);

    if(parentStep.first_child == -1)
      {
	LOG_ERROR(logger, "No children at step " << stepNum << ", so no promotions to backpropagate.");
	return;
      }

    if(add_child_promotions(stepNum, parentStep.first_child,
			    false, parentStep.successor_constraints,
			    tier_limits::maximum_tier,
			    addPromotion))
      {
	if(parentStep.parent != -1)
	  {
	    LOG_TRACE(logger, "Scheduling step " << parentStep.parent
		      << " for promotion propagation.");
	    steps_pending_promotion_propagation.insert(parentStep.parent);
	  }
      }
    else
      LOG_TRACE(logger, "No new promotion at step " << stepNum);
  }

public:
  /** \brief Attach a promotion to the given step, and schedule it for
   *  propagation.
   */
  void schedule_promotion_propagation(int stepNum,
				      const promotion &p)
  {
    step &targetStep(get_step(stepNum));

    if(targetStep.canonical_clone != -1)
      {
	LOG_TRACE(logger, "Adding the promotion " << p
		  << " to step " << targetStep.canonical_clone
		  << " instead of to its clone, step "
		  << stepNum << ".");
	schedule_promotion_propagation(targetStep.canonical_clone, p);
	return;
      }

    if(targetStep.promotions.size() == max_propagated_promotions)
      {
	LOG_TRACE(logger, "Not adding the promotion " << p
		  << " to step " << stepNum
		  << " since that step has the maximum number of promotions already.");
	return;
      }

    // TODO: could do a slow check for redundant promotions here?

    std::pair<typename std::set<promotion>::iterator, bool>
      insert_info(targetStep.promotions.insert(p));
    if(insert_info.second)
      {
	targetStep.promotions_list.push_back(p);
	if(targetStep.parent != -1)
	  {
	    LOG_TRACE(logger, "Adding a promotion to step " << stepNum
		      << " and scheduling its parent, step " << targetStep.parent << " for propagation: "
		      << p);
	    steps_pending_promotion_propagation.insert(targetStep.parent);
	  }
	else
	  LOG_TRACE(logger, "Adding a promotion to step " << stepNum
		    << "; it has no parent, so not scheduling propagation: "
		    << p);
      }

    if(!targetStep.clones.empty())
      {
	LOG_TRACE(logger, "Also scheduling the parents of the clones of step " << stepNum << " for propagation.");
	for(std::set<int>::const_iterator it = targetStep.clones.begin();
	    it != targetStep.clones.end(); ++it)
	  {
	    int cloneNum = *it;
	    const step &clone(get_step(cloneNum));

	    if(clone.parent != -1)
	      {
		LOG_TRACE(logger, "Scheduling the parent (step "
			  << clone.parent << ") of a clone (step " << cloneNum
			  << ") of step " << stepNum << " for propagation.");
		steps_pending_promotion_propagation.insert(clone.parent);
	      }
	    else
	      // Should never happen, but be careful.  (this would
	      // mean we had a clone of the root node!  Madness!)
	      LOG_ERROR(logger, "Not scheduling the parent of a clone (step " << cloneNum
			<< ") for propagation: it has no parent (something is very wrong!).");
	  }
	LOG_TRACE(logger, "Done scheduling the clones of step " << stepNum << " for propagation.");
      }
  }

  /** \brief Mark the second step as being a clone of the first step.
   *
   *  \param canonicalNum    The step number to use as the canonical copy.
   *  \param cloneNum        The step number to mark as a clone.
   */
  void add_clone(int canonicalNum, int cloneNum)
  {
    LOG_TRACE(logger, "Marking step " << cloneNum << " as a clone of step " << canonicalNum << ".");

    step &canonicalStep(get_step(canonicalNum));
    step &cloneStep(get_step(cloneNum));

    if(cloneStep.canonical_clone == canonicalNum)
      {
	LOG_TRACE(logger, "Not marking step " << cloneNum
		  << " as a clone of step " << canonicalNum
		  << " since it is already listed as a clone.");
	return;
      }

    // These cases should never come up, so assert against them
    // instead of trying to write clever code to handle them.
    eassert(canonicalNum != cloneNum);
    eassert(cloneStep.canonical_clone == -1);
    eassert(cloneStep.clones.empty());

    canonicalStep.clones.insert(cloneNum);
    cloneStep.canonical_clone = canonicalNum;

    // NB: no need to do anything special with successor_constraints;
    // it's only used to generate promotions and we take those from
    // the canonical clone.
    const int cloneParentNum = cloneStep.parent;
    if(!canonicalStep.promotions.empty() &&
       cloneParentNum != -1)
      {
	LOG_TRACE(logger, "The canonical step " << canonicalNum
		  << " has some promotions, so scheduling the parent (step "
		  << cloneParentNum << ") of the clone (step "
		  << cloneNum << ") for backpropagation.");
	steps_pending_promotion_propagation.insert(cloneParentNum);
      }
  }

  /** \brief Execute any pending promotion propagations. */
  template<typename AddPromotion>
  void run_scheduled_promotion_propagations(const AddPromotion &addPromotion)
  {
    while(!steps_pending_promotion_propagation.empty())
      {
	// Make a temporary copy to iterate over.
	std::set<int, std::greater<int> > tmp;
	tmp.swap(steps_pending_promotion_propagation);
	for(std::set<int, std::greater<int> >::const_iterator it = tmp.begin();
	    it != tmp.end(); ++it)
	  maybe_collect_child_promotions(*it, addPromotion);
      }
  }

private:
  struct is_deferred
  {
    bool operator()(const promotion &p) const
    {
      const tier &p_tier(p.get_tier());

      return
	p_tier >= tier_limits::defer_tier &&
	p_tier < tier_limits::already_generated_tier;
    }
  };

public:
  /** \brief Remove any propagated promotions from the deferred
   *  tier.
   *
   *  This should be invoked when the set of deferred solutions might
   *  have changed.
   *
   *  \todo This is no longer right with the incremental resolver; we
   *  can remove exactly the right set of promotions if we want.
   */
  void remove_deferred_propagations()
  {
    is_deferred is_deferred_f;

    for(typename std::deque<step>::iterator step_it = steps.begin();
	step_it != steps.end(); ++step_it)
      {
	step &curr_step(*step_it);

	for(typename std::vector<promotion>::const_iterator p_it =
	      curr_step.promotions_list.begin();
	    p_it != curr_step.promotions_list.end(); ++p_it)
	  {
	    const promotion &p(*p_it);

	    if(is_deferred_f(p))
	      {
		LOG_TRACE(logger, "Removing a promotion from the promotion set of step "
			  << step_it - steps.begin()
			  << ": " << p);
		curr_step.promotions.erase(p);
	      }
	  }

	// Drop the deferred entries from the list of promotions,
	// updating the "new" pointer so that new promotions will be
	// detected as being "new".
	typename std::vector<promotion>::size_type write_loc = 0, read_loc = 0;
	typename std::vector<promotion>::size_type num_old_promotions_deleted = 0;
	while(read_loc < curr_step.promotions_list.size())
	  {
	    while(read_loc < curr_step.promotions_list.size() &&
		  is_deferred_f(curr_step.promotions_list[read_loc]))
	      {
		LOG_TRACE(logger, "Removing a promotion from the promotion list of step "
			  << step_it - steps.begin()
			  << ": " << curr_step.promotions_list[read_loc]);
		if(read_loc < curr_step.promotions_list_first_new_promotion)
		  ++num_old_promotions_deleted;
		++read_loc;
	      }

	    if(read_loc < curr_step.promotions_list.size())
	      {
		if(write_loc != read_loc)
		  curr_step.promotions_list[write_loc] = curr_step.promotions_list[read_loc];

		++write_loc;
		++read_loc;
	      }
	  }
	if(read_loc != write_loc)
	  curr_step.promotions_list.erase(curr_step.promotions_list.begin() + write_loc,
					  curr_step.promotions_list.end());
	curr_step.promotions_list_first_new_promotion -= num_old_promotions_deleted;
      }
  }

  /** \brief Dump a dot-format representation of this graph to the
   *  given stream.
   *
   *  For debugging purposes.
   */
  void write_graph(std::ostream &out)
  {
    out << "digraph {" << std::endl;
    for(typename std::deque<step>::const_iterator it = steps.begin();
	it != steps.end(); ++it)
      {
	out << it->step_num << " [label=\"Step " << it->step_num << "\\n"
	    << it->actions.size() << " actions\", shape=box";

	if(it->is_last_child)
	  out << ", style=filled, fillcolor=lightgray";

	out << "];" << std::endl;

	int i = it->first_child;
	while(i != -1)
	  {
	    const step &child(steps[i]);

	    if(child.parent != it->step_num)
	      // It's a phantom link; graph accordingly.
	      out << it->step_num << " -> " << child.step_num << " [style=dashed];" << std::endl;
	    else
	      out << it->step_num << " -> " << child.step_num << " [label=\"" << child.reason << "\"];" << std::endl;

	    if(child.is_last_child)
	      i = -1;
	    else
	      ++i;
	  }
      }
    out << "}" << std::endl;
  }
};

template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out, const generic_solver_information<PackageUniverse> &info)
{
  return out << "(" << info.get_tier()
	     << ":" << info.get_reasons() << ")";
}

template<typename PackageUniverse>
std::ostream &operator<<(std::ostream &out,
			 const generic_dep_solvers<PackageUniverse> &solvers)
{
  return out << "("
	     << solvers.get_structural_reasons()
	     << ": "
	     << solvers.get_solvers()
	     << ")";
}

#endif
