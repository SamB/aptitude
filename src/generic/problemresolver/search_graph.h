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
#include "choice_set.h"
#include "promotion_set.h"
#include "solution.h"
#include "tier_limits.h"

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
    // If true, this is the last child in its parent's child list.
    // Meaningless if parent is -1 (meaning there is no parent node).
    bool is_last_child : 1;
    // Index of the parent step, or -1 if there is no parent.
    int parent;
    // Index of the first child step, or -1 if there are no children.
    // This is always -1 to start with, and is updated when the step's
    // successors are generated.
    int first_child;

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

    // The solution associated with this step.
    solution sol;
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

    /** \brief Default step constructor; only exists for use
     *  by STL containers.
     */
    step()
      : is_last_child(true),
	parent(-1), first_child(-1), canonical_clone(-1), sol(), reason(),
	successor_constraints(), promotions(),
	promotions_list(), promotions_list_first_new_promotion(0)
    {
    }

    /** \brief Make a step suitable for use at the root.
     *
     *  The step has no parent, children, promotion, or successor
     *  constraints.
     */
    step(const solution &_sol)
      : is_last_child(true),
	parent(-1), first_child(-1), canonical_clone(-1), sol(_sol), reason(),
	successor_constraints(), promotions(),
	promotions_list(), promotions_list_first_new_promotion(0)
    {
    }

    /** \brief Make a step with the given parent.
     *
     *  The step initially has no children or successor constraints.
     */
    step(const solution &_sol, int _parent,
	 const choice &_reason, bool _is_last_child)
      : is_last_child(_is_last_child),
	parent(_parent),
	first_child(-1), canonical_clone(-1), sol(_sol), reason(_reason),
	successor_constraints(), promotions(),
	promotions_list(), promotions_list_first_new_promotion(0)
    {
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

  // Step-related routines.
public:
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

  /** \brief Retrieve the number of steps. */
  typename std::vector<step>::size_type get_num_steps() const
  {
    return steps.size();
  }

  /** \brief Add the root step. */
  void add_root(const solution &sol)
  {
    // In practice we sometimes add a root when this is not empty. o_O
    // This shouldn't happen, but I'm currently just refactoring and
    // keeping old behavior.
    //
    //eassert(steps.empty());
    steps.push_back(step(sol));
  }

  /** \brief Add a step.
   *
   *  The list of steps is not well encapsulated at the moment: the
   *  caller is expected to manage invariants regarding child flags,
   *  etc.
   */
  void add_step(const solution &sol, int parent_step_num,
		const choice &c, bool is_first_child)
  {
    eassert(!steps.empty());
    steps.push_back(step(sol, parent_step_num, c, is_first_child));
  }

  /** \brief Throw away all step information. */
  void clear()
  {
    steps.clear();
    steps_pending_promotion_propagation.clear();
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
  bool add_child_promotions(int parentNum, int childNum, bool has_new_promotion,
			    const choice_set &choices, const tier &t)
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

	if(canonicalParent.sol.get_tier() >= new_tier)
	  // No point in generating a promotion whose tier is below
	  // the parent's tier.
	  {
	    LOG_TRACE(logger, "Not backpropagating this promotion: its tier, "
		      << new_tier
		      << " is not above the tier of step "
		      << canonicalParentNum << ", "
		      << canonicalParent.sol.get_tier());
	    continue;
	  }


	if(child.is_last_child)
	  {
	    promotion new_promotion(new_choices, new_tier);

	    // Stash the promotion away in the global set of
	    // promotions.
	    promotions.insert(new_promotion);

	    // Actually output a new promotion in the canonical
	    // parent.
	    LOG_TRACE(logger, "New backpropagated promotion at step "
		      << canonicalParentNum << ": " << new_promotion);
	    schedule_promotion_propagation(canonicalParentNum, new_promotion);

	    rval = true;
	  }
	else
	  {
	    bool new_has_new_promotion = has_new_promotion || current_is_new_promotion;
	    // Recur.
	    bool generated_anything =
	      add_child_promotions(parentNum, childNum + 1,
				   new_has_new_promotion,
				   new_choices, new_tier);

	    rval = rval || generated_anything;
	  }
      }

    child.promotions_list_first_new_promotion = canonicalChildPromotionsList.size();

    return rval;
  }

  // TODO: log when we first fail to add a promotion because we hit
  // the maximum number -- that's actually not trivial to do without
  // complicating the code.
  void maybe_collect_child_promotions(int stepNum)
  {
    step &parentStep(get_step(stepNum));
    LOG_TRACE(logger, "Backpropagating promotions to step " << stepNum << ": " << parentStep.sol);

    if(parentStep.first_child == -1)
      {
	LOG_ERROR(logger, "No children at step " << stepNum << ", so no promotions to backpropagate.");
	return;
      }

    if(add_child_promotions(stepNum, parentStep.first_child,
			     false, parentStep.successor_constraints,
			    tier_limits::maximum_tier))
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
  void run_scheduled_promotion_propagations()
  {
    while(!steps_pending_promotion_propagation.empty())
      {
	// Make a temporary copy to iterate over.
	std::set<int, std::greater<int> > tmp;
	tmp.swap(steps_pending_promotion_propagation);
	for(std::set<int, std::greater<int> >::const_iterator it = tmp.begin();
	    it != tmp.end(); ++it)
	  maybe_collect_child_promotions(*it);
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
};

#endif
