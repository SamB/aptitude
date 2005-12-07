// immset.h                                     -*-c++-*-
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
// This file defines a class to represent immutable sets.  These sets
// behave like std::set, except that their contents cannot be changed;
// they have no erase() or insert() operators, and only
// const_iterators.  This restriction allows immutable sets to be
// implemented in a way that makes both copying and creating a new set
// by adding an element very efficient (O(1) and O(lg n) respectively;
// with std::set these are both O(n)).

#ifndef IMMSET_H
#define IMMSET_H

#include <generic/util/eassert.h>
#include <iostream>

#include <vector>

namespace imm
{
  /** A generic node in a weighted tree as described in "Implementing
   *  Sets Efficiently In A Functional Language".  The tree invariant
   *  is that the tree is not "too unbalanced"; in this case, that no
   *  subtree is more than 4 times larger than its sibling.  (note
   *  that w must be at least 3.75; see the paper for details)
   *
   *  A brief note on choice of algorithm: while a rbtree is more
   *  common and may be slightly more efficient, it is a much trickier
   *  data structure to implement.
   *
   *  The weighted-tree data structure is reasonably efficient and
   *  much more straightforward to implement correctly.
   */
  template<typename Val, const int w = 4>
  class wtree_node
  {
    class impl
    {
      typedef unsigned int size_type;

      /** Left and right children (may be \b null). */
      wtree_node left, right;

      /** The size of the subtree rooted at this node. */
      size_type size;

      /** The reference-count of this node. */
      mutable int refcount;

      /** The enclosed value. */
      Val val;
    public:
      impl(const Val &_val,
	   const wtree_node &_left, const wtree_node &_right)
	:left(_left), right(_right), size(_left.size()+_right.size()+1),
	 refcount(1), val(_val)
      {
      }

      /** \return the left child. */
      const wtree_node &getLeftChild() const
      {
	return left;
      }

      /** \return the right child. */
      const wtree_node &getRightChild() const
      {
	return right;
      }

      size_type getSize() const
      {
	return size;
      }

      const Val &getVal() const
      {
	return val;
      }

      void incref() const
      {
	eassert(refcount>0);

	++refcount;
      }

      void decref() const
      {
	eassert(refcount>0);
	--refcount;

	if(refcount == 0)
	  delete this;
      }
    };

    const impl *realNode;

  public:
    typedef unsigned int size_type;

    wtree_node(const Val &val,
	       const wtree_node &left, const wtree_node &right)
      :realNode(new impl(val, left, right))
    {
    }

    wtree_node(const Val &val)
      :realNode(new impl(val, wtree_node(), wtree_node()))
    {
    }

    /** Takes possession of the given reference (the caller should
     *  incref() if necessary).
     */
    wtree_node(const impl *_realNode)
      :realNode(_realNode)
    {
    }

    wtree_node()
      :realNode(NULL)
    {
    }

    wtree_node(const wtree_node &other)
      :realNode(other.realNode)
    {
      if(realNode != NULL)
	realNode->incref();
    }

    ~wtree_node()
    {
      if(realNode != NULL)
	realNode->decref();
    }

    wtree_node getLeft() const
    {
      return realNode->getLeftChild();
    }

    wtree_node getRight() const
    {
      return realNode->getRightChild();
    }

    size_type size() const
    {
      if(realNode == NULL)
	return 0;
      else
	return realNode->getSize();
    }

    wtree_node &operator=(const wtree_node &other)
    {
      if(other.realNode != NULL)
	other.realNode->incref();
      if(realNode != NULL)
	realNode->decref();

      realNode = other.realNode;

      return *this;
    }

    /** Pointer comparison. */
    bool operator==(const wtree_node &other) const
    {
      return realNode == other.realNode;
    }

    /** Pointer comparison. */
    bool operator!=(const wtree_node &other) const
    {
      return realNode != other.realNode;
    }

    bool empty() const
    {
      return realNode == NULL;
    }

    bool isValid() const
    {
      return realNode != NULL;
    }

    /** \return the value of this node. */
    const Val &getVal() const
    {
      return realNode->getVal();
    }

    // Tree management routines:

    /** Perform a 'left rotate' operation on this node.  Requires
     *  that the right child is not \b null.
     */
    wtree_node left_rotate_single() const
    {
      wtree_node right = getRight(), left = getLeft();
      wtree_node right_left = right.getLeft(), right_right = right.getRight();

      return wtree_node(right.getVal(),
			wtree_node(getVal(), left, right_left),
			right_right);
    }

    /** Perform a 'right rotate' operation on this node.  Requires
     *  that the left child is not \b null.
     */
    wtree_node right_rotate_single() const
    {
      wtree_node right = getRight(), left = getLeft();
      wtree_node left_left = left.getLeft(), left_right = left.getRight();

      return wtree_node(left.getVal(),
			left_left,
			wtree_node(getVal(), left_right, right));
    }

    /** Perform a 'double left rotate' operation on this node.
     *  Requires that the right child not be \b null and that
     *  its left child is also not \b null.
     */
    wtree_node left_rotate_double() const
    {
      wtree_node right = getRight(), left = getLeft();
      wtree_node right_right = right.getRight(), right_left = right.getLeft();
      wtree_node right_left_left = right_left.getLeft();
      wtree_node right_left_right = right_left.getRight();

      return wtree_node(right_left.getVal(),
			wtree_node(getVal(), left, right_left_left),
			wtree_node(right.getVal(), right_left_right,
				   right_right));
    }

    /** Perform a 'double right rotate' operation on this node.
     *  Requires that the left child not be \b null and that
     *  its right child is also not \b null.
     */
    wtree_node right_rotate_double() const
    {
      wtree_node right = getRight(), left = getLeft();
      wtree_node left_right = left.getRight(), left_left = left.getLeft();
      wtree_node left_right_left = left_right.getLeft();
      wtree_node left_right_right = left_right.getRight();

      return wtree_node(left_right.getVal(),
			wtree_node(left.getVal(), left_left, left_right_left),
			wtree_node(getVal(), left_right_right, right));
    }




    /** Rebalance the given subtree, returning a new subtree
     *  reference.  The subtree should be unbalanced by at most one
     *  element (think inserting or deleting a single element).
     *  Equivalent to T' in the paper.
     */ 
    wtree_node rebalance() const
    {
      wtree_node left = getLeft(), right = getRight();
      size_type left_size = left.size();
      size_type right_size = right.size();

      // If one subtree is empty and the other contains at most one
      // element, there is nothing to do.
      if(left_size + right_size < 2)
	return *this;
      else if(left_size * w < right_size)
	{
	  // The right tree is too heavy.  As explained in the paper,
	  // a single rotation is guaranteed sufficient if its outer
	  // (right) child is larger than its inner child; otherwise a
	  // double rotation is guaranteed sufficient.
	  wtree_node right_left = right.getLeft(), right_right = right.getRight();
	  if(right_left.size() < right_right.size())
	    return left_rotate_single();
	  else
	    return left_rotate_double();
	}
      else if(right_size * w < left_size)
	{
	  // Dual of above.
	  wtree_node left_left = left.getLeft(), left_right = left.getRight();
	  if(left_right.size() < left_left.size())
	    return right_rotate_single();
	  else
	    return right_rotate_double();
	}
      else
	// Nothing to do; the tree is already balanced.
	return *this;
    }

    /** Apply the given operator to each value in the tree in
     *  order.
     */
    template<typename Op>
    void for_each(const Op &o) const
    {
      if(isValid())
	{
	  realNode->getLeftChild().for_each(o);

	  o(getVal());

	  realNode->getRightChild().for_each(o);
	}
    }

    void dump(std::ostream &out,
	      int indent = 2,
	      int level = 0,
	      int mode = -1) const
    {
      if(empty())
	return;

      for(int i = 0; i < indent; ++i)
	for(int j = 0; j < level; ++j)
	  out << " ";

      if(mode == 0)
	out << "L-> ";
      else if(mode == 1)
	out << "R-> ";

      out << getVal() << std::endl;

      getLeft().dump(out, indent, level+1, 0);
      getRight().dump(out, indent, level+1, 1);
    }

    /** Return a new tree that does not share its structure with any
     *  other tree.
     */
    wtree_node clone() const
    {
      if(empty())
	return wtree_node();
      else
	return wtree_node(getVal(), getLeft().clone(), getRight().clone());
    }
  };

  /** An entire weighted tree.
   */
  template<typename Val, typename Compare = std::less<Val>, int w = 4 >
  class set
  {
  public:
    typedef Val value_type;
    typedef wtree_node<Val, w> node;
    typedef typename node::size_type size_type;

    Compare value_compare;

    /** An iterator over a wtree.  Note that the lack of parent
     *  pointers (necessary to allow full memory sharing) forces the
     *  iterator class to allocate!  I don't recommend using iterators
     *  except for the purpose of spitting the tree out for debugging.
     */
    class const_iterator
    {
      /** If the first entry of the pair is \b true, then we have not
       *  yet descended into the right subtree of this entry.  For
       *  nodes other than the las tone, if the first entry is \b
       *  false, then when the node becomes the last node again its
       *  right child will be entered; otherwise the node itself will
       *  be visited.
       */
      typedef std::pair<bool, node > path_entry;

      std::vector<path_entry> path;
    public:
      const_iterator()
      {
      }

      const_iterator(const node &root)
      {
	if(root.isValid())
	  {
	    path.push_back(path_entry(false, root));
	    while(path.back().second.getLeft().isValid())
	      path.push_back(path_entry(false, path.back().second.getLeft()));
	  }
      }

      const Val &operator*() const
      {
	return path.back().second.getVal();
      }

      const Val *operator->() const
      {
	return &path.back().second.getVal();
      }

      const_iterator &operator=(const const_iterator &other)
      {
	path = other.path;
	return *this;
      }

      bool operator==(const const_iterator &other) const
      {
	return path == other.path;
      }

      bool operator!=(const const_iterator &other) const
      {
	return path != other.path;
      }

      const_iterator &operator++()
      {
	eassert(!path.empty());

	if(!path.back().first)
	  {
	    path.back().first = true;
	    path.push_back(path_entry(false, path.back().second.getRight()));

	    while(!path.empty() && path.back().second.isValid())
	      path.push_back(path_entry(false, path.back().second.getLeft()));
	  }

	// Clear out any invalid nodes or nodes that already fired.
	while(!path.empty() && (!path.back().second.isValid() || path.back().first))
	  path.pop_back();

	// Now either the path is empty, or we're at a node that's
	// valid and hasn't fired yet (meaning we just finished
	// descending into its left subtree, so we should stop and
	// visit it).

	return *this;
      }
    };
  private:
    /** Root of the tree. */
    node root;

    /** Returns a balanced tree constructed by adding x to n.
     */
    node add(const node &n, const Val &x) const
    {
      if(n.empty())
	return node(x, node(), node());
      else if(value_compare(x, n.getVal()))
	return node(n.getVal(),
		    add(n.getLeft(), x),
		    n.getRight()).rebalance();
      else if(value_compare(n.getVal(), x))
	return node(n.getVal(),
		    n.getLeft(),
		    add(n.getRight(), x)).rebalance();
      else
	return n;
    }

    /** Returns a balanced tree constructed by adding x to n.  Will
     *  replace existing nodes equivalent to x.
     */
    node addUpdate(const node &n, const Val &x) const
    {
      if(n.empty())
        return node(x, node(), node());
      else if(value_compare(x, n.getVal()))
	return node(n.getVal(),
		    addUpdate(n.getLeft(), x),
		    n.getRight()).rebalance();
      else if(value_compare(n.getVal(), x))
	return node(n.getVal(),
		    n.getLeft(),
		    addUpdate(n.getRight(), x)).rebalance();
      else
	return node(x, n.getLeft(), n.getRight());
    }

    /** Given a node, find and return its minimum element while
     *  simultaneously constructing a new (balanced) node that doesn't
     *  contain the minimum.
     */
    static std::pair<node, Val> find_and_remove_min(const node &n)
    {
      if(n.getLeft().isValid())
      {
	std::pair<node, Val> tmp = find_and_remove_min(n.getLeft());
	  return std::pair<node, Val>(node(n.getVal(),
					   tmp.first, n.getRight()).rebalance(),
				      tmp.second);
      }
      else
      {
	return std::pair<node, Val>(n.getRight(), n.getVal());
      }
    }

    /** Join together two trees; every element of l must be less than
     *  every element of r.
     */
    static node splice_trees(const node &l, const node &r)
    {
      if(r.empty())
        return l;
      else if(l.empty())
        return r;

      std::pair<node, Val> tmp = find_and_remove_min(r);
      return node(tmp.second, l, tmp.first);
    }

    /** \return \b true if there exist elements x1 and x2 in n1 and n2
     *  respectively such that x1 and x2 are equivalent and f(x1, x2)
     *  holds.  If f is \b lambda x1 x2 . \b true, then this routine
     *  simply tests whether the sets represented by n1 and n2
     *  intersect.
     */
    template<typename F>
    bool nodes_intersect(const node &n1, const node &n2, const F &f) const
    {
      // This algorithm is simple and obviously right, but not
      // terribly efficient; it is a candidate for optimization if it
      // becomes a bottleneck.
      if(n1.empty())
        return false;
      else if(n2.empty())
        return false;
      else if(value_compare(n1.getVal(), n2.getVal()))
        {
	  return
	    nodes_intersect(n1.getRight(), n2, f) ||
	    nodes_intersect(n1, n2.getLeft(), f);
        }
      else if(value_compare(n2.getVal(), n1.getVal()))
        {
	  return
	    nodes_intersect(n1.getLeft(), n2, f) ||
	    nodes_intersect(n1, n2.getRight(), f);
        }
      else if(f(n1.getVal(), n2.getVal()))
        return true;
      else
        return
          nodes_intersect(n1.getLeft(), n2.getLeft(), f) ||
          nodes_intersect(n1.getRight(), n2.getRight(), f);
    }

    /** \return \b true if n1 contains n2 under f; i.e., if for each
     *  element x2 of n2 there exists an element x1 of n1 such that x1
     *  is equivalent to x2 and f(x1, x2) holds.  If f is
     *  \b lambda x1 x2 . \b true, then this is simply set containment.
     */
    template<typename F>
    bool node_contains(const node &n1, const node &n2, const F &f) const
    {
      if(n2.empty())
        return true;
      else if(n1.empty())
        return false;
      else if(value_compare(n1.getVal(), n2.getVal()))
        {
	  // Strip the left subtree of n2.
	  node n2repl = n2.getLeft().empty()
	    ? n2 : node(n2.getVal(), node(), n2.getRight());

	  return node_contains(n1, n2.getLeft(), f) &&
	         node_contains(n1.getRight(), n2repl, f);
        }
      else if(value_compare(n2.getVal(), n1.getVal()))
        {
	  // Strip the right subtree of n2.
	  node n2repl = n2.getRight().empty()
	    ? n2 : node(n2.getVal(), n2.getLeft(), node());

	  return node_contains(n1, n2.getRight(), f) &&
	         node_contains(n1.getLeft(), n2repl, f);
	}
      else
        {
	  return f(n2.getVal(), n1.getVal()) &&
	         node_contains(n1.getLeft(), n2.getLeft(), f) &&
	         node_contains(n1.getRight(), n2.getRight(), f);
	}
    }

    /** Remove the given value from the given tree. */
    node remove(const node &n, const Val &x) const
    {
      if(n.empty())
        return n;
      else if(value_compare(x, n.getVal()))
        return node(n.getVal(),
		    remove(n.getLeft(), x),
		    n.getRight()).rebalance();
      else if(value_compare(n.getVal(), x))
        return node(n.getVal(),
		    n.getLeft(),
		    remove(n.getRight(), x)).rebalance();
      else // found an equivalent node:
        return splice_trees(n.getLeft(), n.getRight());
    }

    set(const node &n, const Compare &_value_compare)
      :value_compare(_value_compare), root(n)
    {
    }

    /** The binary predicate \b lambda x1 x2 . \b true */
    template<typename T>
    struct universal_relation
    {
      bool operator()(const T &a, const T &b) const
      {
	return true;
      }
    };
  public:
    /** Construct an empty tree. */
    set(const Compare &_value_compare = Compare())
      : value_compare(_value_compare)
    {
    }

    /** Apply the given operator to each member of this set. */
    template<typename Op>
    void for_each(const Op &o) const
    {
      root.for_each(o);
    }

    /** Insert an element into a tree, returning a new tree.  This is
     *  a static function to stress that it does NOT modify the old
     *  tree; instead, it returns a new tree containing the element in
     *  addition to the elements of the old tree.
     */
    static set add(const set &old, const Val &x)
    {
      return set(old.add(old.root, x), old.value_compare);
    }

    /** Like add, but updates existing equivalent elements. */
    static set addUpdate(const set &old, const Val &x)
    {
      return set(old.addUpdate(old.root, x), old.value_compare);
    }

    /** Remove x from the tree. */
    static set remove(const set &old, const Val &x)
    {
      return set(old.remove(old.root, x), old.value_compare);
    }

    /** \return \b true if other contains an element equivalent to
     *                  an element in this and related by f.
     *                  f is invoked as (thiselt, otherelt).
     */
    template<typename F>
    bool intersects(const set &other, const F &f) const
    {
      return nodes_intersect(root, other.root, f);
    }

    /** \return \b true if this set intersects the given set. */
    bool intersects(const set &other) const
    {
      return nodes_intersect(root, other.root, universal_relation<Val>());
    }

    /** \return \b true if each element of other is related by f to an
     *                  element in this.  f is invoked as
     *                  (otherelt, thiselt).
     */
    template<typename F>
    bool contains(const set &other, const F &f) const
    {
      return node_contains(root, other.root, f);
    }

    /** \return \b true if each element in other has an equivalent
     *                  element in this set.
     */
    bool contains(const set &other) const
    {
      return node_contains(root, other.root, universal_relation<Val>());
    }

    /** Do an "in-place" update of this set, by replacing the root
     *  with a new root.
     */
    void insert(const Val &x)
    {
      root = add(root, x);
    }

    /** Similar. */
    void insertUpdate(const Val &x)
    {
      root = addUpdate(root, x);
    }

    /** Similar. */
    void erase(const Val &x)
    {
      root = remove(root, x);
    }

    /** Find a tree node by value.  \return the node, or an invalid
     *	node if none exists.
     */
    node find_node(const Val &x) const
    {
      node rval = root;

      while(rval.isValid())
      {
	if(value_compare(x, rval.getVal()))
	  rval = rval.getLeft();
	else if(value_compare(rval.getVal(), x))
	  rval = rval.getRight();
	else
	  return rval;
      }

      return rval;
    }

    /** \return \b true if this set contains the given value. */
    bool contains(const Val &x) const
    {
      return find_node(x).isValid();
    }

    const_iterator begin() const
    {
      return const_iterator(root);
    }

    const_iterator end() const
    {
      return const_iterator();
    }

    node get_root() const
    {
      return root;
    }

    size_type size() const
    {
      return root.size();
    }

    int empty() const
    {
      return root.empty();
    }

    void dump(std::ostream &out) const
    {
      root.dump(out);
    }

    /** Return a new set that does not share memory with the original
     *  set.  It is safe for the old and new sets to be simultaneously
     *  accessed by separate threads.
     */
    set clone() const
    {
      return set(root.clone(), value_compare);
    }
  };

  /** Compare two sets.  Will produce strange results unless the two
   *  sets have the same comparison *object*; you are responsible for
   *  ensuring that this is the case. (i.e., if the comparator has
   *  associated data, it should be identical in the two sets)
   */
  template<typename Val, typename Compare, int w>
  inline bool operator<(const set<Val, Compare, w> &s1,
			const set<Val, Compare, w> &s2)
  {
    typename set<Val, Compare, w>::const_iterator
      i1 = s1.begin(), i2 = s2.begin();

    while(i1 != s1.end() && i2 != s2.end())
      {
	if(*i1 < *i2)
	  return true;
	else if(*i2 < *i1)
	  return false;
	else
	  {
	    ++i1;
	    ++i2;
	  }
      }

    if(i1 != s1.end())
      return false;
    else if(i2 != s2.end())
      return true;
    else
      return false;
  }

  /** Compare two sets for equality, with the same caveat as operator<. */
  template<typename Val, typename Compare, int w>
  inline bool operator==(const set<Val, Compare, w> &s1,
			 const set<Val, Compare, w> &s2)
  {
    typename set<Val, Compare, w>::const_iterator
      i1 = s1.begin(), i2 = s2.begin();

    while(i1 != s1.end() && i2 != s2.end())
      {
	if(!(*i1 == *i2))
	  return false;
	else
	  {
	    ++i1;
	    ++i2;
	  }
      }

    return i1 == s1.end() && i2 == s2.end();
  }

  /** Auxillary class for map; used to order the contents of the map.
   */
  template<typename Key, typename Val, typename Compare>
  struct key_compare
  {
    Compare real_cmp;
  public:
    key_compare(const Compare &_real_cmp)
      :real_cmp(_real_cmp)
    {
    }

    bool operator()(const std::pair<Key, Val> &p1,
		    const std::pair<Key, Val> &p2) const
    {
      return real_cmp(p1.first, p2.first);
    }
  };

  template<typename Key, typename Val, typename Compare = std::less<Key> >
  class map
  {
  public:
    typedef std::pair<Key, Val> binding_type;
    typedef set<binding_type, key_compare<Key, Val, Compare> > mapping_type;
    typedef typename mapping_type::const_iterator const_iterator;
    typedef typename mapping_type::size_type size_type;
    typedef typename mapping_type::node node;

  private:
    mapping_type contents;

  public:
    /** Construct a map directly from a set of bindings. */
    map(const mapping_type &_contents)
      :contents(_contents)
    {
    }

    /** Construct an empty map */
    map(const Compare &value_compare = Compare())
      :contents(mapping_type(key_compare<Key, Val, Compare>(value_compare)))
    {
    }

    /** Apply the given operator to each binding in this map.
     *
     *  \param o a function object that takes a pair (key, val).
     */
    template<typename Op>
    void for_each(const Op &o) const
    {
      return contents.for_each(o);
    }

    mapping_type get_bindings() const
    {
      return contents;
    }

    const_iterator begin() const
    {
      return contents.begin();
    }

    const_iterator end() const
    {
      return contents.end();
    }

    bool empty() const
    {
      return contents.empty();
    }

    size_type size() const
    {
      return contents.size();
    }

    /** \return either the node corresponding to the given key,
     *  or an empty tree.
     */
    node lookup(const Key &k) const
    {
      return contents.find_node(binding_type(k, Val()));
    }

    /** \return either the value of the mapping at k, or dflt if k is
     *  unbound.
     */
    Val get(const Key &k, const Val &dflt) const
    {
      node found = contents.find_node(binding_type(k, Val()));

      if(found.isValid())
	return found.getVal().second;
      else
	return dflt;
    }

    /** \return a new map that binds k to v, overwriting any existing
     *  binding.
     */
    static map bind(const map &m, const Key &k, const Val &v)
    {
      return map(mapping_type::add(m.contents, binding_type(k, v)));
    }

    /** \return a new map based on m in which k is unbound. */
    static map unbind(const map &m, const Key &k)
    {
      return map(mapping_type::erase(m.contents, binding_type(k, Val())));
    } 

    /** Add a binding to this map. */
    void put(const Key &k, const Val &v)
    {
      contents.insertUpdate(binding_type(k, v));
    }

    /** Delete a binding from this map by key. */
    void erase(const Key &k)
    {
      contents.erase(binding_type(k, Val()));
    }

    /** \return \b true if k is in the domain of this mapping. */
    bool domain_contains(const Key &k) const
    {
      return contents.contains(binding_type(k, Val()));
    }

    bool operator<(const map &other) const
    {
      return contents < other.contents;
    }

    bool operator==(const map &other) const
    {
      return contents == other.contents;
    }

    void dump(std::ostream &out) const
    {
      contents.dump(out);
    }

    bool domain_intersects(const map &other) const
    {
      return contents.intersects(other.contents);
    }

    bool shares_value(const map &other) const
    {
      return contents.intersects(other.contents, std::equal_to<std::pair<Key, Val> >());
    }

    /** \return \b true if the two maps have an element with an equivalent
     *                  key such that f(thiselt, otherelt) is \b true.
     */
    template<typename F>
    bool has_related_mapping(const map &other, const F &f) const
    {
      return contents.intersects(other.contents, f);
    }

    /** Return a new identical map that does not share memory with
     *  this map.
     */
    map clone() const
    {
      return map(contents.clone());
    }
  };
};

#endif
