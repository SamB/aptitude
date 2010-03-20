/** \file parsers.h */   // -*-c++-*-

// Copyright (C) 2010 Daniel Burrows

// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

#ifndef PARSERS_H
#define PARSERS_H

#include <exception>
#include <string>
#include <sstream>
#include <ostream>
#include <vector>

// A note regarding boost/fusion/include/mpl.hpp: the documentation
// only says this makes MPL sequences into Fusion sequences, but
// according to
// http://archives.free.net.ph/message/20090113.031604.6f18fda2.en.html#boost,
// you also have to include it in order to use Fusion sequences as MPL
// sequences.

#include <boost/format.hpp>
#include <boost/fusion/adapted/mpl.hpp>
#include <boost/fusion/algorithm/iteration/fold.hpp>
#include <boost/fusion/algorithm/iteration/for_each.hpp>
#include <boost/fusion/algorithm/transformation/clear.hpp>
#include <boost/fusion/algorithm/transformation/join.hpp>
#include <boost/fusion/algorithm/transformation/push_back.hpp>
#include <boost/fusion/algorithm/transformation/push_front.hpp>
#include <boost/fusion/container/generation/make_vector.hpp>
#include <boost/fusion/container/list.hpp>
#include <boost/fusion/container/vector.hpp>
#include <boost/fusion/container/vector/convert.hpp>
#include <boost/fusion/include/join.hpp>
#include <boost/fusion/include/mpl.hpp>
#include <boost/fusion/include/sequence.hpp>
#include <boost/fusion/iterator/equal_to.hpp>
#include <boost/fusion/sequence.hpp>
#include <boost/make_shared.hpp>
#include <boost/mpl/begin.hpp>
#include <boost/mpl/fold.hpp>
#include <boost/mpl/front.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/numeric/conversion/cast.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/type_traits/is_same.hpp>

#include <generic/util/util.h>

#include <aptitude.h>

#include <errno.h>

namespace parsers
{
  /** \defgroup Parser combinators in C++
   *
   *  \page parser Parsers
   *
   *  A parser provides an operator() method that receives an iterator
   *  range and returns a value of the parser's return_type.  Parsers
   *  are generated from rules; see \ref rule_concept.
   *
   *  Parsers must be derived from a specialization of parser_base;
   *  this allows operator overloading to work properly.
   *
   *  Members required:
   *
   *  - return_type: the type returned from operator().  Must be default
   *    constructible, assignable, and copy-constructible.
   *  - operator()(Iter &begin, const Iter &end) const: the actual parse routine.
   *    Iter must be a model of ForwardIterator.  begin will be updated
   *    to point to the first character that was not parsed.  Throws
   *    ParseException if the input could not be parsed.
   *  - get_expected_description(std::ostream &out) const: writes a brief description
   *    of the next token expected by this parser to "out".
   *  - derived(): returns a reference to the object, cast to its
   *    concrete parser type.
   *
   *  \note Parsers are not specialized on the character type of their
   *  input stream; the character type is simply the value_type of the
   *  iterator passed to operator().
   *
   *  \page rule_concept
   *
   *  A rule is the code that defines a parser (see \ref parser_concept).
   *  Rules can be used with the parser_base class to produce an object
   *  modelling parser_concept.
   *
   *  Members required:
   *
   *  - return_type: the type returned from operator().
   *  - parse(Iter &begin, const Iter &end) const: the actual
   *    parse routine.  Iter must be a model of ForwardIterator.  begin
   *    will be updated to point to the first character that was not
   *    parsed.  Throws ParseException if the input could not be parsed.
   *  - get_expected(std::ostream &out) const: writes a brief description
   *    of the next token expected by this rule to "out".
   */

  /** \brief Exception thrown for parse errors. */
  class ParseException : public std::exception
  {
    std::string msg;

  public:
    ParseException(const std::string &_msg)
      : msg(_msg)
    {
    }

    ~ParseException() throw()
    {
    }

    const char *what() throw()
    {
      return msg.c_str();
    }
  };

  /** \brief Used when a parser has no sensible return value. */
  class nil_t { };

  template<typename P>
  class set_expected_p;

  /** \brief The base class for all parser objects.
   *
   *  Having a common base for parsers allows us to do operator
   *  overloading more sanely.  In the future, it could also be a
   *  locus for various sorts of frontend activities (e.g., accepting
   *  a Boost-style range instead of two iterators).
   *
   *  \todo It's not clear that the Curious Template Pattern does much
   *  for us here; just having an empty base class might be equally
   *  useful.
   *
   *  \tparam DerivedT   The derived type that actually implements the parser's
   *                     behavior.  Must be a subclass of this instantiation
   *                     of parser_base, or bad stuff will happen.
   *  \tparam ReturnType The type returned by the parse operation.
   */
  template<typename DerivedT, typename ReturnType>
  class parser_base
  {
  protected:
    parser_base()
    {
    }

    parser_base(const parser_base &other)
    {
    }

    ~parser_base()
    {
    }

  public:
    typedef ReturnType return_type;

    // These names are provided both here *and* in the derived class
    // to allow operators to accept a parser_base<> and still get
    // compile-time resolution of the parse functionality (well, sort
    // of).

    /** \brief Parse a range of text. */
    template<typename Iter>
    return_type operator()(Iter &begin, const Iter &end) const
    {
      return derived().parse(begin, end);
    }

    /** \brief Write a description of what we expect to see here to
     *  the given stream.
     */
    void get_expected_description(std::ostream &out) const
    {
      derived().get_expected(out);
    }

    /** \brief Get a reference to this object, cast to its derived type.
     *
     *  We need a reference to the derived type whenever this is going
     *  to be stored (since you shouldn't store parser_base by
     *  accident).
     */
    DerivedT &derived()
    {
      return *static_cast<DerivedT *>(*this);
    }

    /** \brief Get a reference to this object, cast to its derived type.
     *
     *  We need a reference to the derived type whenever this is going
     *  to be stored (since you shouldn't store parser_base by
     *  accident).
     */
    const DerivedT &derived() const
    {
      return *static_cast<const DerivedT *>(this);
    }

    /** \brief Create a parser that modifies the expected value of
     *  this parser.
     */
    set_expected_p<DerivedT> operator[](const std::string &msg) const
    {
      return set_expected_p<DerivedT>(derived(), msg);
    }
  };

  /** \brief Metafunction class to retrieve the return type of a parser. */
  struct get_return_type
  {
    template<typename P>
    struct apply
    {
      typedef typename P::return_type type;
    };
  };

  /** \brief Atomic parsers */
  // @{

  /** \brief Parse only the given character.
   */
  template<typename CType>
  class ch_p : public parser_base<ch_p<CType>, CType >
  {
    CType c;

  public:
    ch_p(CType _c)
      : c(_c)
    {
    }

    typedef typename parser_base<ch_p<CType>, CType>::return_type return_type;

    template<typename Iter>
    return_type parse(Iter &begin, const Iter &end) const
    {
      if(begin == end)
        throw ParseException((boost::format(_("Expected '%s', but got EOF.")) %  c).str());
      else if(*begin != c)
        throw ParseException((boost::format(_("Expected '%s', but got '%s'.")) % c % *begin).str());
      else
        {
          ++begin;
          return c;
        }
    }

    void get_expected(std::ostream &out) const
    {
      out << "'" << c << "'";
    }
  };

  /** \brief Create a parser that accepts only the given character. */
  inline ch_p<char> ch(char c) { return ch_p<char>(c); }
  /** \brief Create a parser that accepts only the given character. */
  inline ch_p<unsigned char> uch(unsigned char c) { return ch_p<unsigned char>(c); }
  /** \brief Create a parser that accepts only the given character. */
  inline ch_p<signed char> sch(signed char c) { return ch_p<signed char>(c); }

  /** \brief Create a parser that accepts only the given character. */
  inline ch_p<wchar_t> wch(wchar_t c) { return ch_p<wchar_t>(c); }

  /** \brief Parse any character. */
  template<typename CType>
  class anychar_p : public parser_base<anychar_p<CType>, CType >
  {
  public:
    anychar_p()
    {
    }

    template<typename Iter>
    CType parse(Iter &begin, const Iter &end) const
    {
      if(begin == end)
        {
          std::ostringstream msg;
          msg << _("Expected any character, but got EOF.");
          throw ParseException(msg.str());
        }
      else
        {
          CType rval = *begin;
          ++begin;
          return rval;
        }
    }

    void get_expected(std::ostream &out) const
    {
      out << _("any character");
    }
  };

  /** \brief Create a parser that accepts any character. */
  inline anychar_p<char> anychar() { return anychar_p<char>(); }

  /** \brief Create a parser that accepts any unsigned char. */
  inline anychar_p<unsigned char> anyuchar() { return anychar_p<unsigned char>(); }

  /** \brief Create a parser that accepts any signed char. */
  inline anychar_p<signed char> anyschar() { return anychar_p<signed char>(); }

  /** \brief Create a parser that accepts any wide character. */
  inline anychar_p<wchar_t> anywchar() { return anychar_p<wchar_t>(); }



  /** \brief Create a parser that accepts any character passing a
   *  predicate.
   */
  template<typename CType, typename F>
  class charif_p : public parser_base<charif_p<CType, F>, CType>
  {
    F f;
    std::string description;

  public:
    charif_p(const F &_f, const std::string &description)
      : f(_f)
    {
    }

    typedef CType return_type;

    template<typename Iter>
    return_type parse(Iter &begin, const Iter &end) const
    {
      if(begin == end)
        throw ParseException((boost::format(_("Expected %s, but got EOF.")) % description).str());
      else
        {
          CType c(*begin);

          if(f(c))
            {
              ++begin;
              return c;
            }
          else
            throw ParseException((boost::format(_("Expected %s, but got '%c'.")) % description % c).str());
        }
    }

    void get_expected(std::ostream &out) const
    {
      out << description;
    }
  };

  /** \brief Parsers for character classes.
   *
   *  These use the standard C character classes.  Arguably these
   *  should be implemented by hand instead, as the standard character
   *  classes are locale-dependent.  However, aptitude's existing
   *  parsers use character classes, so this is not a regression (file
   *  under "don't try to fix the whole world at once").
   */

  // @{

  class alnum_f
  {
  public:
    bool operator()(char c) const { return isalnum(c); }
  };
  typedef charif_p<char, alnum_f> alnum_p;
  /** \brief Create a parser that accepts only letters and numbers. */
  inline alnum_p alnum() { return alnum_p(alnum_f(), "letter or digit"); }


  class alpha_f
  {
  public:
    bool operator()(char c) const { return isalpha(c); }
  };
  typedef charif_p<char, alpha_f> alpha_p;
  /** \brief Create a parser that accepts only letters. */
  inline alpha_p alpha() { return alpha_p(alpha_f(), "letter"); }


  class ascii_f
  {
  public:
    bool operator()(char c) const { return isascii(c); }
  };
  typedef charif_p<char, ascii_f> ascii_p;
  inline ascii_p ascii() { return ascii_p(ascii_f(), "an ASCII character"); }


  class blank_f
  {
  public:
    bool operator()(char c) const { return isblank(c); }
  };
  typedef charif_p<char, blank_f> blank_p;
  /** \brief Create a parser that accepts only blank characters. */
  inline blank_p blank() { return blank_p(blank_f(), "a blank"); }


  class cntrl_f
  {
  public:
    bool operator()(char c) const { return iscntrl(c); }
  };
  typedef charif_p<char, cntrl_f> cntrl_p;
  /** \brief Create a parser that accepts only control characters. */
  inline cntrl_p cntrl() { return cntrl_p(cntrl_f(), "a control character"); }


  class digit_f
  {
  public:
    bool operator()(char c) const { return isdigit(c); }
  };
  typedef charif_p<char, digit_f> digit_p;
  /** \brief Create a parser that accepts only digits. */
  inline digit_p digit() { return digit_p(digit_f(), "a digit"); }


  class graph_f
  {
  public:
    bool operator()(char c) const { return isgraph(c); }
  };
  typedef charif_p<char, graph_f> graph_p;
  /** \brief Create a parser that accepts only printable characters
   *  that aren't space.
   */
  inline graph_p graph() { return graph_p(graph_f(), "a visible character"); }


  class lower_f
  {
  public:
    bool operator()(char c) const { return islower(c); }
  };
  typedef charif_p<char, lower_f> lower_p;
  /** \brief Create a parser that accepts only lower-case characters.
   */
  inline lower_p lower() { return lower_p(lower_f(), "a lower-case letter"); }


  class print_f
  {
  public:
    bool operator()(char c) const { return isprint(c); }
  };
  typedef charif_p<char, print_f> print_p;
  /** \brief Create a parser that accepts only printable characters. */
  inline print_p print() { return print_p(print_f(), "a printable character"); }


  class punct_f
  {
  public:
    bool operator()(char c) const { return ispunct(c); }
  };
  typedef charif_p<char, punct_f> punct_p;
  /** \brief Create a parser that accepts only punctuation characters. */
  inline punct_p punct() { return punct_p(punct_f(), "punctuation"); }


  class space_f
  {
  public:
    bool operator()(char c) const { return isspace(c); }
  };
  typedef charif_p<char, space_f> space_p;
  /** \brief Create a parser that accepts only whitespace characters. */
  inline space_p space() { return space_p(space_f(), "whitespace"); }


  class upper_f
  {
  public:
    bool operator()(char c) const { return isupper(c); }
  };
  typedef charif_p<char, upper_f> upper_p;
  /** \brief Create a parser that accepts only upper-case
   *  characters.
   */
  inline upper_p upper() { return upper_p(upper_f(), "an upper-case letter"); }


  class xdigit_f
  {
  public:
    bool operator()(char c) const { return isxdigit(c); }
  };
  typedef charif_p<char, xdigit_f> xdigit_p;
  /** \brief Create a parser that accepts only hexadecimal digits. */
  inline xdigit_p xdigit() { return xdigit_p(xdigit_f(), "a hexadecimal digit"); }

  // @}

  /** \brief A parser for integer values.
   *
   *  Stops at the first non-digit; it's up to the client to require
   *  trailing whitespace as necessary.
   *
   *  Note: this is locale-dependent.  Should it be changed to not be?
   */
  class integer_p : public parser_base<integer_p, int>
  {
  public:
    typedef int return_type;

    template<typename Iter>
    int parse(Iter &begin, const Iter &end) const
    {
      if(begin == end)
        throw ParseException(_("Expected an integer, got EOF."));

      Iter start = begin;

      // First, parse the sign (if any).
      if(begin != end && *begin == '-')
        ++begin;

      if(begin == end)
        throw ParseException(_("Expected an integer following '-', got EOF."));

      while(begin != end && isdigit(*begin))
        ++begin;

      if(start == begin)
        throw ParseException((boost::format(_("Expected an integer, got '%c'.")) % *begin).str());

      // Lean on strtol for now.
      std::string s(start, begin);
      char *endptr;
      errno = 0;
      long rval = strtol(s.c_str(), &endptr, 0);
      if(errno != 0)
        {
          int errnum = errno;
          throw ParseException(sstrerror(errnum));
        }

      if(*endptr != '\0')
        throw ParseException((boost::format(_("Invalid integer: \"%s\".")) % s).str());

      try
        {
          return boost::numeric_cast<int>(rval);
        }
      catch(boost::bad_numeric_cast &ex)
        {
          throw ParseException((boost::format(_("Invalid integer: \"%s\".")) % ex.what()).str());
        }
    }
  };

  /** \brief Create a parser that recognizes integers. */
  inline integer_p integer() { return integer_p(); }

  /** \brief A parser that recognizes EOF. */
  class eof : public parser_base<eof, nil_t>
  {
  public:
    typedef nil_t return_type;

    template<typename Iter>
    return_type parse(Iter &begin, const Iter &end) const
    {
      if(begin != end)
        throw ParseException((boost::format(_("Expected EOF, got '%c'.")) % *begin).str());

      return nil_t();
    }

    void get_expected(std::ostream &out) const
    {
      out << "EOF";
    }
  };

  /** \brief A parser that recognizes a particular string.
   *
   *  \note For the sake of efficiency, returns nothing (normally this
   *  is invoked with a constant string and the caller doesn't care
   *  about getting it back).
   */
  class str : public parser_base<str, nil_t>
  {
    std::string s;
  public:
    explicit str(const std::string &_s)
      : s(_s)
    {
    }

    typedef nil_t return_type;

    template<typename Iter>
    return_type parse(Iter &begin, const Iter &end) const
    {
      Iter start = begin;

      std::string::const_iterator s_begin(s.begin()), s_end(s.end());

      while(begin != end && s_begin != s_end)
        {
          if(*begin != *s_begin)
            throw ParseException((boost::format("Expected \"%s\", but got '%c' following \"%s\".")
                                  % s % *begin % std::string(start, begin)).str());

          ++begin;
          ++s_begin;
        }

      if(s_begin != s_end)
        throw ParseException((boost::format("Expected \"%s\", but got EOF following \"%s\".")
                              % s % std::string(start, begin)).str());

      return nil_t();
    }

    void get_expected(std::ostream &out) const
    {
      out << '"' << s << '"';
    }
  };


  /** \brief A parser that parses nothing and returns a fixed value.
   *
   *  \tparam T   The value type to return; must be CopyConstructible.
   */
  template<typename T>
  class val_p : public parser_base<val_p<T>, T>
  {
    T value;

  public:
    val_p(const T &_value)
      : value(_value)
    {
    }

    typedef T return_type;

    template<typename Iter>
    T parse(Iter &begin, const Iter &end) const
    {
      return value;
    }

    void get_expected(std::ostream &out) const
    {
      out << _("anything");
    }
  };

  /** \brief Create a parser that parses nothing and returns a value. */
  template<typename T>
  inline val_p<T> val(const T &value)
  {
    return val_p<T>(value);
  }

  /** \brief Create a parser that parses nothing and returns a string.
   *
   *  This overload is purely so val("abc") works; otherwise the
   *  compiler tries to save an array or a char pointer.
   */
  inline val_p<std::string> val(const char *s)
  {
    return val_p<std::string>(s);
  }

  // @}

  /** \brief Combine two parsers in sequence, throwing away the first
   *  parser's result.
   *
   *  \todo Implement this using Boost.Fusion containers, like or_p.
   */
  template<typename P1, typename P2>
  class andthen_p : public parser_base<andthen_p<P1, P2>, typename P2::return_type>
  {
    P1 p1;
    P2 p2;

  public:
    andthen_p(const P1 &_p1, const P2 &_p2)
      : p1(_p1), p2(_p2)
    {
    }

    typedef typename P2::return_type return_type;

    template<typename Iter>
    return_type parse(Iter &begin, const Iter &end) const
    {
      p1(begin, end);
      return p2(begin, end);
    }

    void get_expected(std::ostream &out) const
    {
      p1.get_expected_description(out);
    }
  };

  /** \brief Combine two parsers in sequence, throwing away the first
   *  parser's result.
   */
  template<typename Rule1, typename ReturnType1, typename Rule2, typename ReturnType2>
  andthen_p<Rule1, Rule2>
  inline operator>>(const parser_base<Rule1, ReturnType1> &p1, const parser_base<Rule2, ReturnType2> &p2)
  {
    return andthen_p<Rule1, Rule2>(p1.derived(), p2.derived());
  }

  /** \brief A parser that modifies the expected value of its target. */
  template<typename P>
  class set_expected_p : public parser_base<set_expected_p<P>, typename P::return_type>
  {
    P p;
    std::string msg;

  public:
    set_expected_p(const P &_p, const std::string &_msg)
      : p(_p), msg(_msg)
    {
    }

    typedef typename P::return_type return_type;

    template<typename Iter>
    return_type parse(Iter &begin, const Iter &end) const
    {
      return p(begin, end);
    }

    void get_expected(std::ostream &out) const
    {
      out << msg;
    }
  };

  /** \brief Parse zero or more occurrences of the input pattern,
   *  invoking a function object for each occurrence.
   *
   *  Has no return value (it returns nil_t).  If the functor throws a
   *  ParseException, this parser will fail with that exception.
   */
  template<typename P, typename F>
  class foreach_p : public parser_base<foreach_p<P, F>, nil_t>
  {
    P p;
    F f;

  public:
    foreach_p(const P &_p, const F &_f)
      : p(_p), f(_f)
    {
    }

    typedef nil_t return_type;

    template<typename Iter>
    return_type parse(Iter &begin, const Iter &end) const
    {
      while(true)
        {
          typename P::return_type result;

          try
            {
              result = p(begin, end);
            }
          catch(ParseException &)
            {
              break;
            }

          f(result);
        }

      return nil_t();
    }

    void get_expected(std::ostream &out) const
    {
      p.get_expected(out);
    }
  };

  /** \brief Parse zero of more occurrences of the input pattern,
   *  invoking a function object for each occurrence.
   */
  template<typename P, typename F>
  inline foreach_p<P, F> foreach(const P &p, const F &f)
  {
    return foreach_p<P, F>(p, f);
  }

  /** \brief Useful functors. */
  // @{

  /** \brief Push incoming values onto an STL-style sequence. */
  template<typename C>
  class push_back_f
  {
    C &c;

  public:
    /** \brief Construct a push-back functor for the given container. */
    push_back_f(C &_c)
      : c(_c)
    {
    }

    template<typename V>
    void operator()(const V &v) const
    {
      c.push_back(v);
    }
  };

  /** \brief Push incoming values onto an STL-style sequence. */
  template<typename C>
  push_back_f<C> push_back_a(C &c)
  {
    return push_back_f<C>(c);
  }

  // @}

  /** \brief Parse zero or more occurrences of the input pattern,
   *  ignoring the resulting values.
   */
  template<typename P>
  class skip_p : public parser_base<skip_p<P>, nil_t>
  {
    P p;
  public:

    skip_p(const P &_p)
      : p(_p)
      {
      }

    typedef nil_t return_type;

    template<typename Iter>
    return_type parse(Iter &begin, const Iter &end) const
    {
      while(true)
        {
          Iter where = begin;
          try
            {
              p(begin, end);
            }
          catch(ParseException &)
            {
              if(begin != where)
                throw;
              break;
            }
        }

      return nil_t();
    }
  };

  /** \brief Create a parser that skips zero or more copies of the
   *  input parser.
   */
  template<typename P>
  inline skip_p<P> skip(const P &p)
  {
    return skip_p<P>(p);
  }

  /** \brief A parser that applies a sub-parser zero or more times,
   *  collecting its parses.
   *
   *  \tparam P  The sub-parser.
   *  \tparam Container  The container type to return.  e.g., change
   *                     to std::string to produce a string value.
   *
   *  The returned container is wrapped in a shared_ptr to avoid
   *  unnecessary copies.
   */
  template<typename P, typename Container = std::vector<typename P::return_type> >
  class many_p : public parser_base<many_p<P, Container>, boost::shared_ptr<Container> >
  {
    P p;
  public:
    many_p(const P &_p)
      : p(_p)
    {
    }

    typedef boost::shared_ptr<Container> return_type;

    template<typename Iter>
    return_type parse(Iter &begin, const Iter &end) const
    {
      boost::shared_ptr<Container> rval =
        boost::make_shared<Container>();

      while(true)
        {
          Iter where = begin;
          try
            {
              rval->push_back(p.parse(begin, end));
            }
          catch(ParseException &ex)
            {
              if(where != begin)
                throw;
              break;
            }
        }

      return rval;
    }
  };

  /** \brief Apply the input parser zero or more times, saving
   *  the results to a vector.
   */
  template<typename P>
  inline many_p<P> many(const P &p)
  {
    return many_p<P>(p);
  }

  /** \brief Apply the input parser zero or more times, saving
   *  the results to a string.
   *
   *  \note The input parser must have a return type of char.
   */
  template<typename P>
  inline many_p<P, std::string> many_string(const P &p)
  {
    return many_p<P, std::string>(p);
  }

  /** \brief Given a Boost.Fusion sequence, try each of the parsers it
   *  contains in turn.
   *
   *  If the first parser fails after consuming no input, the second
   *  parser is tried.  If the first parser fails after consuming some
   *  input, this parser fails.  If none of the parsers succeed, this
   *  parser fails.
   *
   *  The return_type of or_p is the return_type of the first parser.
   *  The parsers that follow it must be convertible to the same
   *  return_type.
   */
  template<typename C>
  class or_p : public parser_base<or_p<C>, typename boost::mpl::front<C>::type::return_type>
  {
  public:
    typedef typename boost::mpl::front<C>::type::return_type return_type;

  private:
    // Metaprogramming to verify that all the sub-types are the same.
    typedef typename boost::mpl::transform<C, get_return_type>::type C_return_types;
    typedef typename boost::mpl::transform<C_return_types,
                                           boost::is_same<return_type,
                                                          boost::mpl::_1> >::type C_return_types_equal_to_front;
    typedef typename boost::mpl::fold<C_return_types_equal_to_front,
                                      boost::mpl::true_,
                                      boost::mpl::and_<boost::mpl::_1, boost::mpl::_2> >::type all_subtypes_are_equal;

    BOOST_STATIC_ASSERT(all_subtypes_are_equal::value);

    C values;

    class do_get_expected
    {
      std::ostream &out;
      bool &first;

    public:
      do_get_expected(std::ostream &_out, bool &_first)
        : out(_out),
          first(_first)
      {
      }

      template<typename Rule>
      void operator()(const Rule &r) const
      {
        if(!first)
          out << _(" or ");
        else
          first = false;

        r.get_expected(out);
      }
    };

    // The do_or routine is constructed so that the base vs non-base
    // case can be resolved as compile-time (it has to be since *iter
    // doesn't compile at all for the end iter).  The frontend routine
    // "do_or" is called as the main entry point and for recursive
    // calls, and it dispatches to "do_or_conditional" using
    // overloading and boost::fusion::equal_to to decide which case to
    // take.

    // Base case (we ran out of branches, so fail):
    template<typename TextIter, typename CIter>
    return_type do_or_conditional(TextIter &begin, const TextIter &initialBegin, const TextIter &end,
                                  const CIter &valuesIter, boost::mpl::true_) const
    {
      std::ostringstream msg;
      get_expected(msg);

      // ForTranslators: this is used to generate an error
      // message; a brief description of what we expected to see
      // is inserted into it.
      throw ParseException((boost::format(_("Expected %s")) % msg.str()).str());
    }

    // Non-base case (try the first branch):
    template<typename TextIter, typename CIter>
    return_type do_or_conditional(TextIter &begin, const TextIter &initialBegin, const TextIter &end,
                                  const CIter &valuesIter, boost::mpl::false_) const
    {
      try
        {
          return (*valuesIter).parse(begin, end);
        }
      catch(ParseException &ex)
        {
          if(initialBegin != begin)
            throw;
        }

      // We only get here if the parse failed, so go to the next entry
      // in "values".
      return do_or(begin, initialBegin, end, boost::fusion::next(valuesIter));
    }

    template<typename TextIter, typename CIter>
    return_type do_or(TextIter &begin, const TextIter &initialBegin, const TextIter &end,
                      const CIter &valuesIter) const
    {
      typedef typename boost::fusion::result_of::end<C>::type EndCIter;
      typedef typename boost::fusion::result_of::equal_to<CIter, EndCIter>::type IsAtEnd;

      return do_or_conditional(begin, initialBegin, end, valuesIter, IsAtEnd());
    }

  public:
    or_p(const C &_values)
      : values(_values)
    {
    }

    /** \brief Retrieve the sub-parsers of this parser.
     *
     *  Mainly used by the implementation of operator|.
     */
    const C &get_values() const { return values; }

    template<typename Iter>
    return_type parse(Iter &begin, const Iter &end) const
    {
      Iter initialBegin = begin;
      return do_or(begin, initialBegin, end, boost::fusion::begin(values));
    }

    void get_expected(std::ostream &out) const
    {
      bool first = true;
      boost::fusion::for_each(values, do_get_expected(out, first));
    }
  };

  // Note that operator| has several overloads that use the magic of
  // Boost.Fusion to flatten expressions at compile-time.  (hopefully
  // at compile-time)

  // Note: this could be a bit more efficient if I was able to build
  // an intermediate container type that could be used only as a
  // constructor argument to "or".  That way I wouldn't need to
  // eagerly copy structures around all over.  However, doing this now
  // would be premature optimization.

  // Each of these routines creates an "or" that stores a Fusion
  // vector with copies of the input arguments.  Doing this is a bit
  // tricky since a lot of Fusion objects like to store references to
  // their input arguments.

  /** \brief Create a parser that tries the left-hand argument; if it
   *  fails without advancing "begin", then the right-hand argument is
   *  tried.
   */
  template<typename C1, typename C2>
  inline or_p<typename boost::fusion::result_of::as_vector<boost::fusion::joint_view<C1, C2> >::type>
  operator|(const or_p<C1> &o1, const or_p<C2> &o2)
  {
    // We use joint_view directly instead of via join() because
    // template parameter inference puts too many consts into the
    // template arguments otherwise.
    typedef boost::fusion::joint_view<C1, C2> interim_container;
    typedef typename boost::fusion::result_of::as_vector<interim_container>::type result_container;
    return or_p<result_container>(boost::fusion::as_vector(boost::fusion::join(o1.get_values(),
                                                                               o2.get_values())));
  }

  /** \brief Create a parser that tries the left-hand argument; if it
   *  fails without advancing "begin", then the right-hand argument is
   *  tried.
   */
  template<typename C, typename Rule, typename ReturnType>
  inline or_p<typename boost::fusion::result_of::as_vector<typename boost::fusion::result_of::push_back<C, Rule>::type>::type>
  operator|(const or_p<C> &o, const parser_base<Rule, ReturnType> &p)
  {
    typedef typename boost::fusion::result_of::push_back<C, Rule>::type
      interim_container;
    typedef typename boost::fusion::result_of::as_vector<interim_container>::type
      result_container;
    return or_p<result_container>(boost::fusion::as_vector(boost::fusion::push_back(o.get_values(),
                                                                                    p.derived())));
  }

  /** \brief Create a parser that tries the left-hand argument; if it
   *  fails without advancing "begin", then the right-hand argument is
   *  tried.
   */
  template<typename C, typename Rule, typename ReturnType>
  inline or_p<typename boost::fusion::result_of::as_vector<boost::fusion::joint_view<boost::fusion::cons<Rule>, C> >::type>
  operator|(const parser_base<Rule, ReturnType> &p, const or_p<C> &o)
  {
    typedef boost::fusion::result_of::push_front<C, Rule>
      interim_container;
    typedef typename boost::fusion::result_of::as_vector<interim_container>::type
      result_container;
    return or_p<result_container>(boost::fusion::as_vector(boost::fusion::push_front(o.get_values(),
                                                                                     p.derived())));
  }

  /** \brief Create a parser that tries the left-hand argument; if it
   *  fails without advancing "begin", then the right-hand argument is
   *  tried.
   */
  template<typename Rule1, typename ReturnType1, typename Rule2, typename ReturnType2>
  inline or_p<boost::fusion::vector<Rule1, Rule2> >
  operator|(const parser_base<Rule1, ReturnType1> &p1, const parser_base<Rule2, ReturnType2> &p2)
  {
    typedef boost::fusion::vector<Rule1, Rule2>
      result_container;

    return or_p<result_container>(result_container(p1.derived(), p2.derived()));
  }


  /** \brief A parser that attempts its sub-parser, restoring the value of
   *  "begin" if it fails.
   *
   *  Essentially makes every failure into a zero-length failure.
   */
  template<typename P>
  class maybe_p : public parser_base<maybe_p<P>, typename P::return_type>
  {
    P p;
  public:
    maybe_p(const P &_p)
      : p(_p)
    {
    }

    typedef typename P::return_type return_type;

    template<typename Iter>
    return_type parse(Iter &begin, const Iter &end) const
    {
      Iter start = begin;

      try
        {
          return p(begin, end);
        }
      catch(ParseException &ex)
        {
          begin = start;
          throw;
        }
    }

    void get_expected(std::ostream &out) const
    {
      p.get_expected(out);
    }
  };

  /** \brief Create a parser that attempts its parser, restoring the
   *  value of "begin" if it fails.
   */
  template<typename P>
  maybe_p<P> maybe(const P &p)
  {
    return maybe_p<P>(p);
  }

  /** \brief Apply each parser in the given Boost.Fusion container in
   *  turn, returning a tuple of their results if they all match and
   *  throwing an exception otherwise.
   *
   *  An instance of this object is constructed using operator, like
   *  so:
   *
   *  (p1 p2, p3, p4)
   */
  template<typename C>
  class tuple_p : public parser_base<tuple_p<C>,
                                     typename boost::fusion::result_of::as_vector<typename boost::mpl::transform<typename boost::fusion::result_of::as_vector<C>::type, get_return_type>::type >::type>
  // as_vector is invoked on C before we pass it to mpl::transform,
  // because mpl::transform doesn't seem to work on arbitrary fusion
  // containers (e.g., joint_view produces an error).
  {
  public:
    typedef typename boost::fusion::result_of::as_vector<typename boost::mpl::transform<typename boost::fusion::result_of::as_vector<C>::type, get_return_type>::type>::type return_type;
    typedef typename boost::fusion::result_of::as_vector<C>::type values_type;

  private:

    // Used to fold down the list of parsers and produce the output
    // list.  The state parameter will be the final return value; we
    // build it left-to-right (since fold works left-to-right).
    template<typename TextIter>
    class do_parse
    {
      TextIter &begin;
      const TextIter &end;

    public:
      do_parse(TextIter &_begin, const TextIter &_end)
        : begin(_begin), end(_end)
      {
      }

      template<typename Args>
      struct result;

      template<typename Element, typename ResultIn>
      struct result<do_parse(const Element &, const ResultIn &)>
      {
        typedef typename boost::fusion::result_of::push_back<const ResultIn, typename Element::return_type>::type type;
      };

      template<typename Element, typename ResultIn>
      typename boost::fusion::result_of::push_back<const ResultIn, typename Element::return_type>::type
      operator()(const Element &e, const ResultIn &result) const
      {
        return boost::fusion::push_back(result, e.parse(begin, end));
      }
    };

    values_type values;

  public:
    // Note that D only has to be convertible to C.  This works around
    // some places where slightly different types than we expect are
    // passed in, but was can convert them to values_type via
    // as_vector anyway.
    template<typename D>
    tuple_p(const D &_values)
      : values(boost::fusion::as_vector(_values))
    {
    }

    const values_type &get_values() const { return values; }

    template<typename TextIter>
    return_type parse(TextIter &begin, const TextIter &end) const
    {
      return boost::fusion::as_vector(boost::fusion::fold(values, boost::fusion::make_vector(), do_parse<TextIter>(begin, end)));
    }

    void get_expected(std::ostream &out) const
    {
      boost::fusion::front(values).get_expected_description(out);
    }
  };

  /** \brief Combine two tuple parsers to produce a new parser
   *  that concatenates the two tuple parsers.
   *
   *  The generated parser parses the first tuple followed by the
   *  second tuple; it returns a single tuple that contains the
   *  elements returned by the first parser, followed by the elements
   *  returned by the second parser.
   */
  template<typename C1, typename C2>
  inline tuple_p<typename boost::fusion::result_of::join<
                   typename tuple_p<C1>::values_type,
                   typename tuple_p<C2>::values_type>::type>
  operator,(const tuple_p<C1> &t1, const tuple_p<C2> &t2)
  {
    typedef typename boost::fusion::result_of::join<typename tuple_p<C1>::values_type, typename tuple_p<C2>::values_type>
      result_container;

    return tuple_p<result_container>(boost::fusion::join(t1.get_values(), t2.get_values()));
  }

  /** \brief Add a new entry to the left of a tuple parser.
   *
   *  The generated parser parses the non-tuple element followed by
   *  the elements of the tuple; it returns a single tuple that
   *  contains the element returned by the new non-tuple parser,
   *  followed by the elements returned by the tuple parser.
   */
  template<typename Rule, typename ReturnType, typename C>
  inline tuple_p<typename boost::fusion::result_of::push_front<
                   typename tuple_p<C>::values_type,
                   Rule>::type>
  operator,(const parser_base<Rule, ReturnType> &p1, const tuple_p<C> &t2)
  {
    typedef typename boost::fusion::result_of::push_front<typename tuple_p<C>::values_type, Rule>::type
      result_container;

    return tuple_p<result_container>(boost::fusion::push_front(t2.get_values(), p1.derived()));
  }

  /** \brief Add a new entry to the right of a tuple parser.
   *
   *  The generated parser parses the elements of the tuple, followed
   *  by the new non-tuple element; it returns a single tuple that
   *  contains the elements returned by the tuple parser, followed by
   *  the element returned by the new non-tuple parser
   */
  template<typename Rule, typename ReturnType, typename C>
  inline tuple_p<typename boost::fusion::result_of::push_back<
                   typename tuple_p<C>::values_type,
                   Rule>::type>
  operator,(const tuple_p<C> &t1, const parser_base<Rule, ReturnType> &p2)
  {
    typedef typename boost::fusion::result_of::push_back<typename tuple_p<C>::values_type, Rule>::type
      result_container;

    return tuple_p<result_container>(boost::fusion::push_back(t1.get_values(), p2.derived()));
  }

  /** \brief Join two parsers into a tuple parser.
   *
   *  The generated parser runs the first parser and then the second
   *  parser; it returns a tuple containing the value returned by the
   *  first parser, followed by the value returned by the second
   *  parser.
   */
  template<typename Rule1, typename ReturnType1, typename Rule2, typename ReturnType2>
  inline tuple_p<boost::fusion::vector<Rule1, Rule2> >
  operator,(const parser_base<Rule1, ReturnType1> &p1, const parser_base<Rule2, ReturnType2> &p2)
  {
    return tuple_p<boost::fusion::vector<Rule1, Rule2> >(boost::fusion::vector<Rule1, Rule2>(p1.derived(), p2.derived()));
  }
}

#endif // PARSERS_H
