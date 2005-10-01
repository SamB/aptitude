// bool_accumulate.h                           -*-c++-*-
//
//  Copyright 2005 Daniel Burrows

#ifndef BOOL_ACCUMULATE
#define BOOL_ACCUMULATE

/** Computes the return-value of the signal via a short-circuiting
 *  AND.  If no slots are connected to the signal, returns \b true.
 */
struct accumulate_and
{
  typedef bool result_type;
  template<typename T_iterator>
    result_type operator()(T_iterator first, T_iterator last) const
    {
      for(; first!=last; ++first)
	if(!*first)
	  return false;

      return true;
    }
};

/** Computes the return-value of the signal via a short-circuiting
 *  OR.  If no slots are connected to the signal, returns \b false.
 */
struct accumulate_or
{
  typedef bool result_type;
  template<typename T_iterator>
    result_type operator()(T_iterator first, T_iterator last) const
    {
      for(; first!=last; ++first)
	if(*first)
	  return true;

      return false;
    }
};

#endif
