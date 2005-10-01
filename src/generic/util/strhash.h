// strhash.h, -*-c++-*-
//
//  Copyright 199 Daniel Burrows
//
//  Make hash_map<string, footype> do the Right Thing.

#ifndef STRHASH_H
#define STRHASH_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_HASH_MAP
#include <hash_map>
#else
#ifdef HAVE_EXT_HASH_MAP
#include <ext/hash_map>
#else
// Fallback to the non-hashing map class
#include <map>
#define hash_map map
#endif
#endif

#include <string>

#if defined(HAVE_HASH_MAP) || defined(HAVE_EXT_HASH_MAP)
namespace HASH_NAMESPACE
{
  template <>
  struct hash<std::string>
  {
    inline size_t operator()(const std::string &s) const
    {
      return hash<char *>()(s.c_str());
    }
  };
}
#endif

#endif
