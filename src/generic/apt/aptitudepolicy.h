// aptitudepolicy.h                  -*-c++-*-
//
//  Copyright 2001 Daniel Burrows
//
//  A policy class that allows Recommends and Suggests to be treated as
// "always important", "important for new installs", or "never important".

#ifndef APTITUDEPOLICY_H
#define APTITUDEPOLICY_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <apt-pkg/policy.h>

class aptitudePolicy:public pkgPolicy
{
public:
  aptitudePolicy(pkgCache *Owner)
    :pkgPolicy(Owner) {}

  bool IsImportantDep(pkgCache::DepIterator dep);
};

#endif
