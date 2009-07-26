#!/bin/bash -e

# This script is invoked by "make check" to verify that we have
# correctly declared the exact set of Boost include files that
# "configure" should look for.
#
# Currently, this assumes that every occurrence of "boost/foo.hpp" is
# significant.  If stray strings that look like that find their way
# into the source code or configure, we'll have to look at #include
# lines and/or mark off the section of configure with a tag (and,
# e.g., use "awk" instead of "grep").

echo "Verifying that the configure check tests the correct set of header files."

BOOST_PATTERN='boost/[a-zA-Z0-9_./-]*\.hpp'

# Check that the source code and the configure check look for the same
# Boost headers.
SRC_OCCURRENCES=$((find src \( -name \*.cc -or -name \*.h \) -print0; find tests \( -name \*.cc -or -name \*.h \) -print0) \
                  | xargs -0 grep -h --only-matching "$BOOST_PATTERN" | sort -u)
CONFIGURE_OCCURRENCES=$(grep -h --only-matching "$BOOST_PATTERN" configure.ac | sort -u)

if ! cmp <(echo "$SRC_OCCURRENCES") <(echo "$CONFIGURE_OCCURRENCES")
then
    ONLY_SRC=$(comm -2 -3 <(echo "$SRC_OCCURRENCES") <(echo "$CONFIGURE_OCCURRENCES"))
    ONLY_CONFIG=$(comm -1 -3 <(echo "$SRC_OCCURRENCES") <(echo "$CONFIGURE_OCCURRENCES"))
    echo "Mismatch in Boost header file declarations."
    if [ -n "$ONLY_SRC" ]
    then
	echo "The following headers occur only in the source code:"
	echo "$ONLY_SRC"
    fi
    if [ -n "$ONLY_CONFIG" ]
    then
	echo "The following headers occur only in the configure script:"
	echo "$ONLY_CONFIG"
    fi

    echo "*** BOOST CONFIGURE CHECK MISMATCH."
    echo "*** Please update the configure script to match the source code."

    exit 1
fi

exit 0