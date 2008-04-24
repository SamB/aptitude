// cmdline_parse.h
//
//  Copyright 2002 Daniel Burrows
//

#ifndef CMDLINE_PARSE_H
#define CMDLINE_PARSE_H

#include <vector>
#include <std::string>

#include <sigc++/trackable.h>

/** \brief A comand-line parser
 *
 * 
 *  Mad?  They said I was mad.  But I didn't listen.  I didn't have to...
 * 
 *  Behold...the living horror of the object-oriented command-line parser!
 * 
 *  \file cmdline_parse.h
 */

// I'll parse arguments My Way, thank you.
//
// This provides a callback to parse the head of an input option stream.
//
// It should either:
//   (a) Consume some number of arguments and return true (if there are
//      appropriate arguments), or
//   (b) Return false.
class arg_parser:public sigc::trackable
{
public:
  virtual bool parse_arg(int argc, char *argv[], int &loc)=0;
};

// Caller is responsible for deleting the arg_parsers
bool parse_cmdline(std::vector<arg_parser *> parsers, int argc, char *argv[]);

// Factories for interesting basic arg_parsers.  These generally take either
// a slot to call or a value to set.

// The standard '-x', '--blah' option.  (no value)  If longopt is NULL, there
// is no long option; if shortopt is \0, same.
arg_parser *dashopt(char shortopt, char *longopt,
		    sigc::slot0<void> callback);

// Same, sets "flag" to true if the option is seen.
arg_parser *dashopt(char shortopt, char *longopt, bool &flag);

// The general callback-based version; the multiple forms allow
// implicit conversions.
arg_parser *dashopt_witharg(char shortopt, char *longopt,
			    sigc::slot1<void, std::string> callback);
arg_parser *dashopt_witharg(char shortopt, char *longopt,
			    std::string &val);
arg_parser *dashopt_witharg(char shortopt, char *longopt,
			    int &val);

// This one just accumulates its arguments in the given vector.
// Useful as a "default rule".
arg_parser *arg_accumulate();

#endif
