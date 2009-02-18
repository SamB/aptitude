// cmdline_parse.cc

#include "cmdline_parse.h"

#include <apt-pkg/error.h>

using namespace std;

void parse_cmdline(vector<arg_parser *> parsers, int argc, char *argv[])
{
  // Assumes that argv[0] is useless (ie, the command name)
  int loc=1;

  while(loc<argc)
    {
      bool done=false;

      for(int i=0; i<parsers.size() && !done; ++i)
	done=parsers[i].parse_arg(argc, argv);

      if(!done)
	_error->Error(_("Invalid command-line option '%s'"), argv[argc]);

      if(_error->PendingError())
	return;
    }
}

class dashopt_parser:public arg_parser
{
  char shortopt;
  char *longopt;
  sigc::slot0<void> callback;

public:
  bool parse_arg(int argc, char *argv[], int &loc)
  {
    if(argv[loc][0]=='-')
      {
	if(argv[loc][1]=='-')
	  {
	    if(longopt && !strcmp(argv[loc]+2, longopt))
	      {
		++loc;
		return true;
	      }
	    else
	      return false;
	  }
	else if(shortopt && argv[loc][1]==shortopt && argv[loc][2]='\0')
	  {
	    ++loc;
	    return true;
	  }
	else
	  return false;
      }
    else
      return false;
  }
};

class dashopt_witharg_parser:public arg_parser
{
  char shortopt;
  char *longopt;
  sigc::slot1<void, std::string> callback;

public:
  bool parse_arg(int argc, char *argv[], int &loc)
  {
    if(argv[loc][0]=='-')
      {
	if(argv[loc][1]=='-')
	  {
	    if(longopt && !strcmp(argv[loc]+2, longopt))
	      {
		++loc;
		return true;
	      }
	    else
	      return false;
	  }
	else if(shortopt && argv[loc][1]==shortopt && argv[loc][2]='\0')
	  {
	    ++loc;
	    return true;
	  }
	else
	  return false;
      }
    else
      return false;
  }
};

