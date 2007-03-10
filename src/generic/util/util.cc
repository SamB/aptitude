// util.cc
//
//   Copyright (C) 2005, 2007 Daniel Burrows
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

#include "util.h"

#include <ctype.h>
#include <errno.h>
#include <pwd.h>
#include <stdarg.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include "eassert.h"

using namespace std;

void stripws(string &s)
{
  size_t start = 0;
  while(start < s.size() && isspace(s[start]))
    ++start;

  size_t end = s.size();
  while(end > 0 && isspace(s[end-1]))
    --end;

  if(start >= end)
    s.clear();
  else
    s.assign(s, start, end-start);
}

string ssprintf(const char *format, ...)
{
  va_list ap;

  va_start(ap, format);
  string rval = vssprintf(format, ap);
  va_end(ap);

  return rval;
}

const int initbufsize=512;

string vssprintf(const char *format, va_list ap)
{
  char buf[initbufsize];
  const int amt = vsnprintf(buf, initbufsize, format, ap);

  if(amt < initbufsize)
    return buf;
  else
    {
      const int buf2size = amt + 1;
      char *buf2 = new char[buf2size];

      const int amt2 = vsnprintf(buf2, buf2size, format, ap);

      eassert(amt2 < buf2size);

      string rval(buf2, amt2);

      delete[] buf2;

      return rval;
    }
}

wstring swsprintf(const wchar_t *format, ...)
{
  va_list ap;

  va_start(ap, format);
  wstring rval = vswsprintf(format, ap);
  va_end(ap);

  return rval;
}

wstring vswsprintf(const wchar_t *format, va_list ap)
{
  wchar_t buf[initbufsize];
  int amt = vswprintf(buf, initbufsize, format, ap);

  if(amt < initbufsize)
    return buf;
  else
    {
      wchar_t *buf2 = new wchar_t[amt+1];

      int amt2 = vswprintf(buf2, initbufsize, format, ap);

      eassert(amt2 < amt+1);

      wstring rval(buf2, amt2);

      delete[] buf2;

      return rval;
    }
}

string sstrftime(const char *format, const tm *tm)
{
  size_t bufsize = 512;

  while(bufsize < 512 * 512)
    {
      char *buf = new char[bufsize];

      buf[0] = '\1';
      size_t result = strftime(buf, bufsize, format, tm);

      if(result == 0 && buf[0] != '\0')
	{
	  delete[] buf;
	  bufsize *= 2;
	}
      else
	{
	  // Could eliminate this with an "array smart pointer".
	  string rval(buf);
	  delete[] buf;
	  return rval;
	}
    }

  return "";
}

string sstrerror(int errnum)
{
  size_t bufsize = 512;

  while(bufsize < 512 * 512)
    {
      char *buf = new char[bufsize];

      char *result = strerror_r(errnum, buf, bufsize);

      if(result == NULL)
	{
	  delete[] buf;

	  if(errno == EINVAL)
	    return ssprintf("Invalid error code %d", errnum);
	  else if(errno != ERANGE)
	    return ssprintf("Unexpected error from strerror_r: %d", errnum);
	  else
	    bufsize *= 2;
	}
      else
	{
	  string rval(buf);
	  delete[] buf;
	  return rval;
	}
    }

  return "";
}

string get_homedir()
{
  passwd pwbuf;
  passwd *useless;
  uid_t myuid = getuid();

#ifdef _SC_GETPW_R_SIZE_MAX
  long bufsize = sysconf(_SC_GETPW_R_SIZE_MAX);
  char *buf = new char[bufsize];

  if(getpwuid_r(myuid, &pwbuf, buf, bufsize, &useless) != 0)
    {
      delete[] buf;
      return "";
    }
  else
    {
      string rval = pwbuf.pw_dir;
      delete[] buf;
      return rval;
    }
#else
  long bufsize = 512;
  bool done = false;

  // The 512 * 512 is an arbitrary cutoff to avoid allocating
  // unbounded amounts of memory when the system doesn't support a way
  // to directly determine the largest possible password structure.
  while(bufsize < 512 * 512)
    {
      char *buf = new char[bufsize];

      if(getpwuid_r(myuid, &pwbuf, buf, bufsize, &useless) == 0)
	{
	  string rval = pwbuf.pw_dir;
	  delete[] buf;
	  return rval;
	}
      else
	{
	  delete[] buf;
	  bufsize *= 2;
	}
    }

  return "";
#endif
}
