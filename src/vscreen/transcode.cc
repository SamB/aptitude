// transcode.cc
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

#include "transcode.h"

#include <generic/util/eassert.h>
#include <errno.h>
#include <iconv.h>

using namespace std;

static std::string default_wtomb_err(int error,
				     const std::string &partial,
				     const std::wstring &input)
{
  return partial;
}

static std::wstring default_mbtow_err(int error,
				      const std::wstring &partial,
				      const std::string &input)
{
  return partial;
}

std::string (*transcode_wtomb_err)(int error,
				   const std::string &partial,
				   const std::wstring &input)=default_wtomb_err;

std::wstring (*transcode_mbtow_err)(int error,
				    const std::wstring &partial,
				    const std::string &input)=default_mbtow_err;


/** Does the dirty iconv work, given that an iconv session has been
 *  opened and we want to fully decode the "inbuf".  If the outbuf
 *  isn't large enough, it will be repeatedly doubled.
 *
 *  \param state the iconv state to be used
 *
 *  \param outbuf the buffer to which the string should be decoded.
 *         If \b null, a new buffer will be allocated.
 *
 *  \param outbufsize the initial size of "outbuf", updated if
 *      outbuf is increased.  If this value is 0, an arbitrary small
 *      starting value will be used.
 *
 *  \param inbuf the string to be decoded.
 *
 *  \param inbufsize the size of inbuf.
 *
 *  \param decoded location to write the number of bytes in the decoded string.
 *
 *  \param errf a callback to handle encoding errors: it is passed the
 *  current decoding state, and returns 'true' to continue and 'false'
 *  to abort (after possibly adjusting said state).
 */
static bool transcode_buffer(iconv_t &state,
			     char *&outbuf,
			     size_t &outbufsize,
			     const char *inbuf,
			     size_t inbufsize,
			     size_t &decoded,
			     const char *outencoding)
{
  bool rval=true;

  if(outbufsize == 0 || outbuf == NULL)
    {
      free(outbuf);
      // arbitrary initial starting size; expected to be large enough
      // for most "small" strings.
      if(outbufsize == 0)
	outbufsize = 1024;
      outbuf = (char *) malloc(outbufsize);
      if(outbuf == NULL)
	{
	  errno = ENOMEM;
	  decoded=0;
	  return false;
	}
    }

  char *outbufcur = outbuf;

  size_t outremaining = outbufsize;
  size_t inremaining  = inbufsize;

  while(inremaining>0)
    {
      if(iconv(state,
	       const_cast<char **>(&inbuf), &inremaining,
	       &outbufcur, &outremaining) == ((size_t)-1))
	{
	  // Some error conditions can be corrected.  There are three
	  // reasons iconv can terminate abnormally:
	  //
	  //  (1) an invalid multibyte sequence occured.  We do not
	  //      attempt to recover in this case.
	  //
	  //  (2) an incomplete multibyte sequence occured; as the
	  //      input string is all the input we have, this reduces
	  //      to case (1).
	  //
	  //  (3) no room left in the output buffer.  We respond by
	  //      doubling the output buffer's size, or failing if
	  //      it's doubled as far as it can go.

	  if(errno != E2BIG)
	    {
	      rval=false;
	      // Reset the output to initial state.
	      size_t result = iconv(state, NULL, NULL, &outbufcur, &outremaining);

	      while(result == (size_t)(-1))
		{
		  eassert(errno == E2BIG);

		  size_t idx = outbufcur-outbuf;
		  outremaining += outbufsize;
		  outbufsize *= 2;
		  outbuf = (char *) realloc(outbuf,outbufsize);
		  outbufcur = outbuf+idx;

		  result = iconv(state, NULL, NULL, &outbufcur, &outremaining);
		}

	      // Open a *new* iconv to spit a '?' onto the decoded
	      // output.
	      iconv_t state2 = iconv_open(outencoding, "ASCII");

	      if(state2 == (iconv_t)(-1))
		{
		  decoded = outbufsize-outremaining;
		  return false;
		}

	      const char *errbuf = "?";
	      size_t errbufsize = strlen(errbuf);

	      result = iconv(state2, const_cast<char **>(&errbuf),
			     &errbufsize, &outbufcur, &outremaining);


	      while(result == (size_t)(-1))
		{
		  if(errno != E2BIG)
		    {
		      decoded = outbufsize-outremaining;
		      iconv_close(state2);
		      return false;
		    }

		  size_t idx = outbufcur-outbuf;
		  outremaining += outbufsize;
		  outbufsize *= 2;
		  outbuf = (char *) realloc(outbuf, outbufsize);
		  outbufcur = outbuf+idx;

		  result = iconv(state2, const_cast<char **>(&errbuf),
				 &errbufsize, &outbufcur, &outremaining);
		}

	      eassert(errbufsize == 0);

	      // Return again to initial shift state
	      result = iconv(state2, NULL, NULL, &outbufcur, &outremaining);
	      while(result == (size_t)(-1))
		{
		  eassert(errno == E2BIG);

		  size_t idx = outbufcur-outbuf;
		  outremaining += outbufsize;
		  outbufsize *= 2;
		  outbuf = (char *) realloc(outbuf, outbufsize);
		  outbufcur = outbuf+idx;

		  result = iconv(state2, NULL, NULL, &outbufcur, &outremaining);
		}

	      iconv_close(state2);

	      // Ok, skip the bad input character.
	      ++inbuf;
	      --inremaining;
	    }
	  else
	    {
	      size_t idx = outbufcur-outbuf;
	      outremaining += outbufsize;
	      outbufsize *= 2;
	      outbuf = (char *) realloc(outbuf, outbufsize);
	      outbufcur = outbuf + idx;
	    }
	}
      else
	// if this fails, my understanding of iconv is wrong: the
	// iconv docs say that if it doesn't fail, then the whole
	// input sequence was converted.
	eassert(inremaining == 0);
    }

  decoded=outbufsize-outremaining;

  return rval;
}

bool transcode(const char *s,
	       wstring &out,
	       const char *encoding)
{
  if(encoding == NULL)
    encoding = nl_langinfo(CODESET);

  iconv_t converter=iconv_open("WCHAR_T", encoding);

  if(converter == ((iconv_t)-1))
    return false;

  char *outbuf = NULL;
  size_t outbufsize = 0;
  size_t result_size = 0;

  bool rval = transcode_buffer(converter, outbuf, outbufsize,
			       s, strlen(s), result_size, "WCHAR_T");

  if(outbuf != NULL)
    {
      out = wstring((wchar_t *) outbuf, result_size/sizeof(wchar_t));
      free(outbuf);
    }

  if(iconv_close(converter) == -1)
    rval = false;

  return rval;
}

std::wstring transcode(const std::string &s,
		       const char *encoding,
		       std::wstring (*errf)(int error,
					    const std::wstring &partial,
					    const std::string &input))
{
  std::wstring rval;
  if(transcode(s, rval, encoding))
    return rval;
  else
    {
      if(errf == NULL)
	errf=transcode_mbtow_err;
      return errf(errno, rval, s);
    }
}

std::wstring transcode(const char *s,
		       const char *encoding,
		       std::wstring (*errf)(int error,
					    const std::wstring &partial,
					    const std::string &input))
{
  std::wstring rval;
  if(transcode(s, rval, encoding))
    return rval;
  else
    {
      if(errf == NULL)
	errf=transcode_mbtow_err;
      return errf(errno, rval, s);
    }
}

bool transcode(const wchar_t *s,
	       string &out,
	       const char *encoding)
{
  if(encoding == NULL)
    encoding = nl_langinfo(CODESET);

  iconv_t converter = iconv_open(encoding, "WCHAR_T");

  if(converter == ((iconv_t)-1))
    return false;

  char *outbuf = NULL;
  size_t outbufsize = 0;
  size_t result_size = 0;

  bool rval = transcode_buffer(converter, outbuf, outbufsize,
			       (char *) s,
			       wcslen(s)*sizeof(wchar_t),
			       result_size, encoding);

  if(outbuf != NULL)
    {
      out = string(outbuf, result_size);
      free(outbuf);
    }

  if(iconv_close(converter) == -1)
    rval = false;

  return rval;
}

std::string transcode(const std::wstring &s,
		      const char *encoding,
		      std::string (*errf)(int error,
					  const std::string &partial,
					  const std::wstring &input))
{
  std::string rval;
  if(transcode(s, rval, encoding))
    return rval;
  else
    {
      if(errf == NULL)
	errf=transcode_wtomb_err;
      return errf(errno, rval, s);
    }
}

std::string transcode(const wchar_t *s,
		      const char *encoding,
		      std::string (*errf)(int error,
					  const std::string &partial,
					  const std::wstring &input))
{
  std::string rval;
  if(transcode(s, rval, encoding))
    return rval;
  else
    {
      if(errf == NULL)
	errf=transcode_wtomb_err;
      return errf(errno, rval, s);
    }
}
