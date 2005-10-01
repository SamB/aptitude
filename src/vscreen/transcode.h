// transcode.h                                      -*-c++-*-
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

#ifndef TRANSCODE_H
#define TRANSCODE_H

#include <string>

/** Convenience function to convert a multibyte encoding to wide
 *  characters.  This is a wrapper around iconv.
 *
 *  \param s the string to decode
 *  \param out the location to write the transcoded string
 *  \param encoding the encoding of s; if \b null or unspecified,
 *         the value of LC_CTYPE is used.
 *
 *  \return \b true if the entire string was successfully transcoded;
 *  if transcoding failed, returns \b false and sets errno.
 */
bool transcode(const char *s,
	       std::wstring &out,
	       const char *encoding=NULL);

inline bool transcode(const std::string &s,
		      std::wstring &out,
		      const char *encoding=NULL)
{
  return transcode(s.c_str(), out, encoding);
}

/** The error handler for converting multibyte strings to wide
 *  strings: it is passed a partially-decoded string and the error
 *  code, and its return value becomes the return value of the
 *  function.  The default handler just returns "partial".
 */
extern std::wstring (*transcode_mbtow_err)(int error,
					   const std::wstring &partial,
					   const std::string &input);

/** Convenience function to convert a multibyte encoding to wide
 *  characters, where the caller doesn't need to directly handle
 *  errors.  This is a wrapper around iconv.
 *
 *  \param s the string to decode 
 *
 *  \param encoding the encoding of s; if \b null or unspecified, the
 *  value of LC_CTYPE is used.
 *
 *  \param errf the error handler, or \b null to use the default
 *  handler (transcode_mbtow_err).
 */
std::wstring transcode(const std::string &s,
		       const char *encoding=NULL,
		       std::wstring (*errf)(int error,
					    const std::wstring &partial,
					    const std::string &input)=NULL);

/** Convenience function to convert a multibyte encoding to wide
 *  characters, where the caller doesn't need to directly handle
 *  errors.  This is a wrapper around iconv.
 *
 *  \param s the string to decode 
 *
 *  \param encoding the encoding of s; if \b null or unspecified, the
 *  value of LC_CTYPE is used.
 *
 *  \param errf the error handler, or \b null to use the default
 *  handler (transcode_mbtow_err).
 */
std::wstring transcode(const char *s,
		       const char *encoding=NULL,
		       std::wstring (*errf)(int error,
					    const std::wstring &partial,
					    const std::string &input)=NULL);

// Note: would it be saner to express errors via exceptions?

/** Convenience function to convert the native wide character encoding
 *  to a multibyte encoding.  This is a wrapper around iconv.
 *
 *  \param s the wide string to encode
 *  \param out the location to write the multibyte string
 *  \param encoding the encoding of out; if \b null or unspecified,
 *         the value of LC_CTYPE is used.
 *
 *  \return \b true if the entire string was successfully transcoded;
 *  if transcoding failed, returns \b false and sets errno.
 */
bool transcode(const wchar_t *s,
	       std::string &out,
	       const char *encoding=NULL);


inline bool transcode(const std::wstring &s,
		      std::string &out,
		      const char *encoding=NULL)
{
  return transcode(s.c_str(), out, encoding);
}


/** The error handler for converting multibyte strings to wide
 *  strings: it is passed a partially-decoded string and the error
 *  code, and its return value becomes the return value of the
 *  function.  The default handler just returns "partial".
 */
extern std::string (*transcode_wtomb_err)(int error,
					  const std::string &partial,
					  const std::wstring &input);

/** Convenience function to convert a multibyte encoding to wide
 *  characters, where the caller doesn't need to directly handle
 *  errors.  This is a wrapper around iconv.
 *
 *  \param s the string to decode 
 *
 *  \param encoding the encoding of s; if \b null or unspecified, the
 *  value of LC_CTYPE is used.
 *
 *  \param errf the error handler, or \b null to use the default
 *  handler (transcode_mbtow_err).
 */
std::string transcode(const std::wstring &s,
		      const char *encoding=NULL,
		      std::string (*errf)(int error,
					  const std::string &partial,
					  const std::wstring &input)=NULL);

/** Convenience function to convert a multibyte encoding to wide
 *  characters, where the caller doesn't need to directly handle
 *  errors.  This is a wrapper around iconv.
 *
 *  \param s the string to decode 
 *
 *  \param encoding the encoding of s; if \b null or unspecified, the
 *  value of LC_CTYPE is used.
 *
 *  \param errf the error handler, or \b null to use the default
 *  handler (transcode_mbtow_err).
 */
std::string transcode(const wchar_t *s,
		      const char *encoding=NULL,
		      std::string (*errf)(int error,
					  const std::string &partial,
					  const std::wstring &input)=NULL);

#endif // TRANSCODE_H
