// test_temp.cc
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

#include <cppunit/extensions/HelperMacros.h>

#include <generic/util/temp.h>
#include <generic/util/util.h>

#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define ASSERT_STAT(s, buf) \
  do \
  { \
    if(stat((s), (buf)) != 0) \
       CPPUNIT_FAIL(ssprintf("Can't stat %s: %s", (s), sstrerror(errno).c_str())); \
  } while(0)
						    

class TempTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE(TempTest);

  CPPUNIT_TEST(testTempDir);
  CPPUNIT_TEST(testTempName);

  CPPUNIT_TEST_SUITE_END();

public:
  void testTempDir()
  {
    std::string d1name, d2name, d3name, d4name;

    {
      temp::dir d1("tmp");
      temp::dir d2("tmp", d1);
      temp::dir d3("tmp");
      temp::dir d4("tmp", true);

      d1name = d1.get_name();
      d2name = d2.get_name();
      d3name = d3.get_name();
      d4name = d4.get_name();

      int result = access(d1name.c_str(), F_OK);
      if(result != 0)
	CPPUNIT_FAIL(ssprintf("Unable to access %s: %s",
			      d1name.c_str(), sstrerror(errno).c_str()));

      result = access(d2.get_name().c_str(), F_OK);
      if(result != 0)
	CPPUNIT_FAIL(ssprintf("Unable to access %s: %s",
			      d2name.c_str(), sstrerror(errno).c_str()));

      char *d1namecopy = strdup(d1name.c_str());
      std::string base1 = basename(d1namecopy);
      free(d1namecopy);
      d1namecopy = NULL;

      CPPUNIT_ASSERT_EQUAL(std::string("tmp"), std::string(base1, 0, base1.size()-6));

      char *d2namecopy = strdup(d2name.c_str());
      std::string base2 = basename(d2namecopy);
      free(d2namecopy);
      d2namecopy = NULL;

      CPPUNIT_ASSERT_EQUAL(std::string("tmp"), std::string(base2, 0, base2.size()-6));

      struct stat stbuf;

      ASSERT_STAT(d1.get_name().c_str(), &stbuf);
      CPPUNIT_ASSERT(S_ISDIR(stbuf.st_mode));

      ASSERT_STAT(d2.get_name().c_str(), &stbuf);
      CPPUNIT_ASSERT(S_ISDIR(stbuf.st_mode));

      close(creat((d3name + "/" + "foo").c_str(), 0644));
      close(creat((d4name + "/" + "foo").c_str(), 0644));
    }

    int result = access(d1name.c_str(), F_OK);
    CPPUNIT_ASSERT(result != 0);
    CPPUNIT_ASSERT_EQUAL(ENOENT, errno);

    result = access(d2name.c_str(), F_OK);
    CPPUNIT_ASSERT(result != 0);
    CPPUNIT_ASSERT_EQUAL(ENOENT, errno);

    result = access(d3name.c_str(), F_OK);
    CPPUNIT_ASSERT(result == 0);

    CPPUNIT_ASSERT(aptitude::util::recursive_remdir(d3name));

    result = access(d3name.c_str(), F_OK);
    CPPUNIT_ASSERT(result != 0);
    CPPUNIT_ASSERT_EQUAL(ENOENT, errno);

    result = access(d4name.c_str(), F_OK);
    CPPUNIT_ASSERT(result != 0);
    CPPUNIT_ASSERT_EQUAL(ENOENT, errno);
  }

  void testTempName()
  {
    std::string dname;
    std::string fname;

    {
      temp::dir d("tmp");

      temp::name f(d, "tmpf");

      dname = d.get_name();
      fname = f.get_name();

      char *fnamecopy = strdup(fname.c_str());
      std::string base = basename(fnamecopy);
      free(fnamecopy);
      fnamecopy = NULL;

      CPPUNIT_ASSERT_EQUAL(std::string("tmpf"), std::string(base, 0, base.size()-6));

      CPPUNIT_ASSERT(access(f.get_name().c_str(), F_OK) != 0);
      CPPUNIT_ASSERT_EQUAL(ENOENT, errno);

      // Create it.
      int fd = open(fname.c_str(), O_EXCL | O_CREAT | O_WRONLY, 0700);
      if(fd == -1)
	CPPUNIT_FAIL(ssprintf("Can't create \"%s\": %s",
			      fname.c_str(),
			      sstrerror(errno).c_str()));

      CPPUNIT_ASSERT_EQUAL(0, access(f.get_name().c_str(), F_OK));

      close(fd);
    }

    CPPUNIT_ASSERT(access(fname.c_str(), F_OK) != 0);
    CPPUNIT_ASSERT_EQUAL(ENOENT, errno);

    CPPUNIT_ASSERT(access(dname.c_str(), F_OK) != 0);
    CPPUNIT_ASSERT_EQUAL(ENOENT, errno);
  }
};

CPPUNIT_TEST_SUITE_REGISTRATION(TempTest);
