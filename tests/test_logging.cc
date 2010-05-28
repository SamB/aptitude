/** \file test_logging.cc */


// Copyright (C) 2010 Daniel Burrows
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; see the file COPYING.  If not, write to
// the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
// Boston, MA 02111-1307, USA.

// Local includes:
#include <generic/util/logging.h>

// System includes:
#include <boost/test/unit_test.hpp>
#include <gmock/gmock.h>

using aptitude::util::logging::DEBUG_LEVEL;
using aptitude::util::logging::ERROR_LEVEL;
using aptitude::util::logging::FATAL_LEVEL;
using aptitude::util::logging::INFO_LEVEL;
using aptitude::util::logging::LoggerPtr;
using aptitude::util::logging::LoggingSystem;
using aptitude::util::logging::TRACE_LEVEL;
using aptitude::util::logging::WARN_LEVEL;
using aptitude::util::logging::createLoggingSystem;
using aptitude::util::logging::log_level;
using boost::shared_ptr;
using testing::Exactly;
using testing::StrictMock;
using testing::InSequence;
using testing::_;

// Test of the logging code in src/generic/util/logging.{cc,h}.

namespace
{
  /** \brief A mock object that can be used to match signals emitted
   *  by loggers.
   */
  class LoggingReceiver : public sigc::trackable
  {
  public:
    MOCK_METHOD5(message_logged, void(const char *, int, log_level, LoggerPtr, std::string));
  };

  struct LoggingTest
  {
    shared_ptr<LoggingSystem> loggingSystem;

    static const char *sourceFilename1;
    static const int sourceLineNumber1 = 100;
    static std::string msg1;

    static const char *sourceFilename2;
    static const int sourceLineNumber2 = 6089107;
    static std::string msg2;

    /** \brief Retrieve a logger from the internal logging system.
     */
    LoggerPtr getLogger(const std::string &category)
    {
      return loggingSystem->getLogger(category);
    }

    /** \brief Connect a LoggingReceiver to a logger. */
    static sigc::connection connect(LoggingReceiver &receiver, const LoggerPtr &logger)
    {
      return logger->connect_message_logged(sigc::mem_fun(receiver, &LoggingReceiver::message_logged));
    }

    static void log1(const LoggerPtr &logger,
                     log_level level)
    {
      logger->log(sourceFilename1,
                  sourceLineNumber1,
                  level,
                  msg1);
    }

    static void log2(const LoggerPtr &logger,
                     log_level level)
    {
      logger->log(sourceFilename2,
                  sourceLineNumber2,
                  level,
                  msg2);
    }

    LoggingTest()
      : loggingSystem(createLoggingSystem())
    {
    }
  };

  const char *LoggingTest::sourceFilename1 = "abracadabra.cc";
  std::string LoggingTest::msg1 = "Please and thank you";

  const char *LoggingTest::sourceFilename2 = "oracle.cc";
  std::string LoggingTest::msg2 = "Do you desire a major consultation?";
}

#define EXPECT_NO_LOGS(receiver)                                        \
  do {                                                                  \
    EXPECT_CALL(receiver, message_logged(_, _, _, _, _)).Times(Exactly(0)); \
  } while(0)

#define EXPECT_LOG1(receiver, logger, level)    \
  do {                                          \
    EXPECT_CALL(receiver,                       \
      message_logged(sourceFilename1,           \
                     sourceLineNumber1,         \
                     (level),                   \
                     (logger),                  \
                     msg1));                    \
  } while(0)

#define EXPECT_LOG2(receiver, logger, level)    \
  do {                                          \
    EXPECT_CALL(receiver,                       \
      message_logged(sourceFilename2,           \
                     sourceLineNumber2,         \
                     (level),                   \
                     (logger),                  \
                     msg2));                    \
  } while(0)

BOOST_FIXTURE_TEST_CASE(testGetRootLogger, LoggingTest)
{
  BOOST_CHECK(loggingSystem->getLogger("").get() != NULL);
}

BOOST_FIXTURE_TEST_CASE(testGetNonRootLogger, LoggingTest)
{
  BOOST_CHECK(loggingSystem->getLogger("a.b.c").get() != NULL);
}

BOOST_FIXTURE_TEST_CASE(testGetRootLoggerTwice, LoggingTest)
{
  LoggerPtr
    l1 = loggingSystem->getLogger(""),
    l2 = loggingSystem->getLogger("");

  BOOST_CHECK_EQUAL(l1, l2);
}

BOOST_FIXTURE_TEST_CASE(testGetNonRootLoggerTwice, LoggingTest)
{
  LoggerPtr
    l1 = loggingSystem->getLogger("a.b.c"),
    l2 = loggingSystem->getLogger("a.b.c");

  BOOST_CHECK_EQUAL(l1, l2);
}

BOOST_FIXTURE_TEST_CASE(testGetDifferentLoggers, LoggingTest)
{
  LoggerPtr
    l1 = loggingSystem->getLogger("a.b.c"),
    l2 = loggingSystem->getLogger("x.y.z");

  BOOST_CHECK_NE(l1, l2);
}

// I test the individual methods of the logger extensively, then do
// some spot checks that the LOG_ macros behave correctly.

// The only sane test of isEnabledFor (on its own) is to check that
// ERROR and FATAL are enabled by default, and that other levels
// aren't.  Other behavior is really more a test that setLevel does
// the right thing.
BOOST_FIXTURE_TEST_CASE(testLoggerIsEnabledForDefaults, LoggingTest)
{
  LoggerPtr l = loggingSystem->getLogger("a.b.c");

  BOOST_CHECK_EQUAL(ERROR_LEVEL, l->getEffectiveLevel());

  BOOST_CHECK(  l->isEnabledFor(FATAL_LEVEL) );
  BOOST_CHECK(  l->isEnabledFor(ERROR_LEVEL) );
  BOOST_CHECK( !l->isEnabledFor(WARN_LEVEL)  );
  BOOST_CHECK( !l->isEnabledFor(INFO_LEVEL)  );
  BOOST_CHECK( !l->isEnabledFor(DEBUG_LEVEL) );
  BOOST_CHECK( !l->isEnabledFor(TRACE_LEVEL) );
}

BOOST_FIXTURE_TEST_CASE(testLogAtRoot, LoggingTest)
{
  LoggerPtr root = loggingSystem->getLogger("");
  LoggingReceiver receiver;
  // Note that this implicitly tests that log() overrides the defined
  // level, since INFO is disabled by default.
  EXPECT_LOG1(receiver, root, INFO_LEVEL);

  connect(receiver, root);

  log1(root, INFO_LEVEL);
}

BOOST_FIXTURE_TEST_CASE(testLogAtSubcategory, LoggingTest)
{
  LoggerPtr l = loggingSystem->getLogger("a.b.c");
  LoggingReceiver receiver;

  EXPECT_LOG2(receiver, l, ERROR_LEVEL);

  connect(receiver, l);

  log2(l, ERROR_LEVEL);
}

BOOST_FIXTURE_TEST_CASE(testLoggingPropagatesUpwards, LoggingTest)
{
  LoggerPtr l = loggingSystem->getLogger("a.b.c");
  LoggerPtr root = loggingSystem->getLogger("");
  LoggingReceiver receiver;

  EXPECT_LOG1(receiver, l, FATAL_LEVEL);

  connect(receiver, root);

  log1(l, FATAL_LEVEL);
}

BOOST_FIXTURE_TEST_CASE(testLoggingPropagatesUpwardsRegardlessOfCreationOrder, LoggingTest)
{
  LoggerPtr root = loggingSystem->getLogger("");
  LoggerPtr l = loggingSystem->getLogger("a.b.c");
  LoggingReceiver receiver;

  EXPECT_LOG1(receiver, l, FATAL_LEVEL);

  connect(receiver, root);

  log1(l, FATAL_LEVEL);
}

BOOST_FIXTURE_TEST_CASE(testLoggingPropagatesUpwardsMultipleTimes, LoggingTest)
{
  LoggerPtr root = loggingSystem->getLogger("");
  LoggerPtr a = loggingSystem->getLogger("a");
  LoggerPtr abc = loggingSystem->getLogger("a.b.c");
  LoggingReceiver receiverRoot, receiverA, receiverABC;

  EXPECT_LOG2(receiverRoot, abc, FATAL_LEVEL);
  EXPECT_LOG2(receiverA, abc, FATAL_LEVEL);
  EXPECT_LOG2(receiverABC, abc, FATAL_LEVEL);

  connect(receiverRoot, root);
  connect(receiverA, a);
  connect(receiverABC, abc);

  log1(abc, FATAL_LEVEL);
}

BOOST_FIXTURE_TEST_CASE(testLoggingDoesNotPropagateDownwards, LoggingTest)
{
  LoggerPtr root = loggingSystem->getLogger("");
  LoggerPtr a = loggingSystem->getLogger("a");
  LoggerPtr abc = loggingSystem->getLogger("a.b.c");
  StrictMock<LoggingReceiver> receiverA, receiverABC;

  connect(receiverA, a);
  connect(receiverABC, abc);

  EXPECT_NO_LOGS(receiverA);
  EXPECT_NO_LOGS(receiverABC);

  log2(root, FATAL_LEVEL);
}

BOOST_FIXTURE_TEST_CASE(testSetLevelOnRoot, LoggingTest)
{
  LoggerPtr root = getLogger("");
  root->setLevel(INFO_LEVEL);

  BOOST_CHECK_EQUAL(INFO_LEVEL, root->getEffectiveLevel());

  BOOST_CHECK(  root->isEnabledFor(FATAL_LEVEL) );
  BOOST_CHECK(  root->isEnabledFor(ERROR_LEVEL) );
  BOOST_CHECK(  root->isEnabledFor(WARN_LEVEL)  );
  BOOST_CHECK(  root->isEnabledFor(INFO_LEVEL)  );
  BOOST_CHECK( !root->isEnabledFor(DEBUG_LEVEL) );
  BOOST_CHECK( !root->isEnabledFor(TRACE_LEVEL) );
}

BOOST_FIXTURE_TEST_CASE(testSetLevelOnRootPropagatesDownward, LoggingTest)
{
  LoggerPtr root = getLogger("");
  LoggerPtr l = getLogger("x.y.z");
  root->setLevel(INFO_LEVEL);

  BOOST_CHECK_EQUAL(INFO_LEVEL, l->getEffectiveLevel());
}

BOOST_FIXTURE_TEST_CASE(testSetLevelOnRootPropagatesDownwardRegardlessOfCreationOrder, LoggingTest)
{
  LoggerPtr root = getLogger("");
  root->setLevel(INFO_LEVEL);
  LoggerPtr l = getLogger("x.y.z");

  BOOST_CHECK_EQUAL(INFO_LEVEL, l->getEffectiveLevel());
}

BOOST_FIXTURE_TEST_CASE(testSetLevelDoesNotPropagateUpwards, LoggingTest)
{
  LoggerPtr a = getLogger("a");
  LoggerPtr ab = getLogger("ab");

  ab->setLevel(DEBUG_LEVEL);

  BOOST_CHECK_EQUAL(DEBUG_LEVEL, a->getEffectiveLevel());
}

BOOST_FIXTURE_TEST_CASE(testSetLevelDoesNotPropagatePastShadowedNode, LoggingTest)
{
  LoggerPtr root = getLogger("");
  LoggerPtr fluffy = getLogger("fluffy");
  LoggerPtr fluffyBunnies = getLogger("fluffy.bunnies");

  BOOST_CHECK_EQUAL(ERROR_LEVEL, fluffyBunnies->getEffectiveLevel());

  // Shadow the level at "fluffy".
  fluffy->setLevel(DEBUG_LEVEL);

  BOOST_CHECK_EQUAL(DEBUG_LEVEL, fluffyBunnies->getEffectiveLevel());

  // Verify that setting the root logger doesn't modify the level of
  // fluffyBunnies.
  root->setLevel(INFO_LEVEL);

  BOOST_CHECK_EQUAL(DEBUG_LEVEL, fluffyBunnies->getEffectiveLevel());
}

BOOST_FIXTURE_TEST_CASE(testShadowingOverridesPropagatedValue, LoggingTest)
{
  LoggerPtr root = getLogger("");
  LoggerPtr fluffy = getLogger("fluffy");
  LoggerPtr fluffyBunnies = getLogger("fluffy.bunnies");

  BOOST_CHECK_EQUAL(ERROR_LEVEL, fluffyBunnies->getEffectiveLevel());
  BOOST_CHECK_EQUAL(ERROR_LEVEL, fluffy->getEffectiveLevel());

  // Shadow the level at "fluffy".
  fluffy->setLevel(DEBUG_LEVEL);

  BOOST_CHECK_EQUAL(DEBUG_LEVEL, fluffyBunnies->getEffectiveLevel());
  BOOST_CHECK_EQUAL(DEBUG_LEVEL, fluffy->getEffectiveLevel());

  // Verify that setting the root logger doesn't modify the level of
  // fluffy *or* fluffyBunnies.
  root->setLevel(INFO_LEVEL);

  BOOST_CHECK_EQUAL(DEBUG_LEVEL, fluffyBunnies->getEffectiveLevel());
  BOOST_CHECK_EQUAL(DEBUG_LEVEL, fluffy->getEffectiveLevel());
}

// Same test, except that we set levels in the opposite order.
BOOST_FIXTURE_TEST_CASE(testSetLevelOverridesPropagatedValue, LoggingTest)
{
  LoggerPtr root = getLogger("");
  LoggerPtr fish = getLogger("fish");
  LoggerPtr fishChips = getLogger("fish.chips");

  BOOST_CHECK_EQUAL(DEBUG_LEVEL, fish->getEffectiveLevel());
  BOOST_CHECK_EQUAL(DEBUG_LEVEL, fishChips->getEffectiveLevel());

  root->setLevel(TRACE_LEVEL);

  BOOST_CHECK_EQUAL(TRACE_LEVEL, fish->getEffectiveLevel());
  BOOST_CHECK_EQUAL(TRACE_LEVEL, fishChips->getEffectiveLevel());

  fish->setLevel(WARN_LEVEL);

  BOOST_CHECK_EQUAL(WARN_LEVEL, fish->getEffectiveLevel());
  BOOST_CHECK_EQUAL(WARN_LEVEL, fishChips->getEffectiveLevel());
}

// The one specific test for connect() is that its return value
// is a reference to the new connection:
BOOST_FIXTURE_TEST_CASE(testConnectMessageLoggedCanBeDisconnected, LoggingTest)
{
  LoggerPtr root = getLogger("");
  LoggingReceiver receiver;

  EXPECT_NO_LOGS(receiver);

  sigc::connection c = connect(receiver, root);

  c.disconnect();

  log1(root, FATAL_LEVEL);
}

// Spot checks that the log-level macros do the "right thing".  Note
// that each test uses msg1 for a log that should get through, and
// msg2 for a log that shouldn't.

BOOST_FIXTURE_TEST_CASE(testLogTrace, LoggingTest)
{
  LoggerPtr root = getLogger("");
  LoggingReceiver receiver;

  {
    InSequence dummy;
    EXPECT_CALL(receiver,
                // Note that we don't set an expectation regarding the
                // line number -- that would be awkward and fragile.
                // Could be avoided by putting everything on one line;
                // that would be disgusting.
                message_logged(_, _,
                               TRACE_LEVEL,
                               root,
                               msg1));
    EXPECT_NO_LOGS(receiver);
  }


  connect(receiver, root);

  LOG_TRACE(root, msg2);
  root->setLevel(TRACE_LEVEL);
  LOG_TRACE(root, msg1);
}
