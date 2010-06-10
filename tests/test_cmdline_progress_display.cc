/** \file test_cmdline_progress_display.cc */


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
#include <cmdline/cmdline_progress_display.h>
#include <cmdline/mocks/transient_message.h>

#include <generic/util/progress_info.h>

// System includes:
#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

using aptitude::cmdline::create_progress_display;
using aptitude::cmdline::progress_display;
using aptitude::util::progress_info;
using boost::make_shared;
using boost::shared_ptr;
using testing::StrEq;
using testing::StrNe;
using testing::Test;
using testing::_;

namespace mocks = aptitude::cmdline::mocks;

namespace
{
  struct CmdlineProgressDisplayTest : public Test
  {
    shared_ptr<mocks::transient_message> msg;
    shared_ptr<progress_display> progress;

    CmdlineProgressDisplayTest()
      : msg(make_shared<mocks::transient_message>()),
        progress(create_progress_display(msg))
    {
    }

    // Define shorter names for the progress_info constructors.
    progress_info none()
    {
      return progress_info::none();
    }

    progress_info pulse(const std::string &msg)
    {
      return progress_info::pulse(msg);
    }

    progress_info bar(double fraction, const std::string &msg)
    {
      return progress_info::bar(fraction, msg);
    }
  };
}

TEST_F(CmdlineProgressDisplayTest, InitialShowNoneHasNoEffect)
{
  EXPECT_CALL(*msg, set_text(_))
    .Times(0);

  progress->set_progress(none());
}

TEST_F(CmdlineProgressDisplayTest, ShowNoneAfterPulse)
{
  EXPECT_CALL(*msg, set_text(StrNe(L"")));
  EXPECT_CALL(*msg, set_text(StrEq(L"")));

  progress->set_progress(pulse("xyzzy"));
  progress->set_progress(none());
}

TEST_F(CmdlineProgressDisplayTest, ShowPulse)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[----] Hello world")));

  progress->set_progress(pulse("Hello world"));
}

TEST_F(CmdlineProgressDisplayTest, ShowBarNegative)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[  0%] Before the beginning of the world")));

  progress->set_progress(bar(-1, "Before the beginning of the world"));
}

TEST_F(CmdlineProgressDisplayTest, ShowBarZero)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[  0%] Beginning world")));

  progress->set_progress(bar(0, "Beginning world"));
}

TEST_F(CmdlineProgressDisplayTest, ShowBarRoundUp)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[ 50%] Almost middle world")));

  progress->set_progress(bar(0.499, "Almost middle world"));
}

TEST_F(CmdlineProgressDisplayTest, ShowBarRoundUpInMiddle)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[ 50%] Almost middle world")));

  progress->set_progress(bar(0.495, "Almost middle world"));
}

TEST_F(CmdlineProgressDisplayTest, ShowBarMiddleExact)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[ 50%] Middle world")));

  progress->set_progress(bar(0.50, "Middle world"));
}

TEST_F(CmdlineProgressDisplayTest, ShowBarRoundDown)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[ 50%] Just past middle world")));

  progress->set_progress(bar(0.501, "Just past middle world"));
}

TEST_F(CmdlineProgressDisplayTest, ShowBarComplete)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[100%] Finished world")));

  progress->set_progress(bar(1, "Finished world"));
}

TEST_F(CmdlineProgressDisplayTest, FractionTooLarge)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[100%] Past the end of the world")));

  progress->set_progress(bar(100, "Past the end of the world"));
}

TEST_F(CmdlineProgressDisplayTest, SuppressDuplicateNones)
{
  EXPECT_CALL(*msg, set_text(StrNe(L"")));
  EXPECT_CALL(*msg, set_text(StrEq(L"")));

  progress->set_progress(pulse("Irrelevant data"));
  progress->set_progress(none());
  progress->set_progress(none());
}

TEST_F(CmdlineProgressDisplayTest, PulseSuppressesDuplicateMessage)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[----] Some message")));

  progress->set_progress(pulse("Some message"));
  progress->set_progress(pulse("Some message"));
}

TEST_F(CmdlineProgressDisplayTest, BarSuppressesDuplicateSettings)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[ 66%] Some message")));

  progress->set_progress(bar(0.66, "Some message"));
  progress->set_progress(bar(0.66, "Some message"));
}

TEST_F(CmdlineProgressDisplayTest, BarSuppressesAlmostDuplicateSettings)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[ 66%] Some message")));

  progress->set_progress(bar(0.661, "Some message"));
  progress->set_progress(bar(0.662, "Some message"));
}

TEST_F(CmdlineProgressDisplayTest, CanSwitchFromNoneToPulse)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[----] Pulse")));

  progress->set_progress(none());
  progress->set_progress(pulse("Pulse"));
}

TEST_F(CmdlineProgressDisplayTest, CanSwitchFromNoneToEmptyBar)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[  0%] ")));

  progress->set_progress(none());
  progress->set_progress(bar(0, ""));
}

TEST_F(CmdlineProgressDisplayTest, CanSwitchFromNoneToNonEmptyBar)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[ 33%] Pelagic argosy")));

  progress->set_progress(none());
  progress->set_progress(bar(0.33, "Pelagic argosy"));
}

TEST_F(CmdlineProgressDisplayTest, CanSwitchFromPulseToNone)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[----] Beware the leopard")));
  EXPECT_CALL(*msg, set_text(StrEq(L"")));

  progress->set_progress(pulse("Beware the leopard"));
  progress->set_progress(none());
}

TEST_F(CmdlineProgressDisplayTest, CanSwitchFromPulseToZeroBar)
{
  // Check that it doesn't get confused and think that the second
  // setting is identical to the first.
  EXPECT_CALL(*msg, set_text(StrEq(L"[----] Wobbly Weasel"))); // Future Ubuntu codename?
  EXPECT_CALL(*msg, set_text(StrEq(L"[  0%] Wobbly Weasel")));

  progress->set_progress(pulse("Wobbly Weasel"));
  progress->set_progress(bar(0, "Wobbly Weasel"));
}

TEST_F(CmdlineProgressDisplayTest, CanSwitchFromZeroBarToNone)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[  0%] Bar None")));
  EXPECT_CALL(*msg, set_text(StrEq(L"")));

  progress->set_progress(bar(0, "Bar None"));
  progress->set_progress(none());
}

TEST_F(CmdlineProgressDisplayTest, CanSwitchFromNonZeroBarToNone)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[ 21%] Albatross")));
  EXPECT_CALL(*msg, set_text(StrEq(L"")));

  progress->set_progress(bar(0.21, "Albatross"));
  progress->set_progress(none());
}

TEST_F(CmdlineProgressDisplayTest, CanSwitchFromZeroBarToPulse)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[  0%] All I've got is this bloody albatross")));
  EXPECT_CALL(*msg, set_text(StrEq(L"[----] All I've got is this bloody albatross")));

  progress->set_progress(bar(0, "All I've got is this bloody albatross"));
  progress->set_progress(pulse("All I've got is this bloody albatross"));
}

TEST_F(CmdlineProgressDisplayTest, CanSwitchFromNonZeroBarToPulse)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[ 11%] Bop")));
  EXPECT_CALL(*msg, set_text(StrEq(L"[----] Bop")));

  progress->set_progress(bar(0.11, "Bop"));
  progress->set_progress(pulse("Bop"));
}

TEST_F(CmdlineProgressDisplayTest, CanSwitchFromNonZeroBarToDifferentPulse)
{
  EXPECT_CALL(*msg, set_text(StrEq(L"[ 11%] Zip")));
  EXPECT_CALL(*msg, set_text(StrEq(L"[----] Bop")));

  progress->set_progress(bar(0.11, "Zip"));
  progress->set_progress(pulse("Bop"));
}
