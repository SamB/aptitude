// defaults.cc
//
//  Copyright 1999 Daniel Burrows
//
//  Defaults for Aptitude.

#include "vscreen/config/keybindings.h"
#include "vscreen/config/colors.h"

#include "mine/cmine.h"

#include "pkg_tree.h"
#include "pkg_node.h"

static void init_keybindings()
{
  global_bindings.set("CycleNext", key(KEY_F(6), true));
  global_bindings.set("CyclePrev", key(KEY_F(7), true));

  global_bindings.set("QuitProgram", key(L'Q', false));

  global_bindings.set("UpdatePackageList", key(L'u', false));
  global_bindings.set("MarkUpgradable", key(L'U', false));
  global_bindings.set("ForgetNewPackages", key(L'f', false));
  global_bindings.set("ChangePkgTreeLimit", key(L'l', false));
  global_bindings.set("ChangePkgTreeGrouping", key(L'G', false));
  global_bindings.set("ChangePkgTreeSorting", key(L'S', false));

  global_bindings.set("CycleOrder", key(L'o', false));

  global_bindings.set("Install", key(L'+', false));
  global_bindings.set("Remove", key(L'-', false));
  global_bindings.set("Hold", key(L'=', false));
  global_bindings.set("Purge", key(L'_', false));
  global_bindings.set("Keep", key(L':', false));
  global_bindings.set("SetAuto", key(L'M', false));
  global_bindings.set("ClearAuto", key(L'm', false));
  global_bindings.set("ForbidUpgrade", key(L'F', false));
  global_bindings.set("Reinstall", key(L'L', false));

  global_bindings.set("Dependencies", key(L'd', false));
  global_bindings.set("ReverseDependencies", key(L'r', false));
  global_bindings.set("InfoScreen", key(KEY_ENTER, true));
  global_bindings.set("Versions", key(L'v', false));
  global_bindings.set("Changelog", key(L'C', false));

  global_bindings.set("DoInstallRun", key(L'g', false));
  global_bindings.set("InstallSingle", key(L'I', false));
  global_bindings.set("ChangePkgDisplayFormat", key(L'p', false));
  global_bindings.set("ChangePkgStatusFormat", key(L's', false));

  global_bindings.set("ToggleColumnHeaders", key(L'h', false));

  global_bindings.set("ShowHideDescription", key(L'D', false));

  global_bindings.set("DescriptionUp", key(L'a', false));
  global_bindings.set("DescriptionDown", key(L'z', false));
  global_bindings.set("DescriptionCycle", key(L'i', false));

  global_bindings.set("DpkgReconfigure", key(L'R', false));
  global_bindings.set("BugReport", key(L'B', false));

  // Hierarchy editor
  global_bindings.set("Commit", key(L'N', false));
  global_bindings.set("SaveHier", key(L'S', false));
  global_bindings.set("EditHier", key(L'E', false));

  global_bindings.set("SearchBroken", key(L'b', false));

  global_bindings.set("NextSolution", key(L'.', false));
  global_bindings.set("PrevSolution", key(L',', false));
  global_bindings.set("FirstSolution", key(L'<', false));
  global_bindings.set("LastSolution", key(L'>', false));
  global_bindings.set("ExamineSolution", key(L'e', false));
  global_bindings.set("ApplySolution", key(L'!', false));
  global_bindings.set("DumpResolver", key(L'*', false));

  global_bindings.set("SolutionActionReject", key(L'r', false));
  global_bindings.set("SolutionActionApprove", key(L'a', false));

  pkg_tree::init_bindings();
  pkg_tree_node::init_bindings();
  cmine::init_bindings();
}

static void init_styles()
{
  set_style("PkgNotInstalled", style());
  set_style("PkgIsInstalled", style_attrs_on(A_BOLD));
  set_style("PkgToHold", style_fg(COLOR_WHITE) + style_attrs_on(A_REVERSE));
  set_style("PkgToRemove", style_fg(COLOR_MAGENTA) + style_attrs_on(A_REVERSE));
  set_style("PkgBroken", style_fg(COLOR_RED) + style_attrs_on(A_REVERSE));
  set_style("PkgToInstall", style_fg(COLOR_GREEN) + style_attrs_on(A_REVERSE));
  set_style("PkgToUpgrade", style_fg(COLOR_CYAN) + style_attrs_on(A_REVERSE));


  set_style("PkgNotInstalledHighlighted",
	    style_attrs_flip(A_REVERSE));
  set_style("PkgIsInstalledHighlighted",
	    style_attrs_on(A_BOLD) + style_attrs_flip(A_REVERSE));
  set_style("PkgToHoldHighlighted",
	    style_fg(COLOR_WHITE));
  set_style("PkgToRemoveHighlighted",
	    style_fg(COLOR_MAGENTA));
  set_style("PkgBrokenHighlighted",
	    style_fg(COLOR_RED));
  set_style("PkgToInstallHighlighted",
	    style_fg(COLOR_GREEN));
  set_style("PkgToUpgradeHighlighted",
	    style_fg(COLOR_CYAN));




  set_style("DepBroken", style_fg(COLOR_BLACK)+style_bg(COLOR_RED));

  set_style("MediaChange", style_fg(COLOR_YELLOW)+style_bg(COLOR_RED)+style_attrs_on(A_BOLD));
  set_style("Progress", style_fg(COLOR_BLUE)+style_bg(COLOR_YELLOW));
  set_style("DownloadProgress", style_fg(COLOR_BLUE)+style_bg(COLOR_YELLOW));
  set_style("DownloadHit", style_fg(COLOR_BLACK)+style_bg(COLOR_GREEN));

  set_style("ChangelogNewerVersion", style_attrs_on(A_BOLD));
  set_style("Bullet", style_fg(COLOR_YELLOW)+style_attrs_on(A_BOLD));
  set_style("TrustWarning", style_fg(COLOR_RED)+style_bg(COLOR_BLACK)+style_attrs_on(A_BOLD));

  set_style("SolutionActionRejected", style_bg(COLOR_RED));
  set_style("SolutionActionApproved", style_bg(COLOR_GREEN));
}

void init_defaults()
{
  init_keybindings();
  init_styles();
}
