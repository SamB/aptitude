// load_config.h
//
//  Copyright 2000 Daniel Burrows
//
//  Provides routines to load in the vscreen frontend's configuration.

#ifndef LOAD_CONFIG_H
#define LOAD_CONFIG_H

#include "vscreen/config/keybindings.h"

void load_styles(std::string group, bool usetheme);
//  Loads in color definitions from the given group.

void load_bindings(std::string group, keybindings *toload, bool usetheme);
//  Loads values from the given APT configuration group into the given
// keybindings.

#endif
