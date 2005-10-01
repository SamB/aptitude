// apt_options.h
//
//  Copyright 2000 Daniel Burrows
//
//  The main options dialog

#ifndef APT_OPTIONS_H
#define APT_OPTIONS_H

class vscreen_widget;

template<class T> class ref_ptr;

typedef ref_ptr<vscreen_widget> vs_widget_ref;
// hmm, maybe just make this a global variable that gets shown and hidden?
vs_widget_ref make_ui_options_dialog();
vs_widget_ref make_misc_options_dialog();
vs_widget_ref make_dependency_options_dialog();

#endif
