MAINTAINERCLEANFILES=Makefile.in

SUBDIRS=generic cmdline mine

localedir = $(datadir)/locale
INCLUDES = -Wall @WERROR@ -I$(top_builddir) -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/src
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

bin_PROGRAMS=aptitude

LDADD=@LIBINTL@ cmdline/libcmdline.a mine/libcmine.a \
	generic/util/libgeneric-util.a generic/apt/libgeneric-apt.a

aptitude_SOURCES= 	\
	aptitude.h	\
	main.cc		\
	desc_parse.cc   \
	desc_parse.h    \
	apt_config_treeitems.cc \
	apt_config_treeitems.h  \
	apt_config_widgets.cc \
	apt_config_widgets.h  \
	apt_info_tree.cc\
	apt_info_tree.h\
	apt_options.cc	\
	apt_options.h	\
	apt_undo_tree.cc \
	apt_undo_tree.h \
	broken_indicator.cc\
	broken_indicator.h\
	changelog_parse.cc \
	changelog_parse.h  \
	defaults.cc	\
	defaults.h	\
	dep_item.cc	\
	dep_item.h	\
	download_bar.h	\
	download_bar.cc	\
	download_item.h	\
	download_item.cc\
	download_list.h \
	download_list.cc \
	download_screen.h\
	download_screen.cc\
	download_thread.h\
	download_thread.cc\
	edit_pkg_hier.h	\
	edit_pkg_hier.cc\
	load_config.cc	\
	load_config.h	\
	load_grouppolicy.cc\
	load_grouppolicy.h \
	load_pkgview.cc	\
	load_pkgview.h	\
	load_sortpolicy.cc\
	load_sortpolicy.h\
	menu_redirect.cc\
	menu_redirect.h\
	menu_text_layout.cc \
	menu_text_layout.h  \
	menu_tree.cc\
	menu_tree.h\
	pkg_columnizer.cc\
	pkg_columnizer.h\
	pkg_info_screen.cc\
	pkg_info_screen.h \
	pkg_item.cc	\
	pkg_item.h	\
	pkg_item_with_subtree.h \
	pkg_grouppolicy.h\
	pkg_grouppolicy.cc\
	pkg_sortpolicy.h\
	pkg_sortpolicy.cc\
	pkg_node.cc	\
	pkg_node.h	\
	pkg_subtree.cc	\
	pkg_subtree.h	\
	pkg_tree.cc	\
	pkg_tree.h	\
	pkg_ver_item.cc	\
	pkg_ver_item.h	\
	pkg_view.cc	\
	pkg_view.h	\
	reason_fragment.cc\
	reason_fragment.h\
	solution_dialog.cc\
	solution_dialog.h\
	solution_fragment.cc\
	solution_fragment.h\
	solution_item.cc\
	solution_item.h\
	solution_screen.cc\
	solution_screen.h\
	trust.h		\
	trust.cc	\
	ui.cc		\
	ui.h		\
	ui_download_manager.cc \
	ui_download_manager.h  \
	view_changelog.cc \
	view_changelog.h \
	vs_progress.cc  \
	vs_progress.h
