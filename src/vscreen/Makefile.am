MAINTAINERCLEANFILES=Makefile.in

SUBDIRS=config

EXTRA_DIST=README.layout

localedir = $(datadir)/locale
INCLUDES = -Wall @WERROR@ -I$(top_builddir) -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/src
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@
LDADD = @LIBINTL@

noinst_LIBRARIES=libvscreen.a
noinst_PROGRAMS=testvscreen

libvscreen_a_SOURCES=	\
	curses++.cc	\
	curses++.h	\
	fragment.h	\
	fragment.cc	\
	fragment_cache.h\
	fragment_cache.cc\
	fragment_contents.h \
	ref_ptr.h	\
	vscreen.cc	\
	vscreen.h	\
	vscreen_widget.h\
	vscreen_widget.cc\
	vs_button.h	\
	vs_button.cc	\
	vs_bin.h	\
	vs_bin.cc	\
	vs_center.h	\
	vs_center.cc	\
	vs_container.h	\
	vs_container.cc	\
	vs_editline.h	\
	vs_editline.cc	\
	vs_frame.h	\
	vs_frame.cc	\
	vs_label.h	\
	vs_label.cc	\
	vs_layout_item.h\
	vs_layout_item.cc\
	vs_menu.h	\
	vs_menu.cc	\
	vs_menubar.h	\
	vs_menubar.cc	\
	vs_minibuf_win.h \
	vs_minibuf_win.cc\
	vs_multiplex.h	\
	vs_multiplex.cc	\
	vs_pager.h	\
	vs_pager.cc	\
	vs_passthrough.h\
	vs_passthrough.cc\
	vs_scrollbar.h	\
	vs_scrollbar.cc	\
	vs_statuschoice.h\
	vs_statuschoice.cc\
	vs_radiogroup.h	\
	vs_radiogroup.cc\
	vs_size_box.cc  \
	vs_size_box.h   \
	vs_stacked.h	\
	vs_stacked.cc	\
	vs_staticitem.h	\
	vs_staticitem.cc\
	vs_subtree.h	\
	vs_text_layout.h\
	vs_text_layout.cc\
	vs_togglebutton.h\
	vs_togglebutton.cc\
	vs_transient.h  \
	vs_transient.cc \
	vs_treeitem.h	\
	vs_treeitem.cc	\
	vs_util.h	\
	vs_util.cc	\
	vs_table.h	\
	vs_table.cc	\
	vs_tree.h	\
	vs_tree.cc	\
	columnify.h	\
	columnify.cc	\
	transcode.h	\
	transcode.cc

testvscreen_SOURCES=	\
	testvscreen.cc

testvscreen_LDADD=libvscreen.a config/libconf.a $(top_builddir)/src/generic/util/libgeneric-util.a
