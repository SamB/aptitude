#!/bin/sh

aclocal-1.9 -I m4 &&
autoheader &&
automake-1.9 --add-missing &&
aclocal-1.9 -I m4 &&
autoconf &&
autoheader &&
./configure
