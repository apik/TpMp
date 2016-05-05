##
## Makefile.am for TpMp
##
## written by Andrey Pikelner
##
## Process this file with automake to produce Makefile.in
AUTOMAKE_OPTIONS = subdir-objects
ACLOCAL_AMFLAGS = ${ACLOCAL_FLAGS} -I m4

MATHROOT=$(dir $(MPREP))


EX_CFLAGS =  $(TPMP_CFLAGS) -I$(MATHROOT) 
# -I$(top_srcdir)/tsil-1.3 -I$(top_srcdir)/mr $(OPENMP_CFLAGS)

noinst_PROGRAMS = tpmp

tpmp_SOURCES =  src/tpmpimpl.cpp

nodist_tpmp_SOURCES = src/tpmp.cpp
BUILT_SOURCES = src/tpmp.cpp

tpmp_CPPFLAGS = -std=c++11 $(EX_CFLAGS) 
tpmp_LDADD = -L$(MATHROOT)$(ALTMATHPATH) ${LDMATH} -lyaml-cpp -lsqlite3 -lboost_system -lboost_filesystem
# $(OPENMP_CFLAGS) $(MR_LIBS) -L$(MATHROOT)$(ALTMATHPATH) ${LDMATH} -L$(top_srcdir)/mr -lmr -L$(top_srcdir)/tsil-1.3 -ltsil -lm

src/tpmp.cpp: src/tpmp.tm
	$(MPREP) $? -o $@

# EXTRA_DIST = mr.tm mr.m

# if HAVE_MPREP
# SUBDIRS+=math
# endif

# pkgconfigdir = $(libdir)/pkgconfig
# pkgconfig_DATA = mr.pc

# EXTRA_DIST = boost mr.pc.in README
# DISTCLEANFILES = mr.pc

# $(pkgconfig_DATA): config.status

