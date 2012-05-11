INCLUDES = -I$(top_srcdir)/include
EXTRA_DIST = proto/somatic.proto

include_HEADERS = include/somatic.h

nodist_include_HEADERS =

pkginclude_HEADERS = 	                \
	include/somatic.h               \
	include/somatic.pb-c.h          \
	include/somatic/daemon.h        \
	include/somatic/includes.h      \
	include/somatic/lapack.h        \
	include/somatic/motor.h         \
	include/somatic/msg.h           \
	include/somatic/util.h


lib_LTLIBRARIES = libsomatic.la


libsomatic_la_SOURCES =                 \
	src/somatic_util.c              \
	src/somatic.pb-c.c              \
	src/msg.c                       \
	src/daemon.c                    \
	src/motor.c


bin_PROGRAMS = somatic_dump somatic_live_plot somatic_motor_plot somatic_exampled slogd sns sbeep sjstel

somatic_dump_SOURCES = src/somatic_dump.c
somatic_live_plot_SOURCES = src/somatic_live_plot.c
somatic_motor_plot_SOURCES = src/somatic_motor_plot_argp.c src/somatic_motor_plot.cpp
somatic_exampled_SOURCES = src/somatic_exampled.c
slogd_SOURCES = src/slogd.c
sns_SOURCES = src/sns.c
sbeep_SOURCES = src/sbeep.c
sjstel_SOURCES = src/sjstel.c

somatic_dump_LDADD = libsomatic.la
somatic_live_plot_LDADD = libsomatic.la
somatic_motor_plot_LDADD = libsomatic.la
somatic_exampled_LDADD = libsomatic.la
slogd_LDADD = libsomatic.la
sns_LDADD = libsomatic.la
sbeep_LDADD = libsomatic.la
sjstel_LDADD = libsomatic.la



src/somatic.pb-c.c: proto/somatic.proto
	cd proto && \
	  protoc-c --c_out=. somatic.proto
	mv proto/somatic.pb-c.c src
	mv proto/somatic.pb-c.h include

$(INCLUDEDIR)/somatic.pb-c.h: src/somatic.pb-c.c


distclean-local:
	-rm -rf doxygen-doc

clean-local:
	-rm -rf debian/tmp                  \
		debian/files                \
		debian/libamino?            \
		debian/libamino-dev         \
		debian/*.log                \
		debian/*.substvars          \
		debian/*.debhelper          \
		debian/usr/*                \
		debian/DEBIAN/*             \
		texput.log                  \
		$(EXTRA_DX)                 \
		src/somatic.pb-c.c          \
		include/somatic.pb-c.h      \
		*.mod

include aminclude.am
