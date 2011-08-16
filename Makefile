## Makefile for somatic

VERSION := 0.0.20110719

PROJECT := somatic

SHAREDLIBS := somatic
BINFILES := somatic_motor_plot somatic_dump somatic_live_plot slogd sns


default: all


CC := g++
include /usr/share/make-common/common.1.mk


# apparently ach requires this, or at least c99
CFLAGS += --std=gnu99 -O0

# don't spit out warnings for all the static functions in headers
CFLAGS += -Wno-unused-function -Wno-conversion -Wno-deprecated-declarations
CPPFLAGS += -Wno-unused-function -Wno-conversion -Wno-deprecated-declarations

LISPDIR := lisp
ETCDIR := ./etc

all: $(LIBFILES) verbatim/share/somatic/somatic.protobin $(BINFILES)

LIB_OBJS := somatic_util.o somatic.pb-c.o  msg.o daemon.o motor.o

$(call LINKLIB, somatic, $(LIB_OBJS), ach protobuf-c)
$(call LINKBIN, somatic_dump, somatic_dump.o $(LIB_OBJS), ach protobuf-c amino stdc++)

$(call LINKBIN, somatic_live_plot, somatic_live_plot.o $(LIB_OBJS), ach protobuf-c amino stdc++)
$(call LINKBIN, somatic_test_source, somatic_test_source.o $(LIB_OBJS), ach protobuf-c amino stdc++)

$(call LINKBIN, somatic_motor_plot, somatic_motor_plot_argp.o somatic_motor_plot.o $(LIB_OBJS), ach protobuf-c amino stdc++)

$(call LINKBIN, somatic_exampled, somatic_exampled.o $(LIB_OBJS), ach protobuf-c amino stdc++)
$(call LINKBIN, slogd, slogd.o $(LIB_OBJS), ach protobuf-c amino)
$(call LINKBIN, sns, sns.o $(LIB_OBJS), ach protobuf-c amino)

src/somatic.pb-c.c: proto/somatic.proto
	cd proto && \
	  protoc-c --c_out=. somatic.proto
	mv proto/somatic.pb-c.c $(SRCDIR)
	mv proto/somatic.pb-c.h $(INCLUDEDIR)


$(INCLUDEDIR)/somatic.pb-c.h: src/somatic.pb-c.c

$(addprefix $(BUILDDIR)/, $(LIB_OBJS)): $(INCLUDEDIR)/somatic.pb-c.h

somatic.pb-c.o: $(INCLUDEDIR)/somatic.pb-c.h

python/somatic_pb2.py: proto/somatic.proto
	protoc --proto_path=proto --python_out=./python proto/somatic.proto

python/somatic_pb2.pyc: python/somatic_pb2.py
	cd ./python && ./pycompile

verbatim/share/somatic/somatic.protobin: proto/somatic.proto
	protoc -o$@ $<

clean:
	rm -rf .deps $(GENHEADERS) debian *.deb \
		$(BUILDDIR)/* $(LISPDIR)/*.fasl \
		$(INCLUDEDIR)/somatic.pb-c* $(SRCDIR)/somatic.pb-c*  \
		doc python/somatic_pb2.*

.PHONY: doc

doc:
	doxygen
