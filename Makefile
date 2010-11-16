## Makefile for somatic

VERSION := 0.0.200101104

PROJECT := somatic

SHAREDLIBS := somatic
BINFILES := somatic_motor_plot somatic_dump


default: all


CC := g++
include /usr/share/make-common/common.1.mk

#MSG_SRC_C := $(wildcard $(SRCDIR)/msg/*.c)
#MSG_SRC_CPP := $(wildcard $(SRCDIR)/msg/*.cpp)
#MSG_OBJS := $(addsuffix .o, $(basename $(MSG_SRC_C))) $(addsuffix .o, $(basename $(MSG_SRC_CPP)))

# apparently ach requires this, or at least c99
CFLAGS += --std=gnu99

# don't spit out warnings for all the static functions in headers
CFLAGS += -Wno-unused-function -Wno-conversion -Wno-deprecated-declarations
CPPFLAGS += -Wno-unused-function -Wno-conversion -Wno-deprecated-declarations

all: $(LIBFILES) verbatim/share/somatic/somatic.protobin $(BINFILES)

LIB_OBJS := somatic_util.o ez.o somatic.pb-c.o msgply.o msg.o #$(MSG_OBJS)

$(call LINKLIB, somatic, $(LIB_OBJS), ach protobuf-c)
$(call LINKBIN, somatic_dump, somatic_dump.o $(LIB_OBJS), ach protobuf-c amino stdc++)


$(call LINKBIN, somatic_motor_plot, somatic_motor_plot_argp.o somatic_motor_plot.o $(LIB_OBJS), ach protobuf-c amino stdc++)

ez.o: somatic.pb-c.c

$(MSG_OBJS): somatic.pb-c.c

somatic.pb-c.c: proto/somatic.proto
	cd proto && \
	  protoc-c --c_out=. somatic.proto
	mv proto/somatic.pb-c.c $(SRCDIR)
	mv proto/somatic.pb-c.h $(INCLUDEDIR)


$(INCLUDEDIR)/somatic.pb-c.h: somatic.pb-c.c

somatic.pb-c.o: $(INCLUDEDIR)/somatic.pb-c.h

python/somatic_pb2.py: proto/somatic.proto
	protoc --proto_path=proto --python_out=./python proto/somatic.proto

python/somatic_pb2.pyc: python/somatic_pb2.py
	cd ./python && ./pycompile

verbatim/share/somatic/somatic.protobin: proto/somatic.proto
	protoc -o$@ $<

clean:
	rm -rf .deps $(GENHEADERS) debian *.deb *.lzma *.so $(INCLUDEDIR)/somatic.pb-c* $(SRCDIR)/somatic.pb-c* $(SRCDIR)/*.o doc $(MSG_OBJS) python/somatic_pb2.*

.PHONY: doc

doc:
	doxygen
