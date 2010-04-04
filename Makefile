## Makefile for somatic

VERSION := 0.0.200100326
PROJECT := somatic

SHAREDLIBS := somatic
BINFILES :=


default: all


CC := g++
include /usr/share/make-common/common.1.mk

MSG_SRC_C := $(wildcard $(SRCDIR)/msg/*.c)
MSG_SRC_CPP := $(wildcard $(SRCDIR)/msg/*.cpp)
MSG_OBJS := $(addsuffix .o, $(basename $(MSG_SRC_C))) $(addsuffix .o, $(basename $(MSG_SRC_CPP)))

# apparently ach requires this, or at least c99
CFLAGS += --std=gnu99

# don't spit out warnings for all the static functions in headers
CFLAGS += -Wno-unused-function -Wno-conversion
CPPFLAGS += -Wno-unused-function -Wno-conversion

all: $(LIBFILES) verbatim/share/somatic/somatic.protobin $(BINFILES)

$(call LINKLIB, somatic, somatic_util.o ez.o somatic.pb-c.o msgply.o $(MSG_OBJS))

ez.o: somatic.pb-c.c

$(MSG_OBJS): somatic.pb-c.c

somatic.pb-c.c: proto/somatic.proto
	cd proto && \
	  protoc-c --c_out=. somatic.proto
	mv proto/somatic.pb-c.c $(SRCDIR)
	mv proto/somatic.pb-c.h $(INCLUDEDIR)


$(INCLUDEDIR)/somatic.pb-c.h: somatic.pb-c.c

somatic.pb-c.o: $(INCLUDEDIR)/somatic.pb-c.h


verbatim/share/somatic/somatic.protobin: proto/somatic.proto
	protoc -o$@ $<

clean:
	rm -rf .deps $(GENHEADERS) debian *.deb *.lzma *.so $(INCLUDEDIR)/somatic.pb-c* $(SRCDIR)/somatic.pb-c* $(SRCDIR)/*.o doc $(MSG_OBJS)

.PHONY: doc

doc:
	doxygen
