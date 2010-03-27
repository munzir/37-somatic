## Makefile for somatic

VERSION := 0.0.200100213
PROJECT := somatic

SHAREDLIBS := somatic somatic_pb-c
BINFILES :=

#GENHEADERS := include/somatic/motor_msg.h include/somatic/somatic.pb-c.h
GENHEADERS := include/somatic/motor_msg.h

default: $(GENHEADERS) all

CC := g++
include /usr/share/make-common/common.1.mk

# apparently ach requires this, or at least c99
CFLAGS += --std=gnu99

# don't spit out warnings for all the static functions in headers
CFLAGS += -Wno-unused-function -Wno-conversion
CPPFLAGS += -Wno-unused-function -Wno-conversion

all: $(LIBFILES) verbatim/share/somatic/somatic.protobin $(BINFILES)

$(call LINKLIB, somatic, somatic_util.o ez.o)
$(call LINKLIB, somatic_pb-c, somatic.pb-c.o msgply.o)

#include/somatic/motor_msg.h: msg/motor-msg.lisp
#cd include && \
#sbcl --noinform --noprint --eval "(require 'genmsg)"\
#--load ../$< --eval "(quit)"

ez.o: somatic.pb-c.c

somatic.pb-c.c: proto/somatic.proto
	cd proto && \
	  protoc-c --c_out=. somatic.proto
	mv proto/somatic.pb-c.c $(SRCDIR)
	mv proto/somatic.pb-c.h $(INCLUDEDIR)

somatic.pb-c.o: $(INCLUDEDIR)/somatic.pb-c.h


verbatim/share/somatic/somatic.protobin: proto/somatic.proto
	protoc -o$@ $<

clean:
	rm -rf .deps $(GENHEADERS) debian *.deb *.lzma *.so $(INCLUDEDIR)/somatic.pb-c* $(SRCDIR)/somatic.pb-c* $(SRCDIR)/*.o doc

.PHONY: doc

doc:
	doxygen
