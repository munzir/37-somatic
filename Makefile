## Makefile for somatic

VERSION := 0.0.20091204
PROJECT := somatic

SHAREDLIBS := somatic somatic_pb-c
#BINFILES :=

#GENHEADERS := include/somatic/motor_msg.h include/somatic/somatic.pb-c.h
GENHEADERS := include/somatic/motor_msg.h

default: $(GENHEADERS) all

include /usr/share/make-common/common.1.mk

# apparently ach requires this, or at least c99
CFLAGS += --std=gnu99 -Os

all: $(LIBFILES) verbatim/share/somatic/somatic.protobin

$(call LINKLIB, somatic, somatic_motor.o)
$(call LINKLIB, somatic_pb-c, somatic.pb-c.o)

include/somatic/motor_msg.h: msg/motor-msg.lisp
	cd include && \
	  sbcl --noinform --noprint --eval "(require 'genmsg)"\
	  --load ../$< --eval "(quit)"

somatic.pb-c.c: proto/somatic.proto
	cd proto && \
	  protoc-c --c_out=. somatic.proto
	mv proto/somatic.pb-c.c $(SRCDIR)
	mv proto/somatic.pb-c.h $(INCLUDEDIR)

somatic.pb-c.o: $(INCLUDEDIR)/somatic.pb-c.h

verbatim/share/somatic/somatic.protobin: proto/somatic.proto
	protoc -o$@ $<

clean:
	rm -rf .deps $(GENHEADERS) debian *.deb *.lzma *.so $(INCLUDEDIR)/somatic.pb-c* $(SRCDIR)/somatic.pb-c* $(SRCDIR)/*.o

doc:
	doxygen
