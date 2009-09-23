## Makefile for somatic

VERSION := 0.0.20090921
PROJECT := somatic

#SHAREDLIBS :=
#BINFILES :=

GENHEADERS := include/somatic/motor_msg.h include/somatic/somatic.pb-c.h

default: $(GENHEADERS)

include /usr/share/make-common/common.1.mk

# apparently ach requires this, or at least c99
CFLAGS += --std=gnu99

proto/somatic.protobin: proto/somatic.proto
	protoc -o$@ $<

include/somatic/motor_msg.h: msg/motor-msg.lisp
	cd include && \
	  sbcl --noinform --noprint --eval "(require 'genmsg)"\
	  --load ../$< --eval "(quit)"

include/somatic/somatic.pb-c.h: proto/somatic.proto
	cd proto && \
	  protoc-c --c_out=. somatic.proto
	sed -e 's/#include .*//' -i proto/somatic.pb-c.c
	cat proto/somatic.pb-c.h proto/somatic.pb-c.c >include/somatic/somatic.pb-c.h
	rm -f proto/somatic.pb-c.*

clean:
	rm -rf .deps $(GENHEADERS) debian *.deb *.lzma

doc:
	doxygen
