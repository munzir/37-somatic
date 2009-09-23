## Makefile for somatic

VERSION := 0.0.20090823
PROJECT := somatic

#SHAREDLIBS :=
#BINFILES :=

GENHEADERS := include/somatic/motor_msg.h

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
clean:
	rm -rf .deps $(GENHEADERS) debian *.deb *.lzma

doc:
	doxygen
