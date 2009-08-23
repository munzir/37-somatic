## Makefile for js

VERSION := 0.0.20090822
PROJECT := somatic

#SHAREDLIBS := js
#BINFILES := jsstart jsd jscat

GENHEADERS := include/somatic/motor_msg.h

default: $(GENHEADERS)

include /usr/share/make-common/common.1.mk

# apparently ach requires this, or at least c99
CFLAGS += --std=gnu99

include/somatic/motor_msg.h: msg/motor-msg.lisp
	cd include && \
	  sbcl --noinform --noprint --eval "(require 'genmsg)"\
	  --load ../$< --eval "(quit)"



clean:
	rm -rf .deps $(GENHEADERS) debian *.deb *.lzma

doc:
	doxygen
