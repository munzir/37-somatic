#!/bin/bash
# call this script to update the protoc-generated 
# files after adding messages
# jon scholz 7/8/13

echo "compiling somatic.proto"
protoc-c --c_out=. proto/somatic.proto

mv -v proto/somatic.pb-c.c src/somatic.pb-c.cpp
mv -v proto/somatic.pb-c.h include/somatic.pb-c.h
echo "done."
echo "don't forget to remove \"proto/\" from include in somatic.pb-c.cpp"

# should add include guard here
# have to manually remove "proto/" from include in somatic.pb-c.cpp
