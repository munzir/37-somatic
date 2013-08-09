#!/bin/bash
# call this script to update the protoc-generated 
# files after adding messages
# jon scholz 7/8/13

echo "compiling somatic.proto"
protoc-c --c_out=. proto/somatic.proto

mv -v proto/somatic.pb-c.c src/somatic.pb-c.c
mv -v proto/somatic.pb-c.h include/somatic.pb-c.h
sed -i -e "s/#include \"proto\\/somatic.pb-c.h\"/#include \"somatic.pb-c.h\"/" src/somatic.pb-c.c
echo "done."

# should add include guard here
# have to manually remove "proto/" from include in somatic.pb-c.cpp
