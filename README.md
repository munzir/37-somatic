# 37-somatic

Somatic is a package that integrates ach (byte-oriented IPC) and protocol
buffers (marshalling) with specific application to robotics.  It also include a
number of useful utility routines.

## Dependencies

- protobuf-c-compiler

      sudo apt install protobuf-c-compiler

- libprotobuf-c-dev

      sudo apt install libprotobuf-c-dev

- [38-amino (krang branch)](https://github.gatech.edu/WholeBodyControlAttempt1/38-amino/tree/krang) - Follow installation instructions in the git readme.
- [39-ach](https://github.gatech.edu/WholeBodyControlAttempt1/39-ach) - Follow installation instructions in the git readme.

## Installation

    git clone https://github.gatech.edu/WholeBodyControlAttempt1/37-somatic
    cd 37-somatic
    sudo rm -r build
    mkdir build
    cd build
    cmake ..
    make
    sudo make install

## Old Readme

README FOR SOMATIC

----------------------------------------------------------------------------
somatic is a package that integrates ach (byte-oriented IPC) and protocol
buffers (marshalling) with specific application to robotics.  It also include a
number of useful utility routines.

The init script that should be symbolically linked to /etc/init.d folder is
under etc folder.

The proto folder contains the descriptions of structures in protobuf-c.pb.h 
folder. At some point, I think, it was me and Munzir, we did not know
how to use this and we started hardcoding the descriptions in the .h file
directly. 

[Can Erdogan - 6/18/2013]
----------------------------------------------------------------------------
= INSTALLATION =

Depends on some annoying stuff, like ntcan and imud, but as of 7/10/13 these
can be found on thebrain (ssh://thebrain.golems.org/home/git/<dir>/<repo>)
under 3rdparty and drivers respectively.  The ntcan library should just be
copied into /usr/local directly.  

[Jon Scholz - 7/8/2013]
----------------------------------------------------------------------------

= ADDING MESSAGE TYPES =

The basic process for adding a message type is as follows:
    1) Define a message in proto/somatic.proto
    2) Declare a pair of somatic_*_alloc and somatic_*_free in include/somatic/msg.h
       where * is your message name
    3) Define somatic_*_alloc and somatic_*_free in src/msg.cpp, following the 
       examples there

Note:
Can switched us from autotools to cmake, and somewhere in there we 
might have lost a step that automatically calls protoc on proto/somatic.proto
to generate the c and h files and copies them to the correct spots.  That means
you have to do it (header in include/ and .c in src/, and change to cpp.  Oh and
add the extern "C" guard below to the header if you're feeling nice)

#ifdef __cplusplus
extern "C" {
#endif //__cplusplus
...
#ifdef __cplusplus
}
#endif //__cplusplus

[Jon Scholz - 7/8/2013]